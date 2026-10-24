package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{AgreementStage, SourceHit, VoterFamily}
import tools.MutableClock

import java.time.Instant
import scala.collection.mutable

/** One feed for every store's filings: the agreement's version and changes, the model's TMDB keys and venue page keys
 *  come out of it as each source announced them on its own before — side by side, filing by filing. */
class AnswerFeedSpec extends AnyFlatSpec with Matchers {

  /** The family store's own change log as it was before the feed (FamilyAnswerStore's `noteFiled`/`changedSince`), fed
   *  the filings that store made: what the agreement's view must answer exactly. */
  private final class FamilyLogBefore(kept: Int) {
    private val filed   = new java.util.concurrent.atomic.AtomicLong()
    private val changes = new java.util.concurrent.ConcurrentSkipListMap[Long, String]()
    def version: Long = filed.get
    def noteFiled(id: String): Unit = { changes.put(filed.incrementAndGet(), id); while (changes.size > kept) changes.pollFirstEntry(); () }
    def changedSince(version: Long): Option[Set[String]] = {
      import scala.jdk.CollectionConverters._
      Option.when(changes.isEmpty || version >= changes.firstKey - 1 || version >= filed.get)(changes.tailMap(version, false).values.asScala.toSet)
    }
  }

  "the agreement's view, the TMDB store's listeners and the venue pages' subscribers" should
    "see every filing as the stores' own announcements did, whatever the feed carries beside them" in {
    val kept     = 7   // small, so the oldest filings are dropped many times over
    val clock    = new MutableClock(Instant.parse("2026-10-24T00:00:00Z"))
    val docs     = new InMemoryTmdbDocuments
    val feed     = new FiledAnswerFeed(kept)
    val tmdb     = new TmdbStore(docs, clock, feed)
    val families = new FamilyAnswerStore(docs, clock, feed)
    val before   = new FamilyLogBefore(kept)

    val tmdbHeard, venueHeard = mutable.ArrayBuffer.empty[String]
    val tmdbExpected, venueExpected = mutable.ArrayBuffer.empty[String]
    tmdb.onChanged(tmdbHeard += _)
    feed.subscribe(key => if (VenuePageIndex.isKey(key)) venueHeard += key)
    val tmdbValues = mutable.Map.empty[String, Seq[Int]]

    val random = new scala.util.Random(20261024L)
    val titles = Seq("Snow Leopard", "Es", "Kuzma", "Renoir")
    (1 to 400).foreach { step =>
      random.nextInt(6) match {
        case 0 => val (family, title) = (VoterFamily.values(random.nextInt(VoterFamily.values.length)), titles(random.nextInt(titles.size)))
                  families.fileTitled(family, title, Seq(SourceHit(s"id$step", title, None, None)))
                  before.noteFiled(FamilyAnswerStore.titleId(family, title))
        case 1 => val id = s"tt${random.nextInt(5)}"
                  families.fileRecord(VoterFamily.Imdb, id, None)
                  before.noteFiled(FamilyAnswerStore.recordId(VoterFamily.Imdb, id))
        case 2 => val film = random.nextInt(5)   // a TMDB record read again, noted by the asker
                  families.noteFiled(AgreementStage.recordReadId(film))
                  before.noteFiled(AgreementStage.recordReadId(film))
        case 3 => val (imdbId, ids) = (s"tt${random.nextInt(4)}", Seq(random.nextInt(3) + 1))   // moves only when its films do
                  tmdb.question(TmdbStore.findId(imdbId), ids.map(id => Hit(id, s"film $id", None, Some(2000), 1.0)))
                  if (!tmdbValues.get(imdbId).contains(ids)) { tmdbValues(imdbId) = ids; tmdbExpected += TmdbStore.keyOf(TmdbKind.Query, TmdbStore.findId(imdbId)) }
        case 4 => val key = VenuePageIndex.keyOf("kinoteka", s"https://kinoteka.pl/film/${random.nextInt(4)}|$step")
                  feed.publish(key)   // what the worker's VenuePageIndex `changed` does at a settle
                  venueExpected += key
        case _ => tmdb.person(random.nextInt(3) + 100, Nil, Nil)   // a person's credits: filed once, then unmoved
      }
      withClue(s"after filing $step: ") {
        families.version shouldBe before.version
        (0L to before.version).foreach(v => families.changedSince(v) shouldBe before.changedSince(v))
      }
    }
    tmdbHeard.forall(TmdbStore.isKey) shouldBe true   // the family answers and venue pages on the feed reach no TMDB listener
    tmdbHeard.filter(_.startsWith(s"${TmdbKind.Query.collection}:")) shouldBe tmdbExpected
    tmdbHeard.count(_.startsWith(s"${TmdbKind.Person.collection}:")) shouldBe 3
    venueHeard shouldBe venueExpected
    before.version should be > kept.toLong   // the drop of the oldest was met
  }

  "the agreement's view" should "count a TMDB find or record the stage waits on, as the question it waits on — and no other" in {
    val clock    = new MutableClock(Instant.parse("2026-10-24T00:00:00Z"))
    val docs     = new InMemoryTmdbDocuments
    val feed     = new FiledAnswerFeed
    val tmdb     = new TmdbStore(docs, clock, feed)
    val families = new FamilyAnswerStore(docs, clock, feed)
    val awaited  = mutable.Set[Question](Question.Find("tt2"), Question.Record(7))
    families.awaitedBy(awaited.contains)
    def film(id: Int, year: Int): Unit =
      tmdb.filmPartial(id, TmdbStore.Partial.Local, play.api.libs.json.Json.obj("id" -> id, "release_date" -> s"$year-01-01"))
    // a take-up's filings: finds and records none waits on move nothing
    (1 to 50).foreach { id =>
      tmdb.question(TmdbStore.findId(s"tt1$id"), Seq(Hit(id + 200, s"film $id", None, Some(2000), 1.0)))
      film(id + 100, 2000)
    }
    families.version shouldBe 0
    // the find and the record it waits on, under the ids its questions are asked by
    tmdb.question(TmdbStore.findId("tt2"), Nil)
    film(7, 2026)
    families.version shouldBe 2
    families.changedSince(0) shouldBe Some(Set(Question.Find("tt2").id.get, AgreementStage.recordReadId(7)))
    // filed again unmoved, nothing; once no longer waited on, a move counts no more
    tmdb.question(TmdbStore.findId("tt2"), Nil)
    awaited.clear()
    film(7, 2025)
    families.version shouldBe 2
  }

  "a view" should "count only the keys it accepts, each view on its own" in {
    val feed   = new FiledAnswerFeed
    val agreed = feed.view(services.identity.agreement.AnswerFeed.agreementKeys)
    val all    = feed.view(Some(_))
    Seq(TmdbStore.keyOf(TmdbKind.Query, TmdbStore.findId("tt1")), VenuePageIndex.keyOf("g", "p"), "imdb|title|Es", "find|tt1")
      .foreach(feed.publish)
    agreed.version shouldBe 2
    agreed.changedSince(0) shouldBe Some(Set("imdb|title|Es", "find|tt1"))
    all.version shouldBe 4
  }
}
