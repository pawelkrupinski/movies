package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{AgreementStage, VoterFamily}

import scala.collection.mutable
import scala.util.Random

/** The one dependency index (docs/design/identity-no-drift.md, "One dependency index"), as a property over random
 *  receipts and random changes of EVERY kind: a dependency change puts every entry that read it into the next pass — a
 *  filed one through the dirty index ([[Memo.clean]]), one no filing announces (the year, an attempt) through the push
 *  index ([[Memo.moved]]) — and a pushed change reaches no other entry: none that never read it, none that read it as it
 *  reads now. */
class DependencyIndexSpec extends AnyFlatSpec with Matchers {
  private val universe: IndexedSeq[Dependency] = IndexedSeq(
    Dependency.Answer(Question.Family(VoterFamily.Imdb, "title|Lalka")),
    Dependency.Answer(Question.Family(VoterFamily.Wiki, "record|Q1")),
    Dependency.Attempted(Question.Record(913760)),
    Dependency.Attempted(Question.Find("tt1")),
    Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.FilmRecord(913760)),
    Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Cast, Question.FilmRecord(7)),
    Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.Candidate(CandidateQuery.Title("lalka"))),
    Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Answer,
      Question.VenuePage(services.movies.ListingKey.Published("Kino Muza", "Lalka", Some(2025), Nil))),
    Dependency.Year)

  private final case class Entry(reads: Map[Dependency, Long])

  /** One random history: entries recorded over the inputs, then inputs moved (or filed again unchanged) one at a time,
   *  each pass deciding again what the index does not call clean, as the stage does. */
  private final class History(seed: Long) {
    val random   = new Random(seed)
    val inputs   = mutable.Map.from(universe.map(_ -> random.nextInt(3).toLong))
    val filings  = mutable.ArrayBuffer.empty[(Long, String)]
    var version  = 0L
    val memo     = new Memo[String, Entry](_.reads, _.readsTmdb, AgreementStage.pushed)
    val filedSince: Long => Option[Set[String]] = since => Some(filings.iterator.collect { case (at, id) if at > since => id }.toSet)
    def record(key: String): Unit = {
      val reads = universe.filter(_ => random.nextInt(3) == 0)
      memo.put(key, Entry(reads.map(read => read -> inputs(read)).toMap))
      memo.check(key, 0L, version)
    }
    (0 until 30).foreach(i => record(s"e$i"))
    def stands(key: String): Boolean = memo.get(key).exists(memo.stands(_, inputs.get))

    /** One input changed — or filed again as it was — delivered as the worker delivers its kind. Fails with what the
     *  index got wrong. */
    def step(): Option[String] = {
      val read  = universe(random.nextInt(universe.size))
      val same  = random.nextBoolean()
      if (!same) inputs(read) = inputs(read) + 1 + random.nextInt(2)
      val readers = memo.keys.filter(key => memo.get(key).exists(_.reads.contains(read))).toSet
      val movedFor = readers.filter(key => !stands(key))
      val wasChecked = memo.keys.filter(memo.isChecked).toSet
      val pushedWrong = if (AgreementStage.pushed(read)) {
        val reached   = memo.moved(_ == read, inputs.get)
        val unchecked = wasChecked -- memo.keys.filter(memo.isChecked)
        if (reached != movedFor.size) Some(s"$read moved for ${movedFor.size} readers, the push index reached $reached")
        else Option.when(!unchecked.subsetOf(movedFor))(s"$read unchecked ${unchecked -- movedFor}, whose read of it did not move")
      } else { version += 1; filings += version -> read.filing; None }
      if (pushedWrong.isDefined) return pushedWrong
      memo.newPass()
      // the next pass: what the index calls clean stands as it is; the rest is read again, and decided again where moved
      val wrong = memo.keys.toSeq.sorted.flatMap { key =>
        val clean = memo.clean(key, 0L, version, filedSince)
        if (!clean) { if (stands(key)) memo.check(key, 0L, version) else record(key) }
        Option.when(clean && !stands(key))(s"$key read $read, which moved, yet the index calls it clean")
      }.headOption
      if (random.nextInt(8) == 0) record(s"e${random.nextInt(30)}")
      wrong
    }
  }

  "the dependency index" should "put every entry that read a dependency into the next pass when it moves, and a pushed change no other" in {
    val failures = (1L to 200L).flatMap { seed =>
      val history = new History(seed)
      Iterator.continually(history.step()).take(60).collectFirst { case Some(why) => s"seed $seed: $why" }
    }
    failures shouldBe empty
  }

  "a pushed change" should "visit only the entries whose receipt names it, and leave every other checked" in {
    val memo  = new Memo[String, Entry](_.reads, _.readsTmdb, AgreementStage.pushed)
    val tried = Dependency.Attempted(Question.Record(913760))
    memo.put("moved", Entry(Map(tried -> 1L, Dependency.Year -> 5L))); memo.check("moved", 0L, 0L)
    memo.put("same", Entry(Map(tried -> 2L))); memo.check("same", 0L, 0L)
    memo.put("none", Entry(Map(universe.head -> 1L))); memo.check("none", 0L, 0L)
    memo.readersOf(tried) shouldBe Set("moved", "same")
    memo.moved(AgreementStage.isAttempt, Map[Dependency, Long](tried -> 2L).get) shouldBe 1
    Seq("moved", "same", "none").map(memo.isChecked) shouldBe Seq(false, true, true)
    // the year turning: a correction's receipt names no year, and is not read again for it
    memo.check("moved", 0L, 0L)
    memo.moved(_ == Dependency.Year, _ => Some(6L)) shouldBe 1
    Seq("moved", "same", "none").map(memo.isChecked) shouldBe Seq(false, true, true)
    memo.remove("moved")
    memo.readersOf(tried) shouldBe Set("same")
    memo.readersOf(Dependency.Year) shouldBe empty
  }
}
