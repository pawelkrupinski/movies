package services.identity
package adapters

import org.bson.{BsonArray, BsonDocument, BsonInt64}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}
import scala.concurrent.duration._
import scala.util.Success

/** What the model asks TMDB again because it has aged: its questions — a day for what it has not settled, a week for the
 *  rest, oldest first, and a re-fetch that changes nothing re-dates the answer without waking anyone — then the records
 *  and people it reads that TMDB last gave over a month ago, oldest first, a round's share at a time, and nothing it no
 *  longer reads. */
class QuestionRenewalsSpec extends AnyFlatSpec with Matchers {

  private val language = "pl-PL"
  private def search(query: String) =
    s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=${java.net.URLEncoder.encode(query, "UTF-8")}"
  private val body = """{"results":[{"id":1,"title":"Lalka","original_title":"Lalka","release_date":"1968-01-01","popularity":3.5}]}"""

  private final class World(records: InMemoryTmdbDocuments = new InMemoryTmdbDocuments, read: Option[Set[String]] = None,
                            perRound: Int = QuestionRenewals.PerRound, start: Instant = Instant.parse("2026-09-28T12:00:00Z")) {
    val clock      = new MutableClock(start)
    val docs       = new InMemoryTmdbDocuments
    val store      = new TmdbStore(docs, clock)
    val normalizer = new TmdbNormalizer(store)
    var families   = Seq.empty[(Set[CandidateQuery], Boolean)]
    val renewals   = new QuestionRenewals(store, records, language, () => families, () => read, clock, perRound)
    var woken      = 0
    store.onChanged(_ => woken += 1)
    def asked(query: String): Unit = normalizer.filed("GET", search(query), Success(body))
    def later(days: Double): Unit = clock.advance(Duration.ofMinutes((days * 24 * 60).toLong))
    def due(of: (Set[CandidateQuery], Boolean)*): Seq[Question] = { families = of; renewals.due() }
  }

  private val (lalka, rosa, hamlet) = (CandidateQuery.Title("Lalka"), CandidateQuery.Title("Róża"), CandidateQuery.Title("Hamlet"))
  private def candidates(qs: CandidateQuery*) = qs.map(Question.Candidate(_))

  "the renewals" should "ask an unsettled family's questions again after a day, a settled one's after a week, oldest first" in {
    val w = new World
    w.asked("Hamlet"); w.later(0.5); w.asked("Lalka"); w.asked("Róża")
    w.later(2)
    val families = Seq(Set[CandidateQuery](lalka) -> false, Set[CandidateQuery](rosa, hamlet) -> true)
    w.due(families*) shouldBe candidates(lalka)                           // settled ones are not a week old yet
    w.later(6)
    w.due(families*) shouldBe candidates(hamlet, lalka, rosa)             // Hamlet was asked first
  }

  it should "count a question any unsettled family asks as unsettled, and leave a question never answered to the fill" in {
    val w = new World
    w.asked("Lalka"); w.later(2)
    w.due(Set[CandidateQuery](lalka) -> true, Set[CandidateQuery](lalka) -> false) shouldBe candidates(lalka)
    w.due(Set[CandidateQuery](CandidateQuery.Title("Nigdy")) -> false) shouldBe empty
  }

  "a re-fetch that changes nothing" should "re-date the answer at most daily, and wake no one" in {
    val w = new World
    w.asked("Lalka"); w.woken = 0
    val first = w.docs.get(TmdbKind.Query, Seq(TmdbStore.questionId(language, lalka))).values.head
    w.later(0.5); w.asked("Lalka")
    w.docs.get(TmdbKind.Query, Seq(TmdbStore.questionId(language, lalka))).values.head shouldBe first   // within the day: no write
    w.later(1); w.asked("Lalka")
    val redated = w.docs.get(TmdbKind.Query, Seq(TmdbStore.questionId(language, lalka))).values.head
    TmdbStore.fetchedAt(redated).get should be > TmdbStore.fetchedAt(first).get
    w.woken shouldBe 0
    w.due(Set[CandidateQuery](lalka) -> false) shouldBe empty                                         // re-dated: not due
  }

  private val start = Instant.parse("2026-10-24T00:00:00Z")
  private def daysAgo(days: Int) = start.toEpochMilli - days.days.toMillis
  private def doc(fetched: Long) = new BsonDocument("ids", new BsonArray()).append(TmdbStore.FetchedAt, BsonInt64(fetched))
  private def records() = {
    val d = new InMemoryTmdbDocuments
    d.put(TmdbKind.Film, Seq("1" -> doc(daysAgo(40)), "2" -> doc(daysAgo(90)), "3" -> doc(daysAgo(3)), "4" -> doc(daysAgo(90))))
    d.put(TmdbKind.Person, Seq("7" -> doc(daysAgo(60))))
    d
  }
  private val read = Set("1", "2", "3").map(TmdbStore.keyOf(TmdbKind.Film, _)) + TmdbStore.keyOf(TmdbKind.Person, "7")

  "the records and people the model reads" should "be asked again past a month, after the questions, oldest first, a round's share at a time" in {
    val w = new World(records(), Some(read), perRound = 2, start = start)
    w.asked("Lalka"); w.later(2)
    w.due(Set[CandidateQuery](lalka) -> false) shouldBe candidates(lalka) ++ Seq(Question.FilmRecord(2), Question.Person(7))
    w.due() shouldBe Seq(Question.FilmRecord(1))
    w.due() shouldBe empty
  }

  it should "be scanned again only past the scan interval" in {
    val w = new World(records(), Some(read), start = start)
    w.due() should have size 3
    w.due() shouldBe empty
    w.clock.advanceMillis(QuestionRenewals.ScanEvery.toMillis)
    // still aged, still named: a fetch that failed is asked again
    w.due() should have size 3
  }

  it should "be none while no model is taken up" in {
    new World(records(), None, start = start).due() shouldBe empty
  }
}
