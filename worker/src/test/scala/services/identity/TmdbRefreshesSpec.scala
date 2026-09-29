package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}
import scala.util.Success

/** The questions TMDB is asked again because their answer has aged: a day for what the model has not
 *  settled, a week for the rest — oldest first — and a re-fetch that changes nothing re-dates the
 *  answer without waking anyone. */
class TmdbRefreshesSpec extends AnyFlatSpec with Matchers {

  private val language = "pl-PL"
  private def search(query: String) =
    s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=${java.net.URLEncoder.encode(query, "UTF-8")}"
  private val body = """{"results":[{"id":1,"title":"Lalka","original_title":"Lalka","release_date":"1968-01-01","popularity":3.5}]}"""

  private final class World {
    val clock      = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val docs       = new InMemoryTmdbDocuments
    val store      = new TmdbStore(docs, clock)
    val normalizer = new TmdbNormalizer(store)
    val refreshes  = new TmdbRefreshes(store, language, clock)
    var woken      = 0
    store.onChanged(_ => woken += 1)
    def asked(query: String): Unit = normalizer.filed("GET", search(query), Success(body))
    def later(days: Double): Unit = clock.advance(Duration.ofMinutes((days * 24 * 60).toLong))
  }

  private val (lalka, rosa, hamlet) = (CandidateQuery.Title("Lalka"), CandidateQuery.Title("Róża"), CandidateQuery.Title("Hamlet"))

  "the refreshes" should "ask an unsettled family's questions again after a day, a settled one's after a week, oldest first" in {
    val w = new World
    w.asked("Hamlet"); w.later(0.5); w.asked("Lalka"); w.asked("Róża")
    w.later(2)
    val families = Seq(Set[CandidateQuery](lalka) -> false, Set[CandidateQuery](rosa, hamlet) -> true)
    w.refreshes.due(families) shouldBe Seq(lalka)                         // settled ones are not a week old yet
    w.later(6)
    w.refreshes.due(families) shouldBe Seq(hamlet, lalka, rosa)          // Hamlet was asked first
  }

  it should "count a question any unsettled family asks as unsettled, and leave a question never answered to the fill" in {
    val w = new World
    w.asked("Lalka"); w.later(2)
    w.refreshes.due(Seq(Set[CandidateQuery](lalka) -> true, Set[CandidateQuery](lalka) -> false)) shouldBe Seq(lalka)
    w.refreshes.due(Seq(Set[CandidateQuery](CandidateQuery.Title("Nigdy")) -> false)) shouldBe empty
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
    w.refreshes.due(Seq(Set[CandidateQuery](lalka) -> false)) shouldBe empty                           // re-dated: not due
  }
}
