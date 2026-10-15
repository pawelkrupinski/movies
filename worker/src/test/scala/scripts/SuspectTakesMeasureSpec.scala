package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures
import services.identity.SuspectTakes.Signal
import scripts.SuspectTakesMeasure.Graded
import tools.UnmatchedClusters.Label

/** How the suspect-take audit reads a label's film, and what it reports of the graded pairs. */
class SuspectTakesMeasureSpec extends AnyFlatSpec with Matchers {

  private val lalka  = IdentityMeasures.Film("Lalka", year = Some(2025), imdbNumber = 64570)
  private val films  = Map(913760 -> lalka, 7 -> IdentityMeasures.Film("Michael"))

  private def graded(right: Boolean, signals: Set[Signal], note: String = "judged take: page", title: String = "Lalka") =
    Graded("pl", Label("pl", "*", title, "tmdb:913760", right, note), signals, signals.toSeq.map(_.label), today = true)

  "A label's film" should "name its record by TMDB id, or by the IMDb number a record carries, under the TMDB key" in {
    SuspectTakesMeasure.recordOf("tmdb:913760", films) shouldBe Some(("tmdb:913760", Some(913760), lalka))
    SuspectTakesMeasure.recordOf("imdb:tt0064570", films) shouldBe Some(("tmdb:913760", Some(913760), lalka))
    SuspectTakesMeasure.recordOf("imdb:tt0000000", films) shouldBe None   // a record naming no IMDb id is no match for one
    SuspectTakesMeasure.recordOf("tmdb:1", films) shouldBe None
    SuspectTakesMeasure.recordOf("tmdb:x", films) shouldBe None
    SuspectTakesMeasure.recordOf("lalka", films) shouldBe None
  }

  "The signal table" should "give each signal's recall on the wrong pairs and flag rate on the right ones, and ANY's" in {
    val table = SuspectTakesMeasure.signalTable(Seq(graded(right = false, Set(Signal.Era)), graded(right = false, Set.empty),
      graded(right = true, Set(Signal.Era, Signal.Runtime)), graded(right = true, Set.empty)))
    table should contain ("| era | 1/2 | 50.0% | 1/2 | 50.0% |")
    table should contain ("| runtime | 0/2 | 0.0% | 1/2 | 50.0% |")
    table should contain ("| ANY | 1/2 | 50.0% | 1/2 | 50.0% |")
    SuspectTakesMeasure.signalTable(Nil) should contain ("| ANY | 0/0 | - | 0/0 | - |")
  }

  "A wrong label's provenance" should "tell a judged take from a must-not pair, else name the note's first words" in {
    SuspectTakesMeasure.provenance("judged off the capture: Lalka is a 1968 film") shouldBe "judged take"
    SuspectTakesMeasure.provenance("must not link: a 1968 film; review") shouldBe "must-not pair"
    SuspectTakesMeasure.provenance("review page 2026-10-01") shouldBe "review page"
  }

  "--missed" should "list the wrong labels no signal suspects, and beside it the right ones a signal does" in {
    val (missed, flaggedRight) = SuspectTakesMeasure.missedLines(Seq(graded(right = false, Set.empty, title = "Missed"),
      graded(right = false, Set(Signal.Era)), graded(right = true, Set(Signal.Runtime), title = "Flagged"), graded(right = true, Set.empty)))
    missed shouldBe Seq("pl\t*\tMissed\ttmdb:913760\tjudged take: page")
    flaggedRight shouldBe Seq("pl\tFlagged\ttmdb:913760\truntime")
  }
}
