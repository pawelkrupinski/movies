package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The clue a whole-corpus failure prints. Worth its own spec because its only job
 * is to be READ by someone diagnosing a divergence they cannot reproduce — a diff
 * that hides the difference costs a whole investigation, which is exactly what the
 * previous prefix-based rendering did.
 */
class CorpusDiffSpec extends AnyFlatSpec with Matchers {

  // The real shape: two rendered rows identical for hundreds of characters, then
  // differing in a poster URL. A prefix-based diff printed two identical-looking
  // windows and said they differed.
  private val shared = "FilmSchedule(Movie(Ghost in the Shell,Some(83),Some(1995)," + ("x" * 600)

  "the rendered-row diff" should "show the difference, not a common prefix that hides it" in {
    val a = shared + "poster=foto,20677,117444d5.jpg)"
    val b = shared + "poster=foto,20729,46b3c104.jpg)"

    val diff = CorpusDiff.rows(Seq(a), Seq(b), "pass0", "pass2")

    diff should include ("20677")
    diff should include ("20729")
  }

  it should "name the row index and where the two partway" in {
    val diff = CorpusDiff.rows(Seq("same", shared + "A"), Seq("same", shared + "B"), "pass0", "pass2")

    diff should include ("row 1 differs")
    diff should include (s"common prefix: ${shared.length} chars")
  }

  it should "report a size difference rather than pretending the rows line up" in {
    CorpusDiff.rows(Seq("a", "b"), Seq("a"), "pass0", "pass1") should include ("sizes differ: pass0=2 pass1=1")
  }

  // Equal-length, pairwise-equal, yet unequal as sequences is impossible — but an
  // ORDER-only difference between two multisets is not, and a diff that returned
  // an empty string there would read as "nothing differs".
  it should "say so plainly when no row differs pairwise" in {
    CorpusDiff.rows(Seq("a"), Seq("a"), "pass0", "pass1") should include ("no row differs pairwise")
  }

  it should "stay silent about a match on identical corpora" in {
    CorpusDiff.records(Seq.empty, Seq.empty) shouldBe ""
  }
}
