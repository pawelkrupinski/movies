package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The band the convergence legs are judged against. Worth its own spec because the
 * comparison decides whether a leg passes, and it has two properties that are easy
 * to get wrong and invisible when they are: it compares SHARES rather than counts,
 * and it treats a zero baseline strictly.
 */
class ProdCoverageBaselineSpec extends AnyFlatSpec with Matchers {

  private def coverage(films: Int, tmdb: Int = 0, imdb: Int = 0, imdbRating: Int = 0,
                       filmweb: Int = 0, metascore: Int = 0, rt: Int = 0) =
    ProdCoverageBaseline(java.time.Instant.EPOCH, films, tmdb, imdb, imdbRating, filmweb, metascore, rt)

  private val Band = 0.05

  "divergences" should "pass a run whose coverage matches production" in {
    val prod = coverage(films = 727, tmdb = 526, imdb = 515, imdbRating = 478, filmweb = 482, metascore = 308, rt = 354)
    val run  = coverage(films = 741, tmdb = 529, imdb = 518, imdbRating = 489, filmweb = 478, metascore = 307, rt = 359)

    ProdCoverageBaseline.divergences(run, prod, Band) shouldBe empty
  }

  // The corpus is a snapshot and prod is live, so the two never hold the same number
  // of films. Comparing raw counts would fail on that alone and say nothing about
  // enrichment — a run with 10% more films but the SAME resolution rate is not a
  // regression.
  it should "compare enrichment as a share of each side's own films, not as counts" in {
    val prod = coverage(films = 100, tmdb = 70)
    val run  = coverage(films = 110, tmdb = 77)      // +10% films, +10% count, identical 70% rate

    ProdCoverageBaseline.divergences(run, prod, Band).filter(_.contains("tmdbId")) shouldBe empty
  }

  it should "flag an axis that resolved a smaller share than production" in {
    val prod = coverage(films = 100, tmdb = 70)
    val run  = coverage(films = 100, tmdb = 60)      // 60% vs 70% — off by 14%

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged should have size 1
    flagged.head should include ("tmdbId")
  }

  /**
   * Upward drift is a real failure, not an improvement to be waved through. The
   * harness's rating sweep drove Filmweb for every country while production gates it
   * to Poland: the German leg reported 972 Filmweb ratings against prod's 0. No share
   * is within 5% of nothing, so a zero baseline admits only zero.
   */
  it should "flag a source production does not run for this country at all" in {
    val prod = coverage(films = 1171, filmweb = 0)
    val run  = coverage(films = 1220, filmweb = 972)

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged.mkString should include ("filmwebRating")
  }

  it should "accept a zero baseline when the run also has none" in {
    ProdCoverageBaseline.divergences(coverage(films = 1220, filmweb = 0), coverage(films = 1171, filmweb = 0), Band) shouldBe empty
  }

  // The one axis with no share to take — it IS the denominator, so it is compared
  // as a count and a corpus that loses a fifth of the repertoire must fail.
  it should "flag a run that lost a large share of the films entirely" in {
    val flagged = ProdCoverageBaseline.divergences(coverage(films = 741), coverage(films = 931), Band)
    flagged.mkString should include ("films")
  }

  it should "survive a round-trip through the fixture file" in {
    val baseline = coverage(films = 727, tmdb = 526, imdb = 515, imdbRating = 478, filmweb = 482, metascore = 308, rt = 354)
    val code     = s"roundtrip-${ProcessHandle.current().pid()}"
    try {
      ProdCoverageBaseline.write(code, baseline)
      ProdCoverageBaseline.read(code) shouldBe Some(baseline)
    } finally java.nio.file.Files.deleteIfExists(ProdCoverageBaseline.pathFor(code))
  }
}
