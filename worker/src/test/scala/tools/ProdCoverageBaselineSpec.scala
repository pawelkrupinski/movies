package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers

/**
 * The band the convergence legs are judged against. Worth its own spec because the
 * comparison decides whether a leg passes, and it has two properties that are easy
 * to get wrong and invisible when they are: it compares SHARES rather than counts,
 * and it treats a zero baseline strictly.
 */
class ProdCoverageBaselineSpec extends AnyFlatSpec with Matchers with OptionValues {

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

  /** IDENTIFICATION is scored against a wider band than the rating axes, because the
   *  two sides are not measuring the same quantity.
   *
   *  Every rating axis is a share of the films THIS run identified, so both sides are
   *  normalised to their own denominator and a like-for-like 5% holds. `tmdbId` is not:
   *  it is a share of the corpus, and production's numerator is an id INVENTORY built
   *  up over months, while the replay re-derives every id from one snapshot under the
   *  current, deliberately strict rules. Measured 2026-08-23 on the films production
   *  resolved and the replay did not: 17 of 17 were refused BY DESIGN — a title with
   *  several same-title TMDB entries and no year to separate them (six films are
   *  literally called "Zawodowcy"), or a query TMDB answers with nothing at all.
   *  None was a defect, and prod holds ids today's code cannot re-derive.
   *
   *  So the identification axis sits structurally below production and always will.
   *  10% is the same reasoning that chose 5% for the rest — roughly double the
   *  observed spread (3.7-5.0% across runs), not a number picked to make a day pass. */
  it should "judge identification against its own, wider band" in {
    // 450 of 681 is 66.1% against production's 71.0% — a 6.9% offset, PAST the 5% the
    // rating axes are held to and inside the identification band. This is the case that
    // discriminates: under one shared tolerance it is a failure.
    val prod = coverage(films = 683, tmdb = 485)
    val run  = coverage(films = 681, tmdb = 450)

    ProdCoverageBaseline.divergences(run, prod, Band) shouldBe empty
  }

  it should "keep the measured offset comfortably inside that band" in {
    // The real 2026-08-23 Poland numbers, as an anchor: 4.0% of the band it is judged
    // against, so a normal run is not sitting on the line the way it was at 5.0%.
    val prod = coverage(films = 683, tmdb = 485)
    val run  = coverage(films = 681, tmdb = 464)

    ProdCoverageBaseline.divergences(run, prod, Band) shouldBe empty
    ProdCoverageBaseline.report(run, prod, Band).find(_.startsWith("tmdbId")).value should not include "NEARING"
  }

  it should "not extend that licence to the rating axes" in {
    // The SAME proportional gap on a rating axis is still a finding: those are shares
    // of each side's own identified films, so there is no inventory effect to excuse.
    val prod = coverage(films = 683, tmdb = 485, metascore = 253)
    val run  = coverage(films = 683, tmdb = 485, metascore = 180)   // 37% vs 52% of identified

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged.map(_.takeWhile(_ != ' ')) should contain ("metascore")
  }

  // The wider band must still catch what the axis exists to catch. The fallback chain
  // that broke the rating ladders put Poland 281 films from production on one axis;
  // a resolver that stopped answering is nothing like a 4% inventory offset.
  it should "still flag an identification COLLAPSE" in {
    val prod = coverage(films = 683, tmdb = 485)
    val run  = coverage(films = 681, tmdb = 204)      // the shape of a broken resolver

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged should have size 1
    flagged.head should include ("tmdbId")
  }

  it should "flag an axis that resolved a smaller share than production" in {
    val prod = coverage(films = 700, tmdb = 500)
    val run  = coverage(films = 700, tmdb = 300)      // 43% vs 71% — the shape of a real collapse

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged should have size 1
    flagged.head should include ("tmdbId")
  }

  /** Two guards, not one. A ratio alone is noise-dominated on a small corpus — a
   *  ~100-film sample came out 92 films against production's 78 with the per-source
   *  counts almost identical, which reads as a 16% collapse and is capture skew. A raw
   *  count alone is meaningless on a large one. An axis has to fail BOTH. */
  it should "not flag a handful of films' difference, however large the percentage looks" in {
    val prod = coverage(films = 78, tmdb = 69)
    val run  = coverage(films = 92, tmdb = 68)        // 74% vs 88% by share; ONE film apart

    ProdCoverageBaseline.divergences(run, prod, Band).filter(_.contains("tmdbId")) shouldBe empty
  }

  // …and the floor must not swallow a real regression. The fallback chain that broke
  // the rating ladders put Poland 281 films from production on this very axis.
  it should "still flag a collapse that is far larger than the noise floor" in {
    val prod = coverage(films = 726, tmdb = 526, rt = 354)
    val run  = coverage(films = 710, tmdb = 509, rt = 73)

    ProdCoverageBaseline.divergences(run, prod, Band).mkString should include ("rottenTomatoes")
  }

  /**
   * Upward drift is a real failure, not an improvement to be waved through. The
   * harness's rating sweep drove Filmweb for every country while production gates it
   * to Poland: the German leg reported 972 Filmweb ratings against prod's 0. No share
   * is within 5% of nothing, so a zero baseline admits only zero.
   */
  it should "flag a source production does not run for this country at all" in {
    val prod = coverage(films = 1171, tmdb = 1137, filmweb = 0)
    val run  = coverage(films = 1220, tmdb = 1192, filmweb = 972)

    val flagged = ProdCoverageBaseline.divergences(run, prod, Band)
    flagged.mkString should include ("filmwebRating")
  }

  it should "accept a zero baseline when the run also has none" in {
    ProdCoverageBaseline.divergences(
      coverage(films = 1220, tmdb = 1192, filmweb = 0),
      coverage(films = 1171, tmdb = 1137, filmweb = 0), Band) shouldBe empty
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

  /**
   * The distinction `ratingsGivenTmdbId` already draws in the report, now in the
   * assertion: identification is measured against the films, everything downstream
   * against the films that were IDENTIFIED.
   *
   * These are Poland's real numbers from the run that forced this. Judged against all
   * films, `imdbRating` and `filmwebRating` were 5.6% and 6.6% out and failed; judged
   * against identified films they are 92.5% and 90.1% against production's 93.0% and
   * 91.7%. Nothing was wrong with the rating pipeline — the run simply carried 32 more
   * unresolvable rows than production, and every rating axis inherited that.
   */
  it should "not flag ratings for a run that carries more unresolvable films than production" in {
    val prod = coverage(films = 727, tmdb = 531, imdb = 519, imdbRating = 494, filmweb = 487, metascore = 308, rt = 353)
    val run  = coverage(films = 745, tmdb = 517, imdb = 507, imdbRating = 478, filmweb = 466, metascore = 301, rt = 348)

    ProdCoverageBaseline.divergences(run, prod, Band) shouldBe empty
  }
}
