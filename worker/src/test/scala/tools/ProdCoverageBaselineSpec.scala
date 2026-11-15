package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The band the convergence legs are judged against. Worth its own spec because the
 * comparison decides whether a leg passes, and it has two properties that are easy
 * to get wrong and invisible when they are: it compares SHARES rather than counts,
 * and it treats a zero baseline strictly.
 */
class ProdCoverageBaselineSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

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

  // The SAMPLE shape: every rating axis is a few dozen films, so a flat 15-film floor let
  // a source that stopped answering for a third of them through. Poland's sample, measured.
  it should "flag a rating source that lost a third of a sample's films, though under 15 films apart" in {
    val prod = coverage(films = 94, tmdb = 73, metascore = 28)
    val run  = coverage(films = 94, tmdb = 68, metascore = 17)      // 25% vs 38% of the identified

    ProdCoverageBaseline.divergences(run, prod, Band).mkString should include ("metascore")
  }

  it should "keep the whole films floor on the films axis, and scale the others to production's count" in {
    ProdCoverageBaseline.noiseFloor("films", 94) shouldBe ProdCoverageBaseline.NoiseFloorFilms
    ProdCoverageBaseline.noiseFloor("metascore", 28) shouldBe 8          // ⌈1.5·√28⌉
    ProdCoverageBaseline.noiseFloor("tmdbId", 90) shouldBe 15            // ⌈1.5·√90⌉ = 15
    ProdCoverageBaseline.noiseFloor("imdbRating", 1367) shouldBe ProdCoverageBaseline.NoiseFloorFilms
  }

  // Green-history lines the one-tenth floor would have failed: sample noise, not regressions.
  // Every one of the 3,204 band lines in the last 60 convergence and recording runs sits under
  // 1.42·√prod films apart; these are the widest (Poland sample, 2026-09-24/25).
  it should "pass the sample skew a green history has shown on every axis" in {
    ProdCoverageBaseline.divergences(coverage(films = 92, tmdb = 73, rt = 34), coverage(films = 92, tmdb = 73, rt = 39), Band) shouldBe empty
    ProdCoverageBaseline.divergences(coverage(films = 94, tmdb = 59), coverage(films = 94, tmdb = 71), Band) shouldBe empty
    ProdCoverageBaseline.divergences(coverage(films = 94, tmdb = 70, metascore = 24), coverage(films = 94, tmdb = 70, metascore = 31), Band) shouldBe empty
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
    val baseline  = coverage(films = 727, tmdb = 526, imdb = 515, imdbRating = 478, filmweb = 482, metascore = 308, rt = 354)
    val code      = "roundtrip"
    val directory = tempDirectory("prod-coverage-baseline-spec")
    try {
      ProdCoverageBaseline.writeIn(directory, code, baseline)
      ProdCoverageBaseline.readIn(directory, code) shouldBe Some(baseline)
    } finally { java.nio.file.Files.deleteIfExists(ProdCoverageBaseline.pathIn(directory, code)); java.nio.file.Files.deleteIfExists(directory); () }
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

  "A shadow run's coverage, in a recording from before the cut-over," should "be judged on identification alone, for the shadow rates nothing" in {
    val shadow = ShadowCoverage(java.time.Instant.EPOCH, films = 1239, tmdbId = 721)
    val run    = coverage(films = 1236, tmdb = 699, imdb = 756, imdbRating = 708, filmweb = 631, metascore = 309, rt = 413)

    shadow.divergences(run, Band) shouldBe empty
    shadow.divergences(coverage(films = 1236, tmdb = 500), Band).map(_.take(6)) shouldBe Seq("tmdbId")
  }

  "A baseline" should "read without a shadow when it was recorded before one existed, and keep one it holds" in {
    import play.api.libs.json.Json
    val old = coverage(films = 10, tmdb = 5)
    Json.toJson(old).as[ProdCoverageBaseline] shouldBe old
    (Json.toJson(old).as[play.api.libs.json.JsObject] - "shadow").as[ProdCoverageBaseline].shadow shouldBe None
    val withShadow = old.copy(shadow = Some(ShadowCoverage(java.time.Instant.EPOCH, 9, 4)))
    Json.toJson(withShadow).as[ProdCoverageBaseline] shouldBe withShadow
  }

  "A baseline withdrawing production's judged-wrong takes" should "count those films still, on no axis but films" in {
    val wrong = ProdFilm("p-przemytnicy", "przemytnicy|1985", Some(581674), Some("search"), Some("tt0089846"),
      Set("tmdbId", "imdbId", "imdbRating"), Seq("Kino Sława␟przemytnicy"))
    coverage(films = 1249, tmdb = 813, imdb = 790, imdbRating = 760).withdrawing(Seq(wrong)) shouldBe
      coverage(films = 1249, tmdb = 812, imdb = 789, imdbRating = 759)
  }

  /**
   * Comparing like with like. Production's films include takes a stage AFTER its model made — the agreement over the
   * families' answers, a poster vote, a fill — which a hermetic replay never makes, so the run lacked every one of them
   * and the band read that as the pipeline identifying less: Verify scrape recording #31's Poland, tmdbId 64.0% against
   * production's 67.6%, 5.4% off a 5% band, most of it on 19 such takes. Left out of BOTH sides, the shares compare the
   * one population both can take.
   */
  "The band over production's takes a stage after its model made" should "leave them out of both sides" in {
    def film(n: Int, modelTook: Option[Boolean]) =
      ProdFilm(s"p$n", s"film$n|", Some(n), None, None, Set("tmdbId"), Seq(s"Kino␟film$n"), modelTook)
    val modelTakes = (1 to 260).map(film(_, Some(true)))
    val pastModel  = (261 to 280).map(film(_, Some(false)))
    val untaken    = (281 to 400).map(n => ProdFilm(s"p$n", s"film$n|", None, None, None, Set.empty, Seq(s"Kino␟film$n")))
    val prod       = modelTakes ++ pastModel ++ untaken
    // the replay takes every model take and two of the untaken, but none of the agreement's
    val run = (1 to 400).map(n => RunFilm(s"r$n", s"film$n", Option.when(n <= 260 || n > 398)(n), None,
      if (n <= 260 || n > 398) Set("tmdbId") else Set.empty, Seq(RunListing(s"Kino␟film$n", "Kino", s"film$n"))))
    def counted(films: Seq[RunFilm]) = coverage(films = films.size, tmdb = films.count(_.covered("tmdbId")))

    ProdFilms.takenPastTheModel(prod) shouldBe pastModel
    ProdCoverageBaseline.divergences(counted(run), ProdCoverageBaseline.of(java.time.Instant.EPOCH, prod), Band).mkString should
      include ("tmdbId")

    val leftOut = ProdFilms.runSideOf(pastModel, run)
    leftOut shouldBe (261 to 280).map(n => s"r$n").toSet
    val baseline = ProdCoverageBaseline.of(java.time.Instant.EPOCH, prod).without(pastModel)
    (baseline.films, baseline.tmdbId) shouldBe ((380, 260))
    ProdCoverageBaseline.divergences(counted(run.filterNot(f => leftOut(f.id))), baseline, Band) shouldBe empty
  }

  it should "keep a run film that bills another production film's slot too, and withdraw a split's every part" in {
    val agreed = ProdFilm("p-zniknięcie", "znikniecie|1988", Some(8740), None, None, Set("tmdbId"),
      Seq("Kino Amondo␟znikniecie", "Kino Muza␟znikniecie"), modelTook = Some(false))
    def runFilm(id: String, slots: String*) = RunFilm(id, id, None, None, Set.empty, slots.map(slot => RunListing(slot, "", "")))
    ProdFilms.runSideOf(Seq(agreed), Seq(
      runFilm("amondo", "Kino Amondo␟znikniecie"), runFilm("muza", "Kino Muza␟znikniecie"),
      runFilm("merged", "Kino Muza␟znikniecie", "Kino Muza␟werdykt"), runFilm("unlisted"))) shouldBe Set("amondo", "muza")
  }

  "Whether production's model took a film" should "be told from its decisions at the film's listings, unknown without one" in {
    ProdCoverage.modelTookOf(Some(8740), Seq(None, Some(8740))) shouldBe Some(true)
    ProdCoverage.modelTookOf(Some(8740), Seq(None, Some(1)))    shouldBe Some(false)
    ProdCoverage.modelTookOf(Some(8740), Nil)                   shouldBe None
    ProdCoverage.modelTookOf(None, Seq(Some(1)))                shouldBe None
    ProdFilms.takenPastTheModel(Seq(ProdFilm("p", "p|", Some(1), None, None, Set("tmdbId"), Nil))) shouldBe empty
  }

  "A production film recorded before its take's stage was" should "read with none, and so be withdrawn from nothing" in {
    import play.api.libs.json.Json
    val film = ProdFilm("p", "p|", Some(1), None, None, Set("tmdbId"), Seq("Kino␟p"), modelTook = Some(false))
    Json.toJson(film).as[ProdFilm] shouldBe film
    (Json.toJson(film).as[play.api.libs.json.JsObject] - "modelTook").as[ProdFilm].modelTook shouldBe None
  }
}
