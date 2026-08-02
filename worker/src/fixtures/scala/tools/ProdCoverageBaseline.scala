package tools

import play.api.libs.json._

import java.nio.file.{Files, Path, Paths}

/**
 * What production had resolved for a country, captured at the same instant as its
 * corpus so the two describe the SAME repertoire.
 *
 * The convergence legs measure how much of a country's catalogue the pipeline can
 * identify and rate. That number only means something against a reference, and the
 * reference has to be production — but the suite must not reach production to get
 * it: prod has no public IP, every serious failure this suite has had came from
 * dragging data through a `flyctl proxy`, and a leg that phones home is neither
 * reproducible nor runnable offline.
 *
 * So the baseline is recorded where a prod connection ALREADY exists and is already
 * paid for: `RecordCorpusFixture`, which dumps `cinema_scrapes` nightly. Capturing
 * both from one connection is what makes them comparable — the corpus is a snapshot
 * of what was screening at instant T, and this is what prod had enriched for
 * exactly that set at exactly T. A baseline recorded separately would drift against
 * the corpus and turn the guard into a flake.
 *
 * COUNTED OVER THE FILMS SCREENING AT RECORD TIME, not the whole `movies`
 * collection, and that distinction is the whole point. Prod retains a film's row
 * after its last showtime passes — 204 of Poland's 931 rows on the day this was
 * written — so comparing a one-day corpus replay against the full collection reads
 * as a 20% shortfall that is really a back catalogue. Restricted to what is
 * actually screening, the same comparison came out at 741 films against 727.
 */
case class ProdCoverageBaseline(
  recordedAt:     java.time.Instant,
  films:          Int,
  tmdbId:         Int,
  imdbId:         Int,
  imdbRating:     Int,
  filmwebRating:  Int,
  metascore:      Int,
  rottenTomatoes: Int
) {

  /** The metrics by name, in report order — so a comparison walks them all rather
   *  than naming six fields and quietly forgetting the seventh when one is added. */
  def metrics: Seq[(String, Int)] = Seq(
    "films" -> films, "tmdbId" -> tmdbId, "imdbId" -> imdbId, "imdbRating" -> imdbRating,
    "filmwebRating" -> filmwebRating, "metascore" -> metascore, "rottenTomatoes" -> rottenTomatoes)
}

object ProdCoverageBaseline {

  private implicit val instantFormat: Format[java.time.Instant] =
    Format(Reads.DefaultInstantReads, Writes.DefaultInstantWrites)
  implicit val format: OFormat[ProdCoverageBaseline] = Json.format[ProdCoverageBaseline]

  /** Beside the corpus it was captured with, and shipped in the same archive — the
   *  two are only meaningful together, so they must not be able to arrive apart. */
  def pathFor(code: String): Path =
    Paths.get(s"test/resources/fixtures/corpus/prod-coverage-$code.json")

  def exists(code: String): Boolean = Files.exists(pathFor(code))

  def write(code: String, baseline: ProdCoverageBaseline): Path = {
    val path = pathFor(code)
    Files.createDirectories(path.getParent)
    Files.write(path, Json.prettyPrint(Json.toJson(baseline)).getBytes("UTF-8"))
    path
  }

  def read(code: String): Option[ProdCoverageBaseline] =
    Option.when(exists(code))(Json.parse(Files.readAllBytes(pathFor(code))).as[ProdCoverageBaseline])

  /**
   * Which metrics sit further from production than `tolerance` allows, as report
   * lines — empty when every one is inside the band.
   *
   * Compared as a SHARE, not as raw counts — of each side's own film count for
   * identification, and of each side's own IDENTIFIED films for everything downstream. The corpus
   * is a snapshot and prod is live, so the two never hold quite the same number of
   * films; a raw comparison would fail on that alone and say nothing about
   * enrichment. `films` itself is the one metric compared as a count, because there
   * is nothing to take a share of — it IS the denominator.
   *
   * A ZERO baseline is compared strictly rather than proportionally: no share is
   * within 5% of nothing. That case is not hypothetical — production holds 0
   * `filmwebRating` for Germany and the UK because Filmweb is Poland-only, and the
   * harness's rating sweep was driving it anyway and reporting 972 and 1293. This
   * band is what would have caught that on the day it landed.
   */
  /**
   * Films' worth of difference that is never a finding, whatever the percentages say.
   *
   * A corpus and a production database are captured minutes apart at best and a day
   * apart at worst, so a handful of films differ between them by construction — one
   * whose last showtime passed, one a venue added since. On a full corpus that is
   * fractions of a percent and the relative band governs. On the ~100-film sample it
   * is not: each film is a whole percent, and a measured run came out 92 films against
   * production's 78 with the per-source counts almost exactly matching (tmdbId 68
   * vs 69, imdbId 65 vs 66). Judged on shares alone that reads as a 16% collapse; it
   * is capture skew.
   *
   * 15 because the skew observed across three countries and several runs topped out at
   * 14 films, and because the regressions this exists to catch are nothing like that
   * size — the fallback chain that broke the rating ladders put Poland 281 films from
   * production on one axis. The floor is well above the noise and far below anything
   * worth reporting.
   */
  val NoiseFloorFilms = 15

  /**
   * Each axis as `(name, run share, prod share, run count, prod count)`.
   *
   * ONE definition of what every axis is measured against, so the band and the report
   * can never disagree about whether a run passed. Identification is a share of the
   * FILMS; everything downstream is a share of the films that were IDENTIFIED — the
   * distinction `ratingsGivenTmdbId` already draws in the report, because "we could
   * not identify the film" and "we identified it and could not rate it" are different
   * failures with different fixes. Conflating them had Poland failing on `imdbRating`
   * and `filmwebRating` while rating 92.5% and 90.1% of what it had identified against
   * production's 93.0% and 91.7%: a healthy pipeline, flagged for a deficit one level
   * up. `films` is the one axis compared as a count — it IS the denominator.
   */
  private def axes(actual: ProdCoverageBaseline, prod: ProdCoverageBaseline)
      : Seq[(String, Double, Double, Int, Int)] = {
    def share(count: Int, of: Int): Double = if (of == 0) 0.0 else count.toDouble / of
    actual.metrics.zip(prod.metrics).map { case ((name, mine), (_, theirs)) =>
      val (a, b) =
        if (name == "films")       (actual.films.toDouble,        prod.films.toDouble)
        else if (name == "tmdbId") (share(mine, actual.films),    share(theirs, prod.films))
        else                       (share(mine, actual.tmdbId),   share(theirs, prod.tmdbId))
      (name, a, b, mine, theirs)
    }
  }

  /**
   * Every axis with the margin it has left, whether or not it failed.
   *
   * Printed on every run, deliberately, because a band that only speaks when it breaks
   * hides the thing most worth seeing: an axis drifting TOWARDS the line. Poland's
   * identification sat at 5.0% of a 5% band while the rating axes above it failed —
   * the root cause clearing the threshold by a hair while its symptoms were the ones
   * being reported. Silence would have said the run was fine.
   *
   * An axis inside the band but past four fifths of it is marked `NEARING`, so the
   * drift is legible in the log of a PASSING leg rather than discovered by a failing
   * one weeks later.
   */
  def report(actual: ProdCoverageBaseline, prod: ProdCoverageBaseline, tolerance: Double): Seq[String] =
    axes(actual, prod).map { case (name, a, b, mine, theirs) =>
      val off   = if (b == 0.0) (if (a == 0.0) 0.0 else Double.PositiveInfinity) else math.abs(a - b) / b
      val apart = math.abs(mine - theirs)
      val note =
        if (off > tolerance && apart > NoiseFloorFilms) "OUT"
        else if (off > tolerance * 0.8 && apart > NoiseFloorFilms) "NEARING"
        else ""
      f"$name%-15s run=$mine%5d prod=$theirs%5d — ${100 * off}%5.1f%% of a ${100 * tolerance}%.0f%% band, " +
      f"$apart%3d film(s) apart $note"
    }

  def divergences(actual: ProdCoverageBaseline, prod: ProdCoverageBaseline, tolerance: Double): Seq[String] = {
    axes(actual, prod).flatMap { case (name, a, b, mine, theirs) =>
      val off = if (b == 0.0) (if (a == 0.0) 0.0 else Double.PositiveInfinity) else math.abs(a - b) / b
      // Outside the relative band AND more than a few films apart. Either alone
      // misfires: the ratio is noise-dominated on a small corpus, and a raw count is
      // meaningless on a large one.
      val filmsApart = math.abs(mine - theirs)
      Option.when(off > tolerance && filmsApart > NoiseFloorFilms)(
        f"$name%-15s run=$mine%5d (${100 * a}%.1f${if (name == "films") "" else "%"}) " +
        f"prod=$theirs%5d (${100 * b}%.1f${if (name == "films") "" else "%"}) " +
        f"— off by ${100 * off}%.1f%%, band is ${100 * tolerance}%.0f%%")
    }
  }
}
