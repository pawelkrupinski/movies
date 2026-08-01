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
   * Compared as a SHARE of each side's own film count, not as raw counts. The corpus
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
  def divergences(actual: ProdCoverageBaseline, prod: ProdCoverageBaseline, tolerance: Double): Seq[String] = {
    def share(count: Int, films: Int): Double = if (films == 0) 0.0 else count.toDouble / films
    actual.metrics.zip(prod.metrics).flatMap { case ((name, mine), (_, theirs)) =>
      val (a, b) =
        if (name == "films") (actual.films.toDouble, prod.films.toDouble)
        else                 (share(mine, actual.films), share(theirs, prod.films))
      // `b == 0` covers both "prod has none of this" and an empty prod corpus; either
      // way the only value within the band is zero.
      val off = if (b == 0.0) (if (a == 0.0) 0.0 else Double.PositiveInfinity) else math.abs(a - b) / b
      Option.when(off > tolerance)(
        f"$name%-15s run=$mine%5d (${100 * a}%.1f${if (name == "films") "" else "%"}) " +
        f"prod=$theirs%5d (${100 * b}%.1f${if (name == "films") "" else "%"}) " +
        f"— off by ${100 * off}%.1f%%, band is ${100 * tolerance}%.0f%%")
    }
  }
}
