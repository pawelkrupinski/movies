package tools

import models.{Movie, Showtime}
import play.api.libs.json._
import services.scrapes.{ArchivedFilmDto, ArchivedScrape, BarrenAttemptDto, ScrapeArchiveRepository, ScrapeAttempt, StoredScrapeDto}

import java.io.FileOutputStream
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/**
 * A country's real `cinema_scrapes` corpus, captured to a compressed file so a
 * convergence run can replay it WITHOUT reaching prod at all.
 *
 * Same bargain [[ReadModelSnapshot]] makes for the projected read model, for the
 * same reason: the input is expensive to obtain and deterministic once obtained,
 * so it is captured once and replayed. Here the expense is not CPU but a
 * `flyctl proxy` — prod has no public IP, and every convergence failure this repo
 * has seen came from dragging the corpus through that tunnel with ~500 paged
 * queries per leg, each an independent chance to meet a proxy that has exited or
 * wedged.
 *
 * It also makes a leg REPRODUCIBLE, which the live read never was. Prod drifts
 * under a running test — the same Polish corpus measured 7,044, then 7,055, then
 * 7,063 listings inside an hour as venues rescraped — so a divergence found in one
 * run could not be re-examined in the next, because the data had moved. A file
 * pins it, and a failing corpus can be kept.
 *
 * Serialised through the STORAGE DTOs rather than the domain types: `Cinema` is a
 * sealed hierarchy that no JSON macro can encode, and `StoredScrapeDto` already
 * carries the `displayName`↔`Cinema` mapping (and already drops rows whose cinema
 * left the catalogue). Reusing it means the fixture and the database agree on the
 * shape by construction.
 *
 * Gzipped because it is large and highly repetitive — a country is 13-49 MB of
 * JSON that compresses to a fraction, and it lives in the repo's `test/resources`
 * tree beside the other big fixtures.
 */
object CorpusFixture {

  private implicit val instantFormat: Format[Instant] =
    Format(Reads.DefaultInstantReads, Writes.DefaultInstantWrites)

  private implicit val showtimeFormat: OFormat[Showtime]         = Json.format[Showtime]
  private implicit val movieFormat: OFormat[Movie]               = Json.format[Movie]
  private implicit val filmFormat: OFormat[ArchivedFilmDto]      = Json.format[ArchivedFilmDto]
  private implicit val barrenFormat: OFormat[BarrenAttemptDto]   = Json.format[BarrenAttemptDto]
  private implicit val storedFormat: OFormat[StoredScrapeDto]    = Json.format[StoredScrapeDto]

  /** Where a country's corpus lives. Repo-root-relative, like every other
   *  `test/resources/fixtures/…` load, so it resolves the same unforked from sbt
   *  and from a CI checkout. */
  def pathFor(countryCode: String): Path =
    Paths.get("test", "resources", "fixtures", "corpus", s"cinema-scrapes-${countryCode.toLowerCase}.json.gz")

  def exists(countryCode: String): Boolean = Files.exists(pathFor(countryCode))

  /** Capture rows straight from an archive, sorted by `_id` so the file is a pure
   *  function of the corpus rather than of the store's iteration order — the same
   *  discipline `ReadModelSnapshot.capture` applies. */
  def capture(rows: Seq[ArchivedScrape]): Seq[StoredScrapeDto] =
    rows.flatMap(row => row.lastSuccess.map(success =>
      StoredScrapeDto.fromSuccess(row.cinema, row.city, success))).sortBy(_._id)

  /** The fixture's JSON, uncompressed — the seam the round-trip spec asserts on,
   *  mirroring `ReadModelSnapshot.render` / `.parse`. */
  def render(rows: Seq[ArchivedScrape]): String = Json.toJson(capture(rows)).toString

  def parse(json: String): Seq[ArchivedScrape] =
    Json.parse(json).as[Seq[StoredScrapeDto]].flatMap(StoredScrapeDto.toDomain)

  def write(countryCode: String, rows: Seq[ArchivedScrape]): Path = {
    val path = pathFor(countryCode)
    Files.createDirectories(path.getParent)
    val json = render(rows)
    val out  = new GZIPOutputStream(new FileOutputStream(path.toFile))
    try out.write(json.getBytes(java.nio.charset.StandardCharsets.UTF_8)) finally out.close()
    path
  }

  def read(countryCode: String): Seq[ArchivedScrape] = {
    val in = new GZIPInputStream(Files.newInputStream(pathFor(countryCode)))
    val bytes = try in.readAllBytes() finally in.close()
    parse(new String(bytes, java.nio.charset.StandardCharsets.UTF_8))
  }

  /** Replay the fixture into an archive through `record`, so the archive's own
   *  "content only" rule and its BSON round-trip stay on the path exactly as they
   *  are for a generated corpus. */
  def seedInto(archive: ScrapeArchiveRepository, rows: Seq[ArchivedScrape]): Int = {
    rows.foreach { row =>
      row.lastSuccess.foreach { success =>
        archive.record(ScrapeAttempt(
          cinema          = row.cinema,
          city            = row.city,
          at              = success.at,
          listingComplete = success.listingComplete,
          films           = success.films))
      }
    }
    rows.size
  }

  /** Uncompressed size of the rendered JSON, for the capture log line — the number
   *  worth knowing is how much did NOT have to cross the tunnel. */
  def renderedBytes(rows: Seq[ArchivedScrape]): Int =
    render(rows).getBytes(java.nio.charset.StandardCharsets.UTF_8).length
}
