package tools

import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import play.api.libs.json._
import services.readmodel.{ReadModelReader, ReadModelWriter}

import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

/**
 * Serializes the PROJECTED read model (`web_movies` + `web_screenings`) so a
 * fixture-driven test server can LOAD it instead of recomputing it on every
 * boot. `FixtureTestWiring.bootStartup` spends ~110s of CPU running the whole
 * ~1000-film corpus through scrape→enrich→stage→fold→project just to populate
 * the read model that `webReadModel` serves; every page-test runner (Playwright
 * × N, the Scala PageTest specs, mobile LocalServer) paid that independently.
 * The output is deterministic (proven by the *OrderDeterminismSpec suite), so it
 * is captured ONCE into `read-model-snapshot.json` and replayed in milliseconds.
 *
 * Correctness is guarded by `FilmScheduleEndToEndSpec`, which boots the REAL
 * pipeline and asserts its read model equals this snapshot (regenerate-on-
 * mismatch, like `expected-schedules.txt`). Consumers fall back to the real boot
 * when the file is absent (see `FixtureTestWiring.bootFromSnapshotOrPipeline`),
 * so a missing/stale snapshot is slow, never wrong.
 */
object ReadModelSnapshot {

  // play-json is already on worker's classpath (TmdbClient et al.). Pin the
  // LocalDateTime codec explicitly so the macros don't depend on whichever
  // temporal default happens to be in implicit scope.
  private implicit val localDateTimeFormat: Format[LocalDateTime] =
    Format(Reads.DefaultLocalDateTimeReads, Writes.DefaultLocalDateTimeWrites)

  private implicit val showtimeFormat: OFormat[Showtime]       = Json.format[Showtime]
  private implicit val ratingsFormat: OFormat[ResolvedRatings] = Json.format[ResolvedRatings]
  private implicit val movieFormat: OFormat[ResolvedMovie]     = Json.format[ResolvedMovie]
  private implicit val screeningFormat: OFormat[CityScreening] = Json.format[CityScreening]

  final case class Snapshot(movies: Seq[ResolvedMovie], screenings: Seq[CityScreening])
  private implicit val snapshotFormat: OFormat[Snapshot] = Json.format[Snapshot]

  /** Repo-root-relative — the page specs + FixtureServerMain run unforked with
   *  CWD = repo root, exactly like the other `test/resources/fixtures/…` loads. */
  val DefaultPath: Path =
    Paths.get("test", "resources", "fixtures", "08-06-2026", "read-model-snapshot.json")

  /** Capture the projected read model, sorted by id so the rendered JSON is a
   *  pure function of the corpus (independent of the store's iteration order). */
  def capture(reader: ReadModelReader): Snapshot =
    Snapshot(
      reader.findAllMovies().sortBy(_._id),
      reader.findAllScreenings().sortBy(_._id)
    )

  def render(snapshot: Snapshot): String = Json.prettyPrint(Json.toJson(snapshot)) + "\n"

  def parse(json: String): Snapshot = Json.parse(json).as[Snapshot]

  /** Replay a serialized snapshot into a fresh read-model writer. */
  def loadInto(writer: ReadModelWriter, json: String): Unit = {
    val snapshot = parse(json)
    snapshot.movies.foreach(writer.upsertMovie)
    snapshot.screenings.foreach(writer.upsertScreening)
  }

  def exists(path: Path = DefaultPath): Boolean = Files.exists(path)

  def read(path: Path = DefaultPath): String = Files.readString(path)
}
