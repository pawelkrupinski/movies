package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieCodecs
import services.readmodel.ReadModelCodecs
import services.scrapes.ScrapeArchiveCodecs
import tools.Env
import tools.persistence.PersistedRoundTrip

/**
 * Every case class the worker's registries write, FULLY populated — every `Option` set,
 * every collection non-empty — written to a real Mongo through the real codec and read
 * back unchanged. The lists come from the registries themselves
 * ([[services.PersistedCodecs]]): a new persisted type, or a new field on one, is covered
 * with no edit here. `PersistedCodecsLintSpec` fails a registry this spec does not name.
 *
 * THE BUG THIS PINS. 1dd8fb3d3 gave `SourceData` a cache-only `showtimeStartMinutes:
 * IArray[Int]`. `int[]` has no BSON codec, so every write of a cache-stripped record threw
 * "Can't find a codec for class [I" — swallowed into a WARN by the repository, so 48 green
 * `MovieRepositoryIntegrationSpec` tests and the codec specs all passed while UK and US
 * convergence lost merged rows. None of them had populated that field.
 *
 * `web`'s `UserCodecs` go through the same helper in `web/src/it`.
 */
class PersistedCodecsRoundTripSpec extends AnyFlatSpec with Matchers {

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  /** Fields a codec deliberately never writes, each with its reason. */
  private val dropped = Set(
    // Cache-only (`ShowtimesDigest.stripForCache`): the decoder never reads them back,
    // and `MovieCodecs` strips them on the way out.
    "SourceData.showtimesDigest",
    "SourceData.showtimeStartMinutes",
  )

  private def survives(result: (List[String], Seq[String])): Unit = {
    val (covered, findings) = result
    withClue(s"round-tripped ${covered.mkString(", ")}:\n  ${findings.mkString("\n  ")}\n")(findings shouldBe empty)
  }

  "MovieCodecs" should "write and read back every persisted type unchanged" in
    survives(PersistedRoundTrip.registry[MovieCodecs.OmittingNone, MovieCodecs.WritingNone](tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get, MovieCodecs.registry, dropped))

  "ReadModelCodecs" should "write and read back every persisted type unchanged" in
    survives(PersistedRoundTrip.registry[ReadModelCodecs.OmittingNone, ReadModelCodecs.WritingNone](tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get, ReadModelCodecs.registry, dropped))

  "ScrapeArchiveCodecs" should "write and read back every persisted type unchanged" in
    survives(PersistedRoundTrip.registry[ScrapeArchiveCodecs.OmittingNone, ScrapeArchiveCodecs.WritingNone](tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get, ScrapeArchiveCodecs.registry, dropped))
}
