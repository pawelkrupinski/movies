package modules

import clients.tools.FakeHttpFetch
import settings.FixtureRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoAddress
import settings.{MongoDatabaseName, MongoUri, ProcessConfiguration}
import tools.Env

import java.io.File
import java.nio.file.Files
import java.time.LocalDate

/**
 * Covers how the local-stack worker recovers the scrape day for Helios from a
 * dateless `today` directory (its dateless name carries no date, so Helios's
 * date-baked URLs would miss without this). The fetch-replay overrides
 * themselves mirror the proven `FixtureTestWiring` and are compile-checked;
 * exercising them needs a live Mongo (the wiring eagerly builds the repository), so
 * that path is left to the running `localStack` rather than a unit spec.
 */
class LocalFixtureWorkerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private def resolvedFrom(vars: (String, String)*): ProcessConfiguration = new ProcessConfiguration(Env.of(vars*))

  private val temporaryDirectory  = "local-fixture-worker-spec"
  private val temporaryRoot = new File(s"test/resources/fixtures/$temporaryDirectory")

  "FixtureWorkerWiring.captureDate" should "read `date=` from the directory's CAPTURE_DATE file" in {
    temporaryRoot.mkdirs()
    Files.write(new File(temporaryRoot, "CAPTURE_DATE").toPath,
      "date=13-06-2026\ncaptured_at=2026-06-13T17:34:08+02:00\n".getBytes("UTF-8"))
    FixtureWorkerWiring.captureDate(temporaryDirectory) shouldBe Some(LocalDate.of(2026, 6, 13))
  }

  it should "fall back to the directory name when it is itself dd-MM-yyyy and has no CAPTURE_DATE" in {
    FixtureWorkerWiring.captureDate("01-01-2020") shouldBe Some(LocalDate.of(2020, 1, 1))
  }

  it should "be None for a dateless directory with no CAPTURE_DATE (a bare `today` synced before the stamp)" in {
    FixtureWorkerWiring.captureDate("definitely-not-a-fixture-directory") shouldBe None
  }

  // The local stack must target the native brew Mongo (:28017) the rest of the
  // local tooling shares (the /debug mirror, scripts/reset-corpus.sh --local),
  // NOT the retired Docker instance (:27018). A regression here silently splits
  // the worker onto a second mongo — the bug that prompted this.
  "LocalFixtureWorkerMain's Mongo defaults" should "point at the brew :28017 local Mongo, not Docker :27018" in {
    LocalFixtureWorkerMain.DefaultMongoUri should include ("28017")
    LocalFixtureWorkerMain.DefaultMongoUri should not include "27018"
    LocalFixtureWorkerMain.DefaultMongoDb shouldBe "kinowo_local"
  }

  // `.env.local` points MONGODB_URI at PROD. The local stack's address must never pick that
  // up — only a value exported in the process environment itself may override the local
  // defaults — and it reaches the wiring as a value, not as a rewritten MONGODB_URI.
  "LocalFixtureWorkerMain.localMongo" should "ignore a MONGODB_URI that only .env.local carries" in {
    val dotEnvLocal = resolvedFrom("MONGODB_URI" -> "mongodb://prod-tunnel:27017", "MONGODB_DB" -> "kinowo")
    LocalFixtureWorkerMain.localMongo(MongoAddress.Disabled, dotEnvLocal) shouldBe
      MongoAddress(Some(MongoUri(LocalFixtureWorkerMain.DefaultMongoUri)), Some(MongoDatabaseName(LocalFixtureWorkerMain.DefaultMongoDb)))
  }

  it should "let the KINOWO_LOCAL_MONGO_* overrides move it" in {
    LocalFixtureWorkerMain.localMongo(MongoAddress.Disabled,
      resolvedFrom("KINOWO_LOCAL_MONGO_URI" -> "mongodb://127.0.0.1:28099", "KINOWO_LOCAL_MONGO_DB" -> "kinowo_elsewhere")) shouldBe
      MongoAddress(Some(MongoUri("mongodb://127.0.0.1:28099")), Some(MongoDatabaseName("kinowo_elsewhere")))
  }

  it should "let a MONGODB_URI / MONGODB_DB exported in the process environment win" in {
    val exported = MongoAddress(Some(MongoUri("mongodb://exported:1")), Some(MongoDatabaseName("kinowo_exported")))
    LocalFixtureWorkerMain.localMongo(exported, resolvedFrom("KINOWO_LOCAL_MONGO_URI" -> "mongodb://ignored")) shouldBe
      MongoAddress(Some(MongoUri("mongodb://exported:1")), Some(MongoDatabaseName("kinowo_exported")))
  }

  "LocalFixtureWorkerMain.fixtureRootFor" should "walk up from a module directory to the repository's fixtures" in {
    val repository = Files.createTempDirectory("local-fixture-root").toFile
    val fixtures   = new File(repository, "test/resources/fixtures")
    fixtures.mkdirs()
    val module = new File(repository, "worker")
    module.mkdirs()
    try LocalFixtureWorkerMain.fixtureRootFor(resolvedFrom(), module) shouldBe FixtureRoot(fixtures.toPath)
    finally deleteRecursively(repository)
  }

  it should "prefer a KINOWO_FIXTURE_ROOT the process names" in {
    LocalFixtureWorkerMain.fixtureRootFor(resolvedFrom("KINOWO_FIXTURE_ROOT" -> "/somewhere/fixtures"), new File("/")) shouldBe
      FixtureRoot(java.nio.file.Path.of("/somewhere/fixtures"))
  }

  // The forked bg worker's CWD isn't the repository root, so the fetches must be able to
  // resolve the corpus under an absolute root.
  "FixtureRoot" should "default to the repository-relative fixtures path" in {
    FixtureRoot.RepositoryRelative.of("today") shouldBe "test/resources/fixtures/today"
    resolvedFrom().fixtureRoot shouldBe FixtureRoot.RepositoryRelative
  }

  it should "resolve under an absolute root when KINOWO_FIXTURE_ROOT names one" in {
    resolvedFrom("KINOWO_FIXTURE_ROOT" -> "/repo/test/resources/fixtures").fixtureRoot.of("today") shouldBe
      "/repo/test/resources/fixtures/today"
  }

  it should "be where a FakeHttpFetch reads its fixtures from" in {
    new FakeHttpFetch("today", root = FixtureRoot(java.nio.file.Path.of("/elsewhere"))).fixtureRoot shouldBe "/elsewhere/today"
  }

  override def afterAll(): Unit = {
    deleteRecursively(temporaryRoot)
    super.afterAll()
  }

  private def deleteRecursively(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles).foreach(_.foreach(deleteRecursively))
    f.delete()
    ()
  }
}
