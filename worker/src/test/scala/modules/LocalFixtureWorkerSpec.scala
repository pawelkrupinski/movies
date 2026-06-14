package modules

import clients.tools.FakeHttpFetch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Files
import java.time.LocalDate

/**
 * Covers how the local-stack worker recovers the scrape day for Helios from a
 * dateless `today` dir (its dateless name carries no date, so Helios's
 * date-baked URLs would miss without this). The fetch-replay overrides
 * themselves mirror the proven `FixtureTestWiring` and are compile-checked;
 * exercising them needs a live Mongo (the wiring eagerly builds the repository), so
 * that path is left to the running `localStack` rather than a unit spec.
 */
class LocalFixtureWorkerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val tmpDir  = "local-fixture-worker-spec"
  private val tmpRoot = new File(s"test/resources/fixtures/$tmpDir")

  "FixtureWorkerWiring.captureDate" should "read `date=` from the dir's CAPTURE_DATE file" in {
    tmpRoot.mkdirs()
    Files.write(new File(tmpRoot, "CAPTURE_DATE").toPath,
      "date=13-06-2026\ncaptured_at=2026-06-13T17:34:08+02:00\n".getBytes("UTF-8"))
    FixtureWorkerWiring.captureDate(tmpDir) shouldBe Some(LocalDate.of(2026, 6, 13))
  }

  it should "fall back to the dir name when it is itself dd-MM-yyyy and has no CAPTURE_DATE" in {
    FixtureWorkerWiring.captureDate("01-01-2020") shouldBe Some(LocalDate.of(2020, 1, 1))
  }

  it should "be None for a dateless dir with no CAPTURE_DATE (a bare `today` synced before the stamp)" in {
    FixtureWorkerWiring.captureDate("definitely-not-a-fixture-dir") shouldBe None
  }

  // The forked bg worker's CWD isn't the repository root, so FakeHttpFetch must be
  // able to resolve the corpus under an absolute KINOWO_FIXTURE_ROOT base.
  "FakeHttpFetch.rootFor" should "default to the repository-relative fixtures path" in {
    FakeHttpFetch.rootFor("today", None) shouldBe "test/resources/fixtures/today"
  }

  it should "resolve under an absolute base when KINOWO_FIXTURE_ROOT is set" in {
    FakeHttpFetch.rootFor("today", Some("/repo/test/resources/fixtures")) shouldBe
      "/repo/test/resources/fixtures/today"
  }

  override def afterAll(): Unit = {
    deleteRecursively(tmpRoot)
    super.afterAll()
  }

  private def deleteRecursively(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles).foreach(_.foreach(deleteRecursively))
    f.delete()
    ()
  }
}
