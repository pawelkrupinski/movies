package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** `scripts/identity-capture.sh`'s decisions: what it reads from its arguments and the environment, what each country's
 *  JVM is handed, when a recording's corpus and tree are fetched again, and what it reports. */
class IdentityCaptureSpec extends AnyFlatSpec with Matchers {
  import IdentityCapture._

  private val layout = Layout(Path.of("/repo"), Path.of("/repo/target/identity-capture"))

  // ── arguments ──

  "The arguments" should "default to every country, largest first, a 12 GB heap and a real run" in {
    val o = parse(Nil).toOption.get
    o.countries shouldBe Seq("us", "de", "uk", "pl", "es")
    o.heap shouldBe "12g"
    o.dryRun shouldBe false
  }

  they should "name countries in any order and case, and run them largest first" in {
    parse(Seq("ES", "pl", "--dry-run", "--heap", "8g")).toOption.get shouldBe Options(Seq("pl", "es"), dryRun = true, heap = "8g")
  }

  they should "refuse a country the corpus does not record, and an unknown flag" in {
    parse(Seq("fr")).left.toOption.get should include("fr")
    parse(Seq("--fast")).left.toOption.get should include("--fast")
  }

  // ── the environment each country's JVM is handed ──

  "A country's environment" should "default every variable the capture reads, and give the country its own Mongo database" in {
    val env = environment("uk", Map("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k"), layout)
    env shouldBe Map(
      "KINOWO_IDENTITY_UNMATCHED_CAPTURE" -> "/repo/test/resources/fixtures/identity-unmatched",
      "KINOWO_IDENTITY_FULL"              -> "uk",
      "KINOWO_IDENTITY_CORPUS_DIR"        -> "/repo/target/identity-capture/corpus",
      "KINOWO_FIXTURE_ROOT"               -> "/repo/target/identity-capture/trees/test/resources/fixtures",
      "KINOWO_IDENTITY_AGREEMENT_CACHE"   -> "/repo/target/identity-capture/agreement-cache",
      "KINOWO_IDENTITY_FAMILY_SEED"       -> "/repo/target/identity-capture/families",
      "KINOWO_IDENTITY_POSTER_CACHE"      -> "/repo/target/identity-capture/posters",
      "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k",
      "MONGODB_URI"                       -> "mongodb://127.0.0.1:28017/?directConnection=true",
      "MONGODB_DB"                        -> "kinowo_capture_uk")
  }

  it should "keep a variable the caller set, and never hand the family export's prod URI to the capture" in {
    val env = environment("pl", Map("KINOWO_IDENTITY_CORPUS_DIR" -> "/mine", "MONGODB_URI" -> "mongodb://127.0.0.1:27017/",
      FamilyUri -> "mongodb://prod", "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k"), layout)
    env("KINOWO_IDENTITY_CORPUS_DIR") shouldBe "/mine"
    env("MONGODB_URI") shouldBe "mongodb://127.0.0.1:27017/"
    env.values.toSeq should not contain "mongodb://prod"
  }

  "The recording's inputs" should "be fetched only where the caller named no directory of their own" in {
    managedCorpus(Map.empty) shouldBe true
    managedCorpus(Map("KINOWO_IDENTITY_CORPUS_DIR" -> "/mine")) shouldBe false
    managedTree(Map.empty) shouldBe true
    managedTree(Map("KINOWO_FIXTURE_ROOT" -> "/mine")) shouldBe false
  }

  // ── artefact currency ──

  "A country's recording" should "be fetched when nothing is there yet" in {
    currency(present = None, newest = Some("37")) shouldBe Fetch("37", "nothing downloaded yet")
  }

  it should "be kept when it is the newest successful recording's" in {
    currency(present = Some("37"), newest = Some("37")) shouldBe Keep("37", "already the newest recording's (run 37)")
  }

  it should "be fetched again when a newer recording succeeded" in {
    currency(present = Some("36"), newest = Some("37")) shouldBe Fetch("37", "run 36 is older than the newest recording, 37")
  }

  it should "be kept, said so, when the newest recording was not looked up" in {
    currency(present = Some("36"), newest = None) shouldBe Keep("36", "the newest recording was not looked up")
    currency(present = None, newest = None) shouldBe Missing("no recording downloaded, and the newest was not looked up")
  }

  // ── the child JVM ──

  "A country's JVM" should "take sbt's own options with the capture's heap in place of sbt's" in {
    jvmOptions(Seq("# a comment", "-Xmx4g", "", "-XX:+ExitOnOutOfMemoryError", "-Djava.awt.headless=true"), "12g") shouldBe
      Seq("-Xmx12g", "-XX:+ExitOnOutOfMemoryError", "-Djava.awt.headless=true")
  }

  it should "count as done only when its spec said it captured" in {
    succeeded("uk", Seq("noise", "[full-uk] captured 812 listings in 640 clusters; 5 queries")) shouldBe true
    succeeded("uk", Seq("[full-pl] captured 812 listings")) shouldBe false
    succeeded("uk", Seq("Run completed", "All tests passed.")) shouldBe false
  }

  it should "report the listings it captured" in {
    capturedListings(Seq("[full-de] captured 1234 listings in 900 clusters; 5 queries")) shouldBe Some(1234)
    capturedListings(Seq("nothing")) shouldBe None
  }

  // ── the report ──

  "The report" should "list every phase with its time and throughput, and the total" in {
    val text = report(Seq(Phase("download pl", 10.0, Some(200.0 -> "MB")), Phase("capture pl", 120.0, Some(600.0 -> "listings"))), wall = 130.0)
    text should include("download pl")
    text should include("20.0 MB/s")
    text should include("5.0 listings/s")
    text should include("total")
    text should include("2m10s")
  }
}
