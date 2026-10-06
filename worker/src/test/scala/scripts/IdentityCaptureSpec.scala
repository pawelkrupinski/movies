package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.jdk.CollectionConverters._

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

  they should "force a capture or a fill, never both" in {
    parse(Seq("--capture", "pl")).toOption.get.forced shouldBe Some(Mode.Capture)
    parse(Seq("--fill")).toOption.get.forced shouldBe Some(Mode.Fill)
    parse(Nil).toOption.get.forced shouldBe None
    parse(Seq("--capture", "--fill")).isLeft shouldBe true
  }

  they should "run two countries side by side unless told otherwise" in {
    parse(Nil).toOption.get.parallel shouldBe 2
    parse(Seq("--parallel", "3")).toOption.get.parallel shouldBe 3
    parse(Seq("--parallel", "0")).isLeft shouldBe true
  }

  // ── countries side by side ──

  "The countries side by side" should "be as many as asked, as memory holds and as there are countries" in {
    budget(requested = 2, countries = 5, heap = "12g", memoryGb = 36) shouldBe Budget(parallel = 2, perHost = 2)
    budget(requested = 3, countries = 5, heap = "12g", memoryGb = 36) shouldBe Budget(parallel = 2, perHost = 2)
    budget(requested = 2, countries = 5, heap = "12g", memoryGb = 16) shouldBe Budget(parallel = 1, perHost = 4)
    budget(requested = 4, countries = 1, heap = "8g", memoryGb = 64) shouldBe Budget(parallel = 1, perHost = 4)
    budget(requested = 2, countries = 5, heap = "12g", memoryGb = 8) shouldBe Budget(parallel = 1, perHost = 4)
  }

  "A heap" should "read in gigabytes, as the JVM spells it" in {
    heapGigabytes("12g") shouldBe 12
    heapGigabytes("12G") shouldBe 12
    heapGigabytes("8192m") shouldBe 8
  }

  "The run" should "start the largest countries first and never hold more JVMs than its budget" in {
    val dir = java.nio.file.Files.createTempDirectory("identity-capture-spec")
    val now = new java.util.concurrent.atomic.AtomicInteger
    val peak = new java.util.concurrent.atomic.AtomicInteger
    val started = java.util.Collections.synchronizedList(new java.util.ArrayList[String])
    val pair = new java.util.concurrent.CyclicBarrier(2)
    val effects = new Effects {
      def newestRecording(): Option[String] = fail("the caller's corpus needs no lookup")
      def fetchCorpus(run: String, cc: String, layout: Layout): Long = fail("the caller's corpus is not fetched")
      def fetchTree(run: String, cc: String, layout: Layout): Long = fail("the caller's tree is not fetched")
      def exportFamilies(db: String, into: Path): Long = fail("the caller's seed needs no export")
      def run(job: Job): Seq[String] = {
        started.add(job.cc); peak.accumulateAndGet(now.incrementAndGet(), math.max)
        // two JVMs at once or none: each waits for a partner, which a run one at a time never sends
        pair.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
        now.decrementAndGet()
        Seq(s"[full-${job.cc}] captured 10 listings in 5 clusters")
      }
    }
    val env = Map("KINOWO_IDENTITY_CORPUS_DIR" -> dir.toString, "KINOWO_FIXTURE_ROOT" -> dir.toString,
      "KINOWO_IDENTITY_FAMILY_SEED" -> dir.toString, "KINOWO_IDENTITY_UNMATCHED_CAPTURE" -> dir.toString)
    val lines = Seq.newBuilder[String]
    val ok = capture(parse(Seq("pl", "es", "us", "uk")).toOption.get, env, Layout(dir, dir), effects, "cp", Nil, "abc", memoryGb = 64,
      out = line => lines.synchronized { lines += line; () })
    ok shouldBe true
    peak.get shouldBe 2
    started.asScala.take(2).toSet shouldBe Set("us", "uk")
    lines.result().mkString("\n") should include("2 side by side")
  }

  // ── capture or fill ──

  private val now = Inputs("37", "abc")

  "A country" should "fill when its fixture's inputs are unchanged: the same recording, the same resolver code" in {
    choose("pl", fixture = true, stamped = Some(now), current = Some(now), forced = None) shouldBe
      Choice("pl", Mode.Fill, "the fixture's decisions are current: recording 37, resolver code abc")
  }

  it should "capture when it has no fixture yet" in {
    choose("pl", fixture = false, stamped = None, current = Some(now), forced = None).mode shouldBe Mode.Capture
  }

  it should "capture when the fixture's inputs were never stamped" in {
    choose("pl", fixture = true, stamped = None, current = Some(now), forced = None) shouldBe
      Choice("pl", Mode.Capture, "pl.inputs is missing: the fixture's inputs are unknown")
  }

  it should "capture when a newer recording moved the corpus" in {
    choose("pl", fixture = true, stamped = Some(Inputs("36", "abc")), current = Some(now), forced = None) shouldBe
      Choice("pl", Mode.Capture, "the corpus moved: captured from recording 36, now 37")
  }

  it should "capture when the code deciding the model's decisions changed" in {
    choose("pl", fixture = true, stamped = Some(Inputs("37", "old")), current = Some(now), forced = None) shouldBe
      Choice("pl", Mode.Capture, "the resolver's code changed since the capture (old → abc)")
  }

  it should "capture when no recording is at hand to compare with" in {
    choose("pl", fixture = true, stamped = Some(now), current = None, forced = None).mode shouldBe Mode.Capture
  }

  it should "do what it was told, and refuse to fill a fixture that is not there" in {
    choose("pl", fixture = true, stamped = Some(now), current = Some(now), forced = Some(Mode.Capture)) shouldBe
      Choice("pl", Mode.Capture, "--capture given")
    choose("pl", fixture = true, stamped = None, current = Some(now), forced = Some(Mode.Fill)) shouldBe
      Choice("pl", Mode.Fill, "--fill given")
    an[IllegalArgumentException] should be thrownBy choose("pl", fixture = false, stamped = None, current = Some(now), forced = Some(Mode.Fill))
  }

  "A fixture's inputs" should "round-trip through their stamp file" in {
    Inputs.parse(Inputs("37", "abc").render) shouldBe Some(Inputs("37", "abc"))
    Inputs.parse("garbage") shouldBe None
  }

  "The code deciding the model's decisions" should "be the resolver and what feeds it, never the agreement stage" in {
    isDecisionInput("common/src/main/scala/services/identity/IdentityResolver.scala") shouldBe true
    isDecisionInput("common/src/main/resources/identity-weights.json") shouldBe true
    isDecisionInput("common/src/main/scala/services/titlerules/ExtraTitleRules.scala") shouldBe true
    isDecisionInput("worker/src/it/scala/IdentityShadow.scala") shouldBe true
    isDecisionInput("common/src/main/scala/services/identity/agreement/AgreementStage.scala") shouldBe false
    isDecisionInput("web/src/main/scala/controllers/MovieController.scala") shouldBe false
  }

  "A fill" should "hand its JVM the fixture to fill, the country, and the live answers' sources" in {
    val env = fillEnvironment("de", Map("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k", FamilyUri -> "mongodb://prod"), layout, perHost = 2)
    env shouldBe Map(
      "KINOWO_IDENTITY_UNMATCHED_FILL"     -> "/repo/test/resources/fixtures/identity-unmatched",
      "KINOWO_IDENTITY_FULL"               -> "de",
      "KINOWO_IDENTITY_AGREEMENT_CACHE"    -> "/repo/target/identity-capture/agreement-cache",
      "KINOWO_IDENTITY_POSTER_CACHE"       -> "/repo/target/identity-capture/posters",
      "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k",
      "KINOWO_IDENTITY_LIVE_PER_HOST"      -> "2")
  }

  it should "count as done only when its spec said it filled" in {
    succeeded("de", Mode.Fill, Seq("[de] filled after 2 round(s): 40 family answers, 3 finds")) shouldBe true
    succeeded("de", Mode.Fill, Seq("[full-de] captured 1 listings")) shouldBe false
  }

  // ── the environment each country's JVM is handed ──

  "A country's environment" should "default every variable the capture reads, and give the country its own Mongo database" in {
    val env = environment("uk", Map("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k"), layout, perHost = 2)
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
      "MONGODB_DB"                        -> "kinowo_capture_uk",
      "KINOWO_IDENTITY_LIVE_PER_HOST"     -> "2")
  }

  it should "keep a variable the caller set, and never hand the family export's prod URI to the capture" in {
    val env = environment("pl", Map("KINOWO_IDENTITY_CORPUS_DIR" -> "/mine", "MONGODB_URI" -> "mongodb://127.0.0.1:27017/",
      FamilyUri -> "mongodb://prod", "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> "k"), layout, perHost = 4)
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
    succeeded("uk", Mode.Capture, Seq("noise", "[full-uk] captured 812 listings in 640 clusters; 5 queries")) shouldBe true
    succeeded("uk", Mode.Capture, Seq("[full-pl] captured 812 listings")) shouldBe false
    succeeded("uk", Mode.Capture, Seq("Run completed", "All tests passed.")) shouldBe false
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
