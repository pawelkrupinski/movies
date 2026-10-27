package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.sys.process.*

/** The cases of [[K8sTierPathGatingSpec]] that run the real `changed-paths` matcher (bash) over each tier's filter set. */
class K8sTierPathGatingIntegrationSpec extends AnyFlatSpec with Matchers {

  private val Matcher = ".github/actions/changed-paths/matches.sh"

  private lazy val webFilter    = K8sTierPathGatingSpec.webFilter
  private lazy val workerFilter = K8sTierPathGatingSpec.workerFilter

  /** The action's own matcher, over a fabricated push. `true` = this tier rebuilds and redeploys. */
  private def matches(patterns: Seq[String], changed: Seq[String]): Boolean = {
    val patternsFile = Files.createTempFile("changed-paths-patterns", ".txt")
    val changedFile  = Files.createTempFile("changed-paths-files", ".txt")
    try {
      Files.writeString(patternsFile, patterns.mkString("", "\n", "\n"))
      Files.writeString(changedFile, changed.mkString("", "\n", "\n"))
      (Seq("bash", Matcher, patternsFile.toString) #< changedFile.toFile).!!.trim match {
        case "true"  => true
        case "false" => false
        case other   => fail(s"$Matcher printed `$other`, which is neither true nor false")
      }
    } finally {
      Files.deleteIfExists(patternsFile)
      Files.deleteIfExists(changedFile)
    }
  }

  /** A push that touches the web tier and nothing else — the everyday case. */
  private val WebOnlyPush = Seq(
    "web/src/main/scala/controllers/RepertoireController.scala",
    "web/src/main/twirl/views/repertoire.scala.html",
    "web/src/test/resources/expected-poznan.html",
  )

  private val WorkerOnlyPush = Seq(
    "worker/src/main/scala/services/scrape/ScrapeReaper.scala",
    "worker/src/test/scala/deploy/K8sTierPathGatingSpec.scala",
  )

  "a web-only push" should "not match the worker's filter set, so nothing rebuilds or restarts the worker" in {
    withClue(s"worker filter $workerFilter matched $WebOnlyPush: ") {
      matches(workerFilter, WebOnlyPush) shouldBe false
    }
  }

  it should "still match the web's own filter set, or the web tier would stop deploying" in {
    matches(webFilter, WebOnlyPush) shouldBe true
  }

  "a worker-only push" should "not match the web's filter set" in {
    matches(webFilter, WorkerOnlyPush) shouldBe false
  }

  it should "match the worker's own filter set" in {
    matches(workerFilter, WorkerOnlyPush) shouldBe true
  }

  /**
   * The shared inputs are shared on purpose: `common/`, the build definition and
   * the image recipe are compiled into both artifacts, so a change to any of them
   * genuinely does change the worker's deployable artifact.
   */
  it should "still redeploy both tiers when a genuinely shared input moves" in {
    val shared = Seq("common/src/main/scala/models/Movie.scala")
    matches(webFilter, shared) shouldBe true
    matches(workerFilter, shared) shouldBe true
  }

  // The cinema scrapers are their own module, which only the worker is built with. Their specs ship in no image.
  it should "redeploy the worker, not the web, when a scraper moves, and neither for a scraper's spec alone" in {
    val scraper = Seq("scrapers/src/main/scala/services/cinemas/pl/HeliosClient.scala")
    matches(workerFilter, scraper) shouldBe true
    matches(webFilter, scraper) shouldBe false
    matches(workerFilter, Seq("scrapers/src/test/scala/clients/helios/HeliosClientSpec.scala")) shouldBe false
  }

  // The identity resolver is its own module, which both apps are built with: the worker runs it, the web app reads its
  // traces and pins. Its tests ship in neither image.
  it should "redeploy both tiers when the identity module's code moves, and neither for its tests alone" in {
    val resolver = Seq("identity/src/main/scala/services/identity/IncrementalResolver.scala")
    matches(webFilter, resolver) shouldBe true
    matches(workerFilter, resolver) shouldBe true
    matches(workerFilter, Seq("identity/src/test/scala/services/identity/IncrementalResolverSpec.scala")) shouldBe false
  }

  /**
   * The heap-dump budget script lives under infra/ (the Nix flake root, so the node's timer can run
   * the same file) but is copied into BOTH images as `bin/heap-dumps.sh` by build.sbt. A change to
   * it changes both deployable artifacts, so it must redeploy both -- or the pods would keep
   * pruning by the old rules while the node prunes by the new ones.
   */
  it should "redeploy both tiers when the heap-dump script both images ship moves" in {
    val script = Seq("infra/nix/files/heap-dumps.sh")
    matches(webFilter, script) shouldBe true
    matches(workerFilter, script) shouldBe true
  }

  /**
   * build.sbt's `launcherOptions(...)` bakes infra/jvm/<file>.options into each image's launcher, so
   * a change there is a change to that image. Missing from both lists, 9b9aa9e7b's worker JIT flags
   * passed Main and shipped nowhere.
   */
  it should "redeploy the tier whose launcher options move, and only that tier" in {
    matches(workerFilter, Seq("infra/jvm/worker.options")) shouldBe true
    matches(webFilter, Seq("infra/jvm/worker.options")) shouldBe false
    matches(webFilter, Seq("infra/jvm/web.options")) shouldBe true
    matches(workerFilter, Seq("infra/jvm/web.options")) shouldBe false
  }

  /**
   * And the two lists must not converge. The web sources pattern appearing in the
   * worker's set is the one edit that would make every web push restart the worker
   * while every assertion above still passed on the day it was made — the
   * fabricated pushes are a sample, this is the invariant.
   */
  // A worker restart costs ~350 CPU-seconds of JIT and boot re-seeding, and 11 of the 71 pushes that
  // restarted the workers in the 24 h to 2026-10-01 20:50 changed only tests: the image they shipped
  // ran the same code. Tests still run in ci on every push; only the image's publish is gated here.
  "a push that only changes worker or common tests" should "not rebuild or restart the worker" in {
    matches(workerFilter, Seq(
      "worker/src/test/scala/deploy/K8sTierPathGatingSpec.scala",
      "worker/src/it/scala/services/movies/VenueReadIntegrationSpec.scala",
      "worker/src/fixtures/scala/tools/FixturePipeline.scala",
      "common/src/test/scala/services/movies/StringPoolSpec.scala",
      "worker/src/test/resources/fixtures/flicks/page.html",
    )) shouldBe false
  }

  it should "still restart it when the same push also changes the worker's own code" in {
    matches(workerFilter, Seq(
      "worker/src/test/scala/services/tasks/ChunkPageMemoSpec.scala",
      "worker/src/main/scala/services/tasks/ChunkPageMemo.scala",
    )) shouldBe true
    matches(workerFilter, Seq("common/src/main/scala/services/movies/StringPool.scala")) shouldBe true
    matches(workerFilter, Seq("worker/src/main/resources/aot-classes.txt")) shouldBe true
  }

  "the matcher" should "let a `!dir/**` pattern exclude what an include pattern took in, in either order" in {
    matches(Seq("a/**", "!a/b/**"), Seq("a/b/c.txt")) shouldBe false
    matches(Seq("!a/b/**", "a/**"), Seq("a/b/c.txt")) shouldBe false
    matches(Seq("a/**", "!a/b/**"), Seq("a/b/c.txt", "a/d.txt")) shouldBe true
    matches(Seq("a/**", "!a/b.txt"), Seq("a/b.txt")) shouldBe false
    matches(Seq("!a/b/**"), Seq("x.txt")) shouldBe false
  }
}
