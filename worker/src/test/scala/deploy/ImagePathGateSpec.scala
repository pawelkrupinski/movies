package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/**
 * EACH TIER'S PUBLISH GATE COVERS EVERYTHING ITS IMAGE IS BUILT FROM.
 *
 * A tier's image is published (and so deployed) only when its `changed-<tier>` filter in `main.yml` matched a path
 * changed since the tier's `deployed-<tier>` marker ([[K8sTierPathGatingSpec]]). The filter is hand-kept, the image is
 * not: `test/resources/fixtures/identity-judgements.tsv`, bundled into the worker as a resource, was missing from the
 * worker's filter, so an edit to it reached prod only with the next unrelated worker deploy. A path the image holds that
 * its filter misses ships late, silently.
 *
 * The image's inputs come from the build (`project/ImageInputs.scala`: every main source and resource of the
 * deployable's compile closure, the repository files its build bundles, the build definition and the Dockerfile),
 * staged as `image-inputs/<tier>.txt` among these specs' resources. The filter is the real one out of `main.yml`, read
 * with the matcher's semantics ([[ChangedPathsFilter]]; [[K8sTierPathGatingIntegrationSpec]] proves them the same as
 * the action's `matches.sh` over these same paths).
 */
class ImagePathGateSpec extends AnyFlatSpec with Matchers {

  Seq("worker" -> K8sTierPathGatingSpec.workerFilter, "web" -> K8sTierPathGatingSpec.webFilter).foreach {
    case (tier, filter) =>
      s"the $tier tier's publish gate" should "match every path its image is built from" in {
        val missed = ImagePathGateSpec.imageInputs(tier).filterNot(ChangedPathsFilter(filter).matches)
        withClue(s"main.yml's `changed-$tier` filter misses paths the $tier image is built from, so a change to them " +
            s"would not publish it -- add a pattern covering each: ")(missed shouldBe empty)
      }
  }

  "the worker's image inputs" should "hold the resources its build bundles from outside any module" in {
    ImagePathGateSpec.imageInputs("worker") should contain allOf (
      "test/resources/fixtures/identity-judgements.tsv",
      "infra/jvm/worker.options",
      "infra/nix/files/heap-dumps.sh",
      "scrapers/src/main/scala/services/cinemas/CinemaScraperCatalog.scala",
      "Dockerfile",
      "build.sbt")
    ImagePathGateSpec.imageInputs("worker") should not contain "infra/jvm/web.options"
    withClue("a test-only dependency (testkit, itkit) ships nothing: ")(
      ImagePathGateSpec.imageInputs("worker").filter(path => path.startsWith("testkit/") || path.startsWith("itkit/")) shouldBe empty)
    withClue("the web app's code is no worker input: ")(
      ImagePathGateSpec.imageInputs("worker").filter(_.startsWith("web/")) shouldBe empty)
  }

  /** The self-test: the worker's filter as it stood before identity-judgements.tsv was added is caught. */
  "the check" should "catch a filter that misses a bundled resource" in {
    val withoutJudgements = K8sTierPathGatingSpec.workerFilter.filterNot(_.startsWith("test/resources/"))
    ImagePathGateSpec.imageInputs("worker").filterNot(ChangedPathsFilter(withoutJudgements).matches) should
      contain("test/resources/fixtures/identity-judgements.tsv")
  }

  "the filter's reading" should "follow matches.sh: `dir/**` a prefix, anything else exact, any `!` match excluding" in {
    val filter = ChangedPathsFilter(Seq("worker/**", "!worker/src/test/**", "build.sbt", "# a comment", "- 'Dockerfile'"))
    filter.matches("worker/src/main/scala/A.scala") shouldBe true
    filter.matches("worker/src/test/scala/ASpec.scala") shouldBe false
    filter.matches("build.sbt") shouldBe true
    filter.matches("build.sbt.bak") shouldBe false
    filter.matches("Dockerfile") shouldBe true
    filter.matches("workers/x") shouldBe false
  }
}

object ImagePathGateSpec {

  /** The paths the build says `tier`'s image is built from (`image-inputs/<tier>.txt`, staged by the build). */
  def imageInputs(tier: String): Vector[String] = {
    val resource = Option(getClass.getResource(s"/image-inputs/$tier.txt"))
      .getOrElse(throw new IllegalStateException(s"no image-inputs/$tier.txt among the test resources: the build stages it"))
    val source = Source.fromURL(resource, "UTF-8")
    try source.getLines().filter(_.nonEmpty).toVector
    finally source.close()
  }
}

/** A `changed-paths` filter set, read as `.github/actions/changed-paths/matches.sh` reads it: lines trimmed, a leading
 *  `- ` and surrounding quotes dropped, blank and `#` lines ignored; a pattern ending `/` and two stars matches anything under that directory, anything
 *  else exactly that path; a path matches when an include does and no `!` exclude does. */
final case class ChangedPathsFilter(patterns: Seq[String]) {

  private val normalised = patterns.map(_.trim.replaceFirst("^-\\s*", "").replaceAll("^['\"]|['\"]$", ""))
    .filter(line => line.nonEmpty && !line.startsWith("#"))

  private val (excludes, includes) = normalised.partition(_.startsWith("!")) match {
    case (excluded, included) => (excluded.map(_.drop(1)), included)
  }

  private def under(path: String, pattern: String): Boolean =
    if (pattern.endsWith("/**")) path.startsWith(pattern.stripSuffix("**")) else path == pattern

  def matches(path: String): Boolean = includes.exists(under(path, _)) && !excludes.exists(under(path, _))
}
