package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A BUILD BECOMES A DEPLOY ONLY IF ONE REGEX MATCHES ONE TAG. That is the whole
 * of this spec, and the reason it exists is that nothing else would notice if it
 * stopped being true.
 *
 * Deploys used to be an ssh call from CI naming an image. They are now a
 * handshake between two files that never reference each other: `main.yml`
 * pushes a tag, and an `ImagePolicy` in `infra/kubernetes/flux/` decides which
 * tags are candidates and which of them is newest. If the tag format drifts —
 * a date format changed, the `main-` prefix dropped, the short SHA widened —
 * the policy simply matches nothing. Flux does not fail: it reports that it has
 * no candidate image and keeps the cluster on the last one it saw, so the site
 * stays up serving an increasingly old build while every CI run is green. That
 * is the same silent-staleness shape as the outage image automation replaced,
 * which is why this is asserted rather than trusted.
 *
 * So this spec does not compare two strings and hope. It RUNS the shell that CI
 * runs to build the tag, and feeds the result to the REAL pattern out of the
 * ImagePolicy, asking the question directly: would Flux deploy what CI just
 * built? That run of CI's shell is in [[FluxImageAutomationIntegrationSpec]].
 */
class FluxImageAutomationSpec extends AnyFlatSpec with Matchers {

  private lazy val automation = FluxImageAutomationSpec.automation
  private def policyPattern: String = FluxImageAutomationSpec.policyPattern

  private val Apps = Seq("web", "worker")

  /** The `image:` line of each tier's base Deployment, whichever registry it names
   *  (ghcr.io once, registry.kinowo.net since). */
  private def imageLine(app: String): String = {
    val path = s"infra/kubernetes/$app/base/all.yaml"
    RepoFile
      .read(path)
      .linesIterator
      .find(_.trim.startsWith("image: "))
      .getOrElse(throw new AssertionError(s"no `image:` line in $path"))
      .trim
  }

  "the ImagePolicy" should "refuse tags that cannot be ordered" in {
    // The anchoring matters more than it looks: an unanchored pattern lets
    // `latest` and raw SHAs into the candidate set, and then "newest" is
    // decided by comparing text that encodes no time at all.
    val refused = Seq(
      "latest",
      "b5af6c9f17c2a2b42bff9066db3507f296ebb257",
      "main-2026-b5af6c9",             // timestamp too short
      "main-20260903143022-b5af6c9f",  // sha too long
      "pr-20260903143022-b5af6c9",     // not a main build
      "xmain-20260903143022-b5af6c9"   // prefix not anchored
    )
    refused.foreach { tag =>
      withClue(s"`$tag` must NOT be deployable: ")(tag should not(fullyMatch regex policyPattern))
    }
  }

  it should "order candidates by time, so the newest build wins" in {
    val group = policyPattern.r
    val older = "main-20260903143022-aaaaaaa"
    val newer = "main-20260903150000-bbbbbbb"
    def stamp(tag: String): Long = group.findFirstMatchIn(tag).map(_.group("ts").toLong).getOrElse(0L)
    stamp(newer) should be > stamp(older)
    withClue("`asc` is what makes the HIGHEST timestamp the winner: ") {
      automation should include("order: asc")
    }
  }

  "each tier's Deployment" should "hand its image line to the automation" in {
    Apps.foreach { app =>
      withClue(s"$app: ") {
        imageLine(app) should include(s"""{"$$imagepolicy": "flux-system:movies-$app"}""")
      }
    }
  }

  it should "name a policy that exists" in {
    val declared = automation
      .split("(?m)^---$")
      .filter(_.contains("kind: ImagePolicy"))
      .flatMap(_.linesIterator.map(_.trim).find(_.startsWith("name: ")))
      .map(_.stripPrefix("name: "))
      .toSeq
    declared should contain allElementsOf Apps.map("movies-" + _)
  }

  it should "never be pinned to a moving tag" in {
    // A pod restarting under `latest` can come back on a different build than
    // its siblings, with nothing recording which — the failure that made the
    // SHA tag the deploy pin in the first place.
    Apps.foreach { app =>
      withClue(s"$app: ")(imageLine(app) should not include ":latest")
    }
  }

  "each tier's documented base" should "keep its header where the deploy rewrite cannot drop it" in {
    // Learned the expensive way, twice. The first two automated deploys silently
    // deleted these files' headers — decff51a9 the worker's, f32d34f89 web's —
    // including the block explaining that MONGODB_DB is set nowhere, which is the
    // note standing between a reader and merging every country's corpus into one
    // database.
    //
    // Nothing was red: image-automation rewrites these files through kustomize's
    // YAML round-trip, and that round-trip preserves comments ATTACHED TO A NODE
    // while dropping any that float before the first document. A `---` between the
    // header and `apiVersion:` is exactly what makes it float, so the separator is
    // the whole difference between documentation that survives deploys and
    // documentation that one of them quietly eats.
    Apps.foreach { app =>
      val text = RepoFile.read(s"infra/kubernetes/$app/base/all.yaml")
      withClue(s"$app lost its header: ")(text should include("COUNTRY-AGNOSTIC BASE"))
      val beforeFirstNode = text.linesIterator.takeWhile(!_.startsWith("apiVersion")).map(_.trim).toSeq
      withClue(s"$app: a `---` above the header detaches it, and the next deploy deletes it: ") {
        beforeFirstNode should not contain "---"
      }
    }
  }

  "the automation's commit" should "not start another CI run" in {
    // Without the marker: deploy commit -> CI -> new image -> newer tag ->
    // another deploy commit. The loop never settles and it spends the runner
    // budget doing it.
    automation should include("[skip ci]")
  }

  // A policy still scanning ghcr.io never sees a new tag again, and Flux only says so as an ImageRepository that is not
  // Ready (FluxObjectNotReady firing).
  "every ImageRepository" should "scan our own registry, the one CI publishes to, never ghcr.io" in {
    val images = automation.linesIterator.map(_.trim).filter(_.startsWith("image: ")).map(_.stripPrefix("image: ").trim).toSeq
    images should not be empty
    every(images) should startWith("registry.kinowo.net/")
  }
}

/** The ImagePolicies' own tag pattern, shared with [[FluxImageAutomationIntegrationSpec]]. */
object FluxImageAutomationSpec extends Matchers {

  lazy val automation: String = RepoFile.read("infra/kubernetes/image-automation/automation.yaml")

  /** Every `pattern:` in the ImagePolicies, as a JAVA regex.
   *
   *  Flux compiles these with Go's RE2, whose named group is `(?P<x>)`; Java
   *  spells the same thing `(?<x>)` and throws on the P. The translation is the
   *  ONLY difference between the two dialects that this pattern uses, so a
   *  match here is a match there. */
  private def policyPatterns: Seq[String] =
    automation.linesIterator
      .map(_.trim)
      .filter(_.startsWith("pattern:"))
      .map(_.stripPrefix("pattern:").trim.stripPrefix("'").stripSuffix("'"))
      .map(_.replace("(?P<", "(?<"))
      .toSeq

  def policyPattern: String = {
    val all = policyPatterns
    all should not be empty
    withClue("web and worker must select tags the same way, or one tier silently stops deploying: ") {
      all.distinct should have size 1
    }
    all.head
  }
}
