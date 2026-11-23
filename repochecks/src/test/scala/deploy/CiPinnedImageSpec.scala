package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every image .github/pinned-images.tsv pins is pulled BY DIGEST, GitHub's own registry first, and only through
 * ./.github/actions/pull-pinned-image (scripts/ci/pull-pinned-image.sh).
 *
 * Identity model convergence #1286 (run 44526198751, 2026-11-19) lost a leg in its setup, before any build ran: mongo
 * came from mirror.gcr.io first (stalled past 120 s), then Docker Hub as the CI account (its token expired), then Docker
 * Hub anonymously (stalled again). The pin now lives in one table, GHCR holds a copy of its exact digest
 * (.github/workflows/mirror-pinned-images.yml), and one action pulls it from there first. A workflow, action or CI
 * script naming the image itself -- `mongo:<tag>`, `mirror.gcr.io/library/mongo:...`, `docker.io/...` -- goes back to
 * the two registries that failed, and to whatever the tag points at that day.
 */
class CiPinnedImageSpec extends AnyFlatSpec with Matchers {
  private val Pins   = ".github/pinned-images.tsv"
  private val Action = "./.github/actions/pull-pinned-image"
  private val Owner  = "pawelkrupinski"

  private final case class Pin(name: String, repository: String, tag: String, digest: String, mirror: String)
  private val Row = """([\w-]+)\t(docker\.io/[\w./-]+):([\w][\w.-]*)@(sha256:[0-9a-f]{64})\t(\S+)""".r

  private lazy val rows: Seq[String] =
    RepoFile.read(Pins).linesIterator.filterNot(l => l.trim.isEmpty || l.startsWith("#")).toSeq
  private lazy val pins: Seq[Pin] = rows.collect { case Row(n, r, t, d, m) => Pin(n, r, t, d, m) }

  "every pin" should "name its upstream image by tag AND digest, and its GHCR mirror under this repo's owner" in {
    rows should not be empty
    withClue(s"$Pins rows must read <name> TAB docker.io/<repository>:<tag>@sha256:<digest> TAB <mirror>: ") {
      rows.filterNot(Row.matches) shouldBe empty
    }
    pins.foreach(p => withClue(s"${p.name}'s mirror: ")(p.mirror shouldBe s"ghcr.io/$Owner/${p.name}"))
    pins.map(_.name).distinct.size shouldBe pins.size
  }

  // `mongo:` bare or registry-qualified (`docker.io/library/mongo:`, `mirror.gcr.io/library/mongo@`), as the other
  // image specs read a reference.
  private def reference(pin: Pin) =
    ("""(?<![\w.-])(?:[\w.-]+/)*""" + java.util.regex.Pattern.quote(pin.repository.split('/').last) + """[:@][\w$]""").r

  // the shell tools and the Python ones (start_mongo_replset.py runs the mongo image), never their tests
  private def ciScripts: Seq[String] = RepoFile.listed("scripts/ci") { f =>
    val name = f.getName
    (name.endsWith(".sh") && !name.endsWith("-test.sh")) || (name.endsWith(".py") && !name.endsWith("_test.py"))
  }.map(_.getPath)

  "no workflow, action or CI script" should "name a pinned image itself, rather than pull it through the shared action" in {
    pins should not be empty
    val named = for {
      path <- RepoFile.ciFiles() ++ ciScripts
      code  = RepoFile.withoutComments(RepoFile.read(path))
      pin  <- pins
      hit  <- reference(pin).findFirstIn(code)
    } yield s"$path names `$hit` (pin ${pin.name})"
    withClue(s"pull a pinned image with $Action (scripts/ci/pull-pinned-image.sh), by its $Pins row: ")(named shouldBe empty)
  }

  "every CI mongod" should s"start after $Action pulled the pinned image, never from a step's own `run:`" in {
    val direct = for {
      path <- RepoFile.ciFiles()
      step <- RepoFile.runSteps(RepoFile.read(path))
      if step.script.contains("start-mongo-replset.sh")
    } yield s"$path: ${step.script.linesIterator.find(_.contains("start-mongo-replset.sh")).getOrElse("")}"
    withClue(s"start MongoDB with `uses: $Action` (image: mongo, then: scripts/ci/start-mongo-replset.sh): ")(direct shouldBe empty)

    val starting = RepoFile.ciFiles().filterNot(_ == s"${Action.stripPrefix("./")}/action.yml")
      .filter(p => RepoFile.withoutComments(RepoFile.read(p)).contains("start-mongo-replset.sh"))
    starting should not be empty
    all(starting.map(p => RepoFile.withoutComments(RepoFile.read(p)))) should include(s"uses: $Action")
  }

  "the shared action" should "pull through pull-pinned-image.sh, with the job's own token for GHCR" in {
    val action = RepoFile.read(".github/actions/pull-pinned-image/action.yml")
    action should include("scripts/ci/pull-pinned-image.sh")
    action should include("default: ${{ github.token }}")
    val script = RepoFile.withoutComments(RepoFile.read("scripts/ci/pull-pinned-image.sh"))
    // GHCR first, then mirror.gcr.io, then Docker Hub (after only a daemon's own Docker Hub mirror)
    val ghcr = RepoFile.positionOf(script, "pull \"$ghcr_ref\"")
    ghcr should be < RepoFile.positionOf(script, "pull \"$mirror_gcr_ref\"")
    RepoFile.positionOf(script, "pull \"$mirror_gcr_ref\"") should be < script.lastIndexOf("pull \"$hub_ref\"")
    script should include("@$digest")
  }

  "the mirror workflow" should "copy every pin to GHCR weekly and whenever the pins change, with a token that may write it" in {
    val yml = RepoFile.withoutComments(RepoFile.read(".github/workflows/mirror-pinned-images.yml"))
    yml should include("schedule:")
    yml should include("workflow_dispatch:")
    yml should include(s"- $Pins")
    yml should include("packages: write")
    yml should include("scripts/ci/mirror-pinned-images.sh")
    RepoFile.read("scripts/ci/mirror-pinned-images.sh") should include("--preserve-digests")
  }
}
