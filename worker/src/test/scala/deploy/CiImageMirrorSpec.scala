package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every image CI pulls comes through mirror.gcr.io, Google's pull-through cache of Docker Hub,
 * never from Docker Hub itself.
 *
 * GitHub's runners pull anonymously from shared egress IPs. Docker Hub refused `mongo:8.3.11` with
 * its anonymous rate limit (run 37989550418), and a Main run lost both image builds to its token
 * endpoint answering 504 / timing out: buildx booting `moby/buildkit`, and the Dockerfile's
 * `ubuntu` base (run 37993027475). A login does not help the second -- the login IS that endpoint.
 * The mirror serves the same images with neither failure, so any pull that goes back to Docker Hub
 * brings both back. (start-mongo-replset.sh may still fall back to Docker Hub when the mirror
 * refuses; its first choice is what this checks.)
 */
class CiImageMirrorSpec extends AnyFlatSpec with Matchers {
  private val Mirror = "mirror.gcr.io/"

  "the Dockerfile" should "build FROM mirrored base images" in {
    val bases = """(?m)^FROM\s+(\S+)""".r.findAllMatchIn(RepoFile.read("Dockerfile")).map(_.group(1)).toSeq
    bases should not be empty
    all(bases) should startWith(Mirror)
  }

  "every workflow job container" should "be a mirrored image" in {
    val images = for {
      file <- RepoFile.workflows()
      hit  <- """(?m)^\s*container:\s*([^\s{#]+)\s*$""".r.findAllMatchIn(RepoFile.read(file.getPath))
    } yield s"${file.getName}: ${hit.group(1)}"
    images should not be empty
    all(images.map(_.dropWhile(_ != ' ').trim)) should startWith(Mirror)
  }

  "every buildx builder" should "boot BuildKit from the mirror" in {
    for (file <- RepoFile.workflows()) {
      val text = RepoFile.read(file.getPath)
      withClue(s"${file.getName}: every docker/setup-buildx-action needs driver-opts image=${Mirror}moby/buildkit: ") {
        s"image=${Mirror}moby/buildkit:".r.findAllIn(text).size shouldBe "uses: docker/setup-buildx-action@".r.findAllIn(text).size
      }
    }
  }

  "the CI mongod" should "be pulled from the mirror first" in {
    """(?m)^image=(\S+)""".r.findFirstMatchIn(RepoFile.read("scripts/ci/start-mongo-replset.sh")).map(_.group(1)).getOrElse("") should startWith(Mirror)
  }
}
