package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Keeps every throwaway mongod CI and the local scripts start on the SAME server
 * release mongo-1 runs.
 *
 * The production server is `roles/mongodb.nix`'s package; the CI replica set and
 * the local convergence runner are `docker run` of a `mongo:<tag>` image. Nothing tied the two
 * together, and they drifted a whole major apart: CI tested against 7.0 for the
 * month prod ran 8.2, so a query, index or aggregation stage that behaves
 * differently across those releases was green in every layer and only met the real
 * server in production. The tag is pinned to the exact patch, not a floating `8`
 * or `8.3`, so a run is reproducible and a bump is one deliberate edit here plus
 * the nix role — which this spec forces to happen together.
 */
class MongoServerVersionParitySpec extends AnyFlatSpec with Matchers {

  private val role = "infra/nix/modules/roles/mongodb.nix"

  private val dockerStarts = Seq(
    "scripts/ci/start-mongo-replset.sh",
    "scripts/convergence-local.sh"
  )

  private lazy val productionVersion: String =
    """mongodbVersion\s*=\s*"([0-9.]+)"""".r
      .findFirstMatchIn(RepoFile.read(role))
      .map(_.group(1))
      .getOrElse(fail(s"$role no longer names `mongodbVersion = \"x.y.z\"`"))

  // Every `mongo:<tag>` image reference, bare or registry-qualified (`mirror.gcr.io/library/mongo:`,
  // `docker.io/library/mongo:`): the CI script names its image in a variable, a mirror first and
  // Docker Hub as the fallback, and both must be the production release.
  private def dockerTags(path: String): Seq[String] =
    """(?<![\w.-])(?:[\w.-]+/)*mongo:([0-9][0-9.]*)""".r.findAllMatchIn(RepoFile.read(path)).map(_.group(1)).toSeq

  "every docker-started mongod" should "run the exact server release production runs" in {
    dockerStarts.foreach { path =>
      withClue(s"$path must start mongo:$productionVersion, the version $role deploys: ") {
        dockerTags(path) should not be empty
        all(dockerTags(path)) shouldBe productionVersion
      }
    }
  }
}
