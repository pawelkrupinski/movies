package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * CI'S ONLY WAY TO DEPLOY DEPENDS ON ONE FILE'S PERMISSION BITS, and the process
 * that owns that file resets them.
 *
 * `roles/k8s-deploy.nix` runs its forced command as the `k8sdeploy` account,
 * which reads the cluster-admin kubeconfig at /etc/rancher/k3s/k3s.yaml. The
 * grant is a systemd-tmpfiles `z` rule setting `0640 root k8sdeploy`, applied at
 * boot. But k3s REWRITES that file `0600 root:root` every time the server
 * starts, so any restart of k3s.service revokes the grant until the next boot.
 *
 * The failure is remote from its cause in both time and appearance: deploys keep
 * working until the next one is attempted, then fail with `error loading config
 * file ... permission denied` — pointing at CI, at the ssh key, at the endpoint,
 * at anything except the unrelated-looking change that restarted k3s hours
 * earlier. It cost an hour of broken deploys on 2026-09-03, when enabling
 * API-server OIDC restarted k3s.
 *
 * The durable half of the fix is asking k3s to write the mode itself, so the
 * grant is re-applied by the very thing that was destroying it. This spec holds
 * the two halves in agreement, because either one alone is a trap: the tmpfiles
 * rule alone is undone by a restart, and the k3s flag alone sets a mode with no
 * group to read it.
 */
class K8sDeployKubeconfigAccessSpec extends AnyFlatSpec with Matchers {

  private lazy val server = RepoFile.read("infra/nix/modules/roles/k3s-server.nix")
  private lazy val deploy = RepoFile.read("infra/nix/modules/roles/k8s-deploy.nix")

  /** The mode k3s is told to write the kubeconfig with, e.g. "0640". */
  private def k3sWriteMode: String =
    "--write-kubeconfig-mode=([0-7]{3,4})".r
      .findFirstMatchIn(server)
      .map(_.group(1))
      .getOrElse(
        fail("k3s-server.nix must set --write-kubeconfig-mode, or a k3s restart revokes CI's deploy access")
      )

  /** The tmpfiles line granting the deploy account access, split into fields. */
  private def tmpfilesRule: Seq[String] =
    deploy.linesIterator
      .map(_.trim)
      .find(l => l.contains("/etc/rancher/k3s/k3s.yaml") && l.startsWith("\"z "))
      .map(_.stripPrefix("\"").stripSuffix("\"").split("\\s+").toSeq)
      .getOrElse(fail("k8s-deploy.nix must keep a tmpfiles rule granting k8sdeploy the kubeconfig"))

  private def groupDigit(mode: String) = mode.takeRight(2).head.asDigit

  "k3s" should "write the kubeconfig group-readable, so a restart does not revoke CI's deploy access" in {
    withClue(s"--write-kubeconfig-mode=$k3sWriteMode leaves the group unable to read: ") {
      groupDigit(k3sWriteMode) & 4 should not be 0
    }
  }

  it should "not write it world-readable — the file is cluster-admin" in {
    // The whole reason CI is given an ssh forced command instead of this file is
    // that holding it IS the cluster. Widening it to `other` would hand it to
    // every account on the box.
    withClue(s"--write-kubeconfig-mode=$k3sWriteMode is world-accessible: ") {
      k3sWriteMode.last.asDigit shouldBe 0
    }
  }

  "the tmpfiles grant and the k3s flag" should "agree, since either alone is a trap" in {
    val fields = tmpfilesRule
    withClue(s"malformed tmpfiles rule: ${fields.mkString(" ")}: ")(fields.length should be >= 5)
    val (path, mode, owner, group) = (fields(1), fields(2), fields(3), fields(4))
    path shouldBe "/etc/rancher/k3s/k3s.yaml"
    owner shouldBe "root"
    withClue("the group k3s's mode grants must be the deploy account's: ")(group shouldBe "k8sdeploy")
    withClue(s"tmpfiles writes $mode, k3s writes $k3sWriteMode: ") {
      mode.takeRight(3) shouldBe k3sWriteMode.takeRight(3)
    }
  }
}
