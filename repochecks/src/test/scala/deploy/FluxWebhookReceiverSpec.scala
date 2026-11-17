package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A DEPLOY WAITS ON NO FIVE-MINUTE POLL (2026-11-17, owner's "2a"). Before, a green build sat
 * through two back-to-back polls: the registry scan (tag -> gitops commit, 2.8 min p50 / 4.6 p90)
 * and the `movies` GitRepository noticing that commit (-> pod created, 3.4 / 5.0). Now the movies
 * scans and the automation run every minute, and GitHub's push webhook reaches a Flux `Receiver`
 * that reconciles the source at once.
 *
 * The webhook path crosses three files in two repositories that never name each other -- the
 * Receiver and its pinned Service (movies-gitops `image-automation/receiver.yaml`), the Caddy vhost
 * that publishes it (`infra/nix/hosts/monitoring-1/default.nix`), and the token's vault row
 * (`infra/k8s-secrets/secrets.map`). Any one drifting does not fail anything visibly: Flux falls
 * back to its 5-minute poll and deploys just get slow again. So the agreement is asserted here.
 */
class FluxWebhookReceiverSpec extends AnyFlatSpec with Matchers {

  private val ImageAutomationDir = "infra/kubernetes/image-automation"

  /** Every YAML document of a manifest, comments dropped. */
  private def documents(path: String): Seq[String] =
    RepoFile.withoutComments(RepoFile.read(path)).split("(?m)^---\\s*$").map(_.trim).filter(_.nonEmpty).toSeq

  private def ofKind(path: String, kind: String): Seq[String] =
    documents(path).filter(_.linesIterator.exists(_.trim == s"kind: $kind"))

  private def field(document: String, key: String): Option[String] =
    document.linesIterator.map(_.trim).collectFirst { case l if l.startsWith(s"$key:") => l.stripPrefix(s"$key:").trim }

  private def named(documents: Seq[String], name: String): String =
    documents.find(d => field(d, "name").contains(name)).getOrElse(fail(s"no object named `$name`"))

  private lazy val automation = s"$ImageAutomationDir/automation.yaml"
  private lazy val receiverManifest = s"$ImageAutomationDir/receiver.yaml"

  "the movies ImageRepositories" should "scan the registry every minute" in {
    val movies = ofKind(automation, "ImageRepository").filter(field(_, "image").exists(_.startsWith("registry.kinowo.net/movies-")))
    movies should not be empty
    every(movies.map(field(_, "interval"))) shouldBe Some("1m")
  }

  "the ImageUpdateAutomation" should "commit a newly picked tag within a minute" in {
    val automations = ofKind(automation, "ImageUpdateAutomation")
    automations should have size 1
    field(automations.head, "interval") shouldBe Some("1m")
  }

  "the Receiver" should "reconcile the source every Kustomization reads, on a GitHub push" in {
    RepoFile.exists(receiverManifest) shouldBe true
    val receiver = ofKind(receiverManifest, "Receiver").headOption.getOrElse(fail(s"no Receiver in $receiverManifest"))
    field(receiver, "type") shouldBe Some("github")
    receiver should include("- push")
    val targets = receiver.linesIterator.map(_.trim).dropWhile(_ != "resources:").mkString("\n")
    targets should include("kind: GitRepository")
    targets should include("name: movies")
    val sync = RepoFile.read("infra/kubernetes/flux/gotk-sync.yaml")
    sync should include("name: movies\n")
  }

  it should "be served by a notification-controller the bootstrap components install" in {
    val components = RepoFile.read("infra/kubernetes/flux/gotk-components.yaml")
    components should include("image: ghcr.io/fluxcd/notification-controller:")
    components should include("name: receivers.notification.toolkit.fluxcd.io")
  }

  it should "take its token from a vault row push-k8s-secrets pushes" in {
    val receiver = ofKind(receiverManifest, "Receiver").head
    val secretName = receiver.linesIterator.map(_.trim).dropWhile(_ != "secretRef:").drop(1).collectFirst {
      case l if l.startsWith("name:") => l.stripPrefix("name:").trim
    }.getOrElse(fail("the Receiver names no secretRef"))
    val rows = RepoFile.read("infra/k8s-secrets/secrets.map").linesIterator.map(_.trim.split("\\s+").toSeq)
    withClue(s"secrets.map needs a `flux-system/$secretName token <VAULT_KEY>` row: ") {
      rows.exists(r => r.take(2) == Seq(s"flux-system/$secretName", "token")) shouldBe true
    }
  }

  "monitoring-1's flux.kinowo.net vhost" should "proxy /hook to the Receiver's pinned Service, and nothing else" in {
    val service = named(ofKind(receiverManifest, "Service"), "webhook-receiver-pinned")
    service should include("app: notification-controller")
    field(service, "targetPort") shouldBe Some("http-webhook")
    val upstream = s"${field(service, "clusterIP").getOrElse(fail("the Service pins no clusterIP"))}:${field(service, "port").get}"

    val host = RepoFile.withoutComments(RepoFile.read("infra/nix/hosts/monitoring-1/default.nix"))
    val vhost = "(?s)\"flux\\.kinowo\\.net\"\\s*=\\s*\\{(.*?)\\n\\s*\\};".r
      .findFirstMatchIn(host).map(_.group(1)).getOrElse(fail("no flux.kinowo.net vhost on monitoring-1"))
    vhost should include(s"""pathUpstreams."/hook" = "$upstream";""")
    withClue("the vhost must publish only /hook (no `upstream` fallback) and must not ask GitHub to sign in: ") {
      vhost should not include "upstream ="
      vhost should not include "requireGoogleLogin"
    }
  }
}
