package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * TWO FILES HAVE TO AGREE ON A PORT NUMBER, AND NOTHING CHECKED THAT UNTIL NOW.
 *
 * Prometheus runs on monitoring-1, OUTSIDE the cluster, and holds no kubeconfig
 * — deliberately, because `kubernetes_sd_configs` would trade one endpoint's
 * discovery for a credential that can read every Secret in the cluster. The
 * consequence is that in-cluster exporters are reached through NodePorts at
 * FIXED numbers, written down in two places that never reference each other:
 * the `Service` manifest that publishes the port, and
 * `fleet.prometheus.*Targets` in roles/prometheus.nix that scrapes it.
 *
 * Both files say, in their own comments, that nothing verifies the pair — and
 * describe the same symptom: `TargetDown`, which reads as the exporter being
 * broken rather than as a typo. For Flux that misreading is worse than usual,
 * because Flux IS the deploy path now: a wrong port here means the alerts that
 * exist to prove deploys are still happening would themselves be down, and the
 * thing they watch would be unwatched for exactly as long as it took somebody
 * to disbelieve the alert.
 *
 * This is that check.
 */
class PrometheusNodePortAgreementSpec extends AnyFlatSpec with Matchers {

  private lazy val prometheusNix = RepoFile.read("infra/nix/modules/roles/prometheus.nix")

  /** Every `nodePort:` published by a Service manifest. */
  private def publishedPorts(path: String): Set[Int] =
    RepoFile
      .read(path)
      .linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collect { case s"nodePort: $port" => port.trim.toInt }
      .toSet

  /** One `<name> = lib.mkOption { ... }` block out of prometheus.nix.
   *
   *  Not `RepoFile.block`, which keys off a `key:` line and so reads YAML but
   *  not Nix. Same idea though: take the declaration and everything indented
   *  under it, stopping when the indentation returns to the declaration's own. */
  private def optionBlock(option: String): String = {
    val lines = prometheusNix.linesIterator.toVector
    val start = lines.indexWhere(_.trim.startsWith(s"$option = lib.mkOption"))
    require(start >= 0, s"prometheus.nix has no `$option` option")
    val indent = lines(start).takeWhile(_ == ' ').length
    val body = lines
      .drop(start + 1)
      .takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent)
    (lines(start) +: body).mkString("\n")
  }

  /** Every port Prometheus is told to scrape, out of one option's default list. */
  private def scrapedPorts(option: String): Set[Int] = {
    val block = optionBlock(option)
    val ports = ":([0-9]{4,5})\"".r.findAllMatchIn(block).map(_.group(1).toInt).toSet
    withClue(s"no `address:port` defaults found under `$option`: ")(ports should not be empty)
    ports
  }

  "the Flux metrics Services and Prometheus" should "agree on every NodePort" in {
    // Four controllers, four ports. A missing one is a controller nobody watches;
    // a wrong one is an alert that fires about the wrong thing.
    val published = publishedPorts("infra/kubernetes/flux-metrics/services.yaml")
    withClue("all four controllers must publish metrics: ")(published should have size 4)
    withClue(s"published $published, scraped ${scrapedPorts("fluxTargets")}: ") {
      scrapedPorts("fluxTargets") shouldBe published
    }
  }

  "kube-state-metrics and Prometheus" should "agree on its NodePort" in {
    val published = publishedPorts("infra/kubernetes/kube-state-metrics/deployment.yaml")
    withClue(s"published $published, scraped ${scrapedPorts("kubeStateMetricsTargets")}: ") {
      scrapedPorts("kubeStateMetricsTargets") shouldBe published
    }
  }

  "the published NodePorts" should "not collide with each other" in {
    // One number reused is the nastiest version of this bug: both Services apply
    // cleanly, one of them silently does not get the port, and its target goes
    // down for a reason neither manifest mentions.
    val flux = publishedPorts("infra/kubernetes/flux-metrics/services.yaml").toSeq
    val ksm = publishedPorts("infra/kubernetes/kube-state-metrics/deployment.yaml").toSeq
    val all = flux ++ ksm
    withClue(s"duplicate NodePort across the cluster's exporters: $all: ") {
      all.distinct should have size all.size
    }
  }
}
