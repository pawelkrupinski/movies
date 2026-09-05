package deploy

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{CacheOccupancy, WebCacheMetrics, WebHostMetrics, WebHttpMetrics}

import java.io.File
import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters._

/**
 * Every `kinowo_web_*` family this tier REGISTERS must be drawn on a dashboard —
 * enumerated from the registry, not from a list somebody remembers to update.
 *
 * WHY IT LIVES HERE. The worker's `deploy.GrafanaMetricCoverageSpec` does exactly
 * this for `kinowo_worker_*`, mechanically, and cannot reach the web's registry —
 * it is a module the worker does not depend on. So the web half of that spec is a
 * hand-maintained `WebExportedFamilies` list, and a family left off the list was
 * not caught by the guard, it was INVISIBLE to it. That is not hypothetical:
 * `kinowo_web_response_cache_*` was exported and charted nowhere for its entire
 * life, and the spec whose whole job is to catch that never said a word, because
 * nobody added it to the list.
 *
 * Here the enumeration is the registry itself, so a family added tomorrow is
 * covered the day it lands whether or not anyone remembers this file exists.
 *
 * The worker spec's list still has a job — its REVERSE guard, which reads a panel
 * drawing a family nothing exports as dangling, and needs to know these names to
 * avoid flagging live panels. Forgetting it now fails loudly there rather than
 * quietly here.
 *
 * NOT COVERED, and it cannot be: the families `controllers.MetricsController`
 * renders by hand as text (`kinowo_web_movies_served`, the uptime gauges) never
 * enter a registry, so no enumeration can see them. They stay on the worker
 * spec's list.
 */
class GrafanaWebMetricCoverageSpec extends AnyFlatSpec with Matchers {

  /** Every `kinowo_*` family the web registers, base names, straight from a
   *  registry — NOT from the text exposition, which omits a family with no data
   *  points yet and would quietly under-report the metrics most likely to be
   *  forgotten. Each class is constructed for its REGISTRATION side effect; the
   *  arguments only have to be well-formed, since nothing is scraped for value. */
  private lazy val webFamilies: Seq[String] = {
    val registry = new PrometheusRegistry()
    new WebHttpMetrics(registry, "pl")
    new WebHostMetrics(registry, "pl")
    new WebCacheMetrics(registry, "pl", Seq("probe" -> (() => CacheOccupancy(entries = 0L))))
    registry
      .scrape()
      .asScala
      .map(_.getMetadata.getPrometheusName)
      .filter(_.startsWith("kinowo_"))
      .toSeq
      .distinct
      .sorted
  }

  /** Every provisioned dashboard, found by walking the directory rather than by
   *  naming them — a dashboard added tomorrow counts as coverage without this
   *  spec being edited, which is the same argument as enumerating the registry. */
  private lazy val allDashboardJson: String = {
    def jsonUnder(dir: File): Seq[File] =
      Option(dir.listFiles()).getOrElse(Array.empty[File]).toSeq.flatMap {
        case d if d.isDirectory             => jsonUnder(d)
        case f if f.getName.endsWith(".json") => Seq(f)
        case _                              => Nil
      }
    // Found by walking UP from the working directory rather than assumed relative
    // to it: sbt runs each module's tests from its own `baseDirectory`, so the
    // worker's specs see the repo root and the web's see `web/`. Hard-coding
    // either makes this spec pass vacuously in the other module.
    val relative = "infra/nix/files/monitoring/grafana/dashboards"
    val root = Iterator
      .iterate(new File(".").getAbsoluteFile)(_.getParentFile)
      .takeWhile(_ != null)
      .map(dir => new File(dir, relative))
      .find(_.isDirectory)
      .getOrElse(fail(s"no $relative in any parent of ${new File(".").getAbsolutePath}"))
    jsonUnder(root).sortBy(_.getPath).map { f =>
      val src = Source.fromFile(f)(using Codec.UTF8)
      try src.mkString
      finally src.close()
    }.mkString("\n")
  }

  "every web metric family the registry exports" should "be drawn on a dashboard" in {
    webFamilies should not be empty // a broken enumeration must not pass vacuously

    val orphans = webFamilies.filterNot(allDashboardJson.contains)

    withClue(
      s"exported by the web tier but drawn nowhere: ${orphans.mkString(", ")}. Each costs a series on " +
        "every scrape and shows nobody anything. Add a panel under " +
        "infra/nix/files/monitoring/grafana/dashboards/ — kinowo-http.json is this tier's. "
    ) {
      orphans shouldBe empty
    }
  }

  /** The enumeration is only worth its runtime if it actually reaches the classes
   *  that register. A refactor that moves a metric out of one of the three
   *  constructed above would otherwise shrink this spec to a vacuous pass. */
  it should "reach the cache, host and HTTP families it is enumerating" in {
    webFamilies should contain ("kinowo_web_cache_held_bytes")
    webFamilies should contain ("kinowo_web_host_memory_available_bytes")
    // The client appends `_total`; the registry reports the BASE name.
    webFamilies should contain ("kinowo_web_http_requests")
  }
}
