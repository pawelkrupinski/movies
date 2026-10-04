package deploy

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{CacheOccupancy, LegacyUserStateMetrics, UserStateIndexMetrics, UserStateWriteMetrics, WebCacheMetrics, WebDecodeFailureMetrics, WebHostMetrics, WebHttpMetrics}

import java.io.File
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
    new LegacyUserStateMetrics(registry, "pl", java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC))
    new UserStateWriteMetrics(registry, "pl")
    new UserStateIndexMetrics(registry, "pl")
    new WebDecodeFailureMetrics(registry, "pl")
    new services.metrics.WebReadModelStreamMetrics(registry, "pl", _ => true)
    new services.metrics.WebRenderMetrics(registry, "pl")
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
  private lazy val allDashboardJson: String =
    filesUnder(RepoFile.locate("infra/nix/files/monitoring/grafana/dashboards"), ".json")
      .map(RepoFile.read).mkString("\n")

  /** Every file under `dir` whose name ends in `suffix`, sorted by path. */
  private def filesUnder(dir: File, suffix: String): Seq[File] = {
    def walk(d: File): Seq[File] =
      Option(d.listFiles()).getOrElse(Array.empty[File]).toSeq.flatMap {
        case sub if sub.isDirectory       => walk(sub)
        case f if f.getName.endsWith(suffix) => Seq(f)
        case _                            => Nil
      }
    walk(dir).sortBy(_.getPath)
  }

  /** Every family the web's main sources spell as a whole `"kinowo_…"` string literal — what
   *  the tier registers, read from the code rather than from the constructor list above, so the
   *  two can be compared. Complete because no metric name may be interpolated (the worker's
   *  `GrafanaMetricCoverageSpec` lints all three modules for that). */
  private lazy val familiesNamedInSource: Seq[String] =
    filesUnder(RepoFile.locate("web/src/main/scala"), ".scala")
      .flatMap(f => NameLiteral.findAllMatchIn(RepoFile.read(f)).map(_.group(1)))
      .distinct
      .sorted

  private val NameLiteral = raw""""(kinowo_[a-z0-9_]+)"""".r

  /** Families `MetricsController` writes as TEXT, never through a registry, so no enumeration
   *  can reach them — the worker spec's `WebExportedFamilies` keeps them charted. */
  private val HandRendered = Set(
    "kinowo_web_movies_served",      // WebMovieMetrics: per-city served counts, sampled each minute
    "kinowo_uptime_recent_successes", // the in-app /uptime buckets, one series per service
    "kinowo_uptime_recent_failures",
    "kinowo_uptime_recent_zeroes",
    "kinowo_fallback_active_venues",  // per shared scraper client's fallback saturation
    "kinowo_fallback_total_venues"
  )

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

  /** The constructor list above is hand-written, and a metric class left off it is not
   *  caught by the coverage check — it is invisible to it. `kinowo_web_page_render_allocated_bytes`
   *  (`WebRenderMetrics`, 2026-10-02) shipped exactly that way: registered in production,
   *  drawn nowhere, and this spec green. So every family the source names must reach the
   *  enumeration. */
  it should "reach every family the web's sources register" in {
    familiesNamedInSource should not be empty // a broken scan must not pass vacuously
    // A counter registers under its base name, a gauge under its whole one.
    val unreached = familiesNamedInSource
      .filterNot(name => webFamilies.contains(name) || webFamilies.contains(name.stripSuffix("_total")))
      .filterNot(HandRendered.contains)
    withClue(
      s"registered in web/src/main but not constructed by this spec: ${unreached.mkString(", ")}. " +
        "Construct its class in `webFamilies` so the coverage check above can see it. "
    ) {
      unreached shouldBe empty
    }
  }

  it should "not exempt a hand-rendered family the sources no longer write" in {
    val gone = HandRendered.filterNot(familiesNamedInSource.contains)
    withClue(s"exempted as hand-rendered but not in web/src/main any more: ${gone.mkString(", ")}. ") {
      gone shouldBe empty
    }
  }
}
