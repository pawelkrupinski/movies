package deploy

import testsupport.SourceTrees
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

/**
 * EVERY WEB METRIC EXISTS FROM BOOT: once the wiring has started and `metricsAtBoot` has run — what `AppComponents`
 * does before serving its first request — every `kinowo_*` family the web's sources name is registered, and each
 * carries a series, at 0 or, for a reading nothing has taken yet, NaN.
 *
 * WHY. The web tier's metric objects are lazy vals, built by their first user — for some, the first request that
 * touches them — and a family that appears with its first event leaves every alert over it blind until then:
 * `increase()` cannot see a counter's first step from absent to 1, and an absent-series alert cannot tell "nothing
 * happened" from "nothing is exported". The worker's half is `deploy.WorkerMetricsAtBootSpec`, which says more.
 *
 * Driven through the real `Wiring` (`TestWebWiring`, over a disabled Mongo), with the routes read from the routes
 * file, as the router's documentation hands them over in production.
 */
class WebMetricsAtBootSpec extends AnyFlatSpec with Matchers {

  /** Families `MetricsController` writes as text, never through the registry. */
  private val HandRendered = Set(
    "kinowo_web_movies_served", "kinowo_web_films_by_poster_host", "kinowo_web_films_sharing_poster",
    "kinowo_uptime_recent_successes", "kinowo_uptime_recent_failures", "kinowo_uptime_recent_zeroes",
    "kinowo_fallback_active_venues", "kinowo_fallback_total_venues")

  /** Registered families with no series at boot, each with the reason. */
  private val NotSeededAtBoot: Map[String, String] = {
    val lastCalled = "a route or address never called has no last-call time; kinowo_web_route_defined is the inventory"
    val counted    = "published only by a cache that counts it (the production minifier's memo), none in this wiring"
    val mongo      = "one series per database and collection, seeded at 0 as each is opened — this wiring's Mongo is disabled"
    Map(
      "kinowo_web_route_last_called_seconds"          -> lastCalled,
      "kinowo_web_legacy_address_last_called_seconds" -> lastCalled,
      "kinowo_web_cache_hit_ratio"                    -> counted,
      "kinowo_web_cache_evictions"                    -> counted,
      "kinowo_web_mongo_write_errors"                 -> mongo,
      "kinowo_web_mongo_unauthorized"                 -> mongo)
  }

  /** Read from /proc/meminfo: present on every pod, absent where the spec runs off Linux. */
  private val LinuxOnly = Set("kinowo_web_host_memory_available_bytes", "kinowo_web_host_memory_total_bytes")

  private val RouteLine = raw"""^(GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS)\s+(\S+)""".r.unanchored

  /** (method, path) of every route in the routes file. */
  private lazy val routes: Seq[(String, String)] =
    RepoFile.read(SourceTrees("web/src/main/resources/routes").path.toFile).linesIterator.collect {
      case RouteLine(method, path) => method -> path
    }.toSeq

  /** The registry of a booted wiring, scraped once. */
  private lazy val scraped: io.prometheus.metrics.model.snapshots.MetricSnapshots = {
    val wiring = new modules.TestWebWiring()
    wiring.boot()
    try {
      wiring.metricsAtBoot(routes)
      wiring.webJvmMetrics.registry.scrape()
    } finally wiring.shutdown()
  }

  /** family -> series count. */
  private lazy val atBoot: Map[String, Int] =
    scraped.asScala.toSeq
      .map(s => s.getMetadata.getPrometheusName.stripSuffix("_total") -> s.getDataPoints.size)
      .filter(_._1.startsWith("kinowo_"))
      .toMap

  private lazy val namedInSource: Seq[String] = {
    SourceTrees("web/src/main/scala").scalaFiles.map(_.toFile)
      .flatMap(f => raw""""(kinowo_[a-z0-9_]+)"""".r.findAllMatchIn(RepoFile.read(f)).map(_.group(1).stripSuffix("_total")))
      .distinct.sorted
  }

  "every web metric family" should "be registered by the time the wiring has booted" in {
    routes.size should be > 50 // the routes file is read
    val lazily = namedInSource.filterNot(f => atBoot.contains(f) || HandRendered(f))
    withClue(s"named in web/src/main but not registered at boot: ${lazily.mkString(", ")}. Build it in metricsAtBoot. ") {
      lazily shouldBe empty
    }
  }

  it should "carry a series from boot" in {
    val empty = atBoot.collect { case (family, 0) if !NotSeededAtBoot.contains(family) && !LinuxOnly(family) => family }.toSeq.sorted
    withClue(
      s"registered with no series yet: ${empty.mkString(", ")}. Seed each series where the family is registered — a " +
        "counter by touching labelValues, a gauge at 0, or NaN for a reading not taken yet — over every finite label " +
        "value, or put it in NotSeededAtBoot with the reason. "
    ) {
      empty shouldBe empty
    }
  }

  /** The web's half of the metric catalogue (services.metrics.MetricCatalogue). The host-memory gauges read
   *  /proc/meminfo, so they are left out: the file must not depend on the machine the spec runs on. */
  it should "be written out as infra/test/app-metrics-catalogue/web.json" in {
    val portable = new io.prometheus.metrics.model.snapshots.MetricSnapshots(
      scraped.asScala.filterNot(s => LinuxOnly(s.getMetadata.getPrometheusName)).toSeq.asJava)
    val path = RepoFile.locate("infra/test/app-metrics-catalogue").toPath.resolve("web.json")
    services.metrics.MetricCatalogue.check(path, services.metrics.MetricCatalogue.render(portable, "web"),
      "web/testOnly deploy.WebMetricsAtBootSpec") shouldBe None
  }

  /** The order guard. Specs share the forked test JVM and run in parallel (web's `Test / parallelExecution`), and
   *  `services.MongoWriteErrors` is the PROCESS's sink: a test wiring that bound it counted every other spec's Mongo
   *  store into its own registry -- `MongoUserStateRepositoryOutcomesSpec`'s store over a closed client seeded
   *  `kinowo_web_mongo_write_errors{database="closed"}` here when the two ran side by side
   *  (`web/testOnly services.users.* deploy.*`), failing the catalogue and NotSeededAtBoot cases. Only the production
   *  root (`AppComponents`) binds the process's sink, so a test wiring's registry holds only what it built itself. */
  "a test wiring" should "not count another spec's Mongo stores into its registry" in {
    val wiring = new modules.TestWebWiring()
    wiring.boot()
    try {
      wiring.metricsAtBoot(routes)
      services.MongoWriteErrors.opened("another_spec") // what a connection another spec opens does, alongside this one
      val mongoSeries = wiring.webJvmMetrics.registry.scrape().asScala.toSeq
        .map(s => s.getMetadata.getPrometheusName.stripSuffix("_total") -> s.getDataPoints.size)
        .filter(_._1.startsWith("kinowo_web_mongo_")).toMap
      mongoSeries shouldBe Map("kinowo_web_mongo_write_errors" -> 0, "kinowo_web_mongo_unauthorized" -> 0)
    } finally wiring.shutdown()
  }

  "NotSeededAtBoot" should "not exempt a family that has its series after all, or that no longer exists" in {
    NotSeededAtBoot.keys.filterNot(atBoot.contains) shouldBe empty
    NotSeededAtBoot.keys.filter(f => atBoot.getOrElse(f, 0) > 0) shouldBe empty
  }
}
