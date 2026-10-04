package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerMetrics

import scala.jdk.CollectionConverters._

/**
 * Every metric we pay to export must appear on a dashboard.
 *
 * An exported-but-uncharted metric is the worst of both worlds: it costs a
 * sample, a series and a scrape, it reads in code review as "we have visibility
 * on this", and it shows nobody anything. They accumulate silently, because
 * nothing fails when a metric is registered and then never charted — which is
 * how this repo ended up carrying nine of them at once, including
 * `kinowo_worker_native_offbook_gap_bytes` (whose own doc comment calls it "the
 * primary signal" for the ~5-6h native OOM), `kinowo_worker_rating_resolved_not_run`
 * ("so Grafana can alert on it" — Grafana had never heard of it), and
 * `kinowo_worker_corpus_scan_incomplete_total`, the only surviving evidence that
 * the census gauges have gone stale.
 *
 * The worker's family list is derived from a real registry scrape rather than
 * written out here, so a metric added tomorrow is covered by this guard the
 * moment it is registered — the author either charts it or records WHY not in
 * [[UnchartedOnPurpose]]. That map is deliberately empty: there is currently no
 * worker metric worth exporting and not worth drawing, and an empty map is the
 * honest default. Adding an entry is a decision someone has to write down.
 *
 * Names are compared against the BASE family name (`kinowo_worker_tasks_started`),
 * which is a prefix of every form a dashboard can spell — `_total` for counters,
 * `_bucket`/`_sum`/`_count` for histograms.
 *
 * Scope: the worker registry (mechanically enumerable) plus the handful of
 * families the WEB app hand-renders, which are listed explicitly here because
 * they come from `MetricsController`'s own text exposition rather than a
 * Prometheus registry this module can scrape. Their NAMES are guarded on the web
 * side (`MetricsControllerSpec`, `WebMovieMetricsSpec`); what's guarded here is
 * that a dashboard draws them.
 */
class GrafanaMetricCoverageSpec extends AnyFlatSpec with Matchers {

  private val Dashboards = Seq(
    // THE LIVE DASHBOARDS -- the ones monitoring-1's Grafana provisions. They used to have a
    // frozen twin under fly/grafana/provisioning/dashboards for the stopped kinowo-grafana app,
    // and guarding THAT would have meant this spec passing while the dashboards people actually
    // open had no panel for a metric. The twin is gone; there is one copy to guard.
    "infra/nix/files/monitoring/grafana/dashboards/apps/application-health.json",
    "infra/nix/files/monitoring/grafana/dashboards/apps/worker-diagnostics.json",
    "infra/nix/files/monitoring/grafana/dashboards/fleet/kinowo-fleet.json",
    // Added 2026-08-29 with the HTTP dashboard. EVERY live dashboard has to be listed here or the
    // spec's guarantee inverts: a metric charted only on an unlisted dashboard reads as uncharted,
    // and someone "fixes" that by adding a duplicate panel to a listed one.
    "infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json"
  )

  /** Worker metrics deliberately exported without a panel, each with the reason.
   *  Empty on purpose — see the class comment. */
  private val UnchartedOnPurpose: Map[String, String] = Map.empty

  /** Web families, listed here for the REVERSE guard below — so a panel drawing
   *  one is not read as dangling by a module that cannot see the web's registry.
   *
   *  ⚠️ IT IS NO LONGER WHAT MAKES THEM CHARTED. This list used to carry that too,
   *  and being hand-maintained it could not: a family left off it was invisible to
   *  the guard rather than caught by it, which is exactly what happened to
   *  `kinowo_web_response_cache_*` and `kinowo_web_http_response_bytes` — both
   *  exported, both drawn nowhere, for as long as they existed.
   *  `deploy.GrafanaWebMetricCoverageSpec` in the WEB module now enumerates that
   *  tier's registry the way `workerFamilies` does this one, so forgetting this
   *  list fails loudly here (a live panel reads as dangling) instead of quietly
   *  there. The hand-rendered families stay, because `MetricsController` writes
   *  them as text and no registry can enumerate them. */
  private val WebExportedFamilies = Seq(
    "kinowo_web_movies_served",
    // Added 2026-08-29 with the HTTP filter. These two are the web tier's replacement for Fly's
    // proxy metrics (fly_app_http_*), which died with the Fly Prometheus token -- and unlike them
    // they measure the application's own work rather than the edge in front of it.
    "kinowo_web_http_requests_total",
    "kinowo_web_http_request_duration_seconds",
    // Exported since the HTTP filter landed and charted nowhere until the web-side
    // coverage spec enumerated the registry and said so.
    "kinowo_web_http_response_bytes",
    // Added 2026-08-29 alongside them: the web MACHINE's free RAM and free disk, read by the
    // process from its own kernel. Same cause -- fly_instance_memory_* and fly_volume_* died with
    // the Fly Prometheus token, and nothing scrapes the Fly host at all.
    "kinowo_web_host_memory_available_bytes",
    "kinowo_web_host_memory_total_bytes",
    "kinowo_web_host_disk_free_bytes",
    "kinowo_web_host_disk_total_bytes",
    // The web tier's in-heap caches, one family with a `cache` label. Its
    // predecessor `kinowo_web_response_cache_*` was exported and charted NOWHERE
    // for as long as it existed, and this list is why nobody noticed: it is
    // hand-maintained, so a family left off it is invisible to the guard rather
    // than caught by it.
    "kinowo_web_cache_held_bytes",
    "kinowo_web_cache_max_bytes",
    "kinowo_web_cache_entries",
    "kinowo_web_cache_hit_ratio",
    // Added 2026-09-24: documents the web's read-model scans skipped as undecodable.
    "kinowo_web_decode_failures",
    // The read model's change streams per collection: live now, and reopens of an ended one.
    "kinowo_web_readmodel_stream_live",
    "kinowo_web_readmodel_stream_reopens_total",
    "kinowo_web_cache_evictions_total",
    "kinowo_uptime_recent_successes",
    "kinowo_uptime_recent_failures",
    "kinowo_uptime_recent_zeroes",
    // Hand-rendered by `MetricsController` beside the uptime gauges, per shared
    // scraper client: the pair `ChainFallbackSaturated` (chain-fallback.rules)
    // divides. Missing from this list, they were the one alert input charted nowhere.
    "kinowo_fallback_active_venues",
    "kinowo_fallback_total_venues",
    // The legacy `PUT /api/me/state` retirement signal — see
    // `services.metrics.LegacyUserStateMetrics`'s class doc. Charted on
    // kinowo-http.json as `time() - max_over_time(…[60d])` (the pod forgets it on restart);
    // `GrafanaWebMetricCoverageSpec` (web module) confirms it's genuinely on
    // the web registry. Missing from this hand-maintained list read as
    // "exported by nothing" here even though it's real — the same failure
    // mode this list's own comment warns about.
    "kinowo_web_legacy_userstate_put_last_called_seconds",
    // Every atomic user-state write by endpoint and outcome — see
    // `services.metrics.UserStateWriteMetrics`.
    "kinowo_web_user_state_writes",
    // Whether `userStates` has its unique userId index — `UserStateIndexMetrics`.
    "kinowo_web_user_state_userid_index_unique",
    // Heap one listing render allocated — `services.metrics.WebRenderMetrics`.
    "kinowo_web_page_render_allocated_bytes"
  )

  /** `kinowo_*` families exported by the FLEET rather than by either application —
   *  written by a shell script into node_exporter's textfile directory (the mongodump
   *  timer, the heap-dump pruner, the synthetic-probe discovery), so no registry in
   *  this build can enumerate them.
   *
   *  Read from the `# TYPE kinowo_… <type>` header each script echoes, under infra/nix,
   *  rather than listed: this used to be a hand-written pair of mongodump names, and the
   *  five heap-dump families and the probe-discovery timestamp — every one of them an
   *  alert input — were missing from it, charted nowhere, and invisible to both guards. */
  private lazy val fleetFamilies: Seq[String] =
    filesUnder(new java.io.File("infra/nix"))(f => f.getName.endsWith(".sh") || f.getName.endsWith(".nix"))
      .flatMap(f => FleetTypeHeader.findAllMatchIn(RepoFile.read(f.getPath)).map(_.group(1)))
      .distinct
      .sorted

  private val FleetTypeHeader = raw"# TYPE (kinowo_[a-z0-9_]+) ".r

  /** Every file under `dir` that `keep` accepts, sorted by path. */
  private def filesUnder(dir: java.io.File)(keep: java.io.File => Boolean): Seq[java.io.File] = {
    def walk(d: java.io.File): Seq[java.io.File] =
      Option(d.listFiles()).getOrElse(Array.empty[java.io.File]).toSeq.flatMap {
        case sub if sub.isDirectory => walk(sub)
        case f if keep(f)           => Seq(f)
        case _                      => Nil
      }
    walk(dir).sortBy(_.getPath)
  }

  /** Every family the worker's main sources spell as a whole `"kinowo_…"` string literal —
   *  at a builder's `.name(…)` or passed to a helper that calls it. Complete because a metric
   *  name may not be interpolated (see the lint below), so a class `WorkerMetrics` stops
   *  wiring cannot drop out of [[workerFamilies]] unnoticed. */
  private lazy val workerFamiliesNamedInSource: Seq[String] =
    filesUnder(new java.io.File("worker/src/main/scala"))(_.getName.endsWith(".scala"))
      .flatMap(f => NameLiteral.findAllMatchIn(RepoFile.read(f.getPath)).map(_.group(1)))
      .distinct
      .sorted

  private val NameLiteral = raw""""(kinowo_[a-z0-9_]+)"""".r

  /** A metric builder handed an interpolated name: `.name(s"…")`. */
  private val InterpolatedName = raw"""\.name\(s"""".r

  /** Every alerting and recording rule's PromQL — Prometheus's rule files and Grafana's
   *  managed alerts — without the comments and annotations that name metrics in prose
   *  (including the deleted `kinowo_worker_throttled`, kept in a comment as history). */
  private lazy val allAlertRuleText: String =
    (RepoFile.listed("infra/nix/files/monitoring/rules")(_.getName.endsWith(".rules")).map(_.getPath) :+ AlertRule.File)
      .flatMap(path => AlertRule.everyExpression(RepoFile.read(path)))
      .mkString("\n")

  private lazy val exportedFamilies: Seq[String] = (workerFamilies ++ WebExportedFamilies ++ fleetFamilies).distinct

  /** The `kinowo_*` references in `text` that no exported family accounts for. A query
   *  spells a family in several forms — `_total` on a counter, `_bucket`/`_sum`/`_count`
   *  on a histogram, `_created` on either — so match on the BASE family the way
   *  `chartedIn` does, from the other side. A reference ending in `_` is the literal head of
   *  a `__name__=~"kinowo_uptime_recent_(failures|…)"` matcher, and stands for the families
   *  it prefixes. */
  private def danglingIn(text: String): Seq[String] =
    MetricReference.findAllMatchIn(text).map(_.group(0)).toSeq.distinct.sorted
      .filterNot(q => exportedFamilies.exists(f => q == f || q.startsWith(f + "_") || (q.endsWith("_") && f.startsWith(q))))

  /** Every `kinowo_worker_*` family the worker registers, base names, straight
   *  from the registry — NOT from the text exposition, which omits a family that
   *  has no data points yet and would quietly under-report the very metrics most
   *  likely to be forgotten. */
  private lazy val workerFamilies: Seq[String] =
    WorkerMetrics
      .singleCountry(Country.Poland, poolSize = settings.WorkerPoolSize(1))
      .registry
      .scrape()
      .asScala
      .map(_.getMetadata.getPrometheusName)
      .filter(_.startsWith("kinowo_"))
      .toSeq
      .distinct
      .sorted

  private lazy val allDashboardJson: String = Dashboards.map(RepoFile.read).mkString("\n")

  private def chartedIn(family: String): Boolean = allDashboardJson.contains(family)

  "every worker metric family" should "be charted on a dashboard, or recorded as deliberately uncharted" in {
    workerFamilies should not be empty // a broken enumeration must not pass vacuously

    val orphans = workerFamilies.filterNot(chartedIn).filterNot(UnchartedOnPurpose.contains)

    withClue(
      s"exported but drawn nowhere: ${orphans.mkString(", ")}. Every one of these costs a series on " +
        "every scrape and shows nobody anything. Add a panel to one of " + Dashboards.mkString(" / ") +
        ", or add the name to UnchartedOnPurpose with the reason it is worth exporting and not worth " +
        "drawing. "
    ) {
      orphans shouldBe empty
    }
  }

  it should "not carry a stale exemption for a metric that is charted after all" in {
    val stale = UnchartedOnPurpose.keys.filter(chartedIn)
    withClue(s"charted, so the exemption is now misleading: ${stale.mkString(", ")}. ") {
      stale shouldBe empty
    }
  }

  it should "not exempt a metric that no longer exists" in {
    val gone = UnchartedOnPurpose.keys.filterNot(workerFamilies.contains)
    withClue(s"exempted but not registered any more: ${gone.mkString(", ")}. ") {
      gone shouldBe empty
    }
  }

  /**
   * Charted is not the same as visible, and both of these were caught only by
   * querying the live data after the panels shipped:
   *
   *  - `kinowo_worker_native_offbook_gap_bytes` is RSS MINUS NMT-committed, and
   *    committed routinely exceeds resident (committed pages that were never
   *    touched aren't resident), so on a healthy worker it sits at roughly
   *    −200 to −320 MB. A panel with `min: 0` clips every one of those points
   *    and draws an empty chart — charted, provisioned, guarded by the coverage
   *    check above, and showing nothing.
   *
   *  - `kinowo_uptime_recent_zeroes` is per-SERVICE across ~2,800 services, of
   *    which ~120 are legitimately empty at any moment. One series each is 120
   *    lines of spaghetti; the readable signal is how MANY services are empty,
   *    which is what moves when something breaks.
   */
  "the off-book native memory panel" should "not floor its axis at zero on a routinely-negative series" in {
    val panel = panelBlockContaining("kinowo_worker_native_offbook_gap_bytes")
    withClue(
      "the off-book gap is RSS minus NMT-committed and is normally NEGATIVE (committed > resident); " +
        "a min:0 axis clips the whole series and draws an empty panel. "
    ) {
      panel should not include "\"min\": 0"
    }
  }

  "the empty-uptime-checks panel" should "aggregate services rather than drawing one line each" in {
    val panel = panelBlockContaining("kinowo_uptime_recent_zeroes")
    withClue(
      "~120 of ~2,800 services report an empty listing at any moment, so a per-service query draws " +
        "~120 overlapping lines. Aggregate (count) so the panel shows the population, which is the " +
        "thing that moves when a venue stops returning results. "
    ) {
      panel should include("count(")
      panel should not include "by (service)"
    }
  }

  /** One panel's raw JSON, located by a query it runs and bounded at the next
   *  panel's `"id":` so a neighbour's config never leaks into the assertion. */
  private def panelBlockContaining(expr: String): String = {
    val json  = RepoFile.read(Dashboards.find(d => RepoFile.read(d).contains(expr)).getOrElse(
      fail(s"no dashboard queries $expr")))
    val start = json.lastIndexOf("\"id\":", json.indexOf(expr))
    val end   = json.indexOf("\"id\":", json.indexOf(expr)) match {
      case -1 => json.length
      case i  => i
    }
    json.substring(start, end)
  }

  "every web-exported metric family" should "be charted too" in {
    WebExportedFamilies.foreach { family =>
      withClue(s"$family is exported by the web app's /metrics and charted nowhere. ") {
        chartedIn(family) shouldBe true
      }
    }
  }

  /**
   * AND THE OTHER DIRECTION, which is the one that breaks silently.
   *
   * Everything above asks "is this exported metric drawn?" — a panel too few. The
   * failure that actually ships is a panel too many: a dashboard querying a
   * `kinowo_*` family nothing exports any more. Prometheus answers an unknown metric
   * with an empty result, not an error, so the panel renders "No data" and looks
   * exactly like a quiet period. A TEMPLATE VARIABLE built on one is worse — its
   * dropdown empties, every panel scoped by it matches nothing, and a whole dashboard
   * goes blank at once.
   *
   * Both happened here on 2026-09-04: deleting the worker's throttle path removed
   * `kinowo_worker_throttled`, which was charted on one panel AND was the
   * `label_values(...)` source for the Country dropdown on TWO dashboards. Nothing in
   * this spec noticed, because it only ever looked for orphaned metrics, never
   * orphaned queries.
   */
  "every kinowo_ metric a dashboard queries" should "actually be exported by something" in {
    MetricReference.findFirstIn(allDashboardJson) should not be empty // a broken regex must not pass vacuously

    val dangling = danglingIn(allDashboardJson)

    withClue(
      s"queried by a panel or template variable but exported by nothing: ${dangling.mkString(", ")}. " +
        "Prometheus answers an unknown metric with an empty result rather than an error, so each of " +
        "these is a panel that reads as a quiet period — or, if it backs a `label_values` variable, " +
        "a dashboard that goes blank. Either restore the metric or repoint the query. "
    ) {
      dangling shouldBe empty
    }
  }

  /**
   * The same failure in an ALERT is worse than in a panel: a rule over a metric nothing
   * exports evaluates to an empty vector, never fires, and reads in review as coverage.
   * promtool cannot see it — its suites type their own input series — and the three dead
   * Flux alerts of 2026-09-07 passed theirs for months. The worker deleted its old
   * pipeline and its shadow run this week; a rule still watching a family either took
   * with it would be silent here until someone asked Prometheus for a series count.
   * (`absent(x)` of a dead family is not spared: it fires forever, which is a different
   * kind of wrong.)
   */
  "every kinowo_ metric an alerting rule reads" should "actually be exported by something" in {
    MetricReference.findFirstIn(allAlertRuleText) should not be empty // a broken read must not pass vacuously

    val dangling = danglingIn(allAlertRuleText)

    withClue(
      s"read by an alerting rule but exported by nothing: ${dangling.mkString(", ")}. The rule evaluates " +
        "to an empty vector and can never fire. Restore the metric, repoint the rule, or delete it (and " +
        "add it to Grafana's deleteRules if it is Grafana-managed — provisioning never deletes). "
    ) {
      dangling shouldBe empty
    }
  }

  "every fleet-exported metric family" should "be charted too" in {
    fleetFamilies should contain ("kinowo_mongodump_last_success_timestamp_seconds") // the scan reaches the scripts
    val orphans = fleetFamilies.filterNot(chartedIn)
    withClue(s"written by a fleet script and drawn nowhere: ${orphans.mkString(", ")}. kinowo-fleet.json is the fleet's board. ") {
      orphans shouldBe empty
    }
  }

  /** The registry enumeration above is only as good as `WorkerMetrics.singleCountry`'s
   *  wiring: a metric class it stops constructing drops out of [[workerFamilies]] and is
   *  then neither required on a panel nor accepted by the reverse guards. */
  "the worker registry enumeration" should "reach every family the worker's sources name" in {
    workerFamiliesNamedInSource should not be empty // a broken scan must not pass vacuously
    // A counter registers under its base name, a gauge under its whole one.
    val unreached = workerFamiliesNamedInSource
      .filterNot(name => workerFamilies.contains(name) || workerFamilies.contains(name.stripSuffix("_total")))
    withClue(s"named in worker/src/main but not registered by WorkerMetrics.singleCountry: ${unreached.mkString(", ")}. ") {
      unreached shouldBe empty
    }
  }

  /** The scan above is only complete if every name is a whole literal somewhere. RecheckedAudit
   *  used to build its three families as `s"${prefix}_audited"` and friends — six registered
   *  families, alert inputs among them, that no source scan could see. Interpolation is now
   *  refused in any main source file that touches the Prometheus client. */
  "a metric name" should "never be interpolated, so the source scans can see every family" in {
    val prometheusSources =
      Seq("worker", "web", "common").flatMap(m => filesUnder(new java.io.File(s"$m/src/main/scala"))(_.getName.endsWith(".scala")))
        .filter(f => RepoFile.read(f.getPath).contains("io.prometheus"))
    prometheusSources.size should be > 10 // the walk reaches the metric classes
    val offenders = prometheusSources.filter(f => InterpolatedName.findFirstIn(RepoFile.read(f.getPath)).isDefined).map(_.getPath)
    withClue(s"interpolated metric names in: ${offenders.mkString(", ")}. Spell each family as a whole string literal. ") {
      offenders shouldBe empty
    }
  }

  it should "be caught by the lint when it is" in {
    InterpolatedName.findFirstIn("""Counter.builder().name(s"${prefix}_audited")""") shouldBe defined
    InterpolatedName.findFirstIn("""Counter.builder().name("kinowo_worker_x")""") shouldBe empty
    NameLiteral.findAllMatchIn("""Names("kinowo_worker_a_audited", "kinowo_worker_a_audit_suspects")""").map(_.group(1)).toSeq shouldBe
      Seq("kinowo_worker_a_audited", "kinowo_worker_a_audit_suspects")
  }

  /** Every `kinowo_*` identifier appearing anywhere in a dashboard — panel targets and
   *  template-variable queries alike, since both break the same way. Deliberately not
   *  parsed as PromQL: a regex over the raw JSON cannot miss a spelling the parser
   *  would, and over-matching is caught by the base-family comparison above. */
  private val MetricReference = raw"kinowo_[a-z0-9_]+".r
}
