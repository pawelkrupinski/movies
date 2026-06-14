package services.titlerules

import play.api.Logging
import services.movies.TitleNormalizer
import tools.{DaemonExecutors, Env}

import java.util.concurrent.TimeUnit
import scala.util.Try

/** Owns the title-rule business logic that's shared between the real Mongo store
 *  and the in-memory fake: load the rules, compile them into a `TitleRuleSet`,
 *  install it via `install`, and keep it current via the change stream plus a
 *  periodic backstop reload. The repository is the only infrastructure seam.
 *
 *  Runs on BOTH web and worker (each its own JVM, its own `TitleNormalizer`
 *  global). The worker passes `seedIfEmpty = true` so a fresh DB gets the
 *  migrated defaults; the web app is read-only and never seeds. `install`
 *  defaults to mutating the `TitleNormalizer` global but is injectable so tests
 *  capture the installed set instead of racing on process-global state. */
class TitleRulesCache(
  repository: TitleRulesRepository,
  seedIfEmpty: Boolean = false,
  install: TitleRuleSet => Unit = TitleNormalizer.installRules,
  // Fired AFTER a reload that actually changed the rules (never on the first
  // load), with the (previous, current) effective rule lists. The worker hooks
  // this to re-merge / un-merge / re-enrich existing records; the web app leaves
  // it a no-op (it's read-only and picks changes up via the movies stream).
  onRulesChanged: (Seq[TitleRule], Seq[TitleRule]) => Unit = (_, _) => ()
) extends Logging {

  private val scheduler          = DaemonExecutors.scheduler("title-rules-refresh")
  private val IntervalSeconds    = Env.positiveLong("KINOWO_TITLE_RULES_REFRESH_SECONDS", 1800L)
  @volatile private var watchHandle: Option[AutoCloseable] = None
  // Last installed rule set (by value), to detect real changes vs backstop
  // reloads. None until the first reload, so the first load never fires the hook.
  @volatile private var lastRules: Option[Set[TitleRule]] = None

  /** Read the current rules and install them. An empty store → in-code defaults,
   *  so the normaliser is never left rule-less. Fires `onRulesChanged` when the
   *  effective rule set differs from the previously-installed one. */
  def reload(): Unit = {
    val rules = repository.findAll()
    val effective = if (rules.nonEmpty) rules else TitleRuleDefaults.all
    install(TitleRuleSet(effective))
    logger.info(
      if (rules.nonEmpty) s"TitleRulesCache: installed ${rules.size} rules from store."
      else "TitleRulesCache: store empty — using in-code default rules.")

    val current = effective.toSet
    val previous    = lastRules
    lastRules = Some(current)
    if (previous.exists(_ != current)) {
      logger.info("TitleRulesCache: rules changed — running the change hook.")
      try onRulesChanged(previous.get.toSeq, current.toSeq)
      catch { case ex: Throwable => logger.warn(s"onRulesChanged failed: ${ex.getMessage}") }
    }
  }

  def start(): Unit = {
    if (seedIfEmpty && repository.enabled && repository.findAll().isEmpty) {
      val records = TitleRuleRecord.fromRules(TitleRuleDefaults.all)
      logger.info(s"TitleRulesCache: seeding ${TitleRuleDefaults.all.size} default rules " +
        s"(${records.size} records) into empty store.")
      records.foreach(repository.upsertRecord)
    }
    reload()
    watchHandle = repository.watchChanges(() => reload())
    logger.info(
      s"TitleRulesCache change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
      s"backstop reload every ${IntervalSeconds}s.")
    scheduler.scheduleAtFixedRate(
      () => Try(reload()).recover { case ex => logger.warn(s"TitleRulesCache reload tick failed: ${ex.getMessage}") },
      IntervalSeconds, IntervalSeconds, TimeUnit.SECONDS)
  }

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
