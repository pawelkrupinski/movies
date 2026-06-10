package services.titlerules

import play.api.Logging
import services.movies.TitleNormalizer
import tools.{DaemonExecutors, Env}

import java.util.concurrent.TimeUnit
import scala.util.Try

/** Owns the title-rule business logic that's shared between the real Mongo store
 *  and the in-memory fake: load the rules, compile them into a `TitleRuleSet`,
 *  install it on `TitleNormalizer`, and keep it current via the change stream
 *  plus a periodic backstop reload. The repo is the only infrastructure seam.
 *
 *  Runs on BOTH web and worker (each its own JVM, its own `TitleNormalizer`
 *  global). The worker passes `seedIfEmpty = true` so a fresh DB gets the
 *  migrated defaults; the web app is read-only and never seeds. */
class TitleRulesCache(repo: TitleRulesRepo, seedIfEmpty: Boolean = false) extends Logging {

  private val scheduler          = DaemonExecutors.scheduler("title-rules-refresh")
  private val IntervalSeconds    = Env.positiveLong("KINOWO_TITLE_RULES_REFRESH_SECONDS", 1800L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Read the current rules and install them. An empty store → in-code defaults,
   *  so the normaliser is never left rule-less. */
  def reload(): Unit = {
    val rules = repo.findAll()
    if (rules.nonEmpty) {
      TitleNormalizer.installRules(TitleRuleSet(rules))
      logger.info(s"TitleRulesCache: installed ${rules.size} rules from store.")
    } else {
      TitleNormalizer.resetToDefaults()
      logger.info("TitleRulesCache: store empty — using in-code default rules.")
    }
  }

  def start(): Unit = {
    if (seedIfEmpty && repo.enabled && repo.findAll().isEmpty) {
      logger.info(s"TitleRulesCache: seeding ${TitleRuleDefaults.all.size} default rules into empty store.")
      TitleRuleDefaults.all.foreach(repo.upsert)
    }
    reload()
    watchHandle = repo.watchChanges(() => reload())
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
