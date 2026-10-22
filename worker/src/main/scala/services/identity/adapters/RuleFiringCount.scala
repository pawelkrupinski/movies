package services.identity
package adapters

import play.api.Logging
import services.movies.TitleNormalizer

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * How often each RULE fires on the live listings, counted over each WRITTEN projection of the whole corpus: per rule id,
 * the listings it fired on, as `report` exports them (`kinowo_worker_identity_rule_fires_total{rule}`, beside
 * `kinowo_worker_identity_rule_counted_listings_total`, the listings counted) and one log line sums them up
 * (`identity rule firings: counted …`). What tells a rule dead in prod from one dead in the recordings
 * (docs/investigations/2026-10-22-rule-consolidation.md, `scripts/rule-firing-prod`).
 *
 * The ids are the identity trace's ([[RuleFiringCount.idsOf]]): each title rule a listing's title took (`title:<id>` of
 * title-rules.tsv, `format:<tag>`), the model's rules for it (`accept:`, `pooled:`, `veto:`, `join:`, `apart:`,
 * `refused:`) and, for a take of a stage after the model, that stage's rule (`stage:fill:<rule>`, `stage:agreed:<families>`,
 * `stage:correction:<evidence>`, …, [[DecidedBy]]). Rule ids only: a few hundred series a country.
 *
 * A projection [[submit]]s and goes on: the count runs on `runner`, never on the projection's path. Only the latest
 * submission waits; none is counted while `pressed`. A listing's title rules are worked out once per (venue, title) and
 * kept while the listing is live: they are a pure function of the two.
 */
final class RuleFiringCount(normalizer: TitleNormalizer, report: (Map[String, Int], Int) => Unit, runner: Executor,
                            pressed: () => Boolean) extends Logging {

  private val waiting = new AtomicReference[Option[(Resolution, Seq[Listing], Map[String, String])]](None)
  /** Each live listing's title rules, by venue and raw title, as the last count read them. */
  private var titleRules = Map.empty[(String, String), Seq[String]]

  /** Count `resolution`'s rule firings among `listings` on `runner`, once it is free. */
  def submit(resolution: Resolution, listings: Seq[Listing]): Unit =
    if (waiting.getAndSet(Some((resolution, listings, tools.LogFields.current()))).isEmpty) runner.execute(() => countWaiting())

  private def countWaiting(): Unit = synchronized {
    waiting.getAndSet(None).foreach { case (resolution, listings, fields) => tools.LogFields.carrying(fields) {
      if (pressed()) logger.info("identity rule firings: not counted, the heap is near its limit; the next projection of the corpus counts")
      else try apply(resolution, listings)
      catch { case NonFatal(e) => logger.warn("identity rule firings: counting failed", e) }
    } }
  }

  /** Count `resolution`'s rule firings among `listings` (every listing it decided) and report them, on this thread. */
  def apply(resolution: Resolution, listings: Seq[Listing]): Unit = synchronized {
    val byClean = mutable.HashMap.empty[String, Seq[String]]
    val kept    = titleRules
    val now     = mutable.HashMap.empty[(String, String), Seq[String]]
    val counts  = RuleFiringCount.count(resolution, listings, listing => {
      val key = (listing.key.venue, listing.rawTitle)
      now.getOrElseUpdate(key, kept.getOrElse(key, normalizer.firedRules(listing.cinema, listing.rawTitle, byClean)))
    })
    titleRules = now.toMap
    report(counts, listings.size)
    val byKind = counts.toSeq.groupMapReduce { case (rule, _) => RuleFiringCount.kindOf(rule) }(_ => 1)(_ + _)
    tools.LogFields.info(logger.underlyingLogger, s"identity rule firings: counted ${listings.size} listing(s), ${counts.size} rule(s) fired " +
      s"(${byKind.toSeq.sorted.map { case (kind, n) => s"$kind $n" }.mkString(", ")})",
      Seq("listings_counted" -> listings.size, "rules_fired" -> counts.size) ++ byKind.toSeq.sorted.map { case (kind, n) => s"rules_$kind" -> n })
  }
}

object RuleFiringCount {
  /** The stages whose takes are the model's own: their rules are the trace's already. */
  private val ModelStages = Set("model", "relay")

  /** The rule ids `key` fired under `decision`: the trace's ([[DecisionTrace.rulesOf]]) and, for a take of a stage after
   *  the model, that stage's rule (`stage:<rule>`, [[DecidedBy.of]]). */
  def idsOf(decision: ResolverDecision, key: services.movies.ListingKey): Seq[String] =
    decision.trace.rulesOf(key) ++
      Option.unless(ModelStages(DecidedBy.stage(decision, key)))("stage:" + DecidedBy.of(decision, key))

  /** Per rule id, how many of `listings` it fired on (once a listing, however often it fired): the title rules `titleRules`
   *  says each listing's title took, and the rules of the decision `resolution` holds it under (none for one undecided). */
  def count(resolution: Resolution, listings: Seq[Listing], titleRules: Listing => Seq[String]): Map[String, Int] = {
    val decisionOf = resolution.decisionOf
    val counts     = mutable.HashMap.empty[String, Int]
    listings.foreach { listing =>
      val ids = titleRules(listing) ++ decisionOf.get(listing.key).fold(Seq.empty[String])(idsOf(_, listing.key))
      ids.distinct.foreach(id => counts.updateWith(id)(n => Some(n.getOrElse(0) + 1)))
    }
    counts.toMap
  }

  /** A rule id's kind: its first segment (`title`, `accept`, `veto`, `stage`, …). */
  def kindOf(rule: String): String = rule.takeWhile(_ != ':')
}
