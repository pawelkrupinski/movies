package tools

import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.{CounterSnapshot, Labels}

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * Everything a pass over UNCHANGED input could have written, asked, re-keyed or
 * re-projected, counted in one place — so a spec can say "the second pass is a no-op"
 * in one line and be told, when it is not, exactly which axis moved.
 *
 * The class of bug this exists for has shipped nine times: a tick over identical input
 * that still rewrote a film document (`6365b8e95`), rewrote every screening of every
 * film a venue touched (`43b595136`), re-projected ~333 rows every 30 minutes because a
 * heal that wrote nothing kept asking again (`dfe62a96c`), stamped a cached TMDB id with
 * the weakest basis so the row re-resolved on every sweep (`79d5b30f2`), or counted a
 * rejected RT page as a score that changed (`350ebc7b6`). Each was invisible to an
 * assertion on OUTPUTS — the corpus came out identical every time — and each was caught
 * only by counting the work. Production paid 1,211 re-keys in nine days for the same
 * reason. So this counts work, on every axis at once: a spec that asserts one counter
 * (`emissions shouldBe 0`) is blind to the other eight.
 *
 * A probe is anything that yields named, monotonically increasing values: a Prometheus
 * registry (every labelled series of the chosen families, the country label dropped), a
 * change-stream subscription (one increment per delivery), or a plain closure over a
 * counter a decorator keeps. `churnOf(pass)` reads every probe before and after the pass
 * and returns the series that moved.
 */
final class ChurnLedger {
  private val probes  = mutable.ListBuffer.empty[() => Map[String, Double]]
  private val context = mutable.ListBuffer.empty[() => String]

  /** Something to print beside a failure — which keys a counter saw, say — since a count
   *  alone gives a reader nothing to check a hypothesis against. Empty text prints nothing. */
  def explain(describe: => String): this.type = {
    context += (() => describe); this
  }

  /** One named counter read off whatever keeps it. */
  def counter(name: String)(read: => Long): this.type = {
    probes += (() => Map(name -> read.toDouble)); this
  }

  /** A family of named counters read together — for a decorator that keeps one per key. */
  def counters(read: () => Map[String, Double]): this.type = {
    probes += read; this
  }

  /** Every counter series of `families` in `registry`, keyed `family{label=value,…}` with the
   *  `country` label dropped (a ledger is per wiring, and a wiring is per country). `keep`
   *  narrows a family to the series that mean work — `tasks_enqueued{result=deduped}` is a
   *  no-op the queue refused, not a dispatch. */
  def registry(registry: PrometheusRegistry, families: Set[String],
               keep: (String, Labels) => Boolean = (_, _) => true): this.type = {
    probes += (() => ChurnLedger.countersOf(registry, families, keep)); this
  }

  /** Count every delivery a subscription makes. `subscribe` is handed the callback to
   *  register — a change stream, a side collection's apply ring — and the count starts
   *  from the moment it is called, so register the ledger BEFORE the pass it measures. */
  def deliveries(name: String)(subscribe: (() => Unit) => Any): this.type = {
    val seen = new AtomicLong(0)
    subscribe(() => { seen.incrementAndGet(); () })
    counter(name)(seen.get)
  }

  /** Every probe's current value. */
  def reading(): Map[String, Double] = probes.iterator.flatMap(_.apply()).toMap

  /** The series that moved during `pass`, and by how much. Empty is a fixpoint. */
  def churnOf(pass: => Unit): Map[String, Double] = {
    val before = reading()
    pass
    ChurnLedger.delta(before, reading())
  }

  /** Run `pass` and fail, naming every axis that moved, unless it did no work at all.
   *  `label` says what the pass was, so a failure reads as a sentence. */
  def assertNoChurn(label: String)(pass: => Unit): Unit = {
    val moved = churnOf(pass)
    if (moved.nonEmpty)
      org.scalatest.Assertions.fail(
        s"$label did work over input that had not changed — every axis below should have stayed at zero:\n" +
          ChurnLedger.describe(moved) + context.map(_()).filter(_.nonEmpty).map("\n" + _).mkString)
  }
}

object ChurnLedger {

  /** `after - before` per series, keeping only the ones that moved. A series absent
   *  before (a labelled child created during the pass) counts from zero. */
  def delta(before: Map[String, Double], after: Map[String, Double]): Map[String, Double] =
    after.iterator.map { case (k, v) => k -> (v - before.getOrElse(k, 0.0)) }
      .filter(_._2 != 0.0).toMap

  /** One line per moved series, largest first, for a failure message. */
  def describe(moved: Map[String, Double]): String =
    moved.toSeq.sortBy { case (k, v) => (-v, k) }
      .map { case (k, v) => f"  $k%-70s +${v.toLong}%d" }.mkString("\n")

  def countersOf(registry: PrometheusRegistry, families: Set[String],
                                keep: (String, Labels) => Boolean): Map[String, Double] =
    registry.scrape().asScala.iterator.collect {
      case counter: CounterSnapshot if families.contains(counter.getMetadata.getName) => counter
    }.flatMap { counter =>
      val family = counter.getMetadata.getName
      counter.getDataPoints.asScala.iterator.filter(point => keep(family, point.getLabels)).map { point =>
        val labels = point.getLabels.asScala.iterator.filterNot(_.getName == "country")
          .map(l => s"${l.getName}=${l.getValue}").mkString(",")
        (if (labels.isEmpty) family else s"$family{$labels}") -> point.getValue
      }
    }.toMap
}
