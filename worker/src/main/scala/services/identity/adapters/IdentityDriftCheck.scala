package services.identity
package adapters

import play.api.Logging
import services.movies.ListingKey

/**
 * PRODUCTION'S OWN DRIFT CHECK: the live takes — what the incremental state (the model kept event by event, the agreement
 * stage's kept verdicts) last projected — against a WHOLE resolve of exactly the listings that projection read, with
 * nothing kept: the model's resolve over them and a fresh agreement stage over the answers filed now. Every listing whose
 * film differs is DRIFT, logged one `identity drift:` line each with the take-change lines' fields
 * ([[TakeChangeLines.drift]]) and counted ([[IdentityDriftCheck.Metrics]]), and handed to the
 * [[IdentityDriftCheck.Response]] the composition root picked: report only, or adopt the whole resolve's takes.
 *
 * NOT WIRED: run nightly on the worker, its whole resolve OOM-killed worker-us (442cff245). Production's live takes are
 * compared off the worker now, on each nightly recording (`scripts.IdentityLiveDrift`, .github/workflows/identity-live-drift.yml,
 * `kinowo_identity_drift_*`).
 *
 * A listing whose whole take still waits on an answer — the model's or the agreement's ([[WaitingOn]]: a family's
 * search, a TMDB answer its fill reads, its correction's questions) — is no evidence either way: counted as unsettled,
 * never as drift. `scripts/identity-slice <cc> --drift` is the same comparison on a recording.
 */
final class IdentityDriftCheck(live: () => Option[IdentityDriftCheck.Live], whole: IdentityDriftCheck.Live => IdentityDriftCheck.Whole,
                               lines: TakeChangeLines, metrics: IdentityDriftCheck.Metrics, response: IdentityDriftCheck.Response,
                               /** Is the heap near its limit? A whole resolve is a take-up's worth of work: put off then. */
                               pressed: () => Boolean, stopwatch: tools.Stopwatch) extends Logging {
  import IdentityDriftCheck._

  /** One check: `None` when there is nothing projected to check yet, or the heap is pressed. */
  def run(): Option[Report] =
    if (pressed()) { logger.info("identity drift check: put off, the heap is near its limit"); None }
    else live().map { now =>
      val started = stopwatch.start()
      val report  = compare(now, whole(now))
      val logged  = report.drifted.take(LinesLogged)
      logged.foreach(d => log(lines.drift(d.listing, d.live, d.whole, now.listings.get(d.listing))))
      metrics.checked(report)
      tools.LogFields.info(logger.underlyingLogger,
        f"identity drift check: ${report.drifted.size} of ${report.listings} listings drifted from a whole resolve " +
          f"(${report.unsettled} unsettled, ${report.waitingOnAgreement} on the agreement's questions; ${report.byKind.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}) in ${started.seconds}%.1fs",
        Seq("drifted" -> report.drifted.size, "listings" -> report.listings, "unsettled" -> report.unsettled, "waitingOnAgreement" -> report.waitingOnAgreement, "logged" -> logged.size,
          "seconds" -> math.round(started.seconds * 10) / 10.0))
      if (report.drifted.nonEmpty) response.respond(report.drifted)
      report
    }

  private def log(line: TakeChangeLines.Line): Unit = tools.LogFields.info(logger.underlyingLogger, line.message, line.fields)
}

object IdentityDriftCheck {
  /** The live state: what the last projection took (its resolution after the agreement), the model's listings now — what
   *  the whole resolve reads — and which listings the model decides as it did for that projection: only those are
   *  compared, a listing a scrape or an answer moved since being no drift — and what each live take still waits on
   *  ([[agreement.AgreementStage.waitingOn]] of the live stage): one waiting is no drift either, whatever the whole takes. */
  final case class Live(taken: Resolution, listings: Map[ListingKey, Listing], unchanged: ListingKey => Boolean,
                        waitingOn: ResolverDecision => WaitingOn = WaitingOn.model)

  /** The whole resolve: its takes, and what each still waits on ([[agreement.AgreementStage.waitingOn]] of its stage). */
  final case class Whole(resolution: Resolution, waitingOn: ResolverDecision => WaitingOn)
  object Whole {
    /** A whole resolve with no stage of its own: its decisions wait on the model's gaps alone. */
    def model(resolution: Resolution): Whole = Whole(resolution, WaitingOn.model)
  }

  /** A listing whose live take differs from the whole resolve's. */
  final case class Drift(listing: ListingKey, live: Option[ResolverDecision], whole: Option[ResolverDecision]) {
    /** What adopting the whole resolve would do: `lost`, `gained` or `changed` (a TMDB film or a fallback). */
    def kind: String = (live.exists(taken), whole.exists(taken)) match {
      case (true, false) => TakeChange.Kind.Lost.label
      case (false, true) => TakeChange.Kind.Gained.label
      case _             => TakeChange.Kind.Changed.label
    }
  }

  /** `waitingOnAgreement`: of the `unsettled`, those the model's gaps alone would have compared. */
  final case class Report(listings: Int, drifted: Seq[Drift], unsettled: Int, waitingOnAgreement: Int = 0) {
    def byKind: Map[String, Int] = drifted.groupMapReduce(_.kind)(_ => 1)(_ + _)
  }

  /** How many drifted listings are logged a line each: the count says how many more there were. */
  val LinesLogged = 200

  private def taken(d: ResolverDecision): Boolean = d.film.isDefined || d.fallback.isDefined
  /** A decision's film: its TMDB id, else its fallback's, else none. */
  def filmOf(d: ResolverDecision): Option[String] = d.film.map(id => s"tmdb:$id").orElse(d.fallback.map(f => s"${f.source}:${f.id}"))

  /** Every listing of `live` whose film the `whole` resolve takes otherwise, by listing — a listing the whole take leaves
   *  waiting on an answer, the model's or the agreement's, counted unsettled instead. */
  def compare(live: Live, whole: Whole): Report = {
    val keys     = live.listings.keys.filter(k => live.unchanged(k) && live.taken.decisionOf.contains(k)).toSeq.sorted(using ListingKey.ordering)
    // waiting on either side: the whole take's open questions, or the live take's (a correction the live stage waits on
    // answers to — UK Chichester's "The Ties That Bind Us", 2026-10-24 — that the whole resolve read already)
    val waits    = keys.flatMap { k =>
      whole.resolution.decisionOf.get(k).map(whole.waitingOn).filterNot(_.settled)
        .orElse(live.taken.decisionOf.get(k).map(live.waitingOn).filterNot(_.settled)).map(k -> _)
    }.toMap
    val settled  = keys.filterNot(waits.contains)
    val drifted  = settled.flatMap { k =>
      val (was, now) = (live.taken.decisionOf.get(k), whole.resolution.decisionOf.get(k))
      Option.when(was.flatMap(filmOf) != now.flatMap(filmOf))(Drift(k, was, now))
    }
    Report(keys.size, drifted, waits.size, waits.count(_._2.modelGaps == 0))
  }

  /** The whole resolve of `live`'s listings, as a worker with nothing kept takes them: the model's resolve over `lookups`
   *  with `pins`, then a fresh stage (`stage`, over `clock`) applied once. It asks nothing: a cluster settles as the live
   *  stage's does, once every question it waits on was attempted (the memory both read), and one waiting on a question
   *  never attempted stays open. */
  def wholeResolve(live: Live, lookups: IdentityLookups, normalizer: services.movies.TitleNormalizer, pins: PinConstraints,
                   stage: java.time.Clock => agreement.AgreementStage, version: Long, clock: java.time.Clock): Whole = {
    val model = IdentityResolver.resolve(live.listings.values, lookups, normalizer, IdentityCalibration.resolver, pins)
    val fresh = stage(clock)
    Whole(fresh.apply(model, live.listings.get, version, None), fresh.waitingOn)
  }

  /** What the check does with what drifted, picked at the composition root. */
  trait Response { def respond(drifted: Seq[Drift]): Unit }
  object Response {
    /** The default: the lines and the count are the whole of it. */
    val ReportOnly: Response = _ => ()
  }

  /** ADOPT the whole resolve: forget all the stage kept of each drifted listing's live cluster, resolve its families
   *  again, and project — the live state then decides those listings as a worker with nothing kept does. */
  final class Adopt(model: IdentityModelService, stage: agreement.AgreementStage, trigger: EventTrigger) extends Response with Logging {
    def respond(drifted: Seq[Drift]): Unit = {
      val clusters = drifted.flatMap(_.live).map(d => StoredFamily.idOf(d.members)).toSet
      stage.forget(clusters)
      val reResolved = model.reResolve(drifted.map(_.listing).toSet, AdoptTimeout)
      logger.info(s"identity drift check: adopting the whole resolve for ${drifted.size} listings (${clusters.size} clusters forgotten" +
        s"${if (reResolved) ", their families resolved again" else "; the model did not answer in time"})")
      trigger.request()
    }
  }
  val AdoptTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")

  trait Metrics { def checked(report: Report): Unit }
  object Metrics { val Silent: Metrics = _ => () }
}
