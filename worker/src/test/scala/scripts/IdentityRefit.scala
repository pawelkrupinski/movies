package scripts

import play.api.libs.json.Json
import services.identity._
import services.identity.IdentityCalibration.{Bin, SignalWeights, Threshold}
import services.identity.IdentityMeasures.{Category, Measure, Missing, Number}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.hashing.MurmurHash3

/**
 * The weekly identity RULE REFIT (`.github/workflows/identity-refit.yml`): re-tunes the numbers the resolver's EXISTING
 * rules read from `identity-weights.json` — never a new rule, never a new feature — against the ground truth the
 * unmatched-cluster ratchet holds: `labels.tsv` (recall targets, must-not pairs, hand labels, and the review answers
 * `ReviewLabelsCli export` writes there — an answer reaches the refit once exported and committed) and
 * `expected-matches.tsv`.
 *
 * What it may move, one bounded step per parameter per run:
 *  - a probability CUT: the listing-film acceptance cut (`showRatings`) and each scope's cannot-link cut;
 *  - a learned cannot-link's numeric BOUND ("runtime.delta >= 7 AND year.distance >= 4" → "… >= 5");
 *  - a listing-film signal's WEIGHTS, refitted by the calibration's own fit ([[IdentityCalibrate.inOrder]],
 *    [[IdentityCalibrate.monotone]], [[IdentityCalibrate.llr]]) from the counts the artefact holds plus the labelled
 *    units the captures measure under TODAY's code — so a change to a measure's inputs (a film's releases, its runtimes
 *    per translation) is relearned from the units it now measures, and `--relearn <signal,…>` drops the stored counts of
 *    a signal whose inputs changed, refitting it from the fresh units alone.
 *
 * Every proposal is MEASURED as the ratchet measures: each country's captured clusters re-resolved under it and under
 * the control (the calibration so far), the agreement stage replayed over both ([[CaptureReplay]]), and every listing
 * whose take moved judged by the labels. KEPT only when: no take is wrong, unjudged or switched; no right take and no
 * line of `expected-matches.tsv` is lost; at least [[MinSupport]] labelled films of the FITTING fold are newly taken
 * right; and no TMDB question is left unanswered. Labels are grouped by film into [[Folds]] folds; one is HELD OUT —
 * a weight refit never counts its units, and its gains and losses are reported apart from the in-sample ones. Kept
 * changes are applied greedily, the best first, each next proposal measured on top of those before it.
 *
 *   sbt "worker/Test/runMain scripts.IdentityRefit [--apply] [--report <file.md>] [--max-changes <n>] [--relearn <signal,…>] [--weights <file>]"
 */
object IdentityRefit {

  /** The fewest labelled films of the fitting fold a kept change must newly take right. */
  val MinSupport = 3
  /** How far a probability cut may move in one run: this much, and never more than a quarter of itself. */
  val MaxCutStep = 0.05
  /** How far any one weight may move in one run, in log-odds: a larger refit is taken this far along its way. */
  val MaxWeightStep = 0.5
  /** A cannot-link bound moves a tenth of itself per run, at least 1. */
  val BoundStepShare = 0.1
  /** Labels are grouped by film into this many folds; fold 0 is held out. */
  val Folds = 5

  val Artefact: Path = IdentityCalibrate.ResolverArtefact

  /** Is the film (a label's "tmdb:913760", or a listing's own key when no label names its film) in the held-out fold? */
  def heldOut(group: String): Boolean = Math.floorMod(MurmurHash3.stringHash(group), Folds) == 0

  // ── what may move ─────────────────────────────────────────────────────────────────────────

  sealed trait Change {
    def parameter: String
    def from: String
    def to: String
    def applyTo(c: IdentityCalibration): IdentityCalibration
  }

  private def num(x: Double) = if (x == math.rint(x)) x.toLong.toString else f"$x%.4f"

  /** A scope's probability cut moved. */
  final case class CutChange(scope: String, threshold: String, was: Double, now: Double) extends Change {
    def parameter = s"$scope cut '$threshold'"
    def from = num(was); def to = num(now)
    def applyTo(c: IdentityCalibration): IdentityCalibration = {
      val model = c.scopes(scope)
      val old   = model.thresholds(threshold)
      c.copy(scopes = c.scopes.updated(scope, model.copy(thresholds = model.thresholds.updated(threshold,
        Threshold(now, old.measured, s"refit from ${num(was)} by scripts.IdentityRefit on the unmatched-cluster ratchet; was: ${old.basis}")))))
    }
  }

  /** One numeric condition of a learned cannot-link moved: its name follows, as the calibration renders it. */
  final case class BoundChange(scope: String, rule: String, signal: String, was: Double, now: Double) extends Change {
    def parameter = s"$scope cannot-link '$rule' on $signal"
    def from = num(was); def to = num(now)
    def applyTo(c: IdentityCalibration): IdentityCalibration = c.copy(cannotLinks = c.cannotLinks.map { r =>
      if (r.scope != scope || r.name != rule) r else {
        val all = r.all.map { cond =>
          if (cond.signal != signal || cond.in.nonEmpty) cond
          else if (cond.atLeast.contains(was)) cond.copy(atLeast = Some(now))
          else if (cond.atMost.contains(was)) cond.copy(atMost = Some(now))
          else cond
        }
        r.copy(name = all.map(cond => IdentityCalibrate.Atom(cond.signal, cond.in, cond.atLeast, cond.atMost).render).mkString(" AND "), all = all,
          origin = if (r.origin.contains("refit")) r.origin else s"${r.origin}, refit")
      }
    })
  }

  /** A signal's weights refitted. */
  final case class WeightChange(scope: String, signal: String, was: SignalWeights, now: SignalWeights, units: Int, share: Double) extends Change {
    def parameter = s"$scope weights of '$signal'"
    def from = render(was); def to = render(now)
    def applyTo(c: IdentityCalibration): IdentityCalibration = {
      val model = c.scopes(scope)
      c.copy(scopes = c.scopes.updated(scope, model.copy(signals = model.signals.updated(signal, now))))
    }
  }

  /** A signal's weights in a line: `match +1.63 overlap -3.11` or `*..0.5 -4.70 0.5..2.5 -0.57`. */
  def render(w: SignalWeights): String = {
    def end(b: Option[Double]) = b.fold("*")(num)
    val cells = if (w.kind == "numeric") w.bins.map(b => s"${end(b.atLeast)}..${end(b.atMost)}" -> b.weight) else w.categories.toSeq.sortBy(_._1)
    (cells ++ w.missing.toSeq.sortBy(_._1).map { case (s, x) => s"missing:$s" -> x }).map { case (k, x) => f"$k ${if (x >= 0) "+" else ""}$x%.2f" }.mkString(" ")
  }

  /** The cut moves one bounded step each way, inside (0, 1). */
  def cutChanges(c: IdentityCalibration): Seq[CutChange] = c.scopes.toSeq.sortBy(_._1).flatMap { case (scope, model) =>
    model.thresholds.toSeq.sortBy(_._1).flatMap { case (name, t) =>
      val step = math.min(MaxCutStep, t.probability / 4)
      Seq(t.probability - step, t.probability + step).filter(p => p > 0 && p < 1).map(p => CutChange(scope, name, t.probability, p))
    }
  }

  /** Each learned cannot-link's numeric bound moved a tenth of itself (at least 1) each way, never across 0. */
  def boundChanges(c: IdentityCalibration): Seq[BoundChange] = c.cannotLinks.flatMap { r =>
    r.all.filter(_.in.isEmpty).flatMap { cond =>
      cond.atLeast.orElse(cond.atMost).toSeq.flatMap { bound =>
        val step = math.max(1.0, math.rint(math.abs(bound) * BoundStepShare))
        Seq(bound - step, bound + step).filter(b => b != 0 && math.signum(b) == math.signum(bound)).map(BoundChange(r.scope, r.name, cond.signal, bound, _))
      }
    }
  }

  // ── the labelled units ────────────────────────────────────────────────────────────────────

  /** One listing-film pair the labels judge: the film's measures for the listing's node, same film or not, and the film
   *  the labels name for the listing (its fold's group). */
  final case class LabelledUnit(country: String, node: String, tmdbId: Int, group: String, measures: Map[String, Measure], same: Boolean)

  /** The labelled units of one capture, measured as the resolver measures them under `calibration`: every candidate of a
   *  labelled listing's node — the film a right label names same, any other (or one a wrong label names) different. */
  def labelledUnits(capture: UnmatchedClusters.Capture, labels: Seq[UnmatchedClusters.Label], calibration: IdentityCalibration): Seq[LabelledUnit] = {
    val code   = capture.country.code
    val about  = capture.listings.map(l => l.key -> labels.filter(b => b.country == code && b.rawTitle == l.rawTitle && (b.venue == "*" || b.venue == l.venue))).toMap
    val judged = about.filter(_._2.nonEmpty)
    if (judged.isEmpty) Nil else {
      val nodes = IdentityResolver.evidenceOf(capture.listings, new UnmatchedClusters.Replay(capture),
        services.movies.TitleNormalizer.forCountry(capture.country), calibration)(l => judged.contains(l.key))
      nodes.flatMap { node =>
        val said  = node.keys.flatMap(judged.get).flatten.distinct
        val right = said.filter(_.right).map(_.film).distinct
        val wrong = said.filterNot(_.right).map(_.film).toSet
        val group = right.sorted.headOption.getOrElse(s"$code:${node.keys.map(services.movies.ListingKey.serialised).min}")
        val name  = node.keys.map(services.movies.ListingKey.serialised).min
        if (right.size > 1) Nil // a node the labels give two films is no unit of either
        else node.candidates.flatMap { c =>
          val film = s"tmdb:${c.tmdbId}"
          if (right.contains(film)) Some(LabelledUnit(code, name, c.tmdbId, group, c.measures, same = true))
          else if (right.nonEmpty || wrong(film)) Some(LabelledUnit(code, name, c.tmdbId, group, c.measures, same = false))
          else None
        }
      }.distinctBy(u => (u.country, u.node, u.tmdbId))
    }
  }

  // ── refitting a signal ────────────────────────────────────────────────────────────────────

  /** `weights` refitted by the calibration's own fit from the counts it holds (none when `fresh`) plus `units`' (each a
   *  measure, or none, and whether same film), then moved at most [[MaxWeightStep]] from where it was: `None` when its
   *  counts do not reproduce its weights (a table fitted otherwise is not refitted) or nothing moves. */
  def refitSignal(signal: String, weights: SignalWeights, units: Seq[(Option[Measure], Boolean)], fresh: Boolean = false): Option[(SignalWeights, Double)] =
    Option.when(reproduces(signal, weights))(()).flatMap { _ =>
      val counted = added(if (fresh) cleared(weights) else weights, units)
      val moved   = blended(weights, counted, fit(signal, counted))
      Option.when(render(moved._1) != render(weights))(moved)
    }

  /** Does refitting `weights` from its own counts give its weights back? */
  def reproduces(signal: String, weights: SignalWeights): Boolean = {
    val again = fit(signal, weights)
    def close(a: Double, b: Double) = math.abs(a - b) < 1e-6
    if (weights.kind == "numeric") again.bins.size == weights.bins.size && again.bins.zip(weights.bins).forall((a, b) => close(a.weight, b.weight)) &&
      weights.missing.forall { case (s, x) => again.missing.get(s).exists(close(_, x)) }
    else weights.categories.forall { case (k, x) => again.categories.get(k).exists(close(_, x)) } &&
      weights.missing.forall { case (s, x) => again.missing.get(s).exists(close(_, x)) }
  }

  private def cleared(w: SignalWeights): SignalWeights =
    w.copy(bins = w.bins.map(_.copy(positives = 0, negatives = 0)), counts = w.counts.map { case (k, _) => k -> Seq(0, 0) })

  /** `w` with `units` counted in: a number in the bin holding it, a category or a missing side in its own cell. */
  def added(w: SignalWeights, units: Seq[(Option[Measure], Boolean)]): SignalWeights = units.foldLeft(w) { case (acc, (m, same)) =>
    def bump(counts: Map[String, Seq[Int]], key: String) = {
      val Seq(p, n) = counts.getOrElse(key, Seq(0, 0))
      counts.updated(key, if (same) Seq(p + 1, n) else Seq(p, n + 1))
    }
    m match {
      case Some(Number(x)) if acc.kind == "numeric" =>
        acc.copy(bins = acc.bins.map(b => if (b.contains(x)) (if (same) b.copy(positives = b.positives + 1) else b.copy(negatives = b.negatives + 1)) else b))
      case Some(Category(v)) if acc.kind != "numeric" => acc.copy(counts = bump(acc.counts, v))
      case Some(Missing(s)) => acc.copy(counts = bump(acc.counts, s"missing:$s"))
      case _ => acc
    }
  }

  /** The calibration's fit of `w`'s counts: categories pooled under their evidence order, numeric bins under their
   *  direction, each cell weighed by its log-likelihood ratio — a neutral missing side stays 0. */
  def fit(signal: String, w: SignalWeights): SignalWeights = {
    val missing = w.counts.collect { case (k, Seq(p, n)) if k.startsWith("missing:") => k.stripPrefix("missing:") -> (p, n) }
    def missingWeights(totP: Int, totN: Int, cells: Int) =
      w.missing.keySet.union(missing.keySet).toSeq.map(s => s -> (if (w.neutral.contains(s)) 0.0 else missing.get(s).fold(w.missing.getOrElse(s, 0.0))((p, n) =>
        IdentityCalibrate.llr(p, n, totP, totN, cells)))).toMap
    if (w.kind == "numeric") {
      val totP  = w.bins.map(_.positives).sum + missing.values.map(_._1).sum
      val totN  = w.bins.map(_.negatives).sum + missing.values.map(_._2).sum
      val edges = w.bins.map(b => (b.atLeast.getOrElse(Double.NegativeInfinity), b.atMost.getOrElse(Double.PositiveInfinity), b.positives, b.negatives))
      val pooled = IdentityMeasures.NumericDirection.get(signal).fold(edges)(IdentityCalibrate.monotone(edges, _,
        IdentityCalibrate.llr(_, _, totP, totN, edges.size + w.missing.size)))
      val cells = pooled.size + w.missing.size
      w.copy(bins = pooled.map { case (lo, hi, p, n) =>
        Bin(Option.when(!lo.isInfinite)(lo), Option.when(!hi.isInfinite)(hi), IdentityCalibrate.llr(p, n, totP, totN, cells), p, n) },
        missing = missingWeights(totP, totN, cells))
    } else {
      val cats  = w.counts.collect { case (k, Seq(p, n)) if !k.startsWith("missing:") => k -> (p, n) }
      val totP  = (cats.values ++ missing.values).map(_._1).sum
      val totN  = (cats.values ++ missing.values).map(_._2).sum
      val cells = cats.size + missing.size
      val weighed = IdentityCalibrate.inOrder(cats, IdentityMeasures.EvidenceOrder.getOrElse(signal, Nil), IdentityCalibrate.llr(_, _, totP, totN, cells))
      w.copy(categories = weighed.map { case (v, (p, n)) => v -> IdentityCalibrate.llr(p, n, totP, totN, cells) }, missing = missingWeights(totP, totN, cells))
    }
  }

  /** Does `outer` (a bin of a pooled table) span all of `inner` (a bin of the table it pooled)? */
  private def spans(outer: Bin, inner: Bin): Boolean =
    outer.atLeast.forall(lo => inner.atLeast.exists(lo <= _)) && outer.atMost.forall(hi => inner.atMost.exists(hi >= _))

  /** `fitted` as a step from `was`, at most [[MaxWeightStep]] in any cell — on `was`'s own bins (a pooled bin weighs each
   *  bin it spans; a share of two tables monotone the same way is monotone) and cells, with `counted`'s counts (the
   *  counts `fitted` was fitted from, before any pooling): the table, and the share of the refit's way it went. */
  def blended(was: SignalWeights, counted: SignalWeights, fitted: SignalWeights): (SignalWeights, Double) = {
    val binDeltas  = was.bins.map(b => fitted.bins.find(spans(_, b)).fold(0.0)(_.weight) - b.weight)
    val catDeltas  = (was.categories.keySet ++ fitted.categories.keySet).toSeq.sorted.map(k => k -> (fitted.categories.getOrElse(k, 0.0) - was.categories.getOrElse(k, 0.0)))
    val missDeltas = (was.missing.keySet ++ fitted.missing.keySet).toSeq.sorted.map(k => k -> (fitted.missing.getOrElse(k, 0.0) - was.missing.getOrElse(k, 0.0)))
    val largest = (binDeltas ++ catDeltas.map(_._2) ++ missDeltas.map(_._2)).map(math.abs).maxOption.getOrElse(0.0)
    val share   = if (largest <= MaxWeightStep) 1.0 else MaxWeightStep / largest
    (was.copy(
      bins       = counted.bins.zip(binDeltas).zip(was.bins).map { case ((c, d), b) => c.copy(weight = b.weight + share * d) },
      categories = catDeltas.map((k, d) => k -> (was.categories.getOrElse(k, 0.0) + share * d)).toMap,
      missing    = missDeltas.map((k, d) => k -> (was.missing.getOrElse(k, 0.0) + share * d)).toMap,
      counts     = counted.counts), share)
  }

  /** Every listing-film signal refitted from `units` of the fitting fold, those named in `relearn` from them alone. */
  def weightChanges(c: IdentityCalibration, units: Seq[LabelledUnit], relearn: Set[String]): Seq[WeightChange] = {
    val fitting = units.filterNot(u => heldOut(u.group))
    val scope   = IdentityMeasures.ListingFilm
    c.scopes.get(scope).toSeq.flatMap(_.signals.toSeq.sortBy(_._1)).flatMap { case (signal, weights) =>
      refitSignal(signal, weights, fitting.map(u => u.measures.get(signal) -> u.same), fresh = relearn(signal))
        .map((now, share) => WeightChange(scope, signal, weights, now, fitting.size, share))
    }
  }

  // ── measuring ─────────────────────────────────────────────────────────────────────────────

  /** One listing whose take a change moved, judged ([[DecorationDiscovery.judge]]; a lost take that was right is lost). */
  final case class Moved(country: String, venue: String, rawTitle: String, before: String, after: String, verdict: String, group: String) {
    def heldOut: Boolean = IdentityRefit.heldOut(group)
  }
  val LostRight = "lost right"

  /** A change measured: the listings it moved, judged; the TMDB questions it left open beyond its control's; the lines of
   *  `expected-matches.tsv` it lost; and each capture's measure under it (what the next change is measured on top of). */
  final case class Measured(change: Change, moved: Seq[Moved], gaps: Int, expectedLost: Seq[String], after: Seq[CaptureReplay.Measure] = Nil) {
    private def count(v: String) = moved.count(_.verdict == v)
    def right: Int = count(DecorationDiscovery.Right); def wrong: Int = count(DecorationDiscovery.Wrong)
    def switched: Int = count(DecorationDiscovery.Switched); def lostRight: Int = count(LostRight)
    def lost: Int = count(DecorationDiscovery.Lost); def unjudged: Int = count(DecorationDiscovery.Unjudged)
    private def films(held: Boolean) = moved.filter(m => m.verdict == DecorationDiscovery.Right && m.heldOut == held).map(_.group).distinct
    /** The labelled films newly taken right in the fitting fold and in the held-out one. */
    def supportFitting: Seq[String] = films(held = false); def supportHeldOut: Seq[String] = films(held = true)
    def failures: Seq[String] =
      Seq((gaps > 0) -> s"$gaps unanswered", (wrong > 0) -> s"$wrong wrong", (switched > 0) -> s"$switched switched",
        (lostRight > 0) -> s"$lostRight right lost", (unjudged > 0) -> s"$unjudged unjudged",
        expectedLost.nonEmpty -> s"${expectedLost.size} expected lost",
        (supportFitting.size < MinSupport) -> s"support ${supportFitting.size} < $MinSupport").collect { case (true, why) => why }
    def kept: Boolean = failures.isEmpty
  }

  /** What a round measures from: the calibration so far, and each capture's measure under it — at the start the
   *  captured decisions themselves, the ratchet's own takes. */
  final case class Baseline(calibration: IdentityCalibration, captures: Seq[CaptureReplay.Measure]) {
    def takes: Seq[UnmatchedClusters.Take] = captures.flatMap(_.takes)
    def gaps: Int = captures.map(_.gaps).sum
  }

  /** The ratchet as captured, under `calibration`: every capture's decisions, the agreement replayed. */
  def baseline(replays: Seq[CaptureReplay], calibration: IdentityCalibration): Baseline =
    Baseline(calibration, replays.map(r => r.measure(r.capture.decisions, r.resolved(calibration), calibration)))

  /** Every capture measured under `calibration` on top of `base` ([[CaptureReplay.measure]]). */
  def measureOn(replays: Seq[CaptureReplay], base: Baseline, calibration: IdentityCalibration): Seq[CaptureReplay.Measure] =
    replays.zip(base.captures).map((r, b) => r.measure(b.decisions, b.resolved, calibration))

  private def byListing(takes: Seq[UnmatchedClusters.Take]) = takes.groupMapReduce(t => (t.country, t.venue, t.rawTitle))(identity)((a, _) => a)

  /** `now`'s takes judged against `control`'s, listing by listing, and checked against `expected`. */
  def judged(control: Seq[UnmatchedClusters.Take], now: Seq[UnmatchedClusters.Take], labels: Seq[UnmatchedClusters.Label],
             expected: Set[(String, String, String, String)], billsSeveral: ((String, String, String)) => Boolean): (Seq[Moved], Seq[String]) = {
    val (was, is) = (byListing(control), byListing(now))
    def group(t: UnmatchedClusters.Take) =
      labels.filter(l => l.right && l.covers(t)).map(_.film).sorted.headOption.getOrElse(s"${t.country}:${t.rawTitle}")
    val moved = (was.keySet ++ is.keySet).toSeq.sorted.flatMap { k =>
      val (before, after) = (was.get(k), is.get(k))
      DecorationDiscovery.judge(before.fold("")(_.film), after.fold("")(_.film), billsSeveral(k), after.flatMap(UnmatchedClusters.verdict(_, labels))).map { v =>
        val verdict = if (v == DecorationDiscovery.Lost && before.flatMap(UnmatchedClusters.verdict(_, labels)).contains(true)) LostRight else v
        Moved(k._1, k._2, k._3, before.fold("")(t => s"${t.film} ${t.title}"), after.fold("")(t => s"${t.film} ${t.title}"), verdict,
          after.orElse(before).fold(s"${k._1}:${k._3}")(group))
      }
    }
    val held  = control.map(t => (t.country, t.venue, t.rawTitle, t.film)).toSet intersect expected
    val nowIs = now.map(t => (t.country, t.venue, t.rawTitle, t.film)).toSet
    (moved, (held -- nowIs).toSeq.sorted.map(_.productIterator.mkString("\t")))
  }

  /** The whole refit: each round every change not yet made measured on top of the changes kept so far (in parallel), the
   *  best kept one made; until none is kept or `maxChanges` are. */
  def search(replays: Seq[CaptureReplay], labels: Seq[UnmatchedClusters.Label], expected: Set[(String, String, String, String)], start: IdentityCalibration,
             relearn: Set[String], maxChanges: Int, threads: Int, log: String => Unit): (IdentityCalibration, Seq[Measured], Seq[Seq[Measured]]) = {
    val units = replays.flatMap(r => labelledUnits(r.capture, labels, start))
    log(s"labelled units: ${units.size} (${units.count(_.same)} same film, ${units.count(u => heldOut(u.group))} held out)")
    val billsSeveral: ((String, String, String)) => Boolean = {
      val byListing = replays.flatMap(r => r.capture.listings.map(l => (r.capture.country.code, l.venue, l.rawTitle) -> l)).toMap
      k => byListing.get(k).exists(l => ListingShape.billsSeveral(l) ||
        IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing(l.title, Some(l.rawTitle), decorations = TitleDecorations.resolver)))
    }
    val pool = Executors.newFixedThreadPool(threads)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    try {
      var base = baseline(replays, start)
      val verdicts = base.takes.map(UnmatchedClusters.verdict(_, labels))
      log(s"control (the captured decisions, as the ratchet reads them): ${base.takes.size} takes, ${verdicts.count(_.contains(true))} right, " +
        s"${verdicts.count(_.contains(false))} wrong, ${verdicts.count(_.isEmpty)} unjudged; ${base.gaps} TMDB questions its resolve asks unanswered")
      var kept   = Vector.empty[Measured]
      var rounds = Vector.empty[Seq[Measured]]
      var done   = false
      while (!done && kept.size < maxChanges) {
        val made     = kept.map(_.change.parameter).toSet
        val from     = base
        val proposed = (cutChanges(from.calibration) ++ boundChanges(from.calibration) ++ weightChanges(from.calibration, units, relearn))
          .filterNot(c => made(c.parameter))
        log(s"round ${rounds.size + 1}: ${proposed.size} proposals")
        val measured = Await.result(Future.traverse(proposed) { change => Future {
          val after = measureOn(replays, from, change.applyTo(from.calibration))
          val (moved, lost) = judged(from.takes, after.flatMap(_.takes), labels, expected, billsSeveral)
          Measured(change, moved, math.max(0, after.map(_.gaps).sum - from.gaps), lost, after)
        } }, Duration.Inf)
        measured.filter(_.moved.nonEmpty).foreach(m => log(f"  ${if (m.kept) "KEPT" else "    "} ${m.change.parameter}%-70s ${m.change.from} → ${m.change.to}: " +
          s"right ${m.right} (${m.supportFitting.size} films fitting, ${m.supportHeldOut.size} held out) wrong ${m.wrong} switched ${m.switched} " +
          s"lost ${m.lost}+${m.lostRight} unjudged ${m.unjudged}${if (m.kept) "" else m.failures.mkString(" — ", ", ", "")}" +
          m.moved.filterNot(x => x.verdict == DecorationDiscovery.Right || x.verdict == DecorationDiscovery.Lost).take(5)
            .map(x => s"\n         ${x.verdict}: ${x.country} ${x.venue} '${x.rawTitle}' ${x.before} → ${x.after}").mkString))
        rounds :+= measured
        measured.filter(_.kept).sortBy(m => (-m.supportFitting.size, -m.right, m.change.parameter)).headOption match {
          case Some(m) =>
            kept :+= m
            base = Baseline(m.change.applyTo(from.calibration), m.after)
          case None => done = true
        }
      }
      (base.calibration, kept, rounds)
    } finally { pool.shutdown(); pool.awaitTermination(1, TimeUnit.MINUTES) }
  }

  // ── writing ───────────────────────────────────────────────────────────────────────────────

  /** `refitted` as the artefact writes it: its version and provenance naming the changes. */
  def versioned(refitted: IdentityCalibration, start: IdentityCalibration, kept: Seq[Measured]): IdentityCalibration = {
    val base = start.version.replaceAll("-refit-[0-9a-f]+$", "")
    val id   = f"${MurmurHash3.seqHash(kept.map(k => s"${k.change.parameter}:${k.change.to}"))}%08x"
    refitted.copy(version = s"$base-refit-$id", provenance = start.provenance.updated("refit",
      s"scripts.IdentityRefit on the unmatched-cluster ratchet (labels.tsv, expected-matches.tsv): " +
        kept.map(k => s"${k.change.parameter} ${k.change.from} → ${k.change.to}").mkString("; ")))
  }

  private def cell(s: String) = s.replace("|", "\\|").replace("\n", " ")

  /** The PR body: each kept change (old → new), what it gained (named) in and out of sample, and every proposal's measure. */
  def report(kept: Seq[Measured], rounds: Seq[Seq[Measured]], units: String): String = {
    val sb = new StringBuilder
    sb ++= s"## Identity rule refit\n\nRe-tuned the existing rules' numbers in `identity-weights.json` on the unmatched-cluster ratchet " +
      s"(the five captures, `labels.tsv` and `expected-matches.tsv`; $units). A change is kept only with 0 wrong, 0 unjudged, 0 switched, " +
      s"no right or expected take lost, nothing unanswered, and at least $MinSupport labelled films of the fitting fold newly right; " +
      s"films hashed into fold 0 of $Folds are held out (never counted by a weight refit) and reported apart. Steps are capped " +
      s"(cuts ±$MaxCutStep, bounds ±${(BoundStepShare * 100).toInt}%, weights ±$MaxWeightStep log-odds).\n\n"
    if (kept.isEmpty) sb ++= "**Nothing kept.**\n\n"
    else {
      sb ++= "| # | parameter | old | new | right (fitting films / held-out films) | lost (not right) |\n|---|---|---|---|---|---|\n"
      kept.zipWithIndex.foreach { case (k, i) =>
        sb ++= s"| ${i + 1} | ${cell(k.change.parameter)} | `${cell(k.change.from)}` | `${cell(k.change.to)}` | ${k.right} (${k.supportFitting.size} / ${k.supportHeldOut.size}) | ${k.lost} |\n"
      }
      kept.zipWithIndex.foreach { case (k, i) =>
        sb ++= s"\n### ${i + 1}. ${cell(k.change.parameter)}\n\n| country | venue | listing | before | after | verdict | fold |\n|---|---|---|---|---|---|---|\n"
        k.moved.foreach(m => sb ++= s"| ${m.country} | ${cell(m.venue)} | ${cell(m.rawTitle)} | ${cell(m.before)} | ${cell(m.after)} | ${m.verdict} | ${if (m.heldOut) "held out" else "fitting"} |\n")
      }
    }
    sb ++= "\n### Every proposal that moved a take\n\n| round | parameter | old → new | right | fitting / held-out films | wrong | switched | lost | unjudged | outcome |\n|---|---|---|---|---|---|---|---|---|---|\n"
    rounds.zipWithIndex.foreach { case (ms, r) =>
      ms.filter(_.moved.nonEmpty).sortBy(m => (!m.kept, -m.right, m.change.parameter)).foreach { m =>
        sb ++= s"| ${r + 1} | ${cell(m.change.parameter)} | `${cell(m.change.from)}` → `${cell(m.change.to)}` | ${m.right} | ${m.supportFitting.size} / ${m.supportHeldOut.size} | " +
          s"${m.wrong} | ${m.switched} | ${m.lost + m.lostRight} | ${m.unjudged} | ${if (m.kept) "kept" else m.failures.mkString(", ")} |\n"
      }
    }
    // a near miss — nothing wrong, switched or lost, only takes no label judges — waits on a judgement, not on data
    val toJudge = rounds.flatten.filter(m => m.wrong == 0 && m.switched == 0 && m.lostRight == 0 && m.unjudged > 0)
      .flatMap(_.moved.filter(_.verdict == DecorationDiscovery.Unjudged)).distinctBy(m => (m.country, m.venue, m.rawTitle, m.after))
    if (toJudge.nonEmpty) {
      sb ++= "\n### Takes to judge\n\nOnly unjudged takes held these proposals back: judged in `labels.tsv` (by the review page, then " +
        "`ReviewLabelsCli export`), the next run measures them.\n\n| country | venue | listing | would take |\n|---|---|---|---|\n"
      toJudge.foreach(m => sb ++= s"| ${m.country} | ${cell(m.venue)} | ${cell(m.rawTitle)} | ${cell(m.after)} |\n")
    }
    sb.toString
  }

  def main(args: Array[String]): Unit = {
    val apply   = args.contains("--apply")
    val opts    = args.sliding(2).collect { case Array(k, v) if k.startsWith("--") && !v.startsWith("--") => k.stripPrefix("--") -> v }.toMap
    val labels  = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val expected = UnmatchedClusters.readTakeLines(UnmatchedClusters.Directory.resolve("expected-matches.tsv"))
    val relearn = opts.get("relearn").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSet
    // `--weights <file>`: refit (and with --apply, write) another artefact than the shipped one — a positive control
    // starts from a deliberately mistuned copy and must find its way back
    val artefact = opts.get("weights").map(Paths.get(_)).getOrElse(Artefact)
    val start   = Json.parse(Files.readString(artefact)).as[IdentityCalibration]
    start.scopes.get(IdentityMeasures.ListingFilm).foreach(_.signals.toSeq.sortBy(_._1).filterNot((name, w) => reproduces(name, w)).map(_._1) match {
      case Seq() => ()
      case odd   => println(s"not refitted (their counts do not give their weights back): ${odd.mkString(", ")}")
    })
    val replays = CaptureReplay.all()
    println(s"TMDB gaps: ${if (CaptureReplay.asksLive) "asked live" else "NOT asked (no TMDB_API_KEY) — a proposal asking one is unmeasured"}")
    val threads = opts.get("threads").map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    val (refitted, kept, rounds) = search(replays, labels, expected, start, relearn, opts.get("max-changes").map(_.toInt).getOrElse(5), threads, println)
    println(s"kept ${kept.size}: ${kept.map(k => s"${k.change.parameter} ${k.change.from} → ${k.change.to}").mkString("; ")}")
    val units = s"${labels.size} labels, ${expected.size} expected takes${if (relearn.isEmpty) "" else s"; relearned from fresh units: ${relearn.toSeq.sorted.mkString(", ")}"}"
    opts.get("report").map(Paths.get(_)).foreach { p =>
      Files.createDirectories(p.toAbsolutePath.getParent)
      Files.writeString(p, report(kept, rounds, units), StandardCharsets.UTF_8)
    }
    if (apply && kept.nonEmpty) {
      Files.writeString(artefact, Json.prettyPrint(Json.toJson(versioned(refitted, start, kept))) + "\n", StandardCharsets.UTF_8)
      println(s"wrote $artefact")
    }
  }
}
