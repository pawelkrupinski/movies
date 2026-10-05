package scripts

import services.identity.UnifiedEvidence

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * How best to COMBINE the identity signals ([[UnifiedEvidence]]) — every strategy on [[IdentityUnifiedFit]]'s rows, on
 * one bar: the ratchet's fixture (right and wrong listings by hand label) against today's, the whole corpus against
 * today (switched, lost, gained), and the corrections a strategy would make to a film the MODEL took (role B), judged by
 * hand labels alone — the model's own takes are no truth for a correction of them — the unlabelled ones written for
 * review (`test/resources/fixtures/identity-unmatched/review-candidates.tsv`).
 *
 *   worker/Test/runMain scripts.IdentityUnifiedStrategies [--training <tsv.gz>] [--report <md>] [--review <tsv>]
 *
 * Two roles: FILL takes a film for a cluster the model took none for (the agreement stage's role today); CORRECT vetoes
 * or replaces a film the model took (nothing does today). The strategies:
 *  1. today's cascade;
 *  2. GREEDY forward selection of (signal, role) rules on today's stack — each round the rule with the largest labelled
 *     gain and no new wrong listing, until none gains;
 *  3. per-signal likelihood ratios on the hand labels (smoothed), summed, the families' correlated takes counted as one
 *     signal (`count.takers`) rather than five;
 *  4. a weighted majority vote: each signal firing votes its hand-label log-odds, the guards veto, below a margin abstains;
 *  5. stacking: the monotone logistic over every signal, hand labels ×30;
 *  6. priority with vetoes: the most precise signal firing on one unvetoed contender decides — signals ordered by their
 *     hand-label precision, kept while none of them took a wrong film in training;
 *  7. gradient-boosted trees of depth 3, hand labels ×30.
 * 3–7 are trained with whole venues held out and gated by a precision cut (8): the lowest held-out score above every new
 * wrong take, abstaining below it.
 */
object IdentityUnifiedStrategies {
  import IdentityUnifiedFit.{Folds, Row, cutOf, keptOf, read}

  private val names   = UnifiedEvidence.Names
  private val column  = names.zipWithIndex.toMap
  private val rules   = UnifiedEvidence.ModelRules.map(column)
  private val guards  = UnifiedEvidence.Guards
  private val HandWeight = 30.0
  private def fires(row: Row, signal: String) = row.x(column(signal)) > 0
  private def vetoed(row: Row) = guards.exists(fires(row, _))
  def modelTook(row: Row): Boolean = rules.exists(row.x(_) > 0)

  /** A cluster: its contenders, today's take among them, and whether the model's own rules took it. */
  final case class Cluster(id: String, rows: Seq[Row]) {
    val today: Option[Row] = rows.find(_.today)
    val byModel: Boolean   = today.exists(modelTook)
    def head: Row = rows.head
  }

  /** What one strategy did: per cluster, its take (`None`: none). */
  type Takes = Map[String, Option[Row]]

  /** A change against today's stack on one cluster, judged by hand labels where they reach. */
  final case class Change(cluster: Cluster, from: Option[Row], to: Option[Row], by: String) {
    def role: String = if (cluster.byModel) "correct" else "fill"
    def gainedRight: Int = to.map(_.right).getOrElse(0)
    def gainedWrong: Int = to.map(_.wrong).getOrElse(0)
    def lostRight: Int   = from.map(_.right).getOrElse(0)
    def lostWrong: Int   = from.map(_.wrong).getOrElse(0)
    /** Does no hand label reach the change: the film it takes and the film it drops both unjudged? */
    def unlabelled: Boolean = gainedRight + gainedWrong + lostRight + lostWrong == 0
  }

  def changes(clusters: Seq[Cluster], takes: Takes, by: String): Seq[Change] = clusters.flatMap { c =>
    val to = takes.getOrElse(c.id, c.today)
    Option.when(to.map(_.film) != c.today.map(_.film))(Change(c, c.today, to, by))
  }

  /** One strategy's measures: the fixture's right and wrong listings, the corpus's switched, lost and gained listings
   *  (fills only), the corrections it proposes to model takes (labelled right, labelled wrong, unlabelled), per country. */
  final case class Result(name: String, right: Int, wrong: Int, switched: Int, lost: Int, gained: Int, correctRight: Int, correctWrong: Int,
                          correctUnlabelled: Int, explainable: String, perCountry: Map[String, (Int, Int, Int)])

  def measure(name: String, clusters: Seq[Cluster], takes: Takes, explainable: String): Result = {
    val all      = changes(clusters, takes, name)
    val fills    = all.filter(_.role == "fill")
    val corrects = all.filter(_.role == "correct")
    // the fixture as the ratchet counts it, the fills applied and the corrections not
    val applied  = clusters.map(c => c.id -> (if (c.byModel) c.today else takes.getOrElse(c.id, c.today))).toMap
    val fixture  = clusters.filter(_.head.origin == "fixture").flatMap(c => applied(c.id))
    val perCountry = clusters.groupBy(_.head.country).view.mapValues { cs =>
      val ch = fills.filter(f => cs.exists(_.id == f.cluster.id))
      (ch.filter(_.to.isDefined).map(_.cluster.head.listings).sum, ch.filter(_.to.isEmpty).map(_.cluster.head.listings).sum,
        ch.map(c => c.gainedRight - c.lostRight).sum)
    }.toMap
    Result(name, fixture.map(_.right).sum, fixture.map(_.wrong).sum,
      fills.filter(c => c.from.isDefined && c.to.isDefined).map(_.cluster.head.listings).sum,
      fills.filter(c => c.from.isDefined && c.to.isEmpty).map(_.cluster.head.listings).sum,
      fills.filter(c => c.from.isEmpty && c.to.isDefined).map(_.cluster.head.listings).sum,
      corrects.count(c => c.gainedRight > 0 || c.lostWrong > 0), corrects.count(c => c.gainedWrong > 0 || c.lostRight > 0),
      corrects.count(_.unlabelled), explainable, perCountry)
  }

  // ── 2. greedy forward selection ──────────────────────────────────────────────────────────

  /** A rule a greedy round may add: `fill` takes the one unvetoed contender a positive signal fires on, for a cluster
   *  nothing took; `correct` replaces a model take by the one unvetoed contender a positive signal fires on and it does
   *  not, or — a guard — drops a model take the guard fires on. */
  final case class Step(signal: String, role: String) { def name = s"$signal ($role)" }

  def apply(step: Step, clusters: Seq[Cluster], state: Takes): Takes = state ++ clusters.flatMap { c =>
    val now    = state.getOrElse(c.id, c.today)
    val guard  = guards.contains(step.signal)
    def theOne = c.rows.filter(r => !vetoed(r) && fires(r, step.signal)) match { case Seq(one) => Some(one); case _ => None }
    step.role match {
      case "fill" if now.isEmpty && !c.byModel && !guard => theOne.map(one => c.id -> Some(one))
      case "correct" if c.byModel && now.exists(modelTook) =>
        if (guard) Option.when(now.exists(fires(_, step.signal)))(c.id -> None)
        else theOne.filter(one => now.exists(t => t.film != one.film && !fires(t, step.signal))).map(one => c.id -> Some(one))
      case _ => None
    }
  }

  final case class Round(step: Step, gain: Int, newWrong: Int, unlabelled: Int, accepted: Boolean, why: String)

  def greedy(clusters: Seq[Cluster]): (Takes, Seq[Round], Seq[Round], Seq[Change]) = {
    val candidates = names.filterNot(n => n == "model.logit" || n.startsWith("rule.")).flatMap(s => Seq(Step(s, "fill"), Step(s, "correct")))
    var state: Takes = Map.empty
    var accepted = Vector.empty[Round]
    var review   = Vector.empty[Change]
    var remaining = candidates
    var last: Seq[Round] = Nil
    var going = true
    while (going) {
      val tried = remaining.map { step =>
        val next  = apply(step, clusters, state)
        val moved = clusters.flatMap { c =>
          val (was, now) = (state.getOrElse(c.id, c.today), next.getOrElse(c.id, c.today))
          Option.when(was.map(_.film) != now.map(_.film))(Change(c, was, now, step.name))
        }
        val gain     = moved.map(c => c.gainedRight + c.lostWrong - c.lostRight).sum
        val newWrong = moved.map(_.gainedWrong).sum
        val why = if (moved.isEmpty) "changes nothing" else if (newWrong > 0) s"$newWrong new wrong listing(s)"
                  else if (moved.exists(_.lostRight > 0)) s"drops ${moved.map(_.lostRight).sum} right listing(s)"
                  else if (gain <= 0) "no labelled gain" else "gains"
        (Round(step, gain, newWrong, moved.count(_.unlabelled), why == "gains", why), next, moved)
      }
      tried.filter(_._1.accepted).sortBy { case (r, _, _) => (-r.gain, r.unlabelled, r.step.name) }.headOption match {
        case Some((round, next, moved)) =>
          accepted :+= round; state = next; review ++= moved.filter(_.unlabelled); remaining = remaining.filterNot(_ == round.step)
        case None => last = tried.map(_._1); going = false
      }
    }
    (state, accepted, last, review)
  }

  /** Greedy selection held out by venue: the rules each fold's other venues select, applied in their order to its own. */
  def greedyHeldOut(clusters: Seq[Cluster]): Takes = (0 until Folds).flatMap { fold =>
    val (test, train) = clusters.partition(_.head.fold == fold)
    val steps = greedy(train)._2.map(_.step)
    steps.foldLeft(Map.empty: Takes)((state, step) => apply(step, test, state))
  }.toMap

  /** STABLE selection: the rules greedy selects on the whole rows that it also selects with every fold's venues held
   *  out — a rule one fold's labels alone carry is no rule (held out, those made the wrong takes). In the whole rows'
   *  order. */
  def stableSteps(clusters: Seq[Cluster]): Seq[Step] = {
    val perFold = (0 until Folds).map(fold => greedy(clusters.filterNot(_.head.fold == fold))._2.map(_.step).toSet)
    perFold.zipWithIndex.foreach { case (steps, fold) => println(s"fold $fold selects: ${steps.map(_.name).toSeq.sorted.mkString(", ")}") }
    greedy(clusters)._2.map(_.step).filter(step => perFold.forall(_(step)))
  }

  /** The stable rules, each fold's own taken by those it selects stably without it — held out twice over. */
  def stableHeldOut(clusters: Seq[Cluster]): Takes = (0 until Folds).flatMap { fold =>
    val (test, train) = clusters.partition(_.head.fold == fold)
    stableSteps(train).foldLeft(Map.empty: Takes)((state, step) => apply(step, test, state))
  }.toMap

  def applyAll(steps: Seq[Step], clusters: Seq[Cluster]): Takes = steps.foldLeft(Map.empty: Takes)((state, step) => apply(step, clusters, state))

  // ── 3–7. scorers, held out by venue, gated by the precision cut ──────────────────────────

  /** A scorer trained on rows. */
  trait Scorer { def name: String; def explainable: String; def train(rows: Seq[Row]): Row => Double }

  private def handRows(rows: Seq[Row]) = rows.filter(r => r.hand && r.label.isDefined)

  /** 3. Each signal's smoothed likelihood ratio on the hand labels, summed in log-odds; the five families' takes and
   *  leans counted once each, as their counts, since families citing each other are no independent evidence. */
  object LikelihoodRatios extends Scorer {
    val name = "likelihood ratios"; val explainable = "yes (one ratio per signal)"
    private val independent = names.filterNot(n => n.startsWith("family.") && (n.endsWith(".took") || n.endsWith(".leans")))
      .filterNot(n => n.startsWith("count.takers") && n != "count.takers").filterNot(_.startsWith("and."))
    def train(rows: Seq[Row]): Row => Double = {
      val hand = handRows(rows); val pos = hand.count(_.label.contains(true)).toDouble; val neg = hand.size - pos
      def bucket(row: Row, s: String) = math.min(row.x(column(s)), 3.0)
      val ratios = independent.map { s =>
        s -> hand.groupBy(bucket(_, s)).view.mapValues { rs =>
          val p = (rs.count(_.label.contains(true)) + 1.0) / (pos + 2); val q = (rs.count(_.label.contains(false)) + 1.0) / (neg + 2)
          math.log(p / q)
        }.toMap
      }.toMap
      val prior = math.log((pos + 1) / (neg + 1))
      row => services.identity.LogisticFit.sigmoid(prior + independent.map(s => ratios(s).getOrElse(bucket(row, s), 0.0)).sum)
    }
  }

  /** 4. Each positive signal firing votes its hand-label log-odds (precision against its misses); a guard vetoes. */
  object WeightedVote extends Scorer {
    val name = "weighted majority vote"; val explainable = "yes (votes listed)"
    def train(rows: Seq[Row]): Row => Double = {
      val hand = handRows(rows)
      val positive = UnifiedEvidence.Signals.filter(_.direction > 0).map(_.name).filterNot(_ == "model.logit")
      val weight = positive.map { s =>
        val firing = hand.filter(fires(_, s))
        s -> math.log((firing.count(_.label.contains(true)) + 1.0) / (firing.count(_.label.contains(false)) + 1.0))
      }.toMap
      row => if (vetoed(row)) 0.0 else services.identity.LogisticFit.sigmoid(positive.filter(fires(row, _)).map(weight).sum - 1)
    }
  }

  /** 5. The monotone logistic over every signal, the guards hard, hand labels ×[[HandWeight]] — a meta-learner over
   *  each signal's verdict. */
  object Stacking extends Scorer {
    val name = "stacking (monotone logistic)"; val explainable = "yes (weighted contributions)"
    def train(rows: Seq[Row]): Row => Double = {
      val w = IdentityUnifiedFit.weightsOf(rows.filterNot(vetoed), keptOf(guards), HandWeight)
      row => if (vetoed(row)) 0.0 else IdentityUnifiedFit.probability(w, row)
    }
  }

  /** 6. The most precise signal firing on the contender decides: signals ranked by hand-label precision, kept while
   *  none of the kept took a wrong film in training; the score is the precision of the best kept signal firing. */
  object Priority extends Scorer {
    val name = "priority with vetoes"; val explainable = "yes (the deciding signal)"
    def train(rows: Seq[Row]): Row => Double = {
      val hand = handRows(rows).filterNot(vetoed)
      val positive = UnifiedEvidence.Signals.filter(_.direction > 0).map(_.name).filterNot(_ == "model.logit")
      val ranked = positive.map { s =>
        val firing = hand.filter(fires(_, s)); val good = firing.count(_.label.contains(true))
        (s, (good + 1.0) / (firing.size + 2.0), firing.count(_.label.contains(false)))
      }.sortBy { case (s, precision, _) => (-precision, s) }
      val kept = ranked.takeWhile(_._3 == 0).map(r => r._1 -> r._2).toMap
      row => if (vetoed(row)) 0.0 else kept.collect { case (s, precision) if fires(row, s) => precision }.maxOption.getOrElse(0.0)
    }
  }

  /** 7. Gradient-boosted trees of depth 3 over every signal but the guards, hand labels ×[[HandWeight]]. */
  object Trees extends Scorer {
    val name = "boosted trees depth 3"; val explainable = "partial (readable trees, many of them)"
    def train(rows: Seq[Row]): Row => Double = {
      val learner = IdentityUnifiedExperiments.Boosted(keptOf(guards), HandWeight, 3, label = "")
      val m = learner.model(rows.filterNot(vetoed))
      row => if (vetoed(row)) 0.0 else m.probability(row.x)
    }
  }

  /** A scorer held out by venue, gated by the precision cut on what it fills, and its corrections at the same cut. */
  def gated(scorer: Scorer, clusters: Seq[Cluster]): Takes = {
    val rows = clusters.flatMap(_.rows)
    val held = (0 until Folds).flatMap { fold =>
      val score = scorer.train(rows.filter(_.fold != fold))
      rows.filter(_.fold == fold).map(r => r -> score(r))
    }
    val byCluster = held.groupBy(_._1.cluster)
    val fillable  = clusters.filterNot(_.byModel).flatMap(c => byCluster.getOrElse(c.id, Nil))
    val cut = cutOf(fillable)
    clusters.map { c =>
      val best = byCluster.getOrElse(c.id, Nil).sortBy { case (r, p) => (-p, r.film) }.headOption.filter(_._2 >= cut).map(_._1)
      c.id -> (if (c.byModel) best.orElse(c.today) else best)
    }.toMap
  }

  def main(args: Array[String]): Unit = {
    val opts     = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val rows     = read(opts.get("training").map(Paths.get(_)).getOrElse(IdentityUnifiedFit.Training))
    val clusters = rows.groupBy(_.cluster).toSeq.sortBy(_._1).map { case (id, rs) => Cluster(id, rs.sortBy(_.film)) }
    val (greedyTakes, accepted, rejected, _) = greedy(clusters)
    val stable = stableSteps(clusters)
    val stableTakes = applyAll(stable, clusters)
    val greedyReview = changes(clusters, stableTakes, "stable greedy").filter(_.unlabelled)
    val scorers  = Seq(LikelihoodRatios, WeightedVote, Stacking, Priority, Trees)
    val gatedTakes = scorers.map(s => s -> gated(s, clusters))
    val results = Seq(measure("1. today's cascade", clusters, Map.empty, "yes (rule lines)"),
      measure("2. greedy forward selection (in sample)", clusters, greedyTakes, "yes (the accepted rules)"),
      measure("2. greedy forward selection (held out)", clusters, greedyHeldOut(clusters), "yes (the accepted rules)"),
      measure("2b. stable greedy (in sample)", clusters, applyAll(stable, clusters), "yes (the accepted rules)"),
      measure("2b. stable greedy (held out)", clusters, stableHeldOut(clusters), "yes (the accepted rules)")) ++
      gatedTakes.zipWithIndex.map { case ((s, t), i) => measure(s"${i + 3}. ${s.name} + precision cut", clusters, t, s.explainable) }

    val b = new StringBuilder
    def line(s: String) = b ++= s ++= "\n"
    line("| strategy | ratchet right | wrong | corpus switched | lost | gained | corrections right / wrong / unlabelled | explainable |")
    line("|---|---|---|---|---|---|---|---|")
    results.foreach(r => line(s"| ${r.name} | ${r.right} | ${r.wrong} | ${r.switched} | ${r.lost} | ${r.gained} | ${r.correctRight} / ${r.correctWrong} / ${r.correctUnlabelled} | ${r.explainable} |"))
    line("\nPer country (fills: gained listings, lost listings, labelled net):\n")
    results.foreach(r => line(s"- ${r.name}: " + r.perCountry.toSeq.sorted.map { case (cc, (g, l, n)) => s"$cc +$g −$l net $n" }.mkString(", ")))
    line("\n## Greedy, held out: the wrong listings\n")
    changes(clusters, greedyHeldOut(clusters), "greedy").filter(_.gainedWrong > 0).foreach(c =>
      line(s"- ${c.cluster.head.country} ${c.cluster.head.rawTitle} ×${c.cluster.head.listings} → ${c.to.fold("none")(r => s"${r.film} ${r.filmTitle}")}"))
    line(s"\n## Stable greedy: the rules every fold selects, in order\n\n${stable.map(s => s"- ${s.name}").mkString("\n")}")
    line("\n## Stable greedy, held out: the wrong listings\n")
    changes(clusters, stableHeldOut(clusters), "stable").filter(_.gainedWrong > 0).foreach(c =>
      line(s"- ${c.cluster.head.country} ${c.cluster.head.rawTitle} ×${c.cluster.head.listings} → ${c.to.fold("none")(r => s"${r.film} ${r.filmTitle}")}"))
    line("\n## Greedy forward selection: accepted, in order\n")
    if (accepted.isEmpty) line("(none)")
    accepted.foreach(r => line(s"- ${r.step.name}: +${r.gain} labelled listings, 0 new wrong, ${r.unlabelled} unlabelled change(s) for review"))
    line("\n## Greedy: the rest, and why each was refused (last round)\n")
    rejected.filter(_.why != "changes nothing").sortBy(r => (r.why, r.step.name)).foreach(r =>
      line(s"- ${r.step.name}: ${r.why}${if (r.unlabelled > 0) s"; ${r.unlabelled} unlabelled change(s)" else ""}"))
    line(s"\n(${rejected.count(_.why == "changes nothing")} (signal, role) pairs change nothing.)")
    val report = b.toString
    // the stable rules, pinned for the agreement stage to apply
    val shipped = rulesOf(stable, results.find(_.name.startsWith("2b. stable greedy (in sample)")), results.find(_.name.startsWith("2b. stable greedy (held out)")),
      IdentityUnifiedFit.versionOf(opts.get("training").map(Paths.get(_)).getOrElse(IdentityUnifiedFit.Training)))
    Files.writeString(opts.get("rules").map(Paths.get(_)).getOrElse(RulesArtefact), play.api.libs.json.Json.prettyPrint(play.api.libs.json.Json.toJson(shipped)) + "\n")
    opts.get("report").foreach(path => Files.writeString(Paths.get(path), report))
    println(report)

    // greedy's held-out wrong listings, named
    val heldOutWrong = changes(clusters, greedyHeldOut(clusters), "greedy").filter(_.gainedWrong > 0)
    println(heldOutWrong.map(c => s"held-out wrong: ${c.cluster.head.country} ${c.cluster.head.rawTitle} → ${c.to.map(r => s"${r.film} ${r.filmTitle}")}")
      .mkString("\n"))
    // the changes the adopted strategy (greedy) makes that no hand label judges, for review
    writeReview(opts.get("review").map(Paths.get(_)).getOrElse(ReviewFile), greedyReview)
  }

  val RulesArtefact: Path = Paths.get("common/src/main/resources", services.identity.UnifiedRules.ResourcePath)

  /** The stable steps as the pinned artefact, with what they measured. */
  def rulesOf(stable: Seq[Step], inSample: Option[Result], heldOut: Option[Result], version: String): services.identity.UnifiedRules =
    services.identity.UnifiedRules(version, guards, stable.filter(_.role == "fill").map(_.signal), stable.filter(_.role == "correct").map(_.signal),
      (inSample.toSeq.flatMap(r => Seq("right" -> r.right, "wrong" -> r.wrong, "gained" -> r.gained, "lost" -> r.lost, "switched" -> r.switched)) ++
        heldOut.toSeq.flatMap(r => Seq("heldOutRight" -> r.right, "heldOutWrong" -> r.wrong))).map { case (k, v) => k -> v.toDouble }.toMap)

  val ReviewFile: Path = Paths.get("test/resources/fixtures/identity-unmatched/review-candidates.tsv")

  def writeReview(path: Path, review: Seq[Change]): Unit = {
    def film(row: Option[Row]) = row.fold("none")(r => s"${r.film} ${r.filmTitle}")
    def evidence(row: Option[Row]) = row.fold("")(r => names.indices.filter(i => r.x(i) != 0).map(i => s"${names(i)}=${r.x(i)}").mkString(" "))
    val lines = review.groupBy(c => (c.cluster.id, c.to.map(_.film))).values.map(cs => cs.head -> cs.map(_.by).distinct.sorted.mkString(", ")).toSeq
      .sortBy { case (c, _) => (c.cluster.head.country, c.cluster.head.venue, c.cluster.head.rawTitle) }
      .map { case (c, by) => Seq(c.cluster.head.country, c.cluster.head.venue, c.cluster.head.rawTitle, film(c.from), film(c.to), s"$by [${c.role}]",
        s"×${c.cluster.head.listings}; proposed: ${evidence(c.to)}; model's: ${evidence(c.from)}").mkString("\t") }
    Files.writeString(path, ("country\tvenue\trawTitle\tmodel film\tproposed film\tsignal\tevidence" +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    println(s"wrote ${lines.size} review candidate(s) to $path")
  }
}
