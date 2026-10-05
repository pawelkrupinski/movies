package scripts

import play.api.libs.json.Json
import services.identity.{LogisticFit, UnifiedEvidence, UnifiedWeights}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

/**
 * Fits the UNIFIED evidence model ([[UnifiedWeights]]) from the contender rows `integration.IdentityUnifiedDataset`
 * writes (the checked-in copy is [[Training]]), and measures it against today's stack — the model and the agreement
 * stage after it — on the same rows:
 *
 *   worker/Test/runMain scripts.IdentityUnifiedFit [--training <tsv.gz>] [--report <md>] [--version <v>]
 *
 * It writes two artefacts: the UNIFIED model ([[Artefact]], every signal compensatory) and the HYBRID
 * ([[HybridArtefact]]): the non-compensatory guards ([[UnifiedEvidence.Guards]]) hard — a contender tripping one is
 * never scored — and the fitted score in place of the hand-set thresholds among the contenders passing them.
 *
 * The fit: a logistic regression over every signal of [[UnifiedEvidence.Signals]], each weight held to its signal's
 * direction ([[LogisticFit.fitSigned]]), L2 [[L2]], on the labelled rows. Whole VENUES are held out ([[Folds]] folds by
 * the venue a cluster goes by), so no cluster is scored by a model that saw its venue's other clusters. The CUT: each
 * held-out cluster takes its best contender, and the cut is the lowest probability above every held-out take a hand
 * label calls wrong and every take that would move a film today's stack shows — no wrong take, no switched listing.
 * A function of the rows: `IdentityUnifiedFitSpec` pins that the shipped artefact is what this fits from [[Training]].
 */
object IdentityUnifiedFit {

  val Training: Path = Paths.get("test/resources/fixtures/identity-unified/training.tsv.gz")
  val Artefact: Path = Paths.get("common/src/main/resources", UnifiedWeights.ResourcePath)
  val HybridArtefact: Path = Paths.get("common/src/main/resources", UnifiedWeights.HybridResourcePath)
  val L2    = 1.0
  val Folds = 5

  /** One contender: its cluster and where that comes from, the venue its fold goes by, the cluster's listings, the film,
   *  whether today's stack takes it, its label (none: unlabelled), the label's source, the listings a hand label judges it
   *  right and wrong for, and its signals in [[UnifiedEvidence.Names]]'s order. */
  final case class Row(country: String, cluster: String, origin: String, venue: String, listings: Int, rawTitle: String, film: String, filmTitle: String,
                       today: Boolean, label: Option[Boolean], source: String, right: Int, wrong: Int, x: IndexedSeq[Double]) {
    def fold: Int = Math.floorMod(scala.util.hashing.MurmurHash3.stringHash(venue), Folds)
    def hand: Boolean = source == "hand"
  }

  def main(args: Array[String]): Unit = {
    val opts     = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val training = opts.get("training").map(Paths.get(_)).getOrElse(Training)
    val rows     = read(training)
    val version  = opts.getOrElse("version", versionOf(training))
    // the unified model (every signal compensatory) and the HYBRID (the guards hard, the score among what passes them)
    val models = Seq(Artefact -> Nil, HybridArtefact -> UnifiedEvidence.Guards).map { case (path, guards) =>
      val fitted  = fit(rows, version, guards)
      val weights = fitted.copy(ablation = ablate(rows, fitted))
      Files.writeString(path, Json.prettyPrint(Json.toJson(weights)) + "\n")
      weights
    }
    val report = models.map(Report.of(rows, _)).mkString("\n") + Report.guarding(rows, models.head, models.last)
    opts.get("report").foreach(path => Files.writeString(Paths.get(path), report))
    println(report)
  }

  /** The version a fit is filed under: the rows' own digest, so the same rows always fit the same artefact. */
  def versionOf(training: Path): String =
    f"unified-${scala.util.hashing.MurmurHash3.bytesHash(Files.readAllBytes(training)) & 0xffffffffL}%08x"

  def read(path: Path): Seq[Row] = {
    val in = new GZIPInputStream(Files.newInputStream(path))
    val lines = try new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\n").toSeq finally in.close()
    val header = lines.head.split("\t").toSeq
    val columns = UnifiedEvidence.Names.map(name => header.indexOf(name))
    require(!columns.contains(-1), s"$path lacks signals ${UnifiedEvidence.Names.filterNot(header.contains).mkString(", ")}: re-emit it")
    lines.tail.filter(_.nonEmpty).map(_.split("\t", -1)).map { a =>
      Row(a(0), a(1), a(2), a(3), a(4).toInt, a(5), a(6), a(7), a(8) == "1", a(9) match { case "1" => Some(true); case "0" => Some(false); case _ => None },
        a(10), a(11).toInt, a(12).toInt, columns.map(i => a(i).toDouble).toIndexedSeq)
    }
  }

  // ── fitting ───────────────────────────────────────────────────────────────────────────────

  private val signs: Seq[Int] = 0 +: UnifiedEvidence.Signals.map(_.direction)
  private val column: Map[String, Int] = UnifiedEvidence.Names.zipWithIndex.toMap

  /** The guards of `guards` a row trips ([[UnifiedEvidence.vetoes]]). */
  def vetoes(row: Row, guards: Seq[String]): Seq[String] = UnifiedEvidence.vetoes(name => row.x(column(name))).filter(guards.contains)
  /** The rows a model with `guards` scores at all: those tripping none. */
  def passing(rows: Seq[Row], guards: Seq[String]): Seq[Row] = rows.filter(vetoes(_, guards).isEmpty)
  private def keptOf(guards: Seq[String]): Set[String] = UnifiedEvidence.Names.toSet -- guards

  /** The weights the labelled `rows` fit — intercept first, then every signal, a dropped one (not in `kept`) at 0. Rows
   *  alike in label and features are folded into one with their count, in a fixed order. */
  def weightsOf(rows: Seq[Row], kept: Set[String] = UnifiedEvidence.Names.toSet): Seq[Double] = {
    val columns = 0 +: UnifiedEvidence.Names.zipWithIndex.collect { case (name, i) if kept(name) => i + 1 }
    val folded  = rows.flatMap(row => row.label.map(y => (if (y) 1.0 else 0.0, columns.drop(1).map(i => row.x(i - 1)))))
      .groupMapReduce(identity)(_ => 1.0)(_ + _).toSeq.sortBy { case ((y, x), _) => (y, x.mkString(",")) }
    val fitted = LogisticFit.fitSigned(folded.map { case ((_, x), _) => (1.0 +: x).toArray }.toArray, folded.map(_._1._1).toArray,
      folded.map(_._2).toArray, columns.map(signs), L2)
    val full = Array.fill(UnifiedEvidence.Names.size + 1)(0.0)
    columns.zip(fitted).foreach { case (i, w) => full(i) = w }
    full.toSeq
  }

  def probability(weights: Seq[Double], row: Row): Double =
    LogisticFit.sigmoid(weights.head + row.x.indices.map(i => weights(i + 1) * row.x(i)).sum)

  /** Every row's probability by the model fitted without its fold's venues. */
  def heldOut(rows: Seq[Row], kept: Set[String] = UnifiedEvidence.Names.toSet): Seq[(Row, Double)] =
    (0 until Folds).flatMap { fold =>
      val weights = weightsOf(rows.filter(_.fold != fold), kept)
      rows.filter(_.fold == fold).map(row => row -> probability(weights, row))
    }

  /** Each cluster's best contender, best first by probability, then by film. */
  def best(scored: Seq[(Row, Double)]): Seq[(Row, Double)] =
    scored.groupBy(_._1.cluster).values.map(_.minBy { case (row, p) => (-p, row.film) }).toSeq.sortBy(_._1.cluster)

  /** A held-out take that must not be taken: a NEW wrong one — a hand label calls it wrong and today's stack does not
   *  take it already (a wrong take inherited from today is listed, not a reason to take nothing) — or one moving a film
   *  today's stack shows. */
  private def bad(take: Row, todays: Map[String, Row]): Boolean =
    (take.hand && take.label.contains(false) && !take.today) || todays.get(take.cluster).exists(_.film != take.film)

  /** The cut: the lowest held-out best probability above every bad take's. */
  def cutOf(scored: Seq[(Row, Double)]): Double = {
    val takes    = best(scored)
    val todays   = todaysOf(scored.map(_._1))
    val worstBad = takes.filter { case (row, _) => bad(row, todays) }.map(_._2).maxOption.getOrElse(0.0)
    takes.map(_._2).filter(_ > worstBad).minOption.getOrElse(1.0)
  }

  def todaysOf(rows: Seq[Row]): Map[String, Row] = rows.filter(_.today).map(row => row.cluster -> row).toMap

  /** Log-loss and accuracy (at 0.5) of the labelled rows among `scored`. */
  def metrics(scored: Seq[(Row, Double)]): (Double, Double, Int) = {
    val labelled = scored.flatMap { case (row, p) => row.label.map(y => (y, math.min(1 - 1e-12, math.max(1e-12, p)))) }
    if (labelled.isEmpty) (0.0, 0.0, 0)
    else (labelled.map { case (y, p) => if (y) -math.log(p) else -math.log(1 - p) }.sum / labelled.size,
      labelled.count { case (y, p) => y == (p >= 0.5) }.toDouble / labelled.size, labelled.size)
  }

  private def rounded(x: Double) = math.rint(x * 1e6) / 1e6

  /** The model: the weights all labelled rows passing `guards` fit (the guards themselves no feature: a row tripping one
   *  is never scored), the cut and what the held-out folds measured. */
  def fit(contenders: Seq[Row], version: String, guards: Seq[String] = Nil): UnifiedWeights = {
    val rows    = passing(contenders, guards)
    val weights = weightsOf(rows, keptOf(guards))
    val scored  = heldOut(rows, keptOf(guards))
    val cut     = cutOf(scored)
    val (all, accuracy, n)     = metrics(scored)
    val (hand, handAccuracy, h) = metrics(scored.filter(_._1.hand))
    val (weak, weakAccuracy, _) = metrics(scored.filter(_._1.source == "weak"))
    UnifiedWeights(version, UnifiedEvidence.Names, weights, rounded(cut), L2, Folds, n,
      Map("logLoss" -> all, "accuracy" -> accuracy, "handLogLoss" -> hand, "handAccuracy" -> handAccuracy, "handRows" -> h.toDouble,
        "weakLogLoss" -> weak, "weakAccuracy" -> weakAccuracy).view.mapValues(rounded).toMap, guards = guards)
  }

  /** Each signal group dropped in turn, the model refitted and measured held out at the full model's cut. */
  def ablate(all: Seq[Row], full: UnifiedWeights): Seq[UnifiedWeights.Ablation] = {
    val rows = passing(all, full.guards)
    UnifiedEvidence.Signals.filterNot(s => full.guards.contains(s.name)).map(_.group).distinct.map { group =>
      val kept   = keptOf(full.guards) -- UnifiedEvidence.Signals.filter(_.group == group).map(_.name)
      val scored = heldOut(rows, kept)
      val takes  = Measure.takes(scored, full.cut)
      UnifiedWeights.Ablation(group, rounded(metrics(scored)._1), rounded(metrics(scored.filter(_._1.hand))._1),
        takes.map(_.right).sum, takes.map(_.wrong).sum)
    }
  }

  // ── measuring against today's stack ────────────────────────────────────────────────────────

  object Measure {
    /** A cluster's take at `cut`: its best contender when it reaches it. */
    def takes(scored: Seq[(Row, Double)], cut: Double): Seq[Row] = best(scored).collect { case (row, p) if p >= cut => row }

    /** What one stack does to the clusters: per listing, right and wrong by the hand labels (unjudged the rest of a
     *  take's), and against today's: switched, lost, gained. */
    final case class Outcome(right: Int, wrong: Int, unjudged: Int, switched: Int, lost: Int, gained: Int)

    def outcome(rows: Seq[Row], takes: Seq[Row], origin: Option[String] = None): Outcome = {
      val clusters = rows.filter(row => origin.forall(_ == row.origin)).groupBy(_.cluster)
      val taken    = takes.map(row => row.cluster -> row).toMap
      clusters.toSeq.map { case (cluster, members) =>
        val today = members.find(_.today)
        val take  = taken.get(cluster)
        val n     = members.head.listings
        Outcome(take.map(_.right).getOrElse(0), take.map(_.wrong).getOrElse(0), take.fold(0)(t => n - t.right - t.wrong),
          if (today.isDefined && take.isDefined && today.map(_.film) != take.map(_.film)) n else 0,
          if (today.isDefined && take.isEmpty) n else 0, if (today.isEmpty && take.isDefined) n else 0)
      }.foldLeft(Outcome(0, 0, 0, 0, 0, 0)) { (a, b) =>
        Outcome(a.right + b.right, a.wrong + b.wrong, a.unjudged + b.unjudged, a.switched + b.switched, a.lost + b.lost, a.gained + b.gained)
      }
    }
  }

  // ── the report ───────────────────────────────────────────────────────────────────────────

  object Report {
    def of(rows: Seq[Row], weights: UnifiedWeights): String = {
      val b = new StringBuilder
      def line(s: String) = b ++= s ++= "\n"
      val today   = rows.filter(_.today)
      val scoredRows = passing(rows, weights.guards)
      val inSample = scoredRows.map(row => row -> probability(weights.weights, row))
      val held    = heldOut(scoredRows, keptOf(weights.guards))
      val unified = Measure.takes(inSample, weights.cut)
      val unifiedHeld = Measure.takes(held, weights.cut)
      line(s"# ${if (weights.guards.isEmpty) "Unified" else "Hybrid"} evidence model ${weights.version}")
      if (weights.guards.nonEmpty) line(s"\nHard guards (a contender tripping one is never scored): ${weights.guards.mkString(", ")}; " +
        s"${rows.size - scoredRows.size} of ${rows.size} contenders vetoed.")
      line(s"\n${rows.size} contender rows, ${rows.map(_.cluster).distinct.size} clusters; labelled ${weights.rows} " +
        s"(${rows.count(_.hand)} hand, ${rows.count(_.source == "weak")} weak). Cut ${f"${weights.cut}%.4f"}.")
      line("\n## Weights (sign-constrained; 0 = held at zero)\n\n| signal | group | direction | weight |\n|---|---|---|---|")
      line(f"| intercept | | | ${weights.weights.head}%.3f |")
      UnifiedEvidence.Signals.zip(weights.weights.drop(1)).filterNot(s => weights.guards.contains(s._1.name)).sortBy(s => -math.abs(s._2)).foreach { case (s, w) =>
        line(f"| ${s.name} | ${s.group} | ${s.direction}%+d | $w%.3f |") }
      line("\n## Held out (whole venues)\n")
      weights.heldOut.toSeq.sortBy(_._1).foreach { case (k, v) => line(f"- $k: $v%.4f") }
      val todaysHeld = todaysOf(rows)
      line("\n## Held-out takes the cut stands above (new wrong, or moving today's film), worst first\n")
      best(held).filter { case (row, _) => bad(row, todaysHeld) }.sortBy(-_._2).take(15).foreach { case (r, p) =>
        line(f"- $p%.4f ${r.country} ${r.rawTitle} ×${r.listings}: ${r.film} ${r.filmTitle}" +
          todaysHeld.get(r.cluster).filter(_.film != r.film).fold(" (hand-wrong)")(t => s" (today ${t.film} ${t.filmTitle})")) }
      line("\n## Wrong takes inherited from today (hand labels)\n")
      today.filter(row => row.hand && row.label.contains(false)).foreach(r => line(s"- ${r.country} ${r.rawTitle} ×${r.listings}: ${r.film} ${r.filmTitle}"))
      line("\n## Ablation (group dropped, refitted, held out, at the full cut)\n\n| group | log-loss | hand log-loss | right | wrong |\n|---|---|---|---|---|")
      val fullHeld = Measure.outcome(rows, unifiedHeld)
      line(f"| (none) | ${weights.heldOut("logLoss")}%.4f | ${weights.heldOut("handLogLoss")}%.4f | ${fullHeld.right} | ${fullHeld.wrong} |")
      weights.ablation.foreach(a => line(f"| ${a.group} | ${a.logLoss}%.4f | ${a.handLogLoss}%.4f | ${a.right} | ${a.wrong} |"))
      def outcomeRow(name: String, o: Measure.Outcome) =
        line(s"| $name | ${o.right} | ${o.wrong} | ${o.unjudged} | ${o.switched} | ${o.lost} | ${o.gained} |")
      line("\n## (a) The unmatched clusters' fixture (listings)\n\n| stack | right | wrong | unjudged | switched | lost | gained |\n|---|---|---|---|---|---|---|")
      outcomeRow("today (model + agreement)", Measure.outcome(rows, today, Some("fixture")))
      outcomeRow("unified (pinned weights)", Measure.outcome(rows, unified, Some("fixture")))
      outcomeRow("unified (held out)", Measure.outcome(rows, unifiedHeld, Some("fixture")))
      line("\n## (b) Whole corpus against today (listings)\n\n| country | stack | right | wrong | unjudged | switched | lost | gained |\n|---|---|---|---|---|---|---|---|")
      rows.map(_.country).distinct.sorted.foreach { cc =>
        val mine = rows.filter(_.country == cc)
        Seq("today" -> today, "unified" -> unified, "unified held out" -> unifiedHeld).foreach { case (name, takes) =>
          val o = Measure.outcome(mine, takes.filter(_.country == cc))
          line(s"| $cc | $name | ${o.right} | ${o.wrong} | ${o.unjudged} | ${o.switched} | ${o.lost} | ${o.gained} |")
        }
      }
      val todays = todaysOf(rows)
      val taken  = unified.map(row => row.cluster -> row).toMap
      val moved  = rows.groupBy(_.cluster).toSeq.sortBy(_._1).flatMap { case (cluster, members) =>
        val was = todays.get(cluster); val now = taken.get(cluster)
        Option.when(was.map(_.film) != now.map(_.film))((members.head, was, now))
      }
      def film(row: Option[Row]) = row.fold("—")(r => s"${r.film} ${r.filmTitle}")
      line("\n## Listings switched to another film (must be none)\n")
      moved.filter { case (_, was, now) => was.isDefined && now.isDefined }.foreach { case (r, was, now) =>
        line(s"- ${r.country} ${r.rawTitle} ×${r.listings}: ${film(was)} → ${film(now)}") }
      line("\n## Matches lost (today takes a film, the unified model none)\n")
      moved.filter { case (_, was, now) => was.isDefined && now.isEmpty }.foreach { case (r, was, _) =>
        line(s"- ${r.country} [${r.origin}] ${r.rawTitle} ×${r.listings}: ${film(was)}") }
      line("\n## Matches gained (today none, the unified model a film)\n")
      moved.filter { case (_, was, now) => was.isEmpty && now.isDefined }.foreach { case (r, _, now) =>
        line(s"- ${r.country} [${r.origin}] ${r.rawTitle} ×${r.listings}: ${film(now)} (right ${now.get.right}, wrong ${now.get.wrong})") }
      b.toString
    }

    /** The unified model's high-scoring held-out wrong takes: which guard trips each, and what the hybrid makes of it. */
    def guarding(rows: Seq[Row], unified: UnifiedWeights, hybrid: UnifiedWeights): String = {
      val todays = todaysOf(rows)
      val hybridHeld = heldOut(passing(rows, hybrid.guards), keptOf(hybrid.guards))
      val hybridTakes = Measure.takes(hybridHeld, hybrid.cut).map(r => r.cluster -> r.film).toSet
      val hybridP = hybridHeld.map { case (r, p) => (r.cluster, r.film) -> p }.toMap
      val wrong = best(heldOut(rows, keptOf(unified.guards))).filter { case (r, p) =>
        p >= 0.9 && r.hand && r.label.contains(false) && !r.today && !todays.get(r.cluster).exists(_.film != r.film) }
      ("\n## The unified model's held-out wrong takes >= 0.90, under the hybrid\n" +: wrong.sortBy(-_._2).map { case (r, p) =>
        val tripped = vetoes(r, UnifiedEvidence.Guards)
        val now = if (tripped.nonEmpty) s"vetoed by ${tripped.mkString(", ")}"
          else hybridP.get((r.cluster, r.film)).fold("not scored")(q => f"scored $q%.4f, ${if (hybridTakes((r.cluster, r.film))) "TAKEN" else "not taken"}")
        f"- ${r.country} ${r.rawTitle}: ${r.film} ${r.filmTitle} unified $p%.4f -> hybrid $now"
      }).mkString("\n") + "\n"
    }
  }
}
