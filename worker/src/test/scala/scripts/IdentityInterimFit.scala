package scripts

import play.api.libs.json.Json
import services.identity.{IdentityWeights, Signals}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * The INTERIM fit of the identity resolver's weights, until the calibration's artefact
 * (`identity-weights.json`, branch `identity-calibration`) lands: a logistic regression over the
 * shadow harness's calibration dataset (`<corpus>-dataset.tsv`, one row per scored (node,
 * candidate)), fitted on the TRAIN families only, and the acceptance threshold that maximises
 * train node-level agreement. Reproducible: the same datasets give the same artefact.
 *
 * Labels: a row is positive when its candidate is the film today's pipeline gave the node's
 * listings ("pipeline"), or — with `--labels corroborated` — the node's corroborated label. A
 * node with no label contributes negatives only under "pipeline" (the pipeline matched nothing),
 * and nothing under "corroborated".
 *
 *   sbt "worker/Test/runMain scripts.IdentityInterimFit <out.json> [--labels pipeline|corroborated]
 *        [--exclude director,runtime,country] [--threshold bayes] <dataset.tsv>..."
 */
object IdentityInterimFit {

  final case class Row(node: String, split: String, tmdbId: Int, label: Option[Boolean], x: Array[Double])

  def read(paths: Seq[Path], labels: String): Seq[Row] = paths.flatMap { p =>
    val lines  = Files.readAllLines(p, StandardCharsets.UTF_8).asScala.toSeq
    val header = lines.head.split("\t", -1).toSeq
    val at     = header.zipWithIndex.toMap
    require(header.drop(at("p") + 1) == Signals.ModelInputs, s"$p was written for another signal set: re-run the harness")
    val unreachable = Seq("title=none", "source=family").map(n => at("p") + 1 + Signals.ModelInputs.indexOf(n))
    // A node's rows are consecutive; a dataset written before the `node` column existed is split
    // into nodes where (family, listings, pipeline film) changes.
    var block = 0; var previous = ""
    lines.tail.filter(_.nonEmpty).map(_.split("\t", -1)).map { f =>
      val node = at.get("node").map(f(_)).getOrElse {
        val here = Seq("family", "listings", "pipelineTmdb").map(c => f(at(c))).mkString("/")
        if (here != previous) { block += 1; previous = here }
        block.toString
      }
      (node, f)
    }
      // A row with no evidence path (the resolver's `reachable`), from a dataset written before the
      // resolver stopped scoring those: it could never be chosen, so it teaches nothing.
      .filterNot { case (_, f) => unreachable.forall(i => f(i) != "0") }
      .map { case (node, f) =>
      val id    = f(at("tmdbId")).toInt
      val label = labels match {
        case "corroborated" => Option(f(at("label"))).filter(_.nonEmpty).map(_ == "1")
        case _              => Some(f(at("pipelineTmdb")) == id.toString)
      }
      Row(s"${f(at("corpus"))}/$node", f(at("split")), id, label,
        f.drop(at("p") + 1).map(_.toDouble))
    }
  }

  /** L2-regularised logistic regression by Newton's method: deterministic, no learning rate. */
  def fit(rows: Seq[Row], lambda: Double = 1.0, iterations: Int = 25): Array[Double] = {
    val d = Signals.ModelInputs.size
    val w = new Array[Double](d)
    val labelled = rows.flatMap(r => r.label.map(y => (r.x, if (y) 1.0 else 0.0)))
    (1 to iterations).foreach { _ =>
      val g = Array.tabulate(d)(j => lambda * w(j) * (if (j == 0) 0 else 1))
      val h = Array.tabulate(d, d)((i, j) => if (i == j) lambda + 1e-9 else 0.0)
      labelled.foreach { case (x, y) =>
        var z = 0.0; var i = 0
        while (i < d) { z += w(i) * x(i); i += 1 }
        val p = 1 / (1 + math.exp(-z)); val s = p * (1 - p)
        i = 0
        while (i < d) {
          if (x(i) != 0) {
            g(i) += (p - y) * x(i)
            var j = 0
            while (j < d) { if (x(j) != 0) h(i)(j) += s * x(i) * x(j); j += 1 }
          }
          i += 1
        }
      }
      val step = solve(h, g)
      (0 until d).foreach(i => w(i) -= step(i))
    }
    w
  }

  /** Gaussian elimination with partial pivoting. */
  private def solve(a0: Array[Array[Double]], b0: Array[Double]): Array[Double] = {
    val n = b0.length; val a = a0.map(_.clone()); val b = b0.clone()
    for (c <- 0 until n) {
      val p = (c until n).maxBy(r => math.abs(a(r)(c)))
      val (ta, tb) = (a(c), b(c)); a(c) = a(p); a(p) = ta; b(c) = b(p); b(p) = tb
      for (r <- c + 1 until n) {
        val f = a(r)(c) / a(c)(c)
        if (f != 0) { for (k <- c until n) a(r)(k) -= f * a(c)(k); b(r) -= f * b(c) }
      }
    }
    val x = new Array[Double](n)
    for (r <- (n - 1) to 0 by -1) x(r) = (b(r) - (r + 1 until n).map(k => a(r)(k) * x(k)).sum) / a(r)(r)
    x
  }

  /** Node-level agreement at threshold `t`: the node's best candidate if its confidence clears `t`, else no
   *  film, against the node's label (its positive candidate, or no film when it has none). */
  def agreement(rows: Seq[Row], w: Array[Double], t: Double): (Int, Int) = {
    val nodes = rows.filter(_.label.isDefined).groupBy(_.node).values.toSeq
    val correct = nodes.count { rs =>
      val scored = rs.map(r => r -> 1 / (1 + math.exp(-r.x.zip(w).map { case (a, b) => a * b }.sum)))
      val best   = scored.maxBy { case (r, p) => (p, -r.tmdbId) }
      // The resolver accepts on CONFIDENCE: the best is the film and no rival is.
      val confidence = best._2 * scored.filterNot(_._1 eq best._1).map(1 - _._2).product
      val chosen = Option.when(confidence >= t)(best._1.tmdbId)
      chosen == rs.find(_.label.contains(true)).map(_.tmdbId)
    }
    (correct, nodes.size)
  }

  /** `rows` with every input of the `signals` named zeroed, so the fit gives them no weight: for a
   *  signal whose PRESENCE in the dataset is an artefact of how it was recorded (a film's director
   *  and runtime are recorded only for the film the pipeline picked, so "director known" alone
   *  predicts the pipeline's answer). */
  def masked(rows: Seq[Row], signals: Seq[String]): Seq[Row] = {
    val drop = Signals.ModelInputs.zipWithIndex.collect { case (n, i) if signals.exists(s => n == s || n.startsWith(s"$s=")) => i }
    if (drop.isEmpty) rows else rows.map(r => r.copy(x = { val x = r.x.clone(); drop.foreach(x(_) = 0); x }))
  }

  def main(args: Array[String]): Unit = {
    val out    = Paths.get(args.head)
    val labels = args.sliding(2).collectFirst { case Array("--labels", l) => l }.getOrElse("pipeline")
    val excluded = args.sliding(2).collectFirst { case Array("--exclude", e) => e.split(",").toSeq }.getOrElse(Nil)
    val fixed    = args.sliding(2).collectFirst { case Array("--threshold", "bayes") => 0.5 }
    val inputs = args.tail.filterNot(a => Set("--labels", labels, "--exclude", excluded.mkString(","), "--threshold", "bayes")(a))
      .map(Paths.get(_)).toSeq.sortBy(_.toString)
    val rows   = masked(read(inputs, labels), excluded)
    val (train, test) = rows.partition(_.split == "train")
    val w      = fit(train)
    val grid   = ((1 to 4) ++ (5 to 95 by 5)).map(_ / 100.0)
    val t      = fixed.getOrElse(grid.maxBy(t => (agreement(train, w, t)._1, -t)))
    val (trainOk, trainN) = agreement(train, w, t)
    val (testOk, testN)   = agreement(test, w, t)
    val weights = IdentityWeights(
      version   = s"interim-${labels}-${Integer.toHexString(inputs.map(p => s"${p.getFileName}:${Files.size(p)}").mkString(",").hashCode)}",
      weights   = Signals.ModelInputs.zip(w).filter(_._2 != 0).map { case (k, v) => k -> math.round(v * 1e4) / 1e4 }.toMap,
      threshold = t,
      provenance = Map(
        "fit"       -> s"L2 logistic regression (lambda 1.0, 25 Newton steps) on ${train.size} train rows, labels=$labels",
        "excluded"  -> (if (excluded.isEmpty) "none" else excluded.mkString(",")),
        "datasets"  -> inputs.map(_.getFileName.toString).mkString(","),
        "threshold" -> fixed.fold(s"$t, maximising train node agreement")(_ =>
          "0.5: the Bayes boundary — accept a match the model finds more likely right than not. Fitting it on " +
          "pipeline agreement drives it to the grid's floor, because the pipeline almost never answers 'no film'"),
        "train"     -> s"$trainOk/$trainN nodes agree",
        "heldOut"   -> s"$testOk/$testN nodes agree (families hashed into 5 folds, fold 0 held out)",
        "status"    -> "INTERIM: replaced by the calibration artefact identity-weights.json"))
    Files.writeString(out, Json.prettyPrint(Json.toJson(weights)), StandardCharsets.UTF_8)
    println(s"wrote $out: threshold $t, train $trainOk/$trainN, held-out $testOk/$testN")
  }
}
