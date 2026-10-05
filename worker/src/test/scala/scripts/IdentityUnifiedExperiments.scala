package scripts

import services.identity.{LogisticFit, UnifiedEvidence}

import java.nio.file.{Files, Paths}

/**
 * Which learned DECISION over the unified evidence ([[IdentityUnifiedFit]]'s rows) matches today's stack — the model
 * and the agreement after it — on the same measures: every variant trained with whole venues held out, its cut the
 * lowest held-out take above every new wrong one and every one moving today's film, then the ratchet's fixture (right,
 * wrong) and the whole corpus (switched, lost, gained) against today.
 *
 *   worker/Test/runMain scripts.IdentityUnifiedExperiments [--training <tsv.gz>] [--report <md>]
 *
 * Variants: the logistic (monotone) over the per-family indicators, with the COUNTS and conjunctions the agreement's
 * thresholds read ([[UnifiedEvidence]]'s `count.*`, `and.*`), the hard guards or not, the hand labels weighted above the
 * model's own takes; and gradient-boosted shallow trees (deterministic, exported as rules) over the same. Each decides
 * either every cluster (`all`) or only those the model's own rules took nothing for (`unmatched`: the model's takes
 * stand, the score replaces the agreement's thresholds alone).
 */
object IdentityUnifiedExperiments {
  import IdentityUnifiedFit.{Folds, Measure, Row, cutOf, keptOf, metrics, passing, read, weightsOf}

  private val names   = UnifiedEvidence.Names
  private val counted = names.filter(n => n.startsWith("count.") || n.startsWith("and."))
  private val ruleColumns = UnifiedEvidence.ModelRules.map(names.indexOf)
  private def modelTook(row: Row) = ruleColumns.exists(row.x(_) > 0)

  /** Something trained on rows that scores a row. */
  trait Learner { def name: String; def train(rows: Seq[Row]): Row => Double }

  final case class Logistic(kept: Set[String], handWeight: Double, label: String) extends Learner {
    def name = s"logistic $label hand×$handWeight"
    def train(rows: Seq[Row]): Row => Double = { val w = weightsOf(rows, kept, handWeight); IdentityUnifiedFit.probability(w, _) }
  }

  final case class Boosted(kept: Set[String], handWeight: Double, depth: Int, rounds: Int = 60, rate: Double = 0.3, label: String) extends Learner {
    def name = s"boosted depth $depth $label hand×$handWeight"
    private val columns = names.zipWithIndex.collect { case (n, i) if kept(n) => i }
    def model(rows: Seq[Row]): Trees = Trees.fit(rows.flatMap(r => r.label.map(y => (r.x, if (y) 1.0 else 0.0, if (r.hand) handWeight else 1.0))),
      columns, depth, rounds, rate)
    def train(rows: Seq[Row]): Row => Double = { val m = model(rows); row => m.probability(row.x) }
  }

  /** A gradient-boosted sum of shallow regression trees on the log-odds (Newton leaves, L2 1.0), deterministic. */
  final case class Trees(base: Double, trees: Seq[Trees.Node]) {
    def probability(x: IndexedSeq[Double]): Double = LogisticFit.sigmoid(base + trees.map(_(x)).sum)
    def rules: Seq[String] = f"base $base%.3f" +: trees.zipWithIndex.flatMap { case (t, i) => s"tree ${i + 1}:" +: t.render("  ") }
  }
  object Trees {
    sealed trait Node { def apply(x: IndexedSeq[Double]): Double; def render(indent: String): Seq[String] }
    final case class Leaf(value: Double) extends Node {
      def apply(x: IndexedSeq[Double]) = value
      def render(indent: String) = Seq(f"$indent→ $value%+.3f")
    }
    final case class Split(feature: Int, threshold: Double, low: Node, high: Node) extends Node {
      def apply(x: IndexedSeq[Double]) = if (x(feature) <= threshold) low(x) else high(x)
      def render(indent: String) = (s"$indent${names(feature)} <= $threshold:" +: low.render(indent + "  ")) ++
        (s"$indent${names(feature)} > $threshold:" +: high.render(indent + "  "))
    }
    private val Lambda = 1.0
    private val MinHessian = 1.0

    def fit(raw: Seq[(IndexedSeq[Double], Double, Double)], columns: Seq[Int], depth: Int, rounds: Int, rate: Double): Trees = {
      // identical rows folded, in a fixed order
      val rows = raw.groupMapReduce(r => (r._1, r._2))(_._3)(_ + _).toSeq.sortBy { case ((x, y), _) => (y, x.mkString(",")) }
      val xs = rows.map(_._1._1).toArray; val ys = rows.map(_._1._2).toArray; val ws = rows.map(_._2).toArray
      val positive = ys.indices.map(i => ws(i) * ys(i)).sum; val total = ws.sum
      val base = math.log(math.max(positive, 1e-6) / math.max(total - positive, 1e-6))
      val f = Array.fill(xs.length)(base)
      val trees = (1 to rounds).map { _ =>
        val p = f.map(LogisticFit.sigmoid)
        val g = Array.tabulate(xs.length)(i => ws(i) * (p(i) - ys(i)))
        val h = Array.tabulate(xs.length)(i => math.max(ws(i) * p(i) * (1 - p(i)), 1e-12))
        val tree = grow(xs.indices, xs, g, h, columns, depth, rate)
        xs.indices.foreach(i => f(i) += tree(xs(i)))
        tree
      }
      Trees(base, trees)
    }

    private def grow(idx: Seq[Int], xs: Array[IndexedSeq[Double]], g: Array[Double], h: Array[Double], columns: Seq[Int], depth: Int, rate: Double): Node = {
      val (gs, hs) = (idx.map(g).sum, idx.map(h).sum)
      def leaf = Leaf(-gs / (hs + Lambda) * rate)
      if (depth == 0) leaf
      else {
        val parent = gs * gs / (hs + Lambda)
        val candidates = columns.flatMap { c =>
          val sorted = idx.sortBy(i => (xs(i)(c), i))
          var (gl, hl) = (0.0, 0.0)
          sorted.indices.dropRight(1).flatMap { k =>
            val i = sorted(k); gl += g(i); hl += h(i)
            val (v, next) = (xs(i)(c), xs(sorted(k + 1))(c))
            Option.when(v != next && hl >= MinHessian && hs - hl >= MinHessian) {
              val gain = gl * gl / (hl + Lambda) + (gs - gl) * (gs - gl) / (hs - hl + Lambda) - parent
              (gain, c, (v + next) / 2)
            }
          }
        }
        candidates.maxByOption { case (gain, c, t) => (gain, -c, -t) }.filter(_._1 > 1e-9).fold[Node](leaf) { case (_, c, t) =>
          val (low, high) = idx.partition(i => xs(i)(c) <= t)
          Split(c, t, grow(low, xs, g, h, columns, depth - 1, rate), grow(high, xs, g, h, columns, depth - 1, rate))
        }
      }
    }
  }

  /** A variant's measures: its cut, the fixture's right and wrong listings, the corpus's switched, lost and gained, and
   *  the hand rows' held-out log-loss. */
  final case class Result(name: String, scope: String, cut: Double, right: Int, wrong: Int, switched: Int, lost: Int, gained: Int, handLogLoss: Double)

  def evaluate(rows: Seq[Row], learner: Learner, guards: Seq[String], scope: String): Result = {
    val scored = passing(rows, guards)
    val held   = (0 until Folds).flatMap { fold =>
      val score = learner.train(scored.filter(_.fold != fold))
      scored.filter(_.fold == fold).map(row => row -> score(row))
    }
    // `unmatched`: the clusters the model's rules took keep today's take; the score decides the rest
    val took    = rows.filter(modelTook).map(_.cluster).toSet
    val decided = if (scope == "unmatched") held.filterNot { case (row, _) => took(row.cluster) } else held
    val cut     = cutOf(decided)
    val kept    = if (scope == "unmatched") rows.filter(r => r.today && took(r.cluster)) else Nil
    val takes   = Measure.takes(decided, cut) ++ kept
    val fixture = Measure.outcome(rows, takes, Some("fixture"))
    val corpus  = Measure.outcome(rows, takes)
    Result(s"${learner.name}${if (guards.nonEmpty) " guarded" else ""}", scope, cut, fixture.right, fixture.wrong, corpus.switched, corpus.lost,
      corpus.gained, metrics(held.filter(_._1.hand))._1)
  }

  def main(args: Array[String]): Unit = {
    val opts   = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val rows   = read(opts.get("training").map(Paths.get(_)).getOrElse(IdentityUnifiedFit.Training))
    val guards = UnifiedEvidence.Guards
    val plain  = keptOf(Nil) -- counted
    val counts = keptOf(Nil)
    val hybrid = keptOf(guards)
    val variants: Seq[(Learner, Seq[String])] =
      Seq(Logistic(plain, 1, "indicators") -> Nil, Logistic(counts, 1, "+counts") -> Nil) ++
        Seq(1.0, 10.0, 30.0, 100.0).map(w => Logistic(counts, w, "+counts") -> guards) ++
        Seq(Logistic(hybrid -- counted, 30, "indicators") -> guards) ++
        Seq(10.0, 30.0).flatMap(w => Seq(2, 3).map(d => Boosted(hybrid, w, d, label = "+counts") -> guards))
    val results = variants.flatMap { case (learner, g) => Seq("all", "unmatched").map { scope =>
      val r = evaluate(rows, learner, g, scope); println(r); r } }
    val today = Measure.outcome(rows, rows.filter(_.today), Some("fixture"))
    val table = (Seq(s"today: fixture ${today.right} right / ${today.wrong} wrong; corpus 0 switched / 0 lost", "",
      "| variant | decides | cut | fixture right | fixture wrong | switched | lost | gained | hand log-loss |", "|---|---|---|---|---|---|---|---|---|") ++
      results.map(r => f"| ${r.name} | ${r.scope} | ${r.cut}%.4f | ${r.right} | ${r.wrong} | ${r.switched} | ${r.lost} | ${r.gained} | ${r.handLogLoss}%.4f |"))
    // the boosted model's rules, as a decision would cite them
    val rules = Boosted(hybrid, 30, 2, label = "+counts").model(passing(rows, guards)).rules.take(60)
    val text  = (table ++ Seq("", "## Boosted depth 2 (+counts, guarded, hand×30), its first trees as rules", "```") ++ rules :+ "```").mkString("\n")
    opts.get("report").foreach(path => Files.writeString(Paths.get(path), text + "\n"))
    println(text)
  }
}
