package scripts

import play.api.libs.json.{JsObject, JsValue, Json}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

/**
 * Whether a gradient-boosted model of the identity ML experiment (docs/design/identity-ml-experiment.md) can score
 * in-JVM, and at what cost: a plain-Scala evaluator of a LightGBM `dump_model()` JSON (no native library, no new
 * dependency), checked against the scores Python's LightGBM gave the same rows, timed and measured by
 * [[tools.ThreadAllocation]] over the whole pinned corpus of contenders:
 *
 *   timeout 1800 <env>/bin/python scripts/ml-identity/identity_ml_experiment.py     # writes target/identity-ml/
 *   sbt "worker/Test/runMain scripts.IdentityMlScorerBench target/identity-ml/gbm-d2.json target/identity-ml/scores-d2.txt"
 *
 * Offline; reads only the experiment's dump and `IdentityUnifiedFit.Training`.
 */
object IdentityMlScorerBench {

  /** One tree, flattened: internal nodes `0..n-1` (feature, threshold, NaN goes left, left/right child: `>= 0` a node,
   *  `< 0` the leaf `~child`), and its leaf values. */
  final case class Tree(feature: Array[Int], threshold: Array[Double], nanLeft: Array[Boolean], left: Array[Int],
                        right: Array[Int], leaf: Array[Double]) {
    def value(x: Array[Double]): Double =
      if (feature.isEmpty) leaf(0)
      else {
        var node = 0
        while (node >= 0) {
          val v = x(feature(node))
          node = if (if (v.isNaN) nanLeft(node) else v <= threshold(node)) left(node) else right(node)
        }
        leaf(~node)
      }
  }

  /** A binary LightGBM ensemble: the probability is the logistic of the trees' summed leaves. */
  final case class Ensemble(features: IndexedSeq[String], trees: Array[Tree]) {
    def probability(x: Array[Double]): Double = {
      var sum = 0.0
      var i   = 0
      while (i < trees.length) { sum += trees(i).value(x); i += 1 }
      1.0 / (1.0 + math.exp(-sum))
    }
  }

  def load(path: Path): Ensemble = {
    val json = Json.parse(Files.readAllBytes(path))
    val features = (json \ "features").as[IndexedSeq[String]]
    val trees = (json \ "model" \ "tree_info").as[Seq[JsObject]].map(t => flatten((t \ "tree_structure").get)).toArray
    Ensemble(features, trees)
  }

  private def flatten(root: JsValue): Tree = {
    val feature, left, right = Array.newBuilder[Int]
    val threshold = Array.newBuilder[Double]
    val nanLeft   = Array.newBuilder[Boolean]
    val leaves    = Array.newBuilder[Double]
    var nodes, leafCount = 0
    // preorder; children are patched once numbered
    val patches = scala.collection.mutable.ArrayBuffer.empty[(Int, Boolean, Int)]
    def walk(n: JsValue): Int = (n \ "leaf_value").asOpt[Double] match {
      case Some(v) => leaves += v; leafCount += 1; ~(leafCount - 1)
      case None =>
        require((n \ "decision_type").as[String] == "<=", s"only numeric splits: $n")
        val id = nodes; nodes += 1
        feature += (n \ "split_feature").as[Int]; threshold += (n \ "threshold").as[Double]
        nanLeft += (n \ "default_left").asOpt[Boolean].getOrElse(true)
        left += 0; right += 0
        patches += ((id, true, walk((n \ "left_child").get)))
        patches += ((id, false, walk((n \ "right_child").get)))
        id
    }
    walk(root)
    val (l, r) = (left.result(), right.result())
    patches.foreach { case (id, isLeft, child) => if (isLeft) l(id) = child else r(id) = child }
    Tree(feature.result(), threshold.result(), nanLeft.result(), l, r, leaves.result())
  }

  /** The pinned contender rows' features in `names`' order, and each row's cluster. */
  def rows(names: IndexedSeq[String]): (Array[Array[Double]], Array[String]) = {
    val in = new GZIPInputStream(Files.newInputStream(IdentityUnifiedFit.Training))
    val lines = try new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\n").filter(_.nonEmpty) finally in.close()
    val header  = lines.head.split("\t")
    val columns = names.map(n => header.indexOf(n))
    require(!columns.contains(-1), s"the rows lack ${names.filterNot(header.contains)}")
    val split = lines.tail.map(_.split("\t", -1))
    (split.map(a => columns.map(i => a(i).toDouble).toArray), split.map(a => a(0) + "\t" + a(1)))
  }

  def main(args: Array[String]): Unit = {
    val model    = load(Paths.get(args(0)))
    val expected = args.lift(1).map(p => Files.readAllLines(Paths.get(p)).toArray(Array.empty[String]).map(_.toDouble))
    val (x, cluster) = rows(model.features)
    val clusters = cluster.distinct.length
    val scores = new Array[Double](x.length)
    def pass(): Unit = { var i = 0; while (i < x.length) { scores(i) = model.probability(x(i)); i += 1 } }
    (1 to 20).foreach(_ => pass()) // warm
    val runs = (1 to 20).map { _ =>
      val started = System.nanoTime()
      val (_, bytes) = tools.ThreadAllocation.of(pass())
      (System.nanoTime() - started, bytes)
    }
    val nanos = runs.map(_._1).sorted.apply(runs.size / 2)
    val bytes = runs.map(_._2).sorted.apply(runs.size / 2)
    val drift = expected.fold(Double.NaN)(e => e.indices.map(i => math.abs(e(i) - scores(i))).max)
    val nodes = model.trees.map(_.feature.length).sum
    println(f"${model.trees.length} trees, $nodes%,d split nodes, ${model.features.size} features; ${x.length}%,d contenders in $clusters%,d clusters")
    println(f"whole corpus: ${nanos / 1e6}%.2f ms, ${bytes}%,d bytes allocated (median of ${runs.size}); " +
      f"${nanos.toDouble / clusters / 1e3}%.3f µs per cluster")
    println(f"max |JVM − LightGBM| over every row: $drift%.2e")
  }
}
