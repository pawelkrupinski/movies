package services.identity

/**
 * The ONE place a must-link / cannot-link conflict is decided (docs/design/identity-resolver.md §3,
 * assumption A2).
 *
 * Input: a SET of nodes under a total order and a SET of constraints between them. Output: a
 * partition. The canonical rule:
 *
 *   1. CANNOT-LINK WINS. Two components are never united while any cannot-link joins them, so no
 *      component ever holds a cannot-linked pair (P3) — by construction, not by check.
 *   2. Must-links are applied in TIERS (strongest evidence first), and within a tier in the order
 *      `(min node, max node)`.
 *   3. An AMBIGUOUS node is left alone for its tier: one whose must-links of this tier reach two
 *      components (as they stand at the START of the tier) that are cannot-linked to each other. A
 *      bare "A Star Is Born" beside the 1954 and 2018 films is evidence for neither, and rule 2
 *      alone would hand it to whichever sorts first.
 *
 * Why the partition is a function of the SET: `solve` reads its input only through sorts under a
 * total order of the elements themselves, so every presentation of one set produces the identical
 * sequences; every later step is a deterministic fold over them, and the union-find keeps the
 * representative of smaller rank, never the one an edge happened to arrive with.
 *
 * [[Presentation.AsGiven]] exists only for the teeth tests: the same fold without the sorts and
 * without tiers — "first wins". The order-independence properties must catch it.
 */
object ConstraintSolver {

  final case class Constraint[K](a: K, b: K, must: Boolean, tier: Int, reason: String)

  private[identity] enum Presentation { case Canonical, AsGiven }

  /** Components as sorted node lists, themselves sorted by their smallest node. */
  def solve[K](nodes: Seq[K], constraints: Seq[Constraint[K]])(implicit ord: Ordering[K]): Seq[Seq[K]] =
    solveAs(nodes, constraints, Presentation.Canonical)

  private[identity] def solveAs[K](nodes: Seq[K], constraints: Seq[Constraint[K]], presentation: Presentation)
                                  (implicit ord: Ordering[K]): Seq[Seq[K]] = {
    val canonical   = presentation == Presentation.Canonical
    val sortedNodes = if (canonical) nodes.distinct.sorted else nodes.distinct
    val index       = sortedNodes.zipWithIndex.toMap
    val parent      = Array.tabulate(sortedNodes.size)(identity)
    def find(i: Int): Int = { var r = i; while (parent(r) != r) { parent(r) = parent(parent(r)); r = parent(r) }; r }

    // Cannot-links between ROOTS, maintained through every union.
    val cannot = scala.collection.mutable.HashMap.empty[Int, scala.collection.mutable.Set[Int]]
    def cannotOf(r: Int) = cannot.getOrElseUpdate(r, scala.collection.mutable.Set.empty[Int])
    constraints.iterator.filterNot(_.must).foreach { c =>
      val (a, b) = (index(c.a), index(c.b))
      if (a != b) { cannotOf(a) += b; cannotOf(b) += a }
    }
    def forbidden(ra: Int, rb: Int): Boolean = cannot.get(ra).exists(_.contains(rb))
    def union(ra: Int, rb: Int): Unit = {
      // The smaller rank survives: a choice by the node's place in the total order.
      val (keep, gone) = if (ra < rb) (ra, rb) else (rb, ra)
      parent(gone) = keep
      cannot.remove(gone).foreach { gs =>
        gs.foreach { r => cannot.get(r).foreach { s => s -= gone; s += keep } }
        cannotOf(keep) ++= gs
      }
    }
    def join(a: Int, b: Int): Unit = {
      val (ra, rb) = (find(a), find(b))
      if (ra != rb && !forbidden(ra, rb)) union(ra, rb)
    }

    val must = constraints.filter(_.must).map { c =>
      val (a, b) = (index(c.a), index(c.b))
      (c.tier, math.min(a, b), math.max(a, b))
    }.filter { case (_, a, b) => a != b }

    if (!canonical) must.foreach { case (_, a, b) => join(a, b) }
    else must.distinct.sorted.groupBy(_._1).toSeq.sortBy(_._1).foreach { case (_, tierEdges) =>
      val edges = tierEdges.sorted
      // Rule 3, judged against the components as they stand at the START of the tier.
      val reach = scala.collection.mutable.HashMap.empty[Int, Set[Int]]
      edges.foreach { case (_, a, b) =>
        reach(a) = reach.getOrElse(a, Set(find(a))) + find(b)
        reach(b) = reach.getOrElse(b, Set(find(b))) + find(a)
      }
      val ambiguous = reach.collect { case (node, roots) if roots.exists(r => roots.exists(forbidden(r, _))) => node }.toSet
      edges.foreach { case (_, a, b) => if (!ambiguous(a) && !ambiguous(b)) join(a, b) }
    }

    sortedNodes.indices.groupBy(find).values.toSeq
      .map(_.map(sortedNodes).sorted)
      .sortBy(_.head)
  }
}
