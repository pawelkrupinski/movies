package services.identity

/**
 * The ONE place a must-link / cannot-link conflict is decided (docs/design/identity-resolver.md §3,
 * assumption A2).
 *
 * Input: a SET of nodes under a total order and a SET of constraints between them. Output: a
 * partition. The canonical rule:
 *
 *   1. CANNOT-LINK WINS. Two components are never united while any cannot-link joins them, so no
 *      component ever holds a cannot-linked pair (P3) — by construction, not by check. Nor while
 *      they hold two different FILMS: a node's film ([[solve]]'s `films`) is a cannot-link to
 *      every node of another film, drawn between the components rather than node by node — the
 *      caller draws its cannot-links only between nodes sharing a key, and must-links chain
 *      through nodes that do not ("Kumotry" and "Ścieżki życia" joined through a festival's
 *      spellings of both).
 *   2. Must-links are applied in TIERS (strongest evidence first), and within a tier in the order
 *      `(min node, max node)`.
 *   3. An AMBIGUOUS node is left alone for its tier and every weaker one: one whose must-links of this tier reach two
 *      components (as they stand at the START of the tier) that are cannot-linked to each other, or
 *      that hold different films. A
 *      bare "A Star Is Born" beside the 1954 and 2018 films is evidence for neither, and rule 2
 *      alone would hand it to whichever sorts first.
 *   4. Left alone means joined to neither side, not split from its own kind: ambiguous nodes that are no side of
 *      their ambiguity (hold no film, are cannot-linked to no component they reach) join EACH OTHER by their
 *      must-links, while every node of both components is such a node — the bare spellings of one title form one
 *      component that still takes neither side. Only where asked (`ambiguousTogether`: the resolve's FINAL partition,
 *      over the films the clusters voted): the partition a family VOTES over leaves each ambiguous node alone, so each
 *      votes on its own evidence and the grouping moves no take — a group voting on its pooled evidence took a film or
 *      none by how many of its spellings had arrived (UK "Romeo and Juliet" relays, PopulationMonotonicitySpec). PL "André Rieu. Pada śnieg!" (2026-11-12): three venues took the
 *      concert's record by its original title, Cinema City's 30 venues denied it, and every bare spelling at ~60 other
 *      venues reached both — each was left a component, and a web film, of its own.
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

  /** Components as sorted node lists, themselves sorted by their smallest node. `films`: the film
   *  each node already holds, if any — no component ever holds two. */
  def solve[K](nodes: Seq[K], constraints: Seq[Constraint[K]], films: Map[K, Int] = Map.empty[K, Int], ambiguousTogether: Boolean = false)
              (implicit ord: Ordering[K]): Seq[Seq[K]] =
    solveAs(nodes, constraints, Presentation.Canonical, films, ambiguousTogether)

  /** Whether roots `ra` and `rb` may not join: a cannot-link between them, or two films. Read without an option per
   *  call — the solver asks it of every candidate union, and `get(..).exists` built a `Some` each time. */
  private[identity] def forbids(cannot: scala.collection.mutable.HashMap[Int, scala.collection.mutable.Set[Int]],
                                film: scala.collection.mutable.HashMap[Int, Int], ra: Int, rb: Int): Boolean = {
    val apart = cannot.getOrElse(ra, null)
    (apart != null && apart.contains(rb)) || (film.contains(ra) && film.contains(rb) && film(ra) != film(rb))
  }

  /** Whether any two of `roots` (a root with itself too) may not join ([[forbids]]): two of them holding different films,
   *  or one cannot-linked to another of them. Rule 3's ambiguity test, read in time linear in the roots: a bare title's
   *  node must-linked to every spelling of a family reaches hundreds of roots, and asking [[forbids]] of every PAIR of
   *  them was cubic in the family (a family of every kept spelling of a title, 2026-11-07: the solver 7 → 238 JFR
   *  samples of a UK resolve). The same answer as the pairwise one: a film pair differs exactly when two films are held,
   *  and a cannot-link pair is a root whose cannot-links meet the roots. */
  private[identity] def anyForbidden(roots: Set[Int], cannot: scala.collection.mutable.HashMap[Int, scala.collection.mutable.Set[Int]],
                                     film: scala.collection.mutable.HashMap[Int, Int]): Boolean = {
    var held = Option.empty[Int]
    roots.exists { r =>
      val own = film.getOrElse(r, Int.MinValue)
      val twoFilms = film.contains(r) && (held match {
        case Some(other) => other != own
        case None        => held = Some(own); false
      })
      twoFilms || {
        val apart = cannot.getOrElse(r, null)
        apart != null && apart.nonEmpty && (if (apart.size < roots.size) apart.exists(roots) else roots.exists(apart.contains))
      }
    }
  }

  private[identity] def solveAs[K](nodes: Seq[K], constraints: Seq[Constraint[K]], presentation: Presentation,
                                   films: Map[K, Int] = Map.empty[K, Int], ambiguousTogether: Boolean = false)
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
    // Each ROOT's film, maintained through every union.
    val film = scala.collection.mutable.HashMap.from(sortedNodes.indices.flatMap(i => films.get(sortedNodes(i)).map(i -> _)))
    def forbidden(ra: Int, rb: Int): Boolean = ConstraintSolver.forbids(cannot, film, ra, rb)
    def union(ra: Int, rb: Int): Unit = {
      // The smaller rank survives: a choice by the node's place in the total order.
      val (keep, gone) = if (ra < rb) (ra, rb) else (rb, ra)
      parent(gone) = keep
      film.remove(gone).foreach(f => film(keep) = f)
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
    else {
      // Rule 3's ambiguity, kept through the weaker tiers: a node a stronger tier found ambiguous is not settled by
      // weaker evidence.
      val ambiguous = scala.collection.mutable.Set.empty[Int]
      val sides     = scala.collection.mutable.Set.empty[Int]
      must.distinct.sorted.groupBy(_._1).toSeq.sortBy(_._1).foreach { case (_, tierEdges) =>
        val edges = tierEdges.sorted
        // Rule 3, judged against the components as they stand at the START of the tier.
        val reach = scala.collection.mutable.HashMap.empty[Int, Set[Int]]
        edges.foreach { case (_, a, b) =>
          reach(a) = reach.getOrElse(a, Set(find(a))) + find(b)
          reach(b) = reach.getOrElse(b, Set(find(b))) + find(a)
        }
        val ambiguousNow = reach.collect { case (node, roots) if ConstraintSolver.anyForbidden(roots, cannot, film) => node -> roots }
        ambiguous ++= ambiguousNow.keys
        // A side of its own ambiguity: a node holding a film, or cannot-linked to a component it reaches.
        sides ++= ambiguousNow.collect { case (node, roots) if film.contains(find(node)) || cannot.get(find(node)).exists(_.exists(roots)) => node }
        edges.foreach { case (_, a, b) => if (!ambiguous(a) && !ambiguous(b)) join(a, b) }
        // Rule 4: ambiguous nodes that are no side join each other, in components holding no other node.
        if (ambiguousTogether) {
          def bare(node: Int) = ambiguous(node) && !sides(node)
          val mixed = scala.collection.mutable.Set.from(sortedNodes.indices.iterator.filterNot(bare).map(find))
          edges.foreach { case (_, a, b) => if (bare(a) && bare(b) && !mixed(find(a)) && !mixed(find(b))) join(a, b) }
        }
      }
    }

    sortedNodes.indices.groupBy(find).values.toSeq
      .map(_.map(sortedNodes).sorted)
      .sortBy(_.head)
  }
}
