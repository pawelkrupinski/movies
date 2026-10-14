package services.identity

import services.movies.TitleNormalizer

/**
 * The FAMILY an identity resolve is scoped to, and the check that the scoping is sound
 * (docs/design/identity-resolver.md, "Families: the block closure").
 *
 * A family is the union-find closure of every listing's BLOCK KEYS — its sanitised title, its
 * search form, its original title (both forms) and, once resolved, its TMDB id (`id:`, added by
 * [[Families]]). Every key is
 * computable from one listing plus its own lookups, so a new listing is routed to its family
 * without reading the corpus, and a resolve scoped to one family equals the global resolve
 * exactly when no constraint edge joins two families.
 *
 * That holds BY CONSTRUCTION only while every edge rule draws between listings sharing a block
 * key. A rule that does not — raw-title containment is the standing example: "Zärtlich kreist
 * die Faust" shares no key with Murnau's "Faust", and the containment edge that joined them is
 * the Faust bug — makes a scoped resolve silently miss the edge. [[crossings]] finds such an
 * edge; a resolve that finds one fails rather than scope (`IdentityResolver`'s `FamilyCrossing`),
 * and the rule needs its block key. Pure.
 */
object FamilyClosure {

  /** A constraint edge between two listings: a must-link or a cannot-link, with its reason. */
  final case class Edge[K](a: K, b: K, must: Boolean, reason: String)

  /** An edge whose endpoints lie in different families — or one endpoint in none. */
  final case class Crossing[K](edge: Edge[K], familyOfA: Option[Int], familyOfB: Option[Int])

  /** The block keys of one listing's evidence. Blank forms are no key: two listings whose
   *  titles sanitise to nothing must not share a family through the empty string.
   *
   *  `segments` are the delimited parts of the title a listing publishes around a programme
   *  banner or a decoration ("Oficjalna premiera: Lalka" → "Oficjalna premiera", "Lalka"), keyed
   *  in the sanitised-title namespace (case- and punctuation-blind) so a segment meets a plain
   *  listing's whole title however either is cased. They put a
   *  decorated spelling in its plain sibling's family — which is what lets the resolver's
   *  group-level signals reach it — without drawing any edge by themselves. */
  def blockKeys(cleanTitle: String, originalTitle: Option[String], normalizer: TitleNormalizer, segments: Seq[String] = Nil): Set[String] = {
    def titleKeys(t: String) = Seq("t:" + normalizer.sanitize(t), "q:" + normalizer.searchQuery(t))
    (titleKeys(cleanTitle) ++ originalTitle.toSeq.flatMap(titleKeys) ++ segments.map(s => "t:" + normalizer.sanitize(s)))
      .filterNot(_.endsWith(":")).toSet
  }

  /** Each listing's family, numbered 0, 1, … in the order of each family's smallest listing —
   *  a function of the key set alone, never of the order `blockKeys` is given in. */
  def families[K](blockKeys: Map[K, Set[String]])(implicit ord: Ordering[K]): Map[K, Int] = {
    val parent = scala.collection.mutable.HashMap.empty[String, String]
    def find(x: String): String = {
      var r = parent.getOrElseUpdate(x, x)
      while (parent(r) != r) r = parent(r)
      var c = x
      while (parent(c) != r) { val n = parent(c); parent(c) = r; c = n }
      r
    }
    def union(x: String, y: String): Unit = {
      val (a, b) = (find(x), find(y))
      if (a != b) { if (a < b) parent(b) = a else parent(a) = b }
    }
    val listings = blockKeys.keys.toSeq.sorted
    val node     = listings.zipWithIndex.map { case (k, i) => k -> s"\u0000$i" }.toMap
    listings.foreach { k => find(node(k)); blockKeys(k).foreach(union(node(k), _)) }
    val number = listings.map(k => find(node(k))).distinct.zipWithIndex.toMap
    listings.map(k => k -> number(find(node(k)))).toMap
  }

  /** Every edge that crosses a family boundary, in the edges' own order. O(edges). */
  def crossings[K](familyOf: Map[K, Int], edges: Seq[Edge[K]]): Seq[Crossing[K]] =
    edges.flatMap { e =>
      val (fa, fb) = (familyOf.get(e.a), familyOf.get(e.b))
      Option.when(fa.isEmpty || fb.isEmpty || fa != fb)(Crossing(e, fa, fb))
    }

}
