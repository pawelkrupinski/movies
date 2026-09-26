package services.identity

import services.movies.TitleNormalizer

/**
 * The FAMILY an identity resolve is scoped to, and the runtime check that the scoping is sound
 * (docs/design/identity-resolver.md, "Families: the block closure").
 *
 * A family is the union-find closure of every listing's BLOCK KEYS — its sanitised title, its
 * search form, its original title (both forms) and, once resolved, its TMDB id. Every key is
 * computable from one listing plus its own lookups, so a new listing is routed to its family
 * without reading the corpus, and a resolve scoped to one family equals the global resolve
 * exactly when no constraint edge joins two families.
 *
 * That holds BY CONSTRUCTION only while every edge rule draws between listings sharing a block
 * key. A rule that does not — raw-title containment is the standing example: "Zärtlich kreist
 * die Faust" shares no key with Murnau's "Faust", and the containment edge that joined them is
 * the Faust bug — makes a scoped resolve silently miss the edge. [[crossings]] finds such an
 * edge; a resolve that finds one must fail rather than scope, and the rule needs its block key.
 *
 * Pure. [[FamilyClosureMetrics]] is the hook the resolver reports through; nothing in
 * production calls this yet (the resolver runs in shadow — see the design doc's migration plan).
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
   *  in the search-form namespace so a segment meets a plain listing's whole title. They put a
   *  decorated spelling in its plain sibling's family — which is what lets the resolver's
   *  group-level signals reach it — without drawing any edge by themselves. */
  def blockKeys(cleanTitle: String, originalTitle: Option[String], tmdbId: Option[Int],
                normalizer: TitleNormalizer, segments: Seq[String] = Nil): Set[String] = {
    def titleKeys(t: String) = Seq("t:" + normalizer.sanitize(t), "q:" + normalizer.searchQuery(t))
    (titleKeys(cleanTitle) ++ originalTitle.toSeq.flatMap(titleKeys) ++ segments.map(s => "q:" + normalizer.searchQuery(s)) ++
      tmdbId.map(id => s"id:$id"))
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

  /** How many of `next`'s families join listings that `previous` held in two or more families —
   *  a family MERGE between consecutive resolves. Correct when a new listing's original title
   *  bridges two titles; worth watching, because a merged family is resolved as one unit and
   *  its cost grows with it. Listings `previous` never saw do not count. */
  def merges[K](previous: Map[K, Int], next: Map[K, Int]): Int =
    next.toSeq.flatMap { case (k, f) => previous.get(k).map(f -> _) }
      .groupMap(_._1)(_._2).count(_._2.distinct.sizeIs > 1)

  /** The runtime check: the families of `blockKeys`, or the edges crossing them. A resolve
   *  that gets a `Left` must not proceed family-scoped. Both outcomes are reported through
   *  `metrics`, with `scope` naming the country or run. */
  def check[K](scope: String, blockKeys: Map[K, Set[String]], edges: Seq[Edge[K]], metrics: FamilyClosureMetrics,
               previous: Map[K, Int] = Map.empty[K, Int])(implicit ord: Ordering[K]): Either[Seq[Crossing[K]], Map[K, Int]] = {
    val familyOf = families(blockKeys)
    val crossing = crossings(familyOf, edges)
    metrics.recordCrossings(scope, crossing.size, crossing.take(5).map(_.edge.reason))
    if (previous.nonEmpty) metrics.recordFamilyMerges(scope, merges(previous, familyOf))
    if (crossing.nonEmpty) Left(crossing) else Right(familyOf)
  }
}

/** Where [[FamilyClosure.check]] reports. Not wired to Prometheus yet; the resolver's shadow run
 *  takes [[FamilyClosureMetrics.logging]], a spec [[FamilyClosureMetrics.noop]]. */
trait FamilyClosureMetrics {
  /** `count` edges crossed a family in one resolve; `reasons` names the first few edge rules. */
  def recordCrossings(scope: String, count: Int, reasons: Seq[String]): Unit
  /** `count` families of this resolve merged families of the previous one. */
  def recordFamilyMerges(scope: String, count: Int): Unit = ()
}

object FamilyClosureMetrics {
  val noop: FamilyClosureMetrics = (_, _, _) => ()

  /** Logs a crossing as an error (a rule was added without its block key) and a merge as info. */
  def logging(logger: play.api.Logger): FamilyClosureMetrics = new FamilyClosureMetrics {
    override def recordCrossings(scope: String, count: Int, reasons: Seq[String]): Unit =
      if (count > 0) logger.error(s"identity[$scope]: $count constraint edge(s) cross a family " +
        s"(rules: ${reasons.distinct.mkString(", ")}) — a rule was added without a block key; the resolve must not scope by family.")
    override def recordFamilyMerges(scope: String, count: Int): Unit =
      if (count > 0) logger.info(s"identity[$scope]: $count family merge(s) since the previous resolve.")
  }
}
