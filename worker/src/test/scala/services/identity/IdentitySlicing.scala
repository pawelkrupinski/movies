package services.identity

import services.movies.{ListingKey, TitleNormalizer}

/**
 * What `scripts.IdentitySlice` cuts out of a whole recorded corpus to decide one listing as the whole corpus does, and
 * how it decides it again from the cut alone (docs/design/identity-slice.md):
 *
 *  - the CORE: every listing of the seeds' families as the whole-corpus resolve formed them ([[Resolution.familyOf]]: the
 *    union of title, search-form, original-title and matched-film block keys) — the listings resolved, family by family,
 *    exactly as [[IdentityResolver.resolveRegion]] resolves a family of the incremental model;
 *  - the CONTEXT: every other listing whose node carries a corpus-context key the core's nodes read ([[CorpusContext.Reads]]:
 *    a title key, title group, search group or title shape) — never resolved, only what the core's
 *    [[CorpusContext]] is derived from, so `recurringSegment`, title groups and same-titled reach read as over the
 *    whole corpus.
 *
 * Package `services.identity` for the resolver's own stages; the slice tool copies this file into an older ref's tree.
 */
object IdentitySlicing {

  /** What a CHANGED projection ([[IdentityProjection.tickChanged]]) hands its agreement stage after scrapes of the
   *  `rescraped` venues: EVERY listing the intake holds — its changed read re-reads only those venues and keeps every
   *  other venue's listings as held (IdentityListingIntake's `Read.Changed`) — and the re-scraped venues' listings as
   *  republished, each of their clusters read again (prod hands only those whose poster, days or names moved: this is
   *  the stricter case). Reading the re-scraped venues' listings alone digested a cluster as part of its listings and
   *  decided it on them, a flip no worker makes (UK CBeebies Panto at b5e156555, 2026-10-14). */
  final case class ChangedRead(listingOf: ListingKey => Option[Listing], republished: Option[Iterable[ListingKey]])

  def changedRead(listings: Seq[Listing], rescraped: Set[String]): ChangedRead = {
    val byKey = listings.map(l => l.key -> l).toMap
    ChangedRead(byKey.get, Some(listings.collect { case l if rescraped(l.venue) => l.key }))
  }

  /** The corpus a recording holds, resolved whole once: each listing's family, and each node's context keys. */
  final case class Index(listings: Seq[Listing], familyOf: Map[ListingKey, Int], nodeOf: Map[ListingKey, String],
                         keysOf: Map[String, Set[String]], nodeListings: Map[String, Seq[ListingKey]]) {
    lazy val byKey: Map[ListingKey, Listing] = listings.map(l => l.key -> l).toMap
    lazy val familyListings: Map[Int, Seq[ListingKey]] = familyOf.toSeq.groupMap(_._2)(_._1)
    lazy val nodesOfKey: Map[String, Seq[String]] = keysOf.toSeq.flatMap { case (node, keys) => keys.map(_ -> node) }.groupMap(_._1)(_._2)
  }

  /** The whole-corpus index of `listings`, read through `lookups` (memoised: the resolve and the node pass ask alike). */
  def index(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer): Index = {
    val resolution = IdentityResolver.resolve(listings, lookups, normalizer)
    val nodes      = nodesOf(listings, lookups, normalizer)
    Index(listings, resolution.familyOf,
      nodes.flatMap(node => node.listings.map(_.key -> node.id)).toMap,
      nodes.map(node => node.id -> keysOf(node, normalizer)).toMap,
      nodes.map(node => node.id -> node.listings.map(_.key)).toMap)
  }

  private def nodesOf(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer): Seq[EvidenceNode] =
    new CandidateGeneration(listings.sorted.distinctBy(_.key), lookups, normalizer, PinConstraints(Nil), TitleDecorations.resolver,
      lazyLookups = false).nodes

  /** A node's context keys, as a family's [[CorpusContext.Reads]] names them: what another node reading any of them reads
   *  this one by. */
  private def keysOf(node: EvidenceNode, normalizer: TitleNormalizer): Set[String] = {
    val reads = CorpusContext.Reads.of(Seq(node), Nil, Set.empty, Set.empty, normalizer.sanitize)
    reads.keys
  }

  /** A slice: the listings resolved (`core`) and those only read for the corpus context (`context`). */
  final case class Selection(core: Set[ListingKey], context: Set[ListingKey])

  /** The seeds' families, and every listing sharing a context key with one of their nodes. `widen` more rings: the context
   *  listings' own families joined to the core, and their context read in turn. */
  def select(index: Index, seeds: Set[ListingKey], widen: Int = 0): Selection = {
    def familiesOf(keys: Set[ListingKey]) = keys.flatMap(k => index.familyOf.get(k).toSeq.flatMap(index.familyListings)) ++ keys
    def contextOf(keys: Set[ListingKey]) = keys.flatMap(index.nodeOf.get).flatMap(index.keysOf).flatMap(k => index.nodesOfKey.getOrElse(k, Nil))
      .flatMap(index.nodeListings).toSet
    val core = (0 until widen).foldLeft(familiesOf(seeds))((core, _) => familiesOf(core ++ contextOf(core)))
    Selection(core, contextOf(core) -- core)
  }

  /** The model's decisions on `core`, its corpus context derived from `core` and `context` — as the incremental model
   *  decides a family against the whole corpus's context. */
  def resolve(core: Seq[Listing], context: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver): Seq[ResolverDecision] = {
    val decorations = TitleDecorations.resolver
    val pins        = PinConstraints(Nil)
    val corpus      = IdentityResolver.contextOf(core ++ context, lookups, normalizer, pins, decorations)
    IdentityResolver.resolveRegion(core, lookups, normalizer, calibration, pins, decorations, corpus).flatMap(_.decisions)
  }
}
