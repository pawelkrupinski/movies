package services.identity

import services.movies.ListingKey

/** A family the size of a US wide release's: one film listed by ~3,000 venues under one page and title, one decision
 *  naming them all, a handful of node texts between them — the shape of the largest `identity_model_families`
 *  documents (~1.3 MB each), what a family's write is measured on. */
object LargeIdentityFamily {
  val Page = "https://www.flicks.us/movie/verity/"
  private val Nodes  = Seq("Verity\u0000Verity\u00002026\u0000Michael Showalter", "Verity\u0000Verity\u00002026", "Verity", "Verity\u0000Showalter")
  private val Chains = Seq("AMC", "Regal", "Cinemark", "Marcus", "Harkins")

  /** `count` venues' names, in no particular order. */
  def venues(count: Int): Seq[String] = (1 to count).map(i => f"${Chains(i % Chains.size)} Theatre $i%04d")

  def of(venues: Seq[String], digest: Long): StoredFamily = {
    val keys = venues.map(venue => ListingKey.Native(venue, Page, "Verity")).sorted(using ListingKey.ordering)
    StoredFamily(StoredFamily.idOf(keys), IdentityResolver.RegionFamily(
      listings  = keys.toSet,
      decisions = Seq(ResolverDecision(keys, Some(1283515), 0.99, ResolverDecision.Basis.OwnMatch,
        Seq(s"'Verity' [2026] ×${keys.size}: own match 1283515 by venues.corroborating=${keys.size}"))()),
      blockKeys = Set("verity"), queries = Set.empty, films = Set(1283515),
      reads     = CorpusContext.Reads(Set("verity"), Set.empty, Set.empty, Set.empty, Set(1283515), Set.empty),
      nodeKeys  = keys.map(key => key -> Nodes(Math.floorMod(key.venue.hashCode, Nodes.size))).toMap), digest)
  }
}
