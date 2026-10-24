package services.identity

import services.movies.ListingKey

/** What the identity projection's kept index says of each published listing, for a spec outside its package: the TMDB
 *  film of the stored film it is on, and its cluster's listings. */
object ProjectionPlacements {
  def of(projection: adapters.IdentityProjection, filmIds: FilmIdCounterStore): (Map[ListingKey, Option[Int]], Map[ListingKey, Set[ListingKey]]) = {
    val index = projection.keptIndex(FilmIdCounters.of(filmIds.allChecked().required).toOption.get)
    (index.previousOf.map { case (k, ref) => k -> ref.tmdbId }, index.clusterOf.map { case (k, id) => k -> index.clusters(id).members })
  }
}
