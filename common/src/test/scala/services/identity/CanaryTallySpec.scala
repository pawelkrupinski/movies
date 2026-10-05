package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

class CanaryTallySpec extends AnyFlatSpec with Matchers {

  /** Each listing's previous film, counting how often one is asked for: what working out a cluster's relation reads. */
  private final class CountingPreviousOf(underlying: Map[ListingKey, PipelineFilmRef]) extends scala.collection.immutable.AbstractMap[ListingKey, PipelineFilmRef] {
    var reads = 0
    def get(key: ListingKey): Option[PipelineFilmRef] = { reads += 1; underlying.get(key) }
    def iterator: Iterator[(ListingKey, PipelineFilmRef)] = underlying.iterator
    def removed(key: ListingKey): Map[ListingKey, PipelineFilmRef] = underlying.removed(key)
    def updated[V1 >: PipelineFilmRef](key: ListingKey, value: V1): Map[ListingKey, V1] = underlying.updated(key, value)
  }

  private def listing(film: Int, i: Int): ListingKey = ListingKey.Native(s"venue$i", s"https://venue$i/$film", s"Film $film")

  /** `films` films, each matched to its own TMDB film and stored as one film over its `venues` listings. */
  private def index(films: Int, venues: Int, previousOf: Map[ListingKey, PipelineFilmRef] => Map[ListingKey, PipelineFilmRef] = identity): ProjectionIndex = {
    val members  = (1 to films).map(f => f -> (1 to venues).map(listing(f, _)).toSet).toMap
    val previous = members.toSeq.flatMap { case (f, ls) => ls.map(_ -> PipelineFilmRef(s"film-$f", Some(f))) }.toMap
    ProjectionIndex(Map.empty, Map.empty, previousOf(previous), members.map { case (f, ls) => s"film-$f" -> ls },
      FilmIdCounters.empty, Nil, members.map { case (f, ls) => (ClusterId.Matched(f): ClusterId) -> Cluster(ls, Some(f)) },
      members.toSeq.flatMap { case (f, ls) => ls.map(_ -> (ClusterId.Matched(f): ClusterId)) }.toMap)
  }

  // On worker-us, every scoped projection walked every cluster's members: 6% of a light projection.
  "a canary tally" should "work out again only the relations a scoped projection can move" in {
    val tally   = new CanaryTally
    val first   = index(films = 20, venues = 50)
    tally.of(first, ProjectionScope(whole = false, Set.empty, Set.empty, Set.empty)) shouldBe IdentityProjectionPlan.canary(first)

    // The index the next projection reads: the same listings on the same films, the same objects.
    val counting = new CountingPreviousOf(first.previousOf)
    val now      = first.copy(previousOf = counting)
    val scope   = ProjectionScope(whole = false, Set("film-1"), Set(ClusterId.Matched(1)), (1 to 50).map(listing(1, _)).toSet)
    val canary  = tally.of(now, scope)

    canary shouldBe IdentityProjectionPlan.canary(index(films = 20, venues = 50))
    canary(ShadowRelation.Identical) shouldBe 20
    counting.reads should be <= 2 * 50   // the one cluster in scope; not 20 clusters' 1,000 listings
  }

  it should "work out again a cluster whose film a listing left, though the scope reaches only the film it joined" in {
    val tally  = new CanaryTally
    val before = index(films = 3, venues = 4)
    tally.of(before, ProjectionScope(whole = false, Set.empty, Set.empty, Set.empty))
    // One of film 1's listings is on film 2 now: the changes name the listing, whose previous film is film 2.
    val left    = listing(1, 1)
    val now     = before.copy(previousOf = before.previousOf.updated(left, PipelineFilmRef("film-2", Some(2))),
      listingsOf = before.listingsOf.updated("film-1", before.listingsOf("film-1") - left).updated("film-2", before.listingsOf("film-2") + left))
    val scope   = ProjectionScope(whole = false, Set("film-2"), Set(ClusterId.Matched(2)), before.listingsOf("film-2") + left)
    tally.of(now, scope) shouldBe IdentityProjectionPlan.canary(now)
  }

  it should "work out every relation again after a projection of the whole corpus" in {
    val tally = new CanaryTally
    tally.of(index(films = 3, venues = 4), ProjectionScope(whole = false, Set.empty, Set.empty, Set.empty))
    tally.reset()
    val moved = index(films = 3, venues = 4, _.map { case (k, ref) => k -> ref.copy(tmdbId = Some(99)) })
    tally.of(moved, ProjectionScope(whole = false, Set.empty, Set.empty, Set.empty)) shouldBe IdentityProjectionPlan.canary(moved)
  }
}
