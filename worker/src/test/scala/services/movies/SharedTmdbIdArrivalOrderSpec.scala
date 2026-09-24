package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{CinemaCityKinepolis, KinoMuza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Two spellings of one film resolve to the same tmdbId in either order — what parallel
 * enrichment does in production, and what made `WarmResolutionCacheSpec` disagree with
 * itself one run in five: 'Nowa fala' (Kino Muza) and 'Unlimited Show - Nowa Fala'
 * (Cinema City), both tmdbId 1254808.
 */
class SharedTmdbIdArrivalOrderSpec extends AnyFlatSpec with Matchers {

  private val tmdbId = 1254808
  private def spelling(cinema: Source, title: String) =
    MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](cinema -> SourceData(title = Some(title))))
  private val plain     = ("Nowa fala", spelling(KinoMuza, "Nowa fala"))
  private val decorated = ("Unlimited Show - Nowa Fala", spelling(CinemaCityKinepolis, "Unlimited Show - Nowa Fala"))

  private def arrive(order: Seq[(String, MovieRecord)]): (InMemoryMovieRepository, CaffeineMovieCache) = {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    order.foreach { case (title, record) => cache.put(cache.keyOf(title, Some(2025)), record) }
    (repository, cache)
  }

  private def stored(repository: InMemoryMovieRepository) =
    repository.findAll().map(r => (r.key(titleNormalizer), r.title, r.year, r.record.tmdbId, r.record.cinemaData.keySet))

  "two spellings concluding one tmdbId" should "settle to the same film, keyed the same, whichever concludes first" in {
    val (a, _) = arrive(Seq(plain, decorated))
    val (b, _) = arrive(Seq(decorated, plain))
    stored(a) should have size 1
    stored(a) shouldBe stored(b)
  }

  it should "keep the id of whichever row exists first — ids are opaque and permanent, never re-derived by a fold" in {
    val (a, _) = arrive(Seq(plain, decorated))
    val (b, _) = arrive(Seq(decorated, plain))
    a.findAll().map(_.id) shouldBe Seq(FilmId.fresh(CacheKey("Nowa fala", Some(2025), titleNormalizer), _ => false))
    b.findAll().map(_.id) shouldBe Seq(FilmId.fresh(CacheKey("Unlimited Show - Nowa Fala", Some(2025), titleNormalizer), _ => false))
  }

  // The pipeline concludes both spellings in STAGING (the log's "TMDB (staging)"), so the
  // folds race, not the cache writes: each folds its own group, and the second into the first.
  private def fold(order: Seq[(String, MovieRecord)]): InMemoryMovieRepository = {
    val staging    = new services.staging.InMemoryStagingRepository(normalizer = titleNormalizer)
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val folder     = new services.staging.InMemoryStagingFolder(staging, repository, titleNormalizer)
    order.foreach { case (title, record) =>
      staging.upsert(record.cinemaData.keys.head, title, Some(2025), record)
      folder.foldGroup(title)
    }
    repository
  }

  "two spellings folding out of staging onto one tmdbId" should "settle to the same film whichever folds first" in {
    val (a, b) = (fold(Seq(plain, decorated)), fold(Seq(decorated, plain)))
    stored(a) should have size 1
    stored(a) shouldBe stored(b)
  }
}
