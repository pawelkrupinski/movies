package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.GetOnlyHttpFetch

/**
 * A resolved row that has lost its `Tmdb` slot gets it back BY ID — never by a
 * search, which for a row with screenings can land on a stranger and prune the card.
 * Prod, 2026-09-06: 483 such rows across PL/UK/DE, all resolved inside the slot
 * migration's window; nothing was going to heal them.
 */
class TmdbSlotRefillSpec extends AnyFlatSpec with Matchers {

  private val Id = 270303   // "It Follows"

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String = {
      if (url.contains("/search/")) throw new RuntimeException(s"a refill must not search: $url")
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    }
  }

  private def tmdbById(details: Boolean = true): TmdbClient = new TmdbClient(http = new StubFetch(
    (if (details) Map(s"/movie/$Id?" ->
      s"""{"id":$Id,"title":"Coś za mną chodzi","original_title":"It Follows","release_date":"2014-05-17","runtime":100,"overview":"…",
         |"genres":[{"id":27,"name":"Horror"}],"credits":{"crew":[{"job":"Director","name":"David Robert Mitchell"}],"cast":[]}}""".stripMargin)
     else Map.empty[String, String]) ++
    Map(s"/movie/$Id/external_ids" -> s"""{"id":$Id,"imdb_id":"tt3235888","wikidata_id":"Q17012047"}""")),
    apiKey = Some("stub"))

  private def slotless: MovieRecord = MovieRecord(
    tmdbId = Some(Id), imdbId = Some("tt3235888"), imdbRating = Some(6.8), filmwebRating = Some(6.5),
    data = Map[Source, SourceData](Helios -> SourceData(title = Some("Coś za mną chodzi"), runtimeMinutes = Some(100))))

  "refillTmdbSlot" should "fetch the slot by the row's own id and carry everything else forward" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val key     = cache.keyOf("Coś za mną chodzi", Some(2015))
    cache.put(key, slotless)
    val service = new MovieService(cache, new InProcessEventBus(), tmdbById())

    service.refillTmdbSlot(key) shouldBe true

    val after = cache.get(key).get
    after.data.get(Tmdb).flatMap(_.originalTitle) shouldBe Some("It Follows")
    after.data.get(Tmdb).flatMap(_.runtimeMinutes) shouldBe Some(100)
    after.tmdbId        shouldBe Some(Id)
    after.imdbRating    shouldBe Some(6.8)      // ratings are the same film's, kept
    after.filmwebRating shouldBe Some(6.5)
    after.wikidataId    shouldBe Some("Q17012047")
    after.data.get(Helios).flatMap(_.title) shouldBe Some("Coś za mną chodzi")
  }

  it should "leave a row that has its slot, or no id, alone" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val withSlot = cache.keyOf("A", Some(2020)); cache.put(withSlot, slotless.copy(data = slotless.data + ((Tmdb: Source) -> SourceData(title = Some("A")))))
    val noId     = cache.keyOf("B", Some(2020)); cache.put(noId, slotless.copy(tmdbId = None))
    val service  = new MovieService(cache, new InProcessEventBus(), tmdbById())
    service.refillTmdbSlot(withSlot) shouldBe false
    service.refillTmdbSlot(noId)     shouldBe false
  }

  it should "write nothing when TMDB cannot answer for the id" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val key     = cache.keyOf("Coś za mną chodzi", Some(2015))
    cache.put(key, slotless)
    val service = new MovieService(cache, new InProcessEventBus(), tmdbById(details = false))
    service.refillTmdbSlot(key) shouldBe false
    cache.get(key).get.data.contains(Tmdb) shouldBe false
  }
}
