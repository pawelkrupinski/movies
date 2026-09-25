package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.InMemoryStagingRepository

/**
 * A bare listing has ONE home: the film its own resolution placed it on.
 *
 * PL, 2026-09-25: Kino Amok lists "Samson i Dalila" with nothing else — no year, no minutes,
 * no director — and resolved it to DeMille's 1949 film (29993), whose row is keyed by another
 * venue's spelling, "Opera-samson i dalila". Kino Nowe Horyzonty's listing of the same title
 * publishes 2026 and Darko Tresnjak: the Met's broadcast, a different film, kept on a row of
 * its own keyed "Samson i Dalila" (2026). On re-scrape the landing matched Amok's bare title
 * to the row KEYED by it — the broadcast — and the settle moved it back: churn on every tick.
 * A listing that says nothing names no other film, so it stays where it is.
 */
class BareListingOneHomeSpec extends AnyFlatSpec with Matchers {

  private def bare(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(Movie(title = title), cinema, None, None, None, Nil, Nil,
      Seq(Showtime(java.time.LocalDateTime.of(2026, 12, 5, 18, 0), None)))

  "a bare re-scrape" should "stay on the resolved film it sits on, not move to a same-titled unresolved row" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, staging = Some(new InMemoryStagingRepository(normalizer = titleNormalizer)),
      normalizer = titleNormalizer,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    val film       = CacheKey("Opera-samson i dalila", Some(1949), titleNormalizer)
    val broadcast  = CacheKey("Samson i Dalila", Some(2026), titleNormalizer)
    val slot       = SourceData(title = Some("Samson i Dalila"), showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 12, 5, 18, 0), None)))
    cache.put(film, MovieRecord(tmdbId = Some(29993), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Samson i Dalia"), releaseYear = Some(1949), director = Seq("Cecil B. DeMille"), runtimeMinutes = Some(131)),
      CinemaShowing.keyFor(KinoMuranow, "Opera-samson i dalila", titleNormalizer) -> SourceData(title = Some("Opera-samson i dalila")),
      CinemaShowing.keyFor(Helios, "Samson i Dalila", titleNormalizer) -> slot)))
    cache.put(broadcast, MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt("searched", java.time.Instant.EPOCH)),
      data = Map[Source, SourceData](CinemaShowing.keyFor(Multikino, "Samson i Dalila", titleNormalizer) ->
        SourceData(title = Some("Samson i Dalila"), releaseYear = Some(2026), director = Seq("Darko Tresnjak"), runtimeMinutes = Some(234)))))
    val before = repository.findAll().sortBy(_.id.value)

    cache.recordCinemaScrape(Helios, Seq(bare(Helios, "Samson i Dalila")))

    def holders = cache.snapshot().filter(_.record.cinemaSlots.exists { case (s, _) => Source.cinemaOf(s).contains(Helios) })
      .map(r => (r.title, r.year))
    holders shouldBe Seq(("Opera-samson i dalila", Some(1949)))
    withClue("an identical re-scrape must write nothing: ")(repository.findAll().sortBy(_.id.value).map(_.record.cinemaSlots.map(_._1).toSet) shouldBe
      before.map(_.record.cinemaSlots.map(_._1).toSet))
  }
}
