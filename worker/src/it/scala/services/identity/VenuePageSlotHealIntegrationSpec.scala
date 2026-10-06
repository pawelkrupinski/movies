package services.identity

import models.{CinemaMovie, CinemaShowing, Country, KinoApollo, Movie, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.{CinemaSlotBuilder, FilmId, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StringPool}
import services.venuepages.{MongoVenuePageStore, VenuePage, VenuePageKey}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * A venue slot whose detail is another page's heals through the projection alone, over the real `movies` /
 * `movie_slots` / `screenings` and `venue_pages`: Kino Iluzjon's 1968 "Lalka" held page /2457's url beside page
 * /7917's year, director, cast and runtime (prod, 2026-10-06), and the venue prints both films under one title.
 */
class VenuePageSlotHealIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val clock    = Clock.fixed(Instant.parse("2026-10-06T10:00:00Z"), ZoneOffset.UTC)
  private val start    = LocalDateTime.of(2026, 10, 7, 18, 0)
  private val has      = "https://www.iluzjon.fn.org.pl/filmy/info/2457/lalka.html"
  private val kawalski = "https://www.iluzjon.fn.org.pl/filmy/info/7917/lalka.html"
  private val hasRead  = FilmDetail(releaseYear = Some(1968), director = Seq("Wojciech Jerzy Has"), runtimeMinutes = Some(159),
    cast = Seq("Mariusz Dmochowski"), synopsis = Some("Wokulski kocha Izabelę."))
  private val kawalskiRead = FilmDetail(releaseYear = Some(2026), director = Seq("Maciej Kawalski"), runtimeMinutes = Some(162),
    cast = Seq("Marcin Dorociński"), synopsis = Some("Nowa ekranizacja."))

  private object Apollo extends DetailEnricher {
    def cinema: models.Cinema = KinoApollo
    def detailGroup: String   = "kinoapollo"
    def fetchFilmDetail(ref: String): Option[FilmDetail] = None // read from venue_pages only
  }

  private def listing(page: String, hour: Int) =
    CinemaMovie(Movie("Lalka"), KinoApollo, None, Some(page), None, Nil, Nil, Seq(Showtime(start.plusHours(hour.toLong), None)))
  private def detailOf(slot: SourceData) = (slot.releaseYear, slot.director, slot.runtimeMinutes, slot.cast, slot.synopsis)
  private def detailOf(read: FilmDetail) = (read.releaseYear, read.director, read.runtimeMinutes, read.cast, read.synopsis)

  "A venue slot holding another page's detail under its own page's url" should "be healed by the next projection, and stay healed" in
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "venue-page-slot-heal") { db =>
      val repository = new MongoMovieRepository(Some(db), clock, screenings = Some(new MongoScreeningsRepository(Some(db))),
        slots = Some(new MongoSlotsRepository(Some(db))), normalizer = ProjectionWorld.normalizer)
      val pages = new MongoVenuePageStore(db)
      Seq(has -> hasRead, kawalski -> kawalskiRead).foreach { case (page, read) =>
        pages.put(VenuePage(VenuePageKey(Apollo.detailGroup, page), VenuePage.Read(read), clock.instant())) shouldBe true
      }
      // The 1968 film as prod stores it: its own page's url, the other page's detail.
      val polluted = SourceData(title = Some("Lalka"), rawTitle = Some("Lalka"), filmUrl = Some(has),
        releaseYear = Some(2026), director = Seq("Maciej Kawalski"), runtimeMinutes = Some(162), cast = Seq("Marcin Dorociński"),
        synopsis = Some("Nowa ekranizacja."), showtimes = listing(has, 0).showtimes)
      repository.upsert(FilmId("f6482ce2e2b03e6c"), "Lalka", Some(1968),
        MovieRecord(data = Map(CinemaShowing(KinoApollo, "lalka") -> polluted)))

      val facts = new IndexedVenuePageFacts(Seq(Apollo), new VenuePageIndex(pages), Country.Poland.language)
      val world = new ProjectionWorld(repository, Seq(KinoApollo), clock,
        slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool, facts))
      world.scrape(Map(KinoApollo -> Seq(listing(has, 0), listing(kawalski, 3))))
      world.projection.tick().refused shouldBe None

      def byPage = repository.findAll().flatMap(f => f.record.data.collect {
        case (CinemaShowing(KinoApollo, _), slot) => slot.filmUrl -> (f.id, detailOf(slot)) }).toMap
      byPage.keySet shouldBe Set(Some(has), Some(kawalski))
      byPage(Some(has))._2 shouldBe detailOf(hasRead)
      byPage(Some(kawalski))._2 shouldBe detailOf(kawalskiRead)
      byPage(Some(has))._1 shouldBe FilmId("f6482ce2e2b03e6c")

      val again = world.projection.tick()
      withClue(s"${again.written} written: ")(again.wroteNothing shouldBe true)
      // A restarted worker reads the healed slots back and builds them alike.
      val restarted = world.restarted
      restarted.projection.tick().wroteNothing shouldBe true
      byPage(Some(has))._2 shouldBe detailOf(hasRead)
    }
}
