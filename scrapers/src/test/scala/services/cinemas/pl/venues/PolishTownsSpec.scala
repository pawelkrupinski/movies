package services.cinemas.pl.venues

import clients.tools.FakeHttpFetch
import models.{KinoKameralne, KinoKryterium, KinoOstrovia, VenueClock}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaEgress, PolishScraperCatalog}
import services.cinemas.pl.{PolishVenue, PolishVenueRegistry}
import services.movies.SingleCountryNormalizer.titleNormalizer
import _root_.tools.{CachingDetailFetch, HttpFetch}

import java.time.LocalDate

/** The Polish towns' venue registries, and the catalog building whatever registries it is handed. */
class PolishTownsSpec extends AnyFlatSpec with Matchers {

  "PolishTowns" should "list each town once per tier" in {
    Seq(PolishTowns.towns, PolishTowns.filmwebCatchments, PolishTowns.nearbyTowns).foreach { tier =>
      tier.distinct should have size tier.size.toLong
    }
  }

  // A tier's list is its scrape order, kept by hand: a town whose catchment or nearby towns it left out would drop
  // those venues from every scrape.
  it should "list in each later tier exactly the towns that fill it" in {
    PolishTowns.filmwebCatchments.toSet shouldBe PolishTowns.towns.filter(_.filmwebCatchment.nonEmpty).toSet
    PolishTowns.nearbyTowns.toSet shouldBe PolishTowns.towns.filter(_.nearbyTowns.nonEmpty).toSet
  }

  it should "compose every tier's venues" in {
    PolishTowns.venues should have size
      PolishTowns.towns.map(town => town.venues.size + town.filmwebCatchment.size + town.nearbyTowns.size).sum.toLong
  }

  private object OneTown extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      biletyna(KinoKameralne, "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe"),
      msi(KinoKryterium, "https://bilety.ck105.koszalin.pl", route = PolishVenue.Route.Zyte),
      msi(KinoOstrovia, "https://bilety.kinoostrovia.pl"),
    )
  }

  "PolishScraperCatalog" should "build the venues it is handed, routing each platform host by its venue's data" in {
    val direct   = new FakeHttpFetch("does-not-exist")
    val biletyna = new FakeHttpFetch("biletyna")
    val zyte     = new FakeHttpFetch("zyte")
    val catalog = new PolishScraperCatalog(
      CinemaEgress(direct, multikino = direct, biletyna = biletyna, zyte = zyte, flicks = direct, vue = direct,
        odeon = direct, odeonBearer = services.cinemas.uk.OdeonBearer.fixed(None)),
      VenueClock.fixedOn(LocalDate.of(2026, 6, 6)), (_, h: HttpFetch, ttl) => new CachingDetailFetch(h, ttl),
      titleNormalizer, OneTown.venues, services.cinemas.CinemaScraperCatalog.OnTheAskingThread)

    catalog.scrapers.map(_.cinema) shouldBe Seq(KinoKameralne, KinoKryterium, KinoOstrovia)
    catalog.egressByHost.keySet shouldBe Set("biletyna.pl", "bilety.ck105.koszalin.pl")
    catalog.egressByHost("biletyna.pl") should be theSameInstanceAs biletyna
    catalog.egressByHost("bilety.ck105.koszalin.pl") should be theSameInstanceAs zyte
  }
}

/** Kino Kijów (Kraków) re-skinned its MSI portal on 2026-11-19: the date-sorted month page its bespoke parser read
 *  (`div.cd-timeline-content`, one block a screening) became the standard `movies-movie__single` page, whose title
 *  heading carries no `title` attribute there, so every scrape parsed nothing and Filmweb covered the venue for a
 *  week. Both layouts are MSI skins, so Kijów is wired as an [[services.cinemas.pl.MsiClient]] like every other MSI
 *  venue. Replays the Kraków registry's own wiring over the portal recorded on 2026-11-27 (the date-sorted November
 *  and December pages, and the event-labels feed). */
class KinoKijowWiringSpec extends AnyFlatSpec with Matchers with org.scalatest.OptionValues {

  private lazy val kijow = {
    val portal = new FakeHttpFetch("kino-kijow")
    val catalog = new PolishScraperCatalog(
      CinemaEgress(portal, multikino = portal, biletyna = portal, zyte = portal, flicks = portal, vue = portal,
        odeon = portal, odeonBearer = services.cinemas.uk.OdeonBearer.fixed(None)),
      VenueClock.fixedOn(LocalDate.of(2026, 11, 27)), (_, h: HttpFetch, ttl) => new CachingDetailFetch(h, ttl),
      titleNormalizer, MalopolskieVenues.KrakowVenues.venues,
      services.cinemas.CinemaScraperCatalog.OnTheAskingThread)
    catalog.scrapers.find(_.cinema == models.KinoKijow).value.fetch()
  }

  private def titled(prefix: String) = kijow.find(_.movie.title.toLowerCase(java.util.Locale.ROOT).startsWith(prefix)).value

  "Kino Kijów's wiring" should "read the venue's standard MSI month pages" in {
    val rocky = titled("jestem rocky")
    rocky.showtimes.map(_.dateTime) should contain(java.time.LocalDateTime.of(2026, 11, 27, 17, 30))
    rocky.showtimes.flatMap(_.bookingUrl).head should startWith("https://kupbilet.kijow.pl/MSI/Default.aspx?event_id=")
  }

  it should "lift the version word off the title into the showtime's format" in {
    titled("hokus").showtimes.map(_.format).head shouldBe List("2D", "DUB")
  }

  it should "drop the portal's plays, filed under Teatr" in {
    kijow.map(_.movie.title.toLowerCase(java.util.Locale.ROOT)).exists(_.contains("spektakl")) shouldBe false
  }
}

/** Kino SDK (Sanok) re-skinned its MSI portal on 2026-11-25: the per-day "lista wydarzeń" page its own reader walks
 *  (`?sort=Flow&date=<day>`, a date picker and `list-group-item` cards) now lists no event and no picker day, while
 *  the standard month pages (`?sort=Name&date=<month>`) carry the programme, so the venue sat on the Filmweb fallback.
 *  Replays the Przemyśl registry's own wiring over the portal recorded on 2026-11-27 (the day page, the November and
 *  December month pages and the event-labels feed). */
class KinoSDKWiringSpec extends AnyFlatSpec with Matchers with org.scalatest.OptionValues {

  private lazy val sanok = {
    val portal = new FakeHttpFetch("kino-sdk-sanok-2026-11")
    val catalog = new PolishScraperCatalog(
      CinemaEgress(portal, multikino = portal, biletyna = portal, zyte = portal, flicks = portal, vue = portal,
        odeon = portal, odeonBearer = services.cinemas.uk.OdeonBearer.fixed(None)),
      VenueClock.fixedOn(LocalDate.of(2026, 11, 27)), (_, h: HttpFetch, ttl) => new CachingDetailFetch(h, ttl),
      titleNormalizer, PodkarpackieVenues.PrzemyslVenues.filmwebCatchment,
      services.cinemas.CinemaScraperCatalog.OnTheAskingThread)
    catalog.scrapers.find(_.cinema == models.KinoSDK).value.fetch()
  }

  "Kino SDK's wiring" should "read the standard month pages when the day page lists nothing" in {
    val rocky = sanok.find(_.movie.title.toLowerCase(java.util.Locale.ROOT) == "jestem rocky").value
    rocky.showtimes should not be empty
    rocky.showtimes.flatMap(_.bookingUrl).head should startWith("https://bilety.sdksanok.pl/MSI/Default.aspx?event_id=")
    sanok.map(_.movie.title.toLowerCase(java.util.Locale.ROOT)) should contain("struś pędziwiatr i zemsta kojota")
  }
}
