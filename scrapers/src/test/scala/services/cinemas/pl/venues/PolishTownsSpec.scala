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
      titleNormalizer, OneTown.venues)

    catalog.scrapers.map(_.cinema) shouldBe Seq(KinoKameralne, KinoKryterium, KinoOstrovia)
    catalog.egressByHost.keySet shouldBe Set("biletyna.pl", "bilety.ck105.koszalin.pl")
    catalog.egressByHost("biletyna.pl") should be theSameInstanceAs biletyna
    catalog.egressByHost("bilety.ck105.koszalin.pl") should be theSameInstanceAs zyte
  }
}
