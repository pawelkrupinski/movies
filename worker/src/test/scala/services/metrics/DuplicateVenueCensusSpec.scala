package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, City, Country, KinoMikro, MikroBronowice, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord
import tools.Slugify

import java.time.LocalDateTime

/**
 * Locks the census of one screen listed twice under two names — two roster venues in one city
 * whose upcoming programmes are (nearly) the same set of (film, start time). Two such venues
 * went unnoticed until a person compared them by eye; a name-based audit cannot see a pair whose
 * names share nothing.
 *
 * Pinned: a same-city pair at 90% overlap counts, one below does not; a pair in two cities does
 * not count at that bar (a chain's national programme is not one screen) but does in the
 * cross-city scope at 95% with 20+ showtimes (one venue's feed under another's name), found
 * exactly among a crowd; a tiny programme cannot match by coincidence; a pair whose showtimes book
 * through each venue's own links is two venues, while one booking the same sessions (on any
 * host) or a venue with no links still counts; the shared allowlist clears every known pair in
 * its scope; and a partial scan publishes nothing.
 */
class DuplicateVenueCensusSpec extends AnyFlatSpec with Matchers {
  import CorpusMetricsFixtures.{clock, now, slot}

  private val city: City = Country.Poland.cities.find(_.cinemas.distinct.size >= 3).get
  private val Seq(first, second, third) = city.cinemas.distinct.take(3)
  private def citiesOf(country: Country, v: Cinema): Set[City] = country.cities.filter(_.cinemas.contains(v)).toSet
  /** A Polish venue that shares no city with `first` — regional hubs list a venue in several. */
  private val elsewhere: Cinema =
    Country.Poland.cities.flatMap(_.cinemas).find(x => (citiesOf(Country.Poland, x) intersect citiesOf(Country.Poland, first)).isEmpty).get

  /** `n` upcoming evening showtimes of one film, a day apart. */
  private def times(n: Int, from: Int = 1): Seq[LocalDateTime] = (from until from + n).map(d => now.plusDays(d.toLong))

  private def row(title: String, slots: (Source, Seq[LocalDateTime])*): StoredMovieRecord =
    StoredMovieRecord.synthesised(title, Some(2026), MovieRecord(tmdbId = Some(1), data = slots.map { case (s, t) => s -> slot(t*) }.toMap), services.movies.SingleCountryNormalizer.titleNormalizer)

  /** `times`, each booked through `url` of its index — a venue's own session links. */
  private def linked(times: Seq[LocalDateTime], url: Int => String): SourceData =
    SourceData(title = Some("x"), showtimes = times.zipWithIndex.map { case (t, i) => Showtime(t, Some(url(i))) })

  private def linkedRow(title: String, slots: (Source, SourceData)*): StoredMovieRecord =
    StoredMovieRecord.synthesised(title, Some(2026), MovieRecord(tmdbId = Some(1), data = slots.toMap), services.movies.SingleCountryNormalizer.titleNormalizer)

  /** A session link at venue `code` on its ticketing backend. */
  private def session(code: String, host: String = "tickets.example.com"): Int => String = i => s"https://$host/$code/purchase/$i"

  private def census(rows: Seq[StoredMovieRecord], complete: Boolean = true, preset: Option[Double] = None,
                     country: Country = Country.Poland, scope: String = DuplicateVenueCensus.SameCity): Double = {
    val gauge  = DuplicateVenueCensus.gauge(new PrometheusRegistry())
    val census = new DuplicateVenueCensus(gauge, country, clock)
    preset.foreach(gauge.labelValues(country.code, scope).set)
    val sampler = census.startSample()
    rows.foreach(sampler.accept)
    sampler.publish(complete)
    gauge.labelValues(country.code, scope).get()
  }

  private def crossCity(rows: StoredMovieRecord*): Double = census(rows, scope = DuplicateVenueCensus.CrossCity)

  /** The pairs, named as the generated rosters store them, that the census still counts when each
   *  pair lists one identical 20-showtime programme -- in the scope the roster puts them in --
   *  each venue booking through the link `links` gives it, or none. */
  private def countedPairs(country: Country, pairs: Seq[(String, String)], links: String => Option[Int => String] = _ => None): Seq[String] =
    pairs.flatMap { case (a, b) =>
      val (x, y) = (Cinema.byDisplayName(a), Cinema.byDisplayName(b))
      val scope =
        if ((citiesOf(country, x) intersect citiesOf(country, y)).nonEmpty) DuplicateVenueCensus.SameCity
        else DuplicateVenueCensus.CrossCity
      def slotOf(name: String) = links(name).fold(slot(times(20)*))(linked(times(20), _))
      val counted = census(Seq(linkedRow("Foo", x -> slotOf(a), y -> slotOf(b))), country = country, scope = scope)
      Option.when(counted != 0.0)(s"$a / $b ($scope): $counted")
    }

  "DuplicateVenueCensus" should "count two venues in one city whose upcoming programmes overlap by 90% or more" in {
    census(Seq(
      row("Foo", first -> times(10), second -> times(10), third -> times(10, from = 20)),
      row("Bar", first -> times(1, from = 40)))) shouldBe 1.0   // first and second share 10 of first's 11: 91%
  }

  it should "not count a pair below 90%, nor a pair in two different cities, nor a programme too small to tell" in {
    census(Seq(row("Foo", first -> times(10), second -> times(8)))) shouldBe 0.0             // 8 of 10
    census(Seq(row("Foo", first -> times(10), elsewhere -> times(10)))) shouldBe 0.0    // identical, but two cities
    census(Seq(row("Foo", first -> times(4), second -> times(4)))) shouldBe 0.0              // under MinShowtimes
  }

  it should "ignore showtimes already past, and count a film's start times per film, not per clock time" in {
    val past = Seq(now.minusDays(2), now.minusDays(3), now.minusDays(4), now.minusDays(5), now.minusDays(6))
    census(Seq(row("Foo", first -> (times(5) ++ past), second -> times(5)))) shouldBe 1.0    // the past five do not dilute
    census(Seq(row("Foo", first -> times(5)), row("Bar", second -> times(5)))) shouldBe 0.0  // same times, different films
  }

  it should "not count a pair on the shared distinct-venue allowlist" in {
    services.cinemas.roster.DistinctVenuePairs.contains(KinoMikro, MikroBronowice) shouldBe true
    // Positive control: without the allowlist they WOULD pair — one city lists both.
    Country.Poland.cities.exists(c => c.cinemas.contains(KinoMikro) && c.cinemas.contains(MikroBronowice)) shouldBe true
    census(Seq(row("Foo", KinoMikro -> times(10), MikroBronowice -> times(10)))) shouldBe 0.0
  }

  it should "not count a pair whose shared showtimes each book through the venue's own session links" in {
    census(Seq(linkedRow("Foo", first -> linked(times(10), session("097")), second -> linked(times(10), session("084"))))) shouldBe 0.0
    crossCity(linkedRow("Foo", first -> linked(times(20), session("097")), elsewhere -> linked(times(20), session("084")))) shouldBe 0.0
  }

  // The Syracuse IN Pickwick, 2026-09-23: Flicks booked it through the Park Ridge IL Pickwick's
  // Veezi purchase ids, served from a regional mirror — ticketing.useast. vs ticketing.us. — so
  // the links differ as strings but name the same session.
  it should "still count a pair booking the same sessions, whatever host serves them" in {
    val pickwick = (host: String) => (i: Int) => s"https://$host/purchase/5085$i?siteToken=pickwick"
    crossCity(linkedRow("Foo", first -> linked(times(20), pickwick("ticketing.useast.veezi.com")),
      elsewhere -> linked(times(20), pickwick("ticketing.us.veezi.com")))) shouldBe 1.0
  }

  // Kino Etiuda OBK (a Filmweb organiser listing, no links) repeated Kino Etiuda (bilety24).
  it should "still count a pair where one venue carries no booking links, which cannot tell them apart" in {
    census(Seq(linkedRow("Foo", first -> linked(times(10), session("etiuda")), second -> slot(times(10)*)))) shouldBe 1.0
  }

  it should "clear a pair only when at least half the shared showtimes book apart" in {
    def split(apart: Int) = linkedRow("Foo", first -> linked(times(20), session("a")),
      elsewhere -> linked(times(20), i => if (i < apart) session("b")(i) else session("a")(i)))
    crossCity(split(9)) shouldBe 1.0
    crossCity(split(10)) shouldBe 0.0
  }

  "The cross-city scope" should "count two venues sharing no city whose programmes are 95%+ the same" in {
    crossCity(row("Foo", first -> times(20), elsewhere -> times(20))) shouldBe 1.0                        // identical
    crossCity(row("Foo", first -> times(20), elsewhere -> times(19)), row("Bar", elsewhere -> times(1))) shouldBe 1.0  // 19 of 20
  }

  it should "not count a pair below 95%, a programme under 20 showtimes, or a same-city pair" in {
    crossCity(row("Foo", first -> times(20), elsewhere -> times(18)), row("Bar", elsewhere -> times(2))) shouldBe 0.0  // 18 of 20
    crossCity(row("Foo", first -> times(19), elsewhere -> times(19))) shouldBe 0.0                        // under 20
    crossCity(row("Foo", first -> times(20), second -> times(20))) shouldBe 0.0                           // same city: the other scope's
    census(Seq(row("Foo", first -> times(20), elsewhere -> times(20)))) shouldBe 0.0                      // …and vice versa
  }

  it should "find the one mirrored pair in a crowd that shares most of its programme" in {
    // 200 venues all showing one blockbuster at the same 25 times, plus 5 showtimes of their own
    // (83% alike, never counted); the mirrored pair shares its own five too. The candidate join
    // keys on each venue's RAREST showtimes, so the crowd must neither hide nor flood the pair.
    val crowd  = Country.Poland.cities.flatMap(_.cinemas).distinct.filterNot(v => v == first || v == elsewhere).take(200)
    val blockbuster = row("Blockbuster", ((crowd :+ first :+ elsewhere).map(_ -> times(25)))*)
    val own    = crowd.zipWithIndex.map { case (v, i) => row(s"Own$i", v -> times(5, from = 40)) }
    val mirror = row("Mirror", first -> times(5, from = 40), elsewhere -> times(5, from = 40))
    crossCity((blockbuster +: mirror +: own)*) shouldBe 1.0
    crossCity((row("Blockbuster", crowd.map(_ -> times(25))*) +: own)*) shouldBe 0.0   // the crowd alone: 83%
  }

  // The pairs prod's first census found on 2026-09-23 that are two real venues programmed alike
  // (the real duplicates — Koło repeating Konin, Syracuse IN repeating Park Ridge IL — were mis-wired
  // feeds and are fixed at the source), in whichever scope each would have counted.
  it should "not count the chain-mates prod's first census found, in either scope" in {
    val found = for {
      pair    <- services.cinemas.roster.DistinctVenuePairs.all.toSeq
      a       <- pair.headOption
      b       <- pair.lastOption
      country <- Country.all.find(c => citiesOf(c, a).nonEmpty && citiesOf(c, b).nonEmpty)
      scope    = if ((citiesOf(country, a) intersect citiesOf(country, b)).nonEmpty) DuplicateVenueCensus.SameCity else DuplicateVenueCensus.CrossCity
    } yield s"${a.displayName} / ${b.displayName}" ->
      census(Seq(row("Foo", a -> times(20), b -> times(20))), country = country, scope = scope)
    found.map(_._1) should contain allOf ("Cineplex Friedrichshafen / Cineplex Singen",
      "Cines Victoria Don Benito / Cines Victoria Mérida", "Columbia St Helens / Mt Hood Theatre Gresham",
      "Flora Cinema Helston / Royal St Ives Cinema")
    found.map(_._1).exists(_.contains("RMC Jacksonville")) shouldBe true
    withClue(found.filter(_._2 != 0.0).mkString("\n")) { all(found.map(_._2)) shouldBe 0.0 }
  }

  // The US pairs the census held from 2026-09-24 00:31Z (DuplicateVenueListing{us} pending in
  // both scopes): Caribbean Cinemas' Puerto Rico houses, which the roster files under one metro
  // city and the chain programmes alike island-wide, rotating which pairs cross the bar — each
  // books through its own home.caribbeancinemas.com/<venue>/checkout — and Auburn/Canandaigua NY,
  // formovietickets chain rochester rtn 104446 vs 346993. Each books through its own links.
  it should "not count two houses of a chain that programmes all of them alike, nor Auburn and Canandaigua" in {
    val counted = countedPairs(Country.UnitedStates, Seq(
      "Caribbean Cinemas Plaza Escorial" -> "Caribbean Cinemas Plaza Guayama",
      "Caribbean Cinemas Distrito VIP Cinemas" -> "Caribbean Cinemas Las Piedras",
      "Caribbean Cinemas Plaza Cayey" -> "Caribbean Cinemas The Outlet 66",
      "Caribbean Cinemas Arecibo" -> "Caribbean Cinemas Western Plaza",
      "Caribbean Cinemas Aguadilla Mall" -> "Caribbean Cinemas Metro",
      "Auburn Movieplex" -> "Canandaigua Theaters"),
      name => Some(session(Slugify.stable(name))))
    withClue(counted.mkString("\n")) { counted shouldBe empty }
  }

  // The pairs pending from 2026-09-24, each two real houses on one programme, checked against the
  // upstream: Phoenix Theatres, which programmes every house alike -- first Clarksville TN and
  // Monroe MI (cross-city, 09-24), then Livonia and Monroe MI (same-city, 09-25; Flicks venues
  // phoenix-theatres-laurel-park vs -mall-of-monroe, 615 vs 571 showtimes of the same six films);
  // Dersa's Damme house and Kinocenter Rahden, 90 km apart (Filmstarts theatres A0438 vs A1730);
  // and two small-town twins 500 miles apart, Castle Twin in East Jamestown TN and Cinema
  // Laurinburg NC (Flicks castle-twin-jamestown vs cinema-laurinburg, 21 vs 22 showtimes of the
  // same three wide releases on different day calendars, checked 2026-09-25). Phoenix (cinemacode
  // 009 / 001 / 002) and Dersa/Rahden book through their own links; Laurinburg carries none, so
  // that pair stays on the allowlist.
  it should "not count two Phoenix Theatres houses, Dersa and Kinocenter Rahden, nor Castle Twin and Cinema Laurinburg" in {
    val ownLinks = (name: String) => Some(session(Slugify.stable(name)))
    val counted =
      countedPairs(Country.UnitedStates, Seq(
        "Phoenix Theatres Governors Square" -> "Phoenix Theatres Mall of Monroe",
        "Phoenix Theatres Laurel Park" -> "Phoenix Theatres Mall of Monroe",
        "Phoenix Theatres Danville 8" -> "Phoenix Theatres New Albany 16"), ownLinks) ++
        countedPairs(Country.UnitedStates, Seq("Castle Twin Jamestown" -> "Cinema Laurinburg")) ++
        countedPairs(Country.Germany, Seq("Dersa Kino-Center" -> "Kinocenter Rahden"), ownLinks)
    withClue(counted.mkString("\n")) { counted shouldBe empty }
  }

  // DuplicateVenueListing{uk, same_city} pending from 2026-09-25 18:49Z: Cineworld Ely and St
  // Neots, both filed under Cambridgeshire, sharing 363 of ~400 showtimes -- two houses booking
  // through their own session ids (web.cineworld.co.uk/order/showtimes/097-* vs 084-*) on their
  // own screens. Cineworld programmes its whole estate from one template since the 2026-09-17
  // relaunch: the archive held 57 pairs of its 87 venues at 75%+ that day, Shrewsbury and
  // Weston-super-Mare at 94.9%, a hair under the cross-city bar.
  it should "not count two Cineworld houses, which the chain programmes alike estate-wide" in {
    val counted = countedPairs(Country.UnitedKingdom, Seq(
      "Cineworld Ely" -> "Cineworld St Neots",
      "Cineworld Huntingdon" -> "Cineworld St Neots",
      "Cineworld Shrewsbury" -> "Cineworld Weston-super-Mare"),
      name => Some(session(Slugify.stable(name), host = "web.cineworld.co.uk")))
    withClue(counted.mkString("\n")) { counted shouldBe empty }
  }

  it should "list only pairs of two venues on one country's roster, so no entry is dead" in {
    val dead = services.cinemas.roster.DistinctVenuePairs.all.filterNot { p =>
      p.size == 2 && Country.all.exists(c => p.forall(v => citiesOf(c, v).nonEmpty))
    }
    withClue(dead.map(_.map(_.displayName)).mkString("\n")) { dead shouldBe empty }
  }

  // A name the generated rosters no longer hold must not throw when the worker loads the
  // list (it would take the census down on every tick); it is skipped, and named here.
  it should "skip a DistinctVenuePairs name no roster holds, instead of failing the load" in {
    val lookup = Map("Here" -> first, "Also here" -> second).get
    services.cinemas.roster.DistinctVenuePairs.resolve(Seq("Here" -> "Also here", "Here" -> "Renamed away"), lookup) shouldBe
      (Set(Set(first, second)), Seq("Renamed away"))
    services.cinemas.roster.DistinctVenuePairs.unresolved shouldBe empty
  }

  it should "publish nothing from a partial scan" in {
    census(Seq(row("Foo", first -> times(10), second -> times(10))), complete = false, preset = Some(3.0)) shouldBe 3.0
  }
}
