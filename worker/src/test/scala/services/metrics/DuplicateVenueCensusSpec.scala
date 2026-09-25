package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, City, Country, KinoMikro, MikroBronowice, MovieRecord, Source}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

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
 * exactly among a crowd; a tiny programme cannot match by coincidence; the shared allowlist
 * clears every known pair in its scope; and a partial scan publishes nothing.
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
   *  pair lists one identical 20-showtime programme -- in the scope the roster puts them in. */
  private def countedPairs(country: Country, pairs: Seq[(String, String)]): Seq[String] =
    pairs.flatMap { case (a, b) =>
      val (x, y) = (Cinema.byDisplayName(a), Cinema.byDisplayName(b))
      val scope =
        if ((citiesOf(country, x) intersect citiesOf(country, y)).nonEmpty) DuplicateVenueCensus.SameCity
        else DuplicateVenueCensus.CrossCity
      val counted = census(Seq(row("Foo", x -> times(20), y -> times(20))), country = country, scope = scope)
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
  // formovietickets chain rochester rtn 104446 vs 346993.
  it should "not count two houses of a chain that programmes all of them alike, nor Auburn and Canandaigua" in {
    val counted = countedPairs(Country.UnitedStates, Seq(
      "Caribbean Cinemas Plaza Escorial" -> "Caribbean Cinemas Plaza Guayama",
      "Caribbean Cinemas Distrito VIP Cinemas" -> "Caribbean Cinemas Las Piedras",
      "Caribbean Cinemas Plaza Cayey" -> "Caribbean Cinemas The Outlet 66",
      "Caribbean Cinemas Arecibo" -> "Caribbean Cinemas Western Plaza",
      "Caribbean Cinemas Aguadilla Mall" -> "Caribbean Cinemas Metro",
      "Auburn Movieplex" -> "Canandaigua Theaters"))
    withClue(counted.mkString("\n")) { counted shouldBe empty }
  }

  // The pairs pending from 2026-09-24, each two real houses on one programme, checked against the
  // upstream: Phoenix Theatres, which programmes every house alike -- first Clarksville TN and
  // Monroe MI (cross-city, 09-24), then Livonia and Monroe MI (same-city, 09-25; Flicks venues
  // phoenix-theatres-laurel-park vs -mall-of-monroe, 615 vs 571 showtimes of the same six films);
  // Dersa's Damme house and Kinocenter Rahden, 90 km apart (Filmstarts theatres A0438 vs A1730);
  // and two small-town twins 500 miles apart, Castle Twin in East Jamestown TN and Cinema
  // Laurinburg NC (Flicks castle-twin-jamestown vs cinema-laurinburg, 21 vs 22 showtimes of the
  // same three wide releases on different day calendars, checked 2026-09-25).
  it should "not count two Phoenix Theatres houses, Dersa and Kinocenter Rahden, nor Castle Twin and Cinema Laurinburg" in {
    val counted =
      countedPairs(Country.UnitedStates, Seq(
        "Phoenix Theatres Governors Square" -> "Phoenix Theatres Mall of Monroe",
        "Phoenix Theatres Laurel Park" -> "Phoenix Theatres Mall of Monroe",
        "Phoenix Theatres Danville 8" -> "Phoenix Theatres New Albany 16",
        "Castle Twin Jamestown" -> "Cinema Laurinburg")) ++
        countedPairs(Country.Germany, Seq("Dersa Kino-Center" -> "Kinocenter Rahden"))
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
