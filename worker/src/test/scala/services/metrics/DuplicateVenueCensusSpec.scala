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
 * Pinned: a same-city pair at 90% overlap counts, one below does not; a pair in two cities never
 * does (a chain's national programme is not one screen); a tiny programme cannot match by
 * coincidence; the shared allowlist clears a known pair; and a partial scan publishes nothing.
 */
class DuplicateVenueCensusSpec extends AnyFlatSpec with Matchers {
  import CorpusMetricsFixtures.{clock, now, slot}

  private val city: City = Country.Poland.cities.find(_.cinemas.distinct.size >= 3).get
  private val Seq(first, second, third) = city.cinemas.distinct.take(3)
  private val elsewhere: Cinema =
    Country.Poland.cities.filterNot(_.cinemas.contains(first)).flatMap(_.cinemas).find(x => !city.cinemas.contains(x)).get

  /** `n` upcoming evening showtimes of one film, a day apart. */
  private def times(n: Int, from: Int = 1): Seq[LocalDateTime] = (from until from + n).map(d => now.plusDays(d.toLong))

  private def row(title: String, slots: (Source, Seq[LocalDateTime])*): StoredMovieRecord =
    StoredMovieRecord(title, Some(2026), MovieRecord(tmdbId = Some(1), data = slots.map { case (s, t) => s -> slot(t*) }.toMap))

  private def census(rows: Seq[StoredMovieRecord], complete: Boolean = true, preset: Option[Double] = None,
                     country: Country = Country.Poland): Double = {
    val gauge  = DuplicateVenueCensus.gauge(new PrometheusRegistry())
    val census = new DuplicateVenueCensus(gauge, country, clock)
    preset.foreach(gauge.labelValues(country.code).set)
    val sampler = census.startSample()
    rows.foreach(sampler.accept)
    sampler.publish(complete)
    gauge.labelValues(country.code).get()
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

  // The pairs prod's first census found on 2026-09-23 that are two real venues programmed alike
  // (the one real duplicate, Koło repeating Konin, was a mis-wired scraper and is fixed there).
  it should "not count the chain-mates prod's first census found, in any country" in {
    val sameProgramme = for {
      country <- Country.all
      city    <- country.cities
      pair    <- services.cinemas.roster.DistinctVenuePairs.all.toSeq
      if pair.subsetOf(city.cinemas.toSet)
      a       <- pair.headOption
      b       <- pair.lastOption
    } yield (country, a, b)
    val found = sameProgramme.map { case (country, a, b) =>
      s"${a.displayName} / ${b.displayName}" -> census(Seq(row("Foo", a -> times(10), b -> times(10))), country = country)
    }
    found.map(_._1) should contain allOf ("Cineplex Friedrichshafen / Cineplex Singen",
      "Cines Victoria Don Benito / Cines Victoria Mérida", "Columbia St Helens / Mt Hood Theatre Gresham",
      "Flora Cinema Helston / Royal St Ives Cinema")
    withClue(found.filter(_._2 != 0.0).mkString("\n")) { all(found.map(_._2)) shouldBe 0.0 }
  }

  it should "list only pairs that share a city, so no entry is dead" in {
    val cities = Country.all.flatMap(_.cities).map(_.cinemas.toSet)
    val dead   = services.cinemas.roster.DistinctVenuePairs.all.filterNot(p => p.size == 2 && cities.exists(p.subsetOf))
    withClue(dead.map(_.map(_.displayName)).mkString("\n")) { dead shouldBe empty }
  }

  it should "publish nothing from a partial scan" in {
    census(Seq(row("Foo", first -> times(10), second -> times(10))), complete = false, preset = Some(3.0)) shouldBe 3.0
  }
}
