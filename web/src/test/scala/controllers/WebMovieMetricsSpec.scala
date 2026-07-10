package controllers

import models.{Helios, HeliosMagnolia, KinoApollo, MovieRecord, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.{Clock, LocalDateTime, ZoneId}

/**
 * Locks the per-city served-film gauges appended to `/metrics`. The counts are
 * the real serving join ([[MovieControllerService.toSchedules]]) so the metric
 * can't drift from what `/:city/` renders. The load-bearing behaviour: the
 * `all` vs `tomorrow` scope split, that only future showings count, and that a
 * city with no films emits an explicit `0` (a vanished series would defeat the
 * swing/floor Grafana alerts).
 */
class WebMovieMetricsSpec extends AnyFlatSpec with Matchers {

  private val warsaw = ZoneId.of("Europe/Warsaw")
  // Fixed "now": 2026-06-08 12:00 Warsaw → tomorrow is 2026-06-09.
  private val now    = LocalDateTime.of(2026, 6, 8, 12, 0)
  private val clock  = Clock.fixed(now.atZone(warsaw).toInstant, warsaw)

  private def slot(times: LocalDateTime*): SourceData =
    SourceData(title = Some("x"), showtimes = times.map(t => Showtime(t, bookingUrl = None)))

  private val today    = LocalDateTime.of(2026, 6, 8, 18, 0)
  private val tomorrow = LocalDateTime.of(2026, 6, 9, 18, 0)
  private val past     = LocalDateTime.of(2026, 6, 8, 9, 0) // before now − 30min, must drop out

  private def metrics = {
    val records = Seq(
      // Poznań: one film today+tomorrow, one today-only, one only in the past.
      ("Today And Tomorrow", Some(2026), MovieRecord(data = Map[Source, SourceData](Helios     -> slot(today, tomorrow)))),
      ("Today Only",         Some(2026), MovieRecord(data = Map[Source, SourceData](KinoApollo -> slot(today)))),
      ("Past Only",          Some(2026), MovieRecord(data = Map[Source, SourceData](Rialto     -> slot(past)))),
      // Wrocław: one film showing only tomorrow.
      ("Wroclaw Tomorrow",   Some(2026), MovieRecord(data = Map[Source, SourceData](HeliosMagnolia -> slot(tomorrow)))),
    )
    val service = new MovieControllerService(TestReadModel.fromRecords(records))
    new WebMovieMetrics(service, clock = clock)
  }

  "sample" should "count distinct served films per city, by scope" in {
    val m = metrics
    m.sample()
    val out = m.render()

    // Poznań: 2 films with a future showing (the past-only one drops out); 1 of
    // them shows tomorrow.
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"poznan\",scope=\"all\"} 2")
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"poznan\",scope=\"tomorrow\"} 1")
    // Wrocław: 1 film, showing tomorrow.
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"wroclaw\",scope=\"all\"} 1")
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"wroclaw\",scope=\"tomorrow\"} 1")
  }

  it should "emit an explicit 0 for a city with no films (so a drop-to-zero is a sample, not an absence)" in {
    val m = metrics
    m.sample()
    val out = m.render()

    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"krakow\",scope=\"all\"} 0")
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"krakow\",scope=\"tomorrow\"} 0")
  }

  "render" should "emit one HELP and TYPE header and be sorted by city slug" in {
    val out = WebMovieMetrics.render(Seq(
      WebMovieMetrics.CityCounts("wroclaw", all = 5, tomorrow = 2),
      WebMovieMetrics.CityCounts("krakow",  all = 3, tomorrow = 1),
    ), "pl")

    out should include ("# TYPE kinowo_web_movies_served gauge")
    out should include ("# HELP kinowo_web_movies_served")
    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"krakow\",scope=\"all\"} 3")
    out.indexOf("city=\"krakow\"") should be < out.indexOf("city=\"wroclaw\"")
  }

  it should "seed every city at zero before the first sample" in {
    // No sample() call — the gauge must still expose every city at 0 rather than
    // an empty body, so a scrape during boot reads zeros not missing series.
    val out = metrics.render()

    out should include ("kinowo_web_movies_served{country=\"pl\",city=\"poznan\",scope=\"all\"} 0")
  }
}
