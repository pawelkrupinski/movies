package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{ExternalService, HttpOutcome}

class WorkerHttpMetricsSpec extends AnyFlatSpec with Matchers {

  private def render(r: PrometheusRegistry) = PrometheusExposition.render(r)

  private val Scrape = WorkerHttpMetrics.Phase.Scrape
  private val Enrich = WorkerHttpMetrics.Phase.Enrich

  "WorkerHttpMetrics" should "seed every country × phase × service × outcome at 0 so no Grafana line starts absent" in {
    val registry = new PrometheusRegistry()
    val _ = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val text = render(registry)
    // Every outcome exists for every country AND every (phase, service) pair from boot.
    for (o <- HttpOutcome.all; (p, s) <- WorkerHttpMetrics.Service.grid; c <- Seq("de", "pl"))
      text should include (s"""kinowo_worker_http_total{country="$c",outcome="$o",phase="$p",service="$s"} 0""")
  }

  it should "seed a bounded grid: the cinema sites and Telegram on scrape, each API and `other` on enrich" in {
    WorkerHttpMetrics.Service.grid.toSet shouldBe
      (Set(Scrape -> ExternalService.Cinema, Scrape -> ExternalService.Telegram, Enrich -> ExternalService.Other) ++
        ExternalService.apis.map(Enrich -> _))
  }

  it should "increment the right (country, phase, outcome) cell via its per-country recorder" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val deScrape = metrics.recorderFor("de", Scrape)

    deScrape.record(HttpOutcome.Http429)
    deScrape.record(HttpOutcome.Http429)
    deScrape.record(HttpOutcome.Success)

    val text = render(registry)
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_429",phase="scrape",service="cinema"} 2""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="success",phase="scrape",service="cinema"} 1""")
    // pl untouched — the recorder is bound to its own country's slice.
    text should include ("""kinowo_worker_http_total{country="pl",outcome="http_429",phase="scrape",service="cinema"} 0""")
  }

  it should "keep scrape and enrich attempts on separate phase series" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerHttpMetrics(Seq("de"), registry)
    metrics.recorderFor("de", Scrape).record(HttpOutcome.Http403)
    metrics.recorderFor("de", Enrich).record(HttpOutcome.Http404)

    val text = render(registry)
    // The whole point of the phase label: a cinema-site 403 and a metadata-API 404
    // land on different series so a per-phase Grafana panel can read each alone.
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_403",phase="scrape",service="cinema"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_404",phase="enrich",service="other"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_403",phase="enrich",service="other"} 0""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_404",phase="scrape",service="cinema"} 0""")
  }

  it should "label an enrich attempt with the external service its URL reaches" in {
    val registry = new PrometheusRegistry()
    val enrich   = new WorkerHttpMetrics(Seq("de"), registry).recorderFor("de", Enrich)
    // OMDb answers "Request limit reached!" as a 401 — it must land on omdb, not on TMDB or `other`.
    enrich.forRequest("https://www.omdbapi.com/?apikey=k&i=tt1").record(HttpOutcome.Http401)
    enrich.forRequest("https://api.themoviedb.org/3/movie/1").record(HttpOutcome.Http429)
    enrich.forRequest("https://unlisted.example/x").record(HttpOutcome.Success)

    val text = render(registry)
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_401",phase="enrich",service="omdb"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_429",phase="enrich",service="tmdb"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="success",phase="enrich",service="other"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_401",phase="enrich",service="tmdb"} 0""")
  }

  it should "label every scrape attempt `cinema` — Filmweb's showtimes scraper included — except Telegram" in {
    val registry = new PrometheusRegistry()
    val scrape   = new WorkerHttpMetrics(Seq("pl"), registry).recorderFor("pl", Scrape)
    scrape.forRequest("https://www.filmweb.pl/showtimes/Warszawa").record(HttpOutcome.Success)
    scrape.forRequest("https://www.cinema-city.pl/repertoire").record(HttpOutcome.Success)
    scrape.forRequest("https://api.telegram.org/bot1/sendMessage").record(HttpOutcome.Http401)

    val text = render(registry)
    text should include ("""kinowo_worker_http_total{country="pl",outcome="success",phase="scrape",service="cinema"} 2""")
    text should include ("""kinowo_worker_http_total{country="pl",outcome="http_401",phase="scrape",service="telegram"} 1""")
    text should not include ("""service="filmweb"} 1""")
  }
}
