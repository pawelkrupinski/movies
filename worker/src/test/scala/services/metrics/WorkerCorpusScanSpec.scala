package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Helios, HeliosMagnolia}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures._
import services.metrics.WorkerCorpusMetrics.Subset
import services.metrics.WorkerSourceFilmsMetrics.Scope
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}

import java.util.concurrent.atomic.AtomicInteger

/**
 * The load-bearing property of [[WorkerCorpusScan]]: the three corpus censuses cost
 * exactly ONE corpus scan per cycle, not one each. Each used to run its own 5-min timer
 * over the same rows — measured on prod (2026-07-18) at 14,704 documents per 5 min for
 * Poland alone, per country — so this spec counts the scans and locks all three gauges'
 * values to prove the sharing didn't change what they publish.
 */
class WorkerCorpusScanSpec extends AnyFlatSpec with Matchers {

  /** Counts every full-corpus traversal, whichever variant a collector reaches for. */
  private class CountingRepository(rows: Seq[StoredMovieRecord])
    extends InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record))) {
    val scans = new AtomicInteger(0)

    override def foreachRecord(f: StoredMovieRecord => Unit): Boolean = {
      scans.incrementAndGet(); super.foreachRecord(f)
    }

    override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): Boolean = {
      scans.incrementAndGet(); super.foreachRecordWithoutShowtimes(f)
    }
  }

  // Poznań: one ready film, 2 upcoming slots (today + tomorrow), rated on IMDb.
  // Wrocław: one ready film, 1 upcoming slot (tomorrow), no ids beyond TMDB.
  private val rows = Seq(
    row("Today And Tomorrow", ready(Helios, 1, today, tomorrow)
      .copy(imdbId = Some("tt1"), imdbRating = Some(7.0))),
    row("Wroclaw Tomorrow",   ready(HeliosMagnolia, 2, tomorrow))
  )

  private def gauge(text: String, name: String, labels: String): Option[Double] =
    PrometheusExposition.sample(text, name, labels)

  "the three corpus censuses" should "share ONE corpus scan per cycle, publishing the same values" in {
    val repository = new CountingRepository(rows)
    val registry   = new PrometheusRegistry()

    val corpus    = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl")
    val films     = new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock)
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock)

    new WorkerCorpusScan(repository, Seq(corpus, films, showtimes)).sample()

    // The point of the change: three censuses, one traversal (was one each).
    repository.scans.get() shouldBe 1

    val text = PrometheusExposition.render(registry)
    // Corpus census — unchanged by riding the stitched scan.
    gauge(text, WorkerCorpusMetrics.Name, s"""country="pl",subset="${Subset.Total}"""")      shouldBe Some(2.0)
    gauge(text, WorkerCorpusMetrics.Name, s"""country="pl",subset="${Subset.ImdbRating}"""") shouldBe Some(1.0)
    // Films served, per city and scope.
    gauge(text, WorkerSourceFilmsMetrics.Name, s"""city="poznan",country="pl",scope="${Scope.All}"""")  shouldBe Some(1.0)
    gauge(text, WorkerSourceFilmsMetrics.Name, s"""city="wroclaw",country="pl",scope="${Scope.All}"""") shouldBe Some(1.0)
    // Individual upcoming slots, per city — 2 in Poznań off the SAME rows.
    gauge(text, WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""")  shouldBe Some(2.0)
    gauge(text, WorkerShowtimesMetrics.Name, s"""city="wroclaw",country="pl"""") shouldBe Some(1.0)
  }

  it should "run one scan per tick, not one per collector, on every subsequent sample" in {
    val repository = new CountingRepository(rows)
    val registry   = new PrometheusRegistry()
    val scan = new WorkerCorpusScan(repository, Seq(
      new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl"),
      new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock),
      new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock)))

    scan.sample()
    scan.sample()
    scan.sample()

    repository.scans.get() shouldBe 3
  }

  // Each tick gets a FRESH accumulator, so a gauge can never read as the sum of every
  // tick so far — the failure mode of hoisting the tally onto the collector.
  it should "publish the same values on a repeated sample, not accumulate across ticks" in {
    val registry  = new PrometheusRegistry()
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock)
    val scan      = new WorkerCorpusScan(new CountingRepository(rows), Seq(showtimes))

    scan.sample()
    scan.sample()

    val text = PrometheusExposition.render(registry)
    gauge(text, WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""") shouldBe Some(2.0)
  }

  // Pre-extraction each sampler ignored its scan's completeness boolean and published
  // whatever it had counted; sharing the scan must not silently change that.
  it should "still publish on an INCOMPLETE scan, as each sampler did on its own scan" in {
    val registry  = new PrometheusRegistry()
    val partial = new InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record))) {
      override def foreachRecord(f: StoredMovieRecord => Unit): Boolean = { super.foreachRecord(f); false }
    }
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock)

    new WorkerCorpusScan(partial, Seq(showtimes)).sample()

    val text = PrometheusExposition.render(registry)
    gauge(text, WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""") shouldBe Some(2.0)
  }
}
