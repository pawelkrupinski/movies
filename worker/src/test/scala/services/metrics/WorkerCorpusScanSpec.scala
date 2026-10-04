package services.metrics

import services.movies.SingleCountryNormalizer

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Helios, HeliosMagnolia}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures._
import services.metrics.WorkerCorpusMetrics.Subset
import services.metrics.WorkerSourceFilmsMetrics.Scope
import services.movies.{IncompleteScanMovieRepository, InMemoryMovieRepository, StoredMovieRecord}

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
    extends InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record)), normalizer = services.movies.SingleCountryNormalizer.titleNormalizer) {
    val scans = new AtomicInteger(0)

    override def foreachRecord(f: StoredMovieRecord => Unit): tools.ScanOutcome = {
      scans.incrementAndGet(); super.foreachRecord(f)
    }

    override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): tools.ScanOutcome = {
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

    val corpus    = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock = _root_.tools.SpecClock.Pinned)
    val films     = new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)

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

  // The boot hydrate has just read every film whole; the census's first pass used to read them all
  // again two minutes later (12.5 s on worker-us, 2026-10-04). It now takes the hydrate's read.
  it should "make its boot pass over the hydrate's read, scanning nothing itself" in {
    import org.scalatest.concurrent.Eventually.{eventually, timeout}
    val repository = new CountingRepository(rows)
    val registry   = new PrometheusRegistry()
    val boot = new BootCensus(Seq(
      new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock = _root_.tools.SpecClock.Pinned),
      new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)))

    boot.bootCorpus(Some(repository.findAll()))
    eventually(timeout(_root_.tools.SpecTimeouts.Settle)) {
      gauge(PrometheusExposition.render(registry), WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""") shouldBe Some(2.0)
    }
    gauge(PrometheusExposition.render(registry), WorkerCorpusMetrics.Name, s"""country="pl",subset="${Subset.Total}"""") shouldBe Some(2.0)
    repository.scans.get() shouldBe 0
    boot.passTaken shouldBe true
  }

  // No complete boot read: nothing published from it, and the schedule's first tick reads as before.
  it should "publish nothing when the boot hydrate had no complete read to hand over" in {
    val registry = new PrometheusRegistry()
    val boot = new BootCensus(Seq(new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock = _root_.tools.SpecClock.Pinned)))
    boot.bootCorpus(None)
    boot.passTaken shouldBe false
    gauge(PrometheusExposition.render(registry), WorkerCorpusMetrics.Name, s"""country="pl",subset="${Subset.Total}"""") shouldBe None
  }

  it should "run one scan per tick, not one per collector, on every subsequent sample" in {
    val repository = new CountingRepository(rows)
    val registry   = new PrometheusRegistry()
    val scan = new WorkerCorpusScan(repository, Seq(
      new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock = _root_.tools.SpecClock.Pinned),
      new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer),
      new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)))

    scan.sample()
    scan.sample()
    scan.sample()

    repository.scans.get() shouldBe 3
  }

  // Each tick gets a FRESH accumulator, so a gauge can never read as the sum of every
  // tick so far — the failure mode of hoisting the tally onto the collector.
  it should "publish the same values on a repeated sample, not accumulate across ticks" in {
    val registry  = new PrometheusRegistry()
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)
    val scan      = new WorkerCorpusScan(new CountingRepository(rows), Seq(showtimes))

    scan.sample()
    scan.sample()

    val text = PrometheusExposition.render(registry)
    gauge(text, WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""") shouldBe Some(2.0)
  }

  // These three used to publish whatever a partial scan had counted, and this spec used
  // to PIN that ("still publish on an INCOMPLETE scan"). Prod settled the argument on
  // 2026-07-27: the `Missing field: sourceData` decode bug failed every batch, so the
  // censuses published 0 for ~50 minutes while all three corpora sat intact — the panel
  // read as "the corpus is gone", and `kinowo-showtime-volume-collapsed` pages on exactly
  // that shape. A partial census is fewer rows READ, not a smaller corpus, and as a gauge
  // value the two cannot be told apart. So it publishes nothing and says so instead.
  it should "keep the last complete values when a scan falls short, not publish a partial count" in {
    val registry  = new PrometheusRegistry()
    val showtimes = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)

    new WorkerCorpusScan(repositoryOf(rows*), Seq(showtimes)).sample()
    gauge(PrometheusExposition.render(registry), WorkerShowtimesMetrics.Name,
      s"""city="poznan",country="pl"""") shouldBe Some(2.0)

    // A pass that fails BEFORE reaching Poznań's row still delivers Wrocław's. Publishing
    // that would drop Poznań 2 → 0: a total city outage, from a read that simply stopped.
    new WorkerCorpusScan(new IncompleteScanMovieRepository(rows.drop(1).map(r => (r.title, r.year, r.record)), titleNormalizer = SingleCountryNormalizer.titleNormalizer),
      Seq(showtimes)).sample()

    val after = PrometheusExposition.render(registry)
    gauge(after, WorkerShowtimesMetrics.Name, s"""city="poznan",country="pl"""")  shouldBe Some(2.0)
    gauge(after, WorkerShowtimesMetrics.Name, s"""city="wroclaw",country="pl"""") shouldBe Some(1.0)
  }

  // The exact prod shape: EVERY batch fails, so the scan delivers no rows at all. Publishing
  // that reads as a total corpus wipe.
  it should "not publish a zero census when the scan reads nothing at all" in {
    val registry = new PrometheusRegistry()
    val corpus   = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock = _root_.tools.SpecClock.Pinned)

    new WorkerCorpusScan(repositoryOf(rows*), Seq(corpus)).sample()
    gauge(PrometheusExposition.render(registry), WorkerCorpusMetrics.Name,
      s"""country="pl",subset="${Subset.Total}"""") shouldBe Some(2.0)

    new WorkerCorpusScan(new IncompleteScanMovieRepository(titleNormalizer = SingleCountryNormalizer.titleNormalizer), Seq(corpus)).sample()

    gauge(PrometheusExposition.render(registry), WorkerCorpusMetrics.Name,
      s"""country="pl",subset="${Subset.Total}"""") shouldBe Some(2.0)
  }

  // Skipping the publish leaves the gauges frozen at plausible numbers, so a census that is
  // genuinely stuck would be invisible without this. The counter is the only signal that
  // says "these gauges have stopped meaning anything".
  it should "count an incomplete pass so a stuck census can't hide behind frozen gauges" in {
    val registry = new PrometheusRegistry()
    val counter  = WorkerCorpusScan.incompleteCounter(registry)
    val scan     = new WorkerCorpusScan(new IncompleteScanMovieRepository(titleNormalizer = SingleCountryNormalizer.titleNormalizer), Seq.empty,
      metrics = CorpusScanMetrics.prometheus(counter, "pl"))

    scan.sample()
    scan.sample()

    PrometheusExposition.sample(PrometheusExposition.render(registry),
      WorkerCorpusScan.IncompleteMetricName, """country="pl"""") shouldBe Some(2.0)
  }

  it should "not count a complete pass" in {
    val registry = new PrometheusRegistry()
    val counter  = WorkerCorpusScan.incompleteCounter(registry)

    new WorkerCorpusScan(repositoryOf(rows*), Seq.empty,
      metrics = CorpusScanMetrics.prometheus(counter, "pl")).sample()

    PrometheusExposition.sample(PrometheusExposition.render(registry),
      WorkerCorpusScan.IncompleteMetricName, """country="pl"""") shouldBe Some(0.0)
  }

  // Which collector a US pass's time goes to was invisible (2026-09-30: ~12 s of a 25–31 s
  // pass unaccounted for past the read), so a pass reports each collector's share.
  it should "report each collector's share of a pass, its publish included" in {
    var now = 0L
    val stopwatch = new tools.Stopwatch(() => now)
    final class Costing(perRow: Long, onPublish: Long) extends CorpusMetricsCollector {
      def startSample(): CorpusRowSampler = new CorpusRowSampler {
        def accept(row: CorpusRow): Unit            = now += perRow
        def publish(scanComplete: Boolean): Unit   = now += onPublish
      }
    }
    val pass = new WorkerCorpusScan(repositoryOf(rows*), Seq(new Costing(1_000_000, 0), new Costing(10_000_000, 5_000_000)),
      stopwatch = stopwatch).sample()

    pass.byCollector.map(_._2.toMillis) shouldBe Seq(2L, 25L)   // two rows each, then the publish
    pass.byCollector.map(_._1).distinct shouldBe Seq("Costing")
    pass.summary should startWith("worker-corpus-scan: pass took 27ms — Costing 25ms, Costing 2ms")
  }

  // Two collectors read every row's projection (films served, upcoming showtimes), and each used to
  // build it itself — the pass's costliest step, twice over. The scan hands them one row, projected once.
  it should "project each row once however many collectors read its screenings" in {
    val seen       = scala.collection.mutable.ArrayBuffer.empty[AnyRef]
    val normalizer = SingleCountryNormalizer.titleNormalizer   // one per country, as the wiring hands every collector
    final class Reading extends CorpusMetricsCollector {
      def startSample(): CorpusRowSampler = new CorpusRowSampler {
        def accept(row: CorpusRow): Unit         = row.venues(normalizer).foreach(seen += _)
        def publish(scanComplete: Boolean): Unit = ()
      }
    }
    new WorkerCorpusScan(repositoryOf(rows*), Seq(new Reading, new Reading)).sample()
    seen should have size (rows.size * 2)
    seen.grouped(2).foreach(pair => (pair(0) eq pair(1)) shouldBe true)
  }
}
