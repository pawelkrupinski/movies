package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures.{repositoryOf, row}
import services.metrics.WorkerCorpusMetrics.{CorpusCounts, Subset}

/**
 * Locks the corpus census the worker exposes for the Grafana "corpus coverage"
 * chart: total records, the any-rating / tmdb-id / imdb-id populations, and the
 * four per-source rating counts (imdb/rt/mc/fw) — all carried on one labelled
 * `kinowo_worker_corpus_movies{subset=…}` gauge.
 */
class WorkerCorpusMetricsSpec extends AnyFlatSpec with Matchers {

  // Distinct titles: the in-memory store keys rows by `sanitize(title)|year`, so
  // same-titled rows would collapse into one.
  private def rows(records: Seq[MovieRecord]) =
    records.zipWithIndex.map { case (r, i) => row(s"A Film $i", r) }

  private def render(registry: PrometheusRegistry): String = PrometheusExposition.render(registry)

  private def gauge(text: String, subset: String): Option[Double] =
    PrometheusExposition.sample(text, WorkerCorpusMetrics.Name, s"""country="pl",subset="$subset"""")

  // A mix exercising every subset: each record opts into a distinct combination.
  private val corpus = Seq(
    MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(7.0)),
    MovieRecord(tmdbId = Some(2), rottenTomatoes = Some(90)),
    MovieRecord(tmdbId = Some(3), metascore = Some(80), filmwebRating = Some(8.1)),
    MovieRecord(imdbId = Some("tt4")),                       // id but no rating
    MovieRecord()                                            // bare: counts only toward total
  )

  // The population that was invisible: rows resolved to a film their own cinemas
  // contradict. Five sat in prod undetected until a hand-written scan found them,
  // so the point of the series is that nobody has to go looking again.
  "CorpusCounts" should "count rows whose cinemas contradict the film they resolved to" in {
    val misresolved = models.MovieRecord(
      tmdbId = Some(1667002),
      data = Map[models.Source, models.SourceData](
        models.Tmdb -> models.SourceData(title = Some("STABAT MATER RV621"), runtimeMinutes = Some(18)),
        models.KinoApollo -> models.SourceData(title = Some("Vivaldi i ja"), runtimeMinutes = Some(110))))
    val corroborated = models.MovieRecord(
      tmdbId = Some(1321666),
      data = Map[models.Source, models.SourceData](
        models.Tmdb -> models.SourceData(title = Some("Lalka"), runtimeMinutes = Some(162)),
        models.KinoApollo -> models.SourceData(title = Some("Lalka"), runtimeMinutes = Some(147))))

    val c = CorpusCounts.from(Seq(misresolved, corroborated))
    c.bySubset.toMap.apply(Subset.Misresolved) shouldBe 1
    c.total shouldBe 2
  }

  "CorpusCounts" should "tally each subset independently" in {
    val c = CorpusCounts.from(corpus)
    c.total         shouldBe 5
    c.withTmdbId    shouldBe 3
    c.withImdbId    shouldBe 2
    c.imdbRating    shouldBe 1
    c.rtRating      shouldBe 1
    c.mcRating      shouldBe 1
    c.fwRating      shouldBe 1
    c.withAnyRating shouldBe 3 // three records carry at least one of imdb/rt/mc/fw
  }

  "an empty corpus" should "count zero everywhere" in {
    CorpusCounts.from(Nil) shouldBe CorpusCounts.empty
  }

  "WorkerCorpusMetrics.sample" should "publish every subset onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl")

    new WorkerCorpusScan(repositoryOf(rows(corpus)*), Seq(metrics)).sample()
    val text = render(registry)

    gauge(text, Subset.Total)         shouldBe Some(5.0)
    gauge(text, Subset.WithTmdbId)    shouldBe Some(3.0)
    gauge(text, Subset.WithImdbId)    shouldBe Some(2.0)
    gauge(text, Subset.WithAnyRating) shouldBe Some(3.0)
    gauge(text, Subset.ImdbRating)    shouldBe Some(1.0)
    gauge(text, Subset.RtRating)      shouldBe Some(1.0)
    gauge(text, Subset.McRating)      shouldBe Some(1.0)
    gauge(text, Subset.FwRating)      shouldBe Some(1.0)
  }

  it should "materialize every subset series at 0 before the first sample" in {
    val registry = new PrometheusRegistry()
    new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl") // constructed, not yet sampled
    val text = render(registry)

    Subset.all.foreach(s => gauge(text, s) shouldBe Some(0.0))
  }
}
