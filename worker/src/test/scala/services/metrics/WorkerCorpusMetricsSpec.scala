package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerCorpusMetrics.{CorpusCounts, Subset}
import services.movies.{MovieRepository, StoredMovieRecord}

/**
 * Locks the corpus census the worker exposes for the Grafana "corpus coverage"
 * chart: total records, the any-rating / tmdb-id / imdb-id populations, and the
 * four per-source rating counts (imdb/rt/mc/fw) — all carried on one labelled
 * `kinowo_worker_corpus_movies{subset=…}` gauge.
 */
class WorkerCorpusMetricsSpec extends AnyFlatSpec with Matchers {

  private def row(record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord("A Film", Some(2025), record)

  private def repositoryOf(records: MovieRecord*): MovieRepository = new MovieRepository {
    def enabled = true
    def findAll() = records.map(row)
    def delete(t: String, y: Option[Int]) = ()
    def deleteById(id: String) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def render(registry: PrometheusRegistry): String = PrometheusExposition.render(registry)

  private def gauge(text: String, subset: String): Option[Double] =
    PrometheusExposition.sample(text, WorkerCorpusMetrics.Name, s"""subset="$subset"""")

  // A mix exercising every subset: each record opts into a distinct combination.
  private val corpus = Seq(
    MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(7.0)),
    MovieRecord(tmdbId = Some(2), rottenTomatoes = Some(90)),
    MovieRecord(tmdbId = Some(3), metascore = Some(80), filmwebRating = Some(8.1)),
    MovieRecord(imdbId = Some("tt4")),                       // id but no rating
    MovieRecord()                                            // bare: counts only toward total
  )

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
    val metrics  = new WorkerCorpusMetrics(repositoryOf(corpus*), registry)

    metrics.sample()
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
    new WorkerCorpusMetrics(repositoryOf(), registry) // constructed, not yet sampled
    val text = render(registry)

    Subset.all.foreach(s => gauge(text, s) shouldBe Some(0.0))
  }
}
