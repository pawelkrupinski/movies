package services.movies

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.costs.{CostScaling, Work}

/**
 * The unscreened cleanup asks the durable store about its CANDIDATES only — never once per
 * row of the corpus it walks. It runs over the whole cache, so a per-row read there is a
 * full-corpus round trip per film, every pass.
 */
class UnscreenedCleanupCostSpec extends AnyFlatSpec with Matchers {

  private def row(title: String, screened: Boolean): (String, Option[Int], MovieRecord) =
    (title, Some(2026), MovieRecord(imdbId = Some(s"tt-$title"),
      data = if (screened) Map[Source, SourceData](Helios -> SourceData(title = Some(title))) else Map.empty))

  /** Rows the cleanup reads from the store, over `corpus` screened films and three that are not. */
  private def storeReads(corpus: Int): Long = {
    val repository = new InMemoryMovieRepository(
      (1 to corpus).map(n => row(s"Screened $n", screened = true)) ++ (1 to 3).map(n => row(s"Gone $n", screened = false)),
      normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val work  = new Work
    new UnscreenedCleanup(cache, Work.counting(classOf[MovieRepository], repository, work)).removeUnscreened() shouldBe 3
    work.reads
  }

  "removeUnscreened" should "read the store for its candidates only, whatever the corpus holds" in
    CostScaling.assertIndependent("store rows the unscreened cleanup reads", n = 25)(storeReads)
}
