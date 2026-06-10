package services.movies

import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Drives the rebuilder against a cache seeded into a STALE state: a Cinema City
 * row stored under its un-stripped key ("Ladies Night - Anora") next to a plain
 * "Anora" row — the shape you'd get if the rows were scraped before the
 * cinema-city rule existed. Under the (default) rules both raw titles now map to
 * the same key, so a rebuild must collapse them into one row. No global rule
 * mutation: the default rule set already strips "Ladies Night - ".
 */
class NormalizationRebuilderSpec extends AnyFlatSpec with Matchers {

  private val disabledRepo = new MovieRepo {
    def enabled = false
    def findAll() = Seq.empty
    def delete(title: String, year: Option[Int]) = ()
    def upsert(title: String, year: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def ccSlot   = SourceData(title = Some("Ladies Night - Anora"), rawTitle = Some("Ladies Night - Anora"), releaseYear = Some(2024))
  private def mkSlot   = SourceData(title = Some("Anora"),                rawTitle = Some("Anora"),                releaseYear = Some(2024))

  private def staleCache(): CaffeineMovieCache = {
    val cache = new CaffeineMovieCache(disabledRepo)
    // Row A: scraped before the rule — stored under the un-stripped key.
    cache.put(cache.keyOf("Ladies Night - Anora", Some(2024)),
      MovieRecord(data = Map[Source, SourceData](CinemaCityKinepolis -> ccSlot)))
    // Row B: the plain title.
    cache.put(cache.keyOf("Anora", Some(2024)),
      MovieRecord(data = Map[Source, SourceData](Multikino -> mkSlot)))
    cache
  }

  "rebuild" should "collapse the stale decorated row onto the plain one (merge)" in {
    val cache = staleCache()
    cache.entries should have size 2

    val result = new NormalizationRebuilder(cache).rebuild()

    cache.entries should have size 1
    val merged = cache.entries.head._2
    merged.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    result.merges should have size 1
    result.merges.head.mergedTitles should contain allOf ("Anora", "Ladies Night - Anora")
    result.splits shouldBe empty
  }

  it should "be a no-op on an already-consistent cache" in {
    val cache = staleCache()
    new NormalizationRebuilder(cache).rebuild()        // first run collapses
    val second = new NormalizationRebuilder(cache).rebuild()
    second.merges shouldBe empty
    second.splits shouldBe empty
    second.changed shouldBe 0
    cache.entries should have size 1
  }

  "rebuild" should "split a row whose cinema slots now map to different keys (un-merge)" in {
    val cache = new CaffeineMovieCache(disabledRepo)
    // A single row that bundles two genuinely different films (the state left
    // behind after a too-broad rule that merged them is deleted), enriched.
    val bundled = MovieRecord(
      tmdbId = Some(111),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Anora"),      rawTitle = Some("Anora")),
        Multikino           -> SourceData(title = Some("Other Film"), rawTitle = Some("Other Film"))))
    cache.put(cache.keyOf("Anora", None), bundled)
    cache.entries should have size 1

    var splitOffs = List.empty[String]
    val result = new NormalizationRebuilder(cache, onSplitOff = (title, _) => splitOffs ::= title).rebuild()

    cache.entries should have size 2
    val byTitle = cache.entries.map { case (k, r) => k.cleanTitle -> r }.toMap
    byTitle.keySet shouldBe Set("Anora", "Other Film")
    byTitle("Anora").tmdbId shouldBe Some(111)   // remnant keeps the enrichment
    byTitle("Other Film").tmdbId shouldBe None    // split-off is fresh
    result.splits should have size 1
    result.splits.head.into should contain allOf ("Anora", "Other Film")
    splitOffs should contain ("Other Film")       // handed off for re-resolution
  }
}
