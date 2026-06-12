package services.movies

import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleDefaults, TitleRuleSet}

/** Pins the fix for the late-added-merge-rule data-loss bug: when a
 *  GlobalStructural strip is added AFTER two docs were written under distinct
 *  keys, they now collide on `CacheKey` (equality is `sanitize`, which runs that
 *  tier). `rehydrate` must UNION the colliding rows, not last-write-wins drop one
 *  — otherwise a hydration silently loses one doc's showtimes until the next
 *  scrape (and on the read-only web app, the row briefly serves half its
 *  cinemas). Reproduces prod: seed two distinct rows under the defaults, install
 *  a `/Kino Cafe` strip so they collide, then hydrate. */
class CacheRehydrateUnionSpec extends AnyFlatSpec with Matchers {

  private def repoOf(rows: StoredMovieRecord*): MovieRepo = new MovieRepo {
    def enabled = true
    def findAll() = rows.toSeq
    def delete(t: String, y: Option[Int]) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def withInstalledRules(rs: TitleRuleSet)(body: => Unit): Unit = {
    val saved = TitleNormalizer.currentRules
    try { TitleNormalizer.installRules(rs); body }
    finally TitleNormalizer.installRules(saved)
  }

  private def row(title: String, cinema: Source): StoredMovieRecord =
    StoredMovieRecord(title, Some(2025),
      MovieRecord(data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = Some(2025)))))

  private val decorated = row("Takie jest życie/Kino Cafe", CinemaCityKinepolis)
  private val base      = row("Takie jest życie",           Multikino)

  // GlobalStructural strip that didn't exist when the rows were written; under it
  // both titles sanitise to the same key.
  private val kinoCafeRule = TitleRule("test-kino-cafe", RuleScope.GlobalStructural, None,
    """(?i)\s*/\s*Kino\s+Cafe\s*$""", "", applyAll = false, order = 100)

  "rehydrate" should "union two docs a late merge-key rule collides, not drop one" in {
    withInstalledRules(TitleRuleSet(TitleRuleDefaults.all :+ kinoCafeRule)) {
      val cache = new CaffeineMovieCache(repoOf(decorated, base))   // rehydrates at construction
      cache.entries should have size 1
      // The union keeps BOTH cinemas; the old last-write-wins kept only the
      // last-iterated row's slot.
      cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    }
  }

  it should "leave non-colliding rows as separate entries (no spurious union)" in {
    // Same two rows, but WITHOUT the /Kino Cafe rule they don't collide.
    withInstalledRules(TitleRuleDefaults.ruleSet) {
      val cache = new CaffeineMovieCache(repoOf(decorated, base))
      cache.entries should have size 2
    }
  }

  // The live duplicate-card / double-rating-run bug at its source: different
  // cinemas report ONE film under different years, both already TMDB-resolved to
  // the same id. They land under distinct `CacheKey`s, so rehydrate's raw put
  // can't fold them (the `put` identity gate is bypassed on hydrate). They'd then
  // sit duplicated — each independently enriched — until the periodic `settle`,
  // which the worker's restart loop keeps resetting. rehydrate must collapse them
  // on load so the duplicate never reaches the read model or the rating pipeline.
  private def resolvedRow(year: Int, cinema: Source): StoredMovieRecord =
    StoredMovieRecord("Kumotry", Some(year),
      MovieRecord(tmdbId = Some(777), data = Map[Source, SourceData](
        cinema -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(year)))))

  "rehydrate" should "collapse two same-tmdbId rows that differ only by year, on load" in {
    val cache = new CaffeineMovieCache(repoOf(
      resolvedRow(2025, Multikino), resolvedRow(2026, CinemaCityKinepolis)))
    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(Multikino, CinemaCityKinepolis)
  }
}
