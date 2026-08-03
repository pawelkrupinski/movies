package services.movies

import services.movies.SingleCountryNormalizer.{titleNormalizer, given}

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The two remaining `stored()` reads that feed a WRITE, held to the rule the rest of the
 * codebase now follows: a failed read is not an absent row.
 *
 * `stored` collapses "no such film" and "could not look" into `None`, and both of these
 * call sites use that `None` as a merge base — so an unreadable row is merged as EMPTY and
 * then written over, nulling the scores the `*Ratings` refreshers own. Showtimes survive
 * either way now (`moveFilm` carries the side rows), which is why this class of bug stopped
 * showing up as a corpus-wide showtime collapse and became a quieter ratings loss.
 *
 * The two sites are NOT equally exposed, and this spec pins both the bug and the
 * non-bug — the second case exists so nobody "fixes" a read that cannot fail.
 */
class StoredReadFailureWritePathsSpec extends AnyFlatSpec with Matchers {

  private val TmdbId = 38757
  /** A resolved row: TMDB's own slot carries the year `resolvedYear` derives the key from. */
  private def rated(score: Double, cinema: Source, title: String) = MovieRecord(
    tmdbId = Some(TmdbId), imdbRating = Some(score),
    data = Map[Source, SourceData](
      cinema -> SourceData(title = Some(title)),
      Tmdb   -> SourceData(title = Some(title), releaseYear = Some(2010))))

  /**
   * REACHABLE. `settleResolved` re-keys a yearless row onto the year TMDB resolved, and
   * folds any prior occupant of that year in first so the re-key can't drop it. `target` is
   * a key the caller never touched, so nothing guarantees it is resident — the repository
   * read is the normal path, not a fallback, and its failure is indistinguishable from an
   * empty year.
   */
  "the settle's prior-occupant fold" should "not overwrite a year it merely FAILED to read" in {
    val repository = new UnreadableByIdMovieRepository()
    val cache      = new CaffeineMovieCache(repository)
    // The prior occupant of (Zaplatani, 2010): enriched, rated, in Mongo, and NOT
    // cache-resident. Written to the repository AFTER the cache boot-hydrated, which is
    // how the row is reached in prod — a second worker's write, or a hydrate that came
    // back empty because Mongo was not ready ("findAll() returned empty on a cold cache").
    repository.upsert("Zaplatani", Some(2010), rated(7.7, Multikino, "Zaplatani"))
    withClue("premise — the row must NOT be cache-resident, or the read under test never happens: ")(
      cache.get(CacheKey("Zaplatani", Some(2010))) shouldBe None)

    // A yearless row for the same film resolves, and settles onto 2010.
    val resolved = MovieRecord(tmdbId = Some(TmdbId), data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Zaplatani")),
      Tmdb   -> SourceData(title = Some("Zaplatani"), releaseYear = Some(2010))))
    cache.settleResolved(CacheKey("Zaplatani", None), resolved)

    // The invariant is about the CORPUS, not about which key the settle chose: a pass that
    // could not read the year it wanted may legitimately defer the re-key, but it may never
    // destroy what it could not read.
    repository.failing = false
    val rows = repository.findAll()
    withClue(s"corpus after the settle: ${rows.map(r => (r.title, r.year, r.record.imdbRating, r.record.data.keySet - Tmdb))}\n") {
      withClue("the rating on the row the settle could not read was destroyed: ")(
        rows.flatMap(_.record.imdbRating) should contain (7.7))
      withClue("a cinema was dropped: ")(
        rows.flatMap(_.record.data.keySet).toSet - Tmdb shouldBe Set[Source](Multikino, Helios))
    }
  }

  it should "re-key onto the resolved year as usual once that read works" in {
    val repository = new UnreadableByIdMovieRepository()
    val cache      = new CaffeineMovieCache(repository)
    repository.upsert("Zaplatani", Some(2010), rated(7.7, Multikino, "Zaplatani"))
    repository.failing = false

    val resolved = MovieRecord(tmdbId = Some(TmdbId), data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Zaplatani")),
      Tmdb   -> SourceData(title = Some("Zaplatani"), releaseYear = Some(2010))))
    val target = cache.settleResolved(CacheKey("Zaplatani", None), resolved)

    // The deferral above must be a deferral, not a new rule — a readable year still gets
    // the re-key AND the prior occupant folded in.
    withClue("the settle stopped re-keying onto the resolved year: ")(target.year shouldBe Some(2010))
    val stored = repository.findById(StoredMovieRecord.idFor(target.cleanTitle, target.year, titleNormalizer))
    stored.flatMap(_.record.imdbRating) shouldBe Some(7.7)
    stored.map(_.record.data.keySet - Tmdb) shouldBe Some(Set[Source](Multikino, Helios))
  }

  /**
   * NOT REACHABLE — and this asserts why, so the `stored` here is not "hardened" into a
   * throw that can only ever fire on a Caffeine eviction race.
   *
   * `foldDeterministically` is reached only from `put`, and only when `siblingKeyByTmdb`
   * just found the sibling by iterating the live Caffeine map. `stored` reads that same map
   * first, so the sibling is resident by construction and the repository branch is not
   * taken. A repository that fails every by-id read therefore changes nothing about the
   * fold — if that ever stops holding, this test goes red and the read needs the same
   * treatment as the one above.
   */
  "the same-tmdbId fold" should "not depend on a repository read at all" in {
    val repository = new UnreadableByIdMovieRepository()
    val cache      = new CaffeineMovieCache(repository)

    cache.put(CacheKey("Zaplatani", Some(2010)), rated(7.7, Multikino, "Zaplatani"))
    // A second spelling of the same film arrives with the same tmdbId — the fold's trigger.
    cache.put(CacheKey("Tangled", Some(2010)), rated(7.7, Helios, "Tangled"))

    repository.failing = false
    val rows = repository.findAll()
    withClue(s"rows after the fold: ${rows.map(r => (r.title, r.record.imdbRating, r.record.data.keySet))}\n") {
      rows.size shouldBe 1
      rows.head.record.imdbRating shouldBe Some(7.7)     // the sibling's rating survived
      rows.head.record.data.keySet - Tmdb shouldBe Set[Source](Multikino, Helios)
    }
  }
}
