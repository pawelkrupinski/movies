package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Reproduces the per-DEPLOY rating-spike flap measured in prod: on every worker
 * boot the hydrate's `canonicalizeBySanitize` fired ~83 `Merge retrigger`s — one
 * per arthouse "Federico Fellini: …" / DKF / programme-prefix row — each
 * invalidating rating freshness + enqueuing the 3 title-ratings (~250 rating
 * tasks within 3s of boot). prod metrics showed merges=0, so it's the SINGLE-row
 * case-drift branch, not a `keys.size>1` merge.
 *
 * Mechanism: `MovieCodecs`/`StoredMovieRecord.fromStorage` rebuilds a row's key
 * as `record.displayTitle(idPrefix)`, but `FilmCanonicalizer.canonical` picks
 * `minSpelling` for a single-title cluster — and those two spelling algorithms
 * DISAGREE for a row whose cinema slots spell the title differently. So every
 * hydrate the recovered key (displayTitle) ≠ the canonical (minSpelling) → the
 * settle re-keys + re-kicks ratings — and `fromStorage` re-derives displayTitle
 * next boot, so it never converges. A re-settle of an already-stored corpus must
 * be a no-op (no retrigger).
 */
class CanonicalizeRetriggerFlapSpec extends AnyFlatSpec with Matchers {

  private class Recorder extends EnrichmentRetrigger {
    val events = scala.collection.mutable.ListBuffer.empty[(String, Set[RetriggerKind])]
    def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = {
      events += ((key.cleanTitle, kinds)); ()
    }
  }

  // A resolved row for "Słodkie życie" (tmdbId 439) reported by two cinemas under
  // DIFFERENT decorated spellings — the prod shape (one `movies` row, several
  // cinema slots disagreeing on punctuation/casing).
  private def felliniRecord: MovieRecord =
    MovieRecord(tmdbId = Some(439), imdbId = Some("tt0053779"),
      data = Map[Source, SourceData](
        (Tmdb: Source)            -> SourceData(title = Some("Słodkie życie"), originalTitle = Some("La dolce vita"), releaseYear = Some(1960)),
        (KinoPionier: Source) -> SourceData(title = Some("Federico Fellini: Słodkie życie"), releaseYear = Some(1960)),
        (KinoMuza: Source)        -> SourceData(title = Some("Federico Fellini SŁODKIE ŻYCIE"), releaseYear = Some(1960))))

  // The settle keys a single-title cluster by `minSpelling` while
  // `StoredMovieRecord.fromStorage` rebuilds the hydrate key by `displayTitle`;
  // for a row whose cinema slots disagree on punctuation/casing those two pick
  // DIFFERENT strings (the sanitize-equal "Federico Fellini: Słodkie życie" vs
  // "…SŁODKIE ŻYCIE"). That disagreement is harmless on its own — the fix is that
  // a sanitize-equal re-key must not be treated as a rating-input change.
  "FilmCanonicalizer.canonical vs fromStorage" should
    "differ only by a sanitize-equal re-spelling for a decorated row (so the re-key must not re-kick ratings)" in {
    val record    = felliniRecord
    val recovered = StoredMovieRecord.fromStorage(StoredMovieRecord.idFor("Federico Fellini: Słodkie życie", Some(1960)), record)
    val storedKey = CacheKey(recovered.title, recovered.year)
    val (canonicalKey, _) = FilmCanonicalizer.canonical(Seq(storedKey -> record))
    TitleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe TitleNormalizer.sanitize(storedKey.cleanTitle)
  }

  "canonicalizeBySanitize" should
    "be a no-op (no retrigger) on a re-settle of already-stored decorated rows — the per-deploy flap" in {
    val recorder = new Recorder
    val c = new CaffeineMovieCache(new InMemoryMovieRepository, retrigger = recorder)
    // Seed the cache exactly as a boot hydrate would: key = fromStorage's displayTitle.
    val record    = felliniRecord
    val id        = StoredMovieRecord.idFor("Federico Fellini: Słodkie życie", Some(1960))
    val recovered = StoredMovieRecord.fromStorage(id, record)
    c.put(CacheKey(recovered.title, recovered.year), record)

    c.canonicalizeBySanitize()
    info(s"retriggers on one settle: ${recorder.events.toList}")
    info(s"rows after settle: ${c.snapshot().map(r => (r.title, r.year)).toSet}")
    withClue(s"settle re-kicked enrichment for an unchanged stored row (the deploy flap): ${recorder.events.toList}\n") {
      recorder.events shouldBe empty
    }
  }
}
