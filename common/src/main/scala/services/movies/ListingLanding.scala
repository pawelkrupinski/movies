package services.movies

import models.{Cinema, MovieRecord}

/**
 * Where a scraped listing lands — the questions `MovieCache.recordCinemaScrape` asks
 * of the corpus before it touches a row, as one value computed once per listing.
 *
 * Every question here has a twin in the settle: a row keyed by this title (the
 * sanitize group), a concluded row listing it as a TMDB alias, a row already holding
 * this venue's slot for it, a resolved film this title DECORATES (`TitleContainment`),
 * a resolved film with the same search-title key (`FilmCanonicalizer.searchKey`), and
 * the cinemas' own veto — this venue describes a DIFFERENT film than the row
 * (`MixedFilmDetector`). Asking them at landing is what keeps the settle from having
 * anything to fold: a listing lands where the settle would have put it a tick later.
 *
 * Pure over the corpus index and the resident records, so the whole decision is
 * unit-tested without a cache, a store or a scrape.
 */
object ListingLanding {

  /** What a scraped listing carries that the questions read. */
  final case class Listing(displayTitle: String, cinema: Cinema, originalTitle: Option[String],
                           runtimeMinutes: Option[Int], releaseYear: Option[Int], director: Seq[String])

  /** The corpus's answers for one listing. */
  final case class Answers(
    /** Rows keyed by this exact sanitized title. */
    sameTitledRows: Seq[MovieRecord],
    holdsTitle: Boolean,
    holdsAlias: Boolean,
    holdsCinemaSlot: Boolean,
    /** Resolved rows this title is a decorated screening of — a banner around the film's
     *  own title — after the cinemas' veto. */
    decorationOf: Set[CacheKey],
    /** Resolved rows under the same search-title key — after the cinemas' veto. */
    sameSearchAs: Set[CacheKey],
    /** Every same-titled row describes a different film than this venue does. */
    aDifferentFilm: Boolean
  ) {
    /** A newcomer: nothing in the corpus holds it, or what holds the title is a different
     *  film. Only when the cache has somewhere to divert TO. */
    def divert(diverting: Boolean): Boolean =
      diverting && ((!holdsTitle && !holdsAlias && !holdsCinemaSlot && decorationOf.isEmpty && sameSearchAs.isEmpty) || aDifferentFilm)

    /** The row to land on when neither a concluded row nor a spelling variant claimed the
     *  listing: the film it decorates, else the film sharing its search key — ranked the
     *  way the settle ranks a fold's survivor. */
    def fallbackKey: Option[CacheKey] =
      decorationOf.minByOption(FilmCanonicalizer.canonicalRank)
        .orElse(sameSearchAs.minByOption(FilmCanonicalizer.canonicalRank))
  }

  def ask(index: CorpusIndexReader, recordOf: CacheKey => Option[MovieRecord], listing: Listing,
          normalizer: TitleNormalizer, diverting: Boolean): Answers = {
    val norm           = normalizer.sanitize(listing.displayTitle)
    val sameTitledRows = index.rowsFor(norm)
    def wouldAddASecondFilm(record: MovieRecord): Boolean =
      MixedFilmDetector.wouldAddASecondFilm(record, listing.originalTitle, listing.runtimeMinutes,
        listing.releaseYear, listing.director, normalizer)
    def notASecondFilm(k: CacheKey): Boolean = recordOf(k).forall(r => !wouldAddASecondFilm(r))
    // A one-word film title runs along the edge of many unrelated titles ("It" → "It
    // Ends With Us", "Her" → "Her Story"), and the veto is structurally blind there:
    // it compares the words of four letters and more, of which such a title has none.
    // The settle's edge folds such a row only after it FAILED to resolve on its own;
    // this runs before any resolution, so a two-word base is required and a listing
    // that only shares one word resolves itself.
    val decorationOf =
      if (sameTitledRows.nonEmpty) Set.empty[CacheKey]
      else index.keysDecoratedBy(TitleContainment.tokens(listing.displayTitle), minBaseTokens = 2).filter(notASecondFilm)
    val sameSearchAs =
      if (sameTitledRows.nonEmpty) Set.empty[CacheKey]
      else index.keysWithSearchKey(FilmCanonicalizer.searchKey(listing.displayTitle, normalizer)).filter(notASecondFilm)
    Answers(
      sameTitledRows  = sameTitledRows,
      holdsTitle      = index.holdsTitle(norm),
      holdsAlias      = index.holdsAlias(norm),
      holdsCinemaSlot = index.holdsCinemaSlot(listing.cinema, norm),
      decorationOf    = decorationOf,
      sameSearchAs    = sameSearchAs,
      aDifferentFilm  = diverting && sameTitledRows.nonEmpty && sameTitledRows.forall(wouldAddASecondFilm))
  }
}
