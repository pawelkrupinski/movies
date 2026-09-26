package services.identity

import models.{MovieRecord, SourceData, TitleSearch, Tmdb}
import services.identity.IdentityMeasures.{Film, Listing, ListingFilm, Measure}

/**
 * How sure a STORED film's TMDB identity is, from the evidence the row itself keeps — the rating
 * gate's confidence (docs/design/identity-resolver.md §15). The row's TMDB slot is the film; each
 * venue slot is a listing, measured against it with the calibration's own measures
 * ([[IdentityMeasures.listingFilm]]): title, original title, year, director, runtime and country
 * agreement, and how many OTHER venues listing the same title back the film by their own year or
 * director (`venues.corroborating`). A venue's detail-page facts are part of its slot, so they
 * count too. The score is [[IdentityCalibration.probability]] over those measures; nothing here
 * weighs or thresholds anything itself.
 *
 * How the row's tmdbId was concluded (`tmdbBasis`) is never read: most rows predate it, and the
 * evidence is what says whether a title-only match is right.
 *
 * The film's standing in each listing title's own TMDB title search is part of the evidence: the
 * TMDB slot keeps it per title (`SourceData.titleSearches`, measured at resolution by
 * [[IdentityMeasures.titleSearch]]) as the film's rank and its same-titled rivals — what makes a
 * bare exact title that TMDB knows as one film credible, and a banner or namesake not. A listing
 * title with no stored search (a row resolved before the field existed, a venue that listed the
 * film later) leaves both out, weighing nothing. Popularity is never used: measured, it adds
 * nothing to rank and rivals (docs/design/identity-resolver.md §13.2).
 */
object StoredIdentityConfidence {

  /** TMDB's record of the film as the row stores it (its TMDB slot). Directors and countries the
   *  slot does not carry are unknown, not "none credited". */
  def film(tmdb: SourceData): Film = Film(
    title             = tmdb.title.getOrElse(""),
    originalTitle     = tmdb.originalTitle.filter(_.trim.nonEmpty),
    alternativeTitles = tmdb.englishTitle.filter(_.trim.nonEmpty).toSeq,
    year              = tmdb.releaseYear,
    runtime           = tmdb.runtimeMinutes.filter(_ > 0),
    directors         = Some(tmdb.director.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty),
    countries         = Some(tmdb.countries.flatMap(IdentityMeasures.countryCode).distinct).filter(_.nonEmpty))

  /** One venue slot as the measures read a listing (as `services.identity.Listing.of` does). */
  def listing(slot: SourceData): Listing = {
    val title = slot.title.orElse(slot.rawTitle).getOrElse("")
    Listing(
      title         = title,
      rawTitle      = slot.rawTitle.filter(_ != title),
      originalTitle = slot.originalTitle.map(_.trim).filter(_.nonEmpty),
      year          = slot.releaseYear,
      runtime       = slot.runtimeMinutes.filter(_ > 0),
      directors     = slot.director.map(_.trim).filter(_.nonEmpty).distinct.sorted,
      countries     = slot.countries.map(_.trim).filter(_.nonEmpty).distinct.sorted)
  }

  /** One listing's measures against the film, with `venues` (every venue slot of the row, by
   *  venue) as its corroborating family — the venues listing the same title key — and `searches`
   *  the film's stored title searches. */
  def measures(film: Film, venue: String, listing: Listing, venues: Seq[(String, Listing)],
               searches: Seq[TitleSearch]): Map[String, Measure] = {
    val titleKey = IdentityMeasures.key(listing.title)
    val family   = venues.filter { case (_, l) => IdentityMeasures.key(l.title) == titleKey }
    val search   = searches.find(_.titleKey == titleKey)
    val measured = IdentityMeasures.listingFilm(listing, film, search.flatMap(_.rank), search.fold(0)(_.rivals),
      IdentityMeasures.corroboratingVenues(film, family, venue)) - "popularity.log2"
    if (search.isDefined) measured else measured -- IdentityMeasures.RankingPriors
  }

  /** The film's confidence: its best-evidenced listing's calibrated probability. The ratings are
   *  the TMDB film's, so one venue whose facts (with its family's) establish that film is enough;
   *  a venue that publishes nothing neither establishes nor refutes it. None without a TMDB
   *  record or a venue listing: nothing to measure. */
  def of(film: Film, venues: Seq[(String, Listing)], searches: Seq[TitleSearch], calibration: IdentityCalibration): Option[Double] =
    venues.map { case (venue, l) =>
      // The search standing can lend a listing confidence, never withdraw it: a film its own
      // facts establish is not doubted for ranking seventh among namesakes. The same rule the
      // resolver's vetoes follow (`IdentityResolver.factsProbability`): rank and rivals are
      // ambiguity, not evidence of a different film, and their weights are the label-selection
      // sensitive ones (docs/design/identity-resolver.md §14.6).
      val withSearch = measures(film, venue, l, venues, searches)
      calibration.probability(ListingFilm, withSearch) max
        calibration.probability(ListingFilm, withSearch -- IdentityMeasures.RankingPriors)
    }.maxOption

  def of(record: MovieRecord, calibration: IdentityCalibration): Option[Double] =
    record.data.get(Tmdb).filter(_ => record.tmdbId.isDefined).flatMap { tmdb =>
      of(film(tmdb), record.cinemaShowings.map { case (cinema, slot) => cinema.displayName -> listing(slot) }, tmdb.titleSearches,
        calibration)
    }
}
