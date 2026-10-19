package services.identity

import services.cinemas.pl.NonMovieEventClassifier

import java.util.Locale

/** A venue's KIND profile (experiment `venue-kinds`): how many of the titles it bills are a RELAY (a screened broadcast —
 *  NT Live, the Met, RBO, a concert film's "live viewing" — or a stage house's billing) or a NON-FILM event (a performer
 *  live, a slam, a talk, a booking — [[agreement.NonFilmEvents]], [[NonMovieEventClassifier]]), read off every listing of
 *  the corpus by the classifiers that exist. A prior, never evidence FOR a film: it only ever asks a take for more.
 *
 *  Leave-one-out by title: the share a billing is read against is the venue's OTHER titles', so a venue billing one relay
 *  is not "relay-heavy" by that relay alone.
 *  @param venues each venue → its title keys and whether each is billed as a relay or event */
private[identity] final class VenueKinds(venues: Map[String, Map[String, Boolean]]) {
  private val totals: Map[String, (Int, Int)] = venues.view.mapValues(titles => (titles.size, titles.count(_._2))).toMap

  /** How many OTHER titles `venue` bills than `key`, and the share of them that are a relay or a non-film event. */
  def nonFilmShare(venue: String, key: String): (Int, Double) = totals.get(venue).fold((0, 0.0)) { case (n, nonFilm) =>
    val own = venues(venue).get(key)
    val others = n - (if (own.isDefined) 1 else 0)
    val nonFilmOthers = nonFilm - (if (own.contains(true)) 1 else 0)
    (others, if (others == 0) 0.0 else nonFilmOthers.toDouble / others)
  }

  /** Is `venue`, by its titles other than `key`, one whose billings are often relays and events ([[VenueKinds.MinTitles]],
   *  [[VenueKinds.MinShare]])? */
  def eventHeavy(venue: String, key: String): Boolean = {
    val (others, share) = nonFilmShare(venue, key)
    others >= VenueKinds.MinTitles && share >= VenueKinds.MinShare
  }
}

private[identity] object VenueKinds {
  val None: VenueKinds = new VenueKinds(Map.empty)

  /** A venue read on fewer other titles has no profile. */
  val MinTitles = 10
  /** The share of relays and events a venue's other titles must reach. */
  val MinShare = 0.20

  /** Is `listing` billed as a relay or a non-film event, by the classifiers that exist? */
  def nonFilm(listing: Listing): Boolean = {
    val raw = listing.rawTitle
    val lower = raw.toLowerCase(Locale.ROOT)
    NonMovieEventClassifier.isScreenedBroadcast(lower) || ListingShape.billsAHouse(listing) ||
      NonMovieEventClassifier.isNonScreening(raw) || NonMovieEventClassifier.isLiveEvent(raw) || agreement.NonFilmEvents.of(listing).isDefined
  }

  /** Every venue's profile from `listings`, each under its title `key`. */
  def of(listings: Seq[(String, Listing)]): VenueKinds = new VenueKinds(
    listings.groupBy(_._2.venue).map { case (venue, keyed) =>
      venue -> keyed.groupMapReduce(_._1)(keyedListing => nonFilm(keyedListing._2))(_ || _) })

  /** Does the BILLING itself look like an event or a relay — by the classifiers on its raw title alone (variant A)? */
  def looksNonFilm(listing: IdentityMeasures.Listing): Boolean = {
    val raw = listing.rawTitle.getOrElse(listing.title)
    NonMovieEventClassifier.isScreenedBroadcast(raw) || listing.billsAStageHouse || NonMovieEventClassifier.isNonScreening(raw) ||
      NonMovieEventClassifier.isLiveEvent(raw)
  }

  /** Does the billing name `film` only by a PIECE — sharing a word with its titles, and saying two words or more beyond
   *  every one of them ("Fallen Angels by Noël Coward", "CBeebies Panto 2026: Treasure Island", "Dave – Music in Motion")
   *  (variant C)? */
  def namesOnlyByAPiece(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean = {
    val billed = IdentityMeasures.countedWords(listing.rawTitle.getOrElse(listing.title)).filterNot(_.forall(_.isDigit)).distinct
    val record = film.titles.flatMap(IdentityMeasures.countedWords).toSet
    billed.exists(record) && billed.count(word => !record(word)) >= 2
  }

  /** Does the listing state a fact of its own film — a year, a running time or a director? */
  def statesAFact(listing: IdentityMeasures.Listing): Boolean =
    listing.year.isDefined || listing.runtime.isDefined || listing.directors.exists(_.trim.nonEmpty)

  /** An ordinary feature: no relay, concert or stage record by its titles, and not a short. */
  def ordinaryFeature(film: IdentityMeasures.Film): Boolean =
    !ShortFilms.isShort(film) && !film.titles.exists(title =>
      NonMovieEventClassifier.isScreenedBroadcast(title) || StageRelays.billsAStageHouse(Seq(title)) || NonMovieEventClassifier.isPerformance(title))
}
