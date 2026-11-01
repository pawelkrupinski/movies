package services.identity

import services.movies.RelayBanner

/**
 * When a PLAIN listing's own facts rule out an event-cinema relay's record (the owner, 2026-11-01: the narrower fix
 * after design B, a blanket relay/film split, measured not landable: box offices bill relays bare and TMDB files relays
 * under plain titles, so a split by listing kind lost ~1,000 right takes).
 *
 * UK Chichester's bare "Prima Facie": its own text credits Susanna White, the 2026 film's director, and never Justin Martin,
 * who filmed the National Theatre's 2022 recording ("National Theatre Live: Prima Facie", 963568). Beside Power Station's
 * "NT Live: Prima Facie" it took the recording, and went back to the film as more bare billings arrived: a flip (drift,
 * uk seed 17). A relay's record whose director the venue's own text never names, while it names whole the director of
 * another record its own searches reached, is another work than the listing bills.
 *
 * Read off the listing's own text ([[Listing.names]]: its synopsis and cast field, never a feed's copy), the record and
 * the records the listing's own searches reach: no other listing, no take. One direction only: a relay's listing is never
 * denied a plain record by this.
 */
private[identity] object RelayRecords {
  val OwnTextDenial = "its venue's own text credits another film's director, and none of this relay's"

  /** Is `film` a relay's record (a title billing a relay's banner, [[RelayBanner]]) crediting a director, that `listings`
   *  -- billing no relay themselves -- contradict: their own text names whole a director of one of `others` (the other
   *  records their searches reach) that is no relay's, and none of `film`'s? */
  def contradictedByOwnText(listings: Seq[Listing], film: IdentityMeasures.Film, others: => Seq[IdentityMeasures.Film]): Boolean =
    RelayBanner.billed(film.titles) && listings.nonEmpty && listings.forall(l => !RelayBanner.billed(Seq(l.rawTitle, l.title))) && {
      val named = listings.map(_.names).filterNot(_.isEmpty)
      def credits(director: String) = named.exists(_.names(director))
      // a record crediting nobody is contradicted by no credit: the Met's 2026/27 records do (UK "Live from the Met:
      // Parsifal", whose text names Syberberg's film's director, lost its record to an empty credit list)
      val directors = film.directors.getOrElse(Nil)
      named.nonEmpty && directors.nonEmpty && !directors.exists(credits) &&
        others.exists(other => !RelayBanner.billed(other.titles) && other.directors.getOrElse(Nil).exists(credits))
    }
}
