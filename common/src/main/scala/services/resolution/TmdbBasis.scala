package services.resolution

/**
 * WHAT a row's `tmdbId` was concluded from — the strength of the evidence, kept
 * beside the conclusion instead of being forgotten the moment it is reached.
 *
 * Resolution today is a one-shot: `resolveTmdbId` runs once, and whatever it
 * decided is indistinguishable ever after from a decision made on overwhelming
 * evidence. That is what let prod carry five films resolved to the wrong title
 * for weeks — "Vivaldi i ja" on an 18-minute concert short, "Das Phantom der
 * Oper" on the 1925 silent — with nothing in the row to say those answers had
 * been guesses from a bare title, nor that better evidence had since arrived on
 * the very same row.
 *
 * The values are ordered by how much they narrow the field: a stronger basis may
 * supersede a weaker one, and a weaker one must never overwrite a stronger.
 */
enum TmdbBasis(val rank: Int) {
  /** A title search with no year and no director to narrow it — the weakest
   *  answer the pipeline can reach, and the only one that is a genuine guess. A
   *  deferred-detail cinema's first scrape carries exactly this much: a title.
   *  Every one of the five mis-resolved prod rows was decided here. */
  case TitleOnly extends TmdbBasis(1)

  /** A title search narrowed by a year — the row's own, an embedded "(YYYY)", or
   *  one a cinema published. Wrong only when the year itself is wrong. */
  case YearScoped extends TmdbBasis(2)

  /** A walk of a reported director's filmography, matched on title. The director
   *  came from a cinema, so the answer is not derived from the resolution it is
   *  checking. */
  case DirectorWalk extends TmdbBasis(3)

  /** An exact reverse lookup from an external id (IMDb, Letterboxd, Wikidata) —
   *  an identity, not a search. */
  case ExternalId extends TmdbBasis(4)
}

object TmdbBasis {
  /** Parse a stored value, tolerating anything unrecognised (including the absent
   *  field on every row written before this existed) as "we don't know". */
  def parse(name: String): Option[TmdbBasis] = TmdbBasis.values.find(_.toString == name)
}
