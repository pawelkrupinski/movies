package services.identity

/** A TMDB-keyed feed's link of a node to one film ([[SourceReliability.tmdbLinked]]: Cineamo's [[SourceReliability.Fact.Link]]) that the title's other sources OVERRULE.
 *
 *  The feed's id is a HINT, not ground truth: the venue's operator picked the film from TMDB, and the facts the feed copies
 *  onto the listing are that record's, so they agree with the linked film by construction and say nothing about whether
 *  the pick was right. DE Cineamo links "Karla" at eleven venues to the 2006 film (9695m, its 99 minutes copied) and
 *  "Momo" at four to the 1986 one (2265m, its year copied), while every other source listing the title — Filmstarts'
 *  entry, kinoheld's page, Cineamo's own other entry — states the 2025 film's year and director.
 *
 *  Overruled when no source other than the feed's own entry backs the linked film ([[VenueBacking]]:
 *  a title naming it beside its year or director) while [[OverrulingSources]] or more back one film the title names as
 *  its own, whole or as one delimited piece — a link no one confirms against a film others independently state. DE Astor's
 *  "ATMOS: Michael", linked to Dreyer's 1924 film with its year copied, is the "Michael" other sources date 2026 and
 *  credit to Antoine Fuqua. The node is then read on its titles alone
 *  ([[Evidence.titlesOnly]]) and the linked film is denied it; a link another source confirms, or one only a single
 *  source contradicts, stands (DE "Kohlhiesels Töchter": Cineamo's 1962 film at one venue, kinoheld's 1920 one at
 *  another — two films, each named once) — but for a current release that source bills at more venues than the link,
 *  [[ReleaseVenues]] at least, against an older linked film ([[venues]]): the operator's pick is then an old namesake of
 *  the film opening that month. DE Werkstattkino's "CAMOUFLAGE", linked to the 2001 comedy (56085m) with its original
 *  title copied, is Gibalenko's 2026 film that Filmstarts' one entry dates and credits at three venues; it took the
 *  comedy whenever Gröben-Lichtspiele's Cineamo entry of the 2026 film, the second source, was away (convergence #2473).
 *
 *  Overruled too when no source backs the linked film and the venue's own title, read without the facts the feed copied,
 *  names another film exactly and the linked one not at all ([[FeedLink.TitledRival]]): the operator typed the title
 *  and picked the record from a search, and the title is the claim. DE Yorck's "Jin-Roh - Die Wolfsbrigade", linked to
 *  Kim Jee-woon's 2018 "Illang" with its 139 minutes and original title copied, is Okiura's 1999 anime. */
private[identity] final case class FeedLink(id: CatalogueId, film: Int, rival: Int, sources: Int, venues: Int = 0) {
  private def because: String =
    if (sources == FeedLink.TitledRival) s"its title names $rival exactly and $film not at all"
    else if (venues > 0) s"$sources other source(s) back $rival, a current release, at $venues venues against an older film's link"
    else s"$sources other source(s) back $rival"
  /** What the decision's explanation says of it. */
  def explanation: String =
    s"feed ${id.source} ${id.id} links $film, overruled: $because and none backs $film — its copied facts set aside"
  /** Why the linked film is denied the node. */
  def denial: String = s"its feed's link (${id.source} ${id.id}) overruled: $because"
}

private[identity] object FeedLink {
  /** How many sources other than the feed's own entry must back a rival the title names to overrule the link. */
  val OverrulingSources = 2
  /** How many venues besides the node's must bill a current release a single source backs for it to overrule a link to an
   *  older film — more than the link's own venues besides ([[FeedLink.venues]]). */
  val ReleaseVenues = 2
  /** The `sources` of a link overruled by the venue's own title naming another film exactly, no source backing either. */
  val TitledRival = 0

  /** Did the feed copy an original title the venue bills no title of — a title the link added, not the venue's? */
  def copiesAnotherTitle(listing: IdentityMeasures.Listing): Boolean =
    listing.originalTitle.exists(copied => !(listing.rawTitle.toSeq :+ listing.title).map(IdentityMeasures.key).contains(IdentityMeasures.key(copied)))

  /** The one TMDB film the TMDB-keyed feeds link `listings` to, with the id that links it — the one definition every
   *  reader of a node's or a cluster's link reads. A listing no such feed lists is silent, not a dissent (a cluster spans
   *  venues no feed lists); listings linked to two films link none. The number is TMDB's: a resolve whose films are a
   *  family's own numbers reads it as nothing ([[IdentityLookups.numbersFilmsByTmdbId]], `FamilyScope.feedLinked`). */
  def linked(listings: Seq[Listing]): Option[(CatalogueId, Int)] =
    listings.flatMap(SourceReliability.tmdbLinked).distinctBy(_._2) match {
      case Seq(link) => Some(link)
      case _         => None
    }
}
