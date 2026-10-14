package services.identity

/** A TMDB-keyed feed's link of a node to one film ([[SourceReliability.tmdbLinked]]: Cineamo's [[SourceReliability.Fact.Link]]) that the title's other sources OVERRULE.
 *
 *  The feed's id is a HINT, not ground truth: the venue's operator picked the film from TMDB, and the facts the feed copies
 *  onto the listing are that record's, so they agree with the linked film by construction and say nothing about whether
 *  the pick was right. DE Cineamo links "Karla" at eleven venues to the 2006 film (9695m, its 99 minutes copied) and
 *  "Momo" at four to the 1986 one (2265m, its year copied), while every other source listing the title — Filmstarts'
 *  entry, kinoheld's page, Cineamo's own other entry — states the 2025 film's year and director.
 *
 *  Overruled when no source other than the feed's own entry backs the linked film ([[IdentityMeasures.VenueBacking]]:
 *  a title naming it beside its year or director) while [[OverrulingSources]] or more back one film the title names as
 *  its own, whole or as one delimited piece — a link no one confirms against a film others independently state. DE Astor's
 *  "ATMOS: Michael", linked to Dreyer's 1924 film with its year copied, is the "Michael" other sources date 2026 and
 *  credit to Antoine Fuqua. The node is then read on its titles alone
 *  ([[Evidence.titlesOnly]]) and the linked film is denied it; a link another source confirms, or one only a single
 *  source contradicts, stands (DE "Kohlhiesels Töchter": Cineamo's 1962 film at one venue, kinoheld's 1920 one at
 *  another — two films, each named once).
 *
 *  Overruled too when no source backs the linked film and the venue's own title, read without the facts the feed copied,
 *  names another film exactly and the linked one not at all ([[FeedLink.TitledRival]]): the operator typed the title
 *  and picked the record from a search, and the title is the claim. DE Yorck's "Jin-Roh - Die Wolfsbrigade", linked to
 *  Kim Jee-woon's 2018 "Illang" with its 139 minutes and original title copied, is Okiura's 1999 anime. */
private[identity] final case class FeedLink(id: CatalogueId, film: Int, rival: Int, sources: Int) {
  private def because: String =
    if (sources == FeedLink.TitledRival) s"its title names $rival exactly and $film not at all" else s"$sources other source(s) back $rival"
  /** What the decision's explanation says of it. */
  def explanation: String =
    s"feed ${id.source} ${id.id} links $film, overruled: $because and none backs $film — its copied facts set aside"
  /** Why the linked film is denied the node. */
  def denial: String = s"its feed's link (${id.source} ${id.id}) overruled: $because"
}

private[identity] object FeedLink {
  /** How many sources other than the feed's own entry must back a rival the title names to overrule the link. */
  val OverrulingSources = 2
  /** The `sources` of a link overruled by the venue's own title naming another film exactly, no source backing either. */
  val TitledRival = 0

  /** Why a TMDB-keyed feed's link of `node` to another film denies it `film`, a SHORT ([[SuspectTakes.ShortMinutes]]): the
   *  feed's pick is a programme's film, and the facts it copies are the pick's — an English title a short carries too is
   *  the pick restated, not a second film. DE "L`Estive" ×3: Cineamo links Lenoir's 2026 documentary (1729636m), copying
   *  its English title "Summer Meadow", which a 3-minute 2024 short carries and took at 62.5%; "HEIMSUCHUNG - MittagsKINO"
   *  took the 18-minute "Visitation" while Cineamo links Schlöndorff's "Heimsuchung", whose English title it is. */
  def deniesShort(node: EvidenceNode, tmdbId: Int, film: IdentityMeasures.Film): Option[String] =
    linked(node).filter { case (_, linkedFilm) => linkedFilm != tmdbId && film.runtimes.nonEmpty && film.runtimes.max < SuspectTakes.ShortMinutes }
      .map { case (id, linkedFilm) => s"a short, its feed (${id.source} ${id.id}) linking $linkedFilm" }

  /** The one TMDB film a TMDB-keyed feed links every listing of `node` to, with the id that links it. */
  def linked(node: EvidenceNode): Option[(CatalogueId, Int)] =
    node.listings.map(SourceReliability.tmdbLinked).distinct match {
      case Seq(Some(link)) => Some(link)
      case _               => None
    }
}
