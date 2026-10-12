package services.identity

/** A TMDB-keyed feed's link of a node to one film ([[CatalogueSources.TmdbKeyed]]) that the title's other sources OVERRULE.
 *
 *  The feed's id is a HINT, not ground truth: the venue's operator picked the film from TMDB, and the facts the feed copies
 *  onto the listing are that record's, so they agree with the linked film by construction and say nothing about whether
 *  the pick was right. DE Cineamo links "Karla" at eleven venues to the 2006 film (9695m, its 99 minutes copied) and
 *  "Momo" at four to the 1986 one (2265m, its year copied), while every other source listing the title — Filmstarts'
 *  entry, kinoheld's page, Cineamo's own other entry — states the 2025 film's year and director.
 *
 *  Overruled when no source other than the feed's own entry backs the linked film ([[IdentityMeasures.VenueBacking]]:
 *  a title naming it beside its year or director) while [[OverrulingSources]] or more back one film the title names as
 *  its own — a link no one confirms against a film others independently state. The node is then read on its titles alone
 *  ([[Evidence.titlesOnly]]) and the linked film is denied it; a link another source confirms, or one only a single
 *  source contradicts, stands (DE "Kohlhiesels Töchter": Cineamo's 1962 film at one venue, kinoheld's 1920 one at
 *  another — two films, each named once). */
private[identity] final case class FeedLink(id: CatalogueId, film: Int, rival: Int, sources: Int) {
  /** What the decision's explanation says of it. */
  def explanation: String =
    s"feed ${id.source} ${id.id} links $film, overruled: $sources other source(s) back $rival and none backs $film — its copied facts set aside"
  /** Why the linked film is denied the node. */
  def denial: String = s"its feed's link (${id.source} ${id.id}) overruled: $sources other source(s) back $rival"
}

private[identity] object FeedLink {
  /** How many sources other than the feed's own entry must back a rival the title names to overrule the link. */
  val OverrulingSources = 2

  /** The one TMDB film a TMDB-keyed feed links every listing of `node` to, with the id that links it. */
  def linked(node: EvidenceNode): Option[(CatalogueId, Int)] =
    node.listings.map(CatalogueSources.tmdbLinked).distinct match {
      case Seq(Some(link)) => Some(link)
      case _               => None
    }
}
