package services.identity

import scala.collection.mutable

/** A family's feed links ([[FeedLink]]) read against its pool ([[FamilyScope]]): the TMDB film a node's TMDB-keyed feed
 *  links it to, that link overruled where the title's other sources back a rival, a current release or the one film its
 *  titles name, and the node's evidence as the family then reads it. */
private[identity] final class FeedLinkOverrule(pool: FamilyPool, numbersFilmsByTmdbId: Boolean, backing: VenueBacking) {

  /** The TMDB film a TMDB-keyed feed links `node` to ([[FeedLink.linked]]) — none where this resolve's films are a
   *  family's own numbers, which no TMDB id names (`numbersFilmsByTmdbId`: [[IdentityLookups.numbersFilmsByTmdbId]]). */
  def linked(node: EvidenceNode): Option[(CatalogueId, Int)] =
    if (numbersFilmsByTmdbId) FeedLink.linked(node.listings) else None

  /** `node`'s feed link, when the title's other sources overrule it ([[FeedLink]]): once per node for this pool's life. */
  def overruled(node: EvidenceNode): Option[FeedLink] = overruledMemo.getOrElseUpdate(node.id, overrule(node))
  /** [[linked]], where the original title on `node` is the feed's copy, only Support ([[SourceReliability.Fact.OriginalTitle]]):
   *  the link the copied-title and short denials read ([[CopiedTitleDenial]]) -- a venue's own original title is its claim,
   *  never a copy's pull -- as the feed entry it names (`<source> <id>`) and the film it links. */
  def copyingLink(node: EvidenceNode): Option[(String, Int)] =
    linked(node).filter(_ => node.listings.forall(listing => !SourceReliability.decides(listing, SourceReliability.Fact.OriginalTitle)))
      .map { case (id, film) => (s"${id.source} ${id.id}", film) }

  /** Why `node`'s feed link to `film` is overruled ([[FeedLink.denial]]), when it is. */
  def overruledDenial(node: EvidenceNode, film: Int): Option[String] = overruled(node).filter(_.film == film).map(_.denial)
  private val overruledMemo = mutable.HashMap.empty[String, Option[FeedLink]]

  /** `node`'s evidence as the family reads it: its titles alone when its feed link is overruled — the facts were the
   *  link's, restated — else its own. */
  def evidenceOf(node: EvidenceNode): Evidence = if (overruled(node).isEmpty) node.evidence else node.evidence.titlesOnly

  private def overrule(node: EvidenceNode): Option[FeedLink] = linked(node).flatMap { case (id, film) =>
    val listing = node.evidence.measured
    // whose claim the link is: the node's present listings' -- a kept one sharing its evidence is no claim of the link's
    val own     = SourceReliability.claimOf(node.listings.find(Authority.of(_) != Authority.Provenance.Kept).getOrElse(node.listings.head))
    val overruling = overrulingRelation(listing, film)
    if (pool.candidates.exists(candidate => candidate.tmdbId == film && overrulingSources(listing, own, candidate) > 0)) None
    else {
      val rivals = pool.candidates.filter(candidate => candidate.tmdbId != film && overruling(pool.titleRelation(listing, candidate).value))
        .map(candidate => candidate -> overrulingSources(listing, own, candidate))
      overrulingLink(node, id, film, rivals)
    }
  }
  /** The link of `node` to `film` overruled: by the rival most sources back, past [[FeedLink.OverrulingSources]], else by
   *  a current release among the backed rivals, else by the one rival the titles alone name. */
  private def overrulingLink(node: EvidenceNode, id: CatalogueId, film: Int, rivals: Seq[(Candidate, Int)]): Option[FeedLink] =
    rivals.filter(_._2 >= FeedLink.OverrulingSources).sortBy { case (rival, count) => (-count, rival.tmdbId) }.headOption
      .map { case (rival, count) => FeedLink(id, film, rival.tmdbId, count) }
      .orElse(currentRelease(node, film, rivals.collect { case (rival, count) if count > 0 => rival })
        .map { case (rival, venues) => FeedLink(id, film, rival, 1, venues) })
      .orElse(titledRival(node, film).map(rival => FeedLink(id, film, rival, FeedLink.TitledRival)))
  /** The PRESENT sources' say for `candidate` against a link claimed by `own`: a kept listing ([[Listing.kept]]) keeps its
   *  own film's details alive, never a link a present listing's own sources overrule (DE "Eternity": Caligari's kept
   *  Freyne listing backed Cineamo's link). */
  private def overrulingSources(listing: MeasuredListing, own: String, candidate: Candidate): Int =
    backing.sources(backingGroups(listing, candidate), candidate.film, own, Authority.Act.Overrule)
  /** The title relations by which a rival of the linked `film` overrules the link. A rival the title names only as one
   *  of its pieces overrules no link to a film it names whole: DE Metropolis's "KIRSCHBLÜTEN - HANAMI", linked to
   *  Dörrie's 2008 film, is not the "Hanami" three venues credit to Denise Fernandes. */
  private def overrulingRelation(listing: MeasuredListing, film: Int): Set[String] =
    if (pool.candidates.exists(candidate => candidate.tmdbId == film && IdentityMeasures.Rivalling(pool.titleRelation(listing, candidate).value)))
      IdentityMeasures.Rivalling
    else IdentityMeasures.TitlesItsOwn
  /** The one current release among `backed` (the rivals a source backs, fewer than [[FeedLink.OverrulingSources]]) that
   *  the title names whole and more of the title's venues bill than `node`'s link, [[FeedLink.ReleaseVenues]] at least,
   *  when the linked `film` is an older one the listing does not date — with the venues billing it. The operator's pick
   *  from a search is then an old namesake of the film opening that month, not one claim against another ([[FeedLink]]).
   *  Never a release the title names only as one of its pieces (DE Babylon's "Nosferatu Live mit Babylon Orchester
   *  Berlin", which Cineamo links to Herzog's 1979 film, is no 2025 "Nosferatu"), nor against a link the listing dates by
   *  its film's year (Melodie's one-night "Spider-Man" [2002] is Raimi's, not the 2026 "Spider-Man: Brand New Day"). */
  private def currentRelease(node: EvidenceNode, film: Int, backed: Seq[Candidate]): Option[(Int, Int)] = {
    val current = currentAt(node)
    val listing = node.evidence.measured
    val older   = pool.candidates.find(_.tmdbId == film).exists(linked => !current(linked) &&
      linked.film.year.exists(year => !node.evidence.statedYear.contains(year)))
    if (!older) None
    else backed.filter(candidate => current(candidate) && IdentityMeasures.Rivalling(pool.titleRelation(listing, candidate).value))
      .map(candidate => candidate.tmdbId -> backing.corroborating(backingGroups(listing, candidate), candidate.film, node.venue,
        SourceReliability.claimOf(node.listings.head), Authority.Act.Overrule))
      .filter { case (_, venues) => venues >= FeedLink.ReleaseVenues && venues > node.venues.size } match {
      case Seq(rival) => Some(rival)
      case _          => None
    }
  }
  /** The title groups whose venues back `candidate` for `listing`, by the one title relation ([[TitleRelation.backingGroups]]). */
  private def backingGroups(listing: MeasuredListing, candidate: Candidate): Seq[String] =
    TitleRelation.backingGroups(listing, candidate.film)
  /** Is a candidate a current release where `node` screens: dated [[ScreeningDays.CurrentRelease]] or fewer years
   *  before its first screening's year? */
  private def currentAt(node: EvidenceNode): Candidate => Boolean = {
    val firstYear = node.listings.flatMap(_.screenings.firstYear).minOption
    candidate => candidate.film.year.exists(year => firstYear.exists(year >= _ - ScreeningDays.CurrentRelease))
  }
  /** The one film of the pool `node`'s titles alone name exactly, when they name the linked `film` not at all — never
   *  an older film against a link to a current release whose title carries the venue's whole, as one edge of it
   *  (`fragment`: [[IdentityMeasures.ContainingRelations]]). A release is billed under the head of its title, which an
   *  old namesake's record may carry whole: DE Cineamo's bare "Lieblingsmenschen", linked to Hufnagel's 2026
   *  "Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes & Amir", is not the 65-minute 2021
   *  "Lieblingsmenschen" — overruled, 95 listings at 68 venues took none, the namesake refused and the release denied.
   *  A link whose film the title does not carry is still overruled, current or not: Yorck's "Jin-Roh - Die
   *  Wolfsbrigade", linked to the 2018 remake "Illang", and Bundesplatz-Studio's "Der Diplomat", linked to Shivam Nair's
   *  2025 "The Diplomat" (its year copied: a feed's copy dates nothing), the 1995 Hessel documentary. */
  private def titledRival(node: EvidenceNode, film: Int): Option[Int] = {
    val titles = node.evidence.titlesOnly.measured
    def relation(candidate: Candidate) = pool.relate(titles, candidate.film).value
    val current = currentAt(node)
    def carriedRelease(linked: Candidate) = current(linked) && IdentityMeasures.ContainingRelations(relation(linked))
    val linked = pool.candidates.filter(_.tmdbId == film)
    if (linked.isEmpty || linked.exists(candidate => IdentityMeasures.NamingRelations(relation(candidate)))) None
    else pool.candidates.filter(candidate => candidate.tmdbId != film && relation(candidate) == "exact") match {
      case Seq(rival) if !(linked.exists(carriedRelease) && !current(rival)) => Some(rival.tmdbId)
      case _                                                                 => None
    }
  }
}
