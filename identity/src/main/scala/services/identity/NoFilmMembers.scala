package services.identity

import services.identity.measures.{IdentityMeasures, MultiFilmBill}
import services.identity.vocabulary.{CandidateQuery, Hit}

/** The members of a family's cluster decided apart from the film the cluster took, as no film ([[ResolverRun]]): a
 *  programme billed under its banner beside a member billing several works, a member billing the film as one piece
 *  beside another thing ([[BilledPiece]]), a member quoting it beside a venue's own screening of it. */
private[identity] final class NoFilmMembers(scope: FamilyScope, generation: CandidateGeneration) {

  /** The members of `unbilled` billing `film` as one piece beside another thing, or beside their screening, with why. */
  def billedAsPieces(unbilled: Seq[EvidenceNode], film: Int): Map[EvidenceNode, String] = {
    val scored       = unbilled.flatMap(node => scope.of(node).find(_.candidate.tmdbId == film).map(node -> _))
    val corroborated = scored.flatMap(_._2.number("venues.corroborating")).maxOption.getOrElse(0.0)
    scored.flatMap { case (node, own) => billedAsPiece(node, own, corroborated, recurs, generation.answeredHits).map(node -> _) }.toMap ++
      besideItsScreening(unbilled)
  }

  // a piece the corpus bills beside two whole titles or more, whatever families they fell in
  private def recurs(shape: String) = generation.repeatedShape(shape)

  /** The members billing a programme UNDER ITS BANNER: a member whose every listing's words are all words of a member
   *  billing several films by a word that says so ([[MultiFilmBill]]: a block of shorts, a marathon), with one beside the
   *  film its cluster took — the programme's banner, not the film's title. It is that programme, as none of its films:
   *  DE "Pornfilmfestival: The more the merrier" is the block "Pornfilmfestival Warm Up: The more the merrier -
   *  shortfilms presented by Paulita Pappel", not Stevens' 1943 film. A film's own bare title beside a bill of it
   *  ("The Dark Knight" beside "The Dark Knight Trilogy") carries no banner, and stays the film. */
  def billingUnderBanner(members: Seq[EvidenceNode], bills: Set[String], filmOf: String => Option[Int],
                         scored: EvidenceNode => Seq[Scored]): Set[String] = {
    def words(listing: Listing) = services.titles.TitleContainment.tokens(listing.rawTitle).toSet
    val programmes = members.filter(node => bills(node.id)).flatMap(_.listings)
      .filter(listing => MultiFilmBill.marker(Seq(listing.rawTitle, listing.title)).isDefined).map(words)
    if (programmes.isEmpty) Set.empty
    else members.filterNot(node => bills(node.id)).flatMap { node =>
      filmOf(node.id).flatMap(film => scored(node).find(_.candidate.tmdbId == film)).map(_.candidate.film).filter { film =>
        val titled = film.titles.flatMap(services.titles.TitleContainment.tokens).toSet
        node.listings.forall(listing => programmes.exists(programme => words(listing).subsetOf(programme)) && (words(listing) -- titled).nonEmpty)
      }.map(_ => node.id)
    }.toSet
  }

  /** Why the film a member's cluster took is none of that member's: its billing names the film as ONE PIECE beside another
   *  thing, which the venue's own words around a film do not explain ([[BilledPiece]], the agreement stage's rule too).
   *  `scored`: the member's own scoring of the film, its record and measures. */
  private def billedAsPiece(node: EvidenceNode, scored: Scored, corroborated: Double,
                            recurs: String => Boolean, answered: CandidateQuery => Seq[Hit]): Option[String] = {
    val film = scored.candidate.film
    BilledPiece.of(node.evidence, node.listings, node.country, film, Some(scored.candidate.tmdbId), scored.measures, corroborated, recurs, answered)
      .map(piece => s"no film: one piece of the billing names '${film.title}'${if (piece.staging) " beside a staging" else ""} — ${piece.why}")
  }
  /** The members QUOTING their cluster's film within words of their own, at a venue another member of the cluster bills
   *  the film by itself at — that very title, unquoted, no bill: what that venue screens beside the film — a warm-up, a talk — not the film. The quotes name
   *  the work (PL "Festiwal Korelacje: „Zemsta” z autorską narracją Artura Andrusa", [[ListingShape.billedBeside]]),
   *  but a venue billing the work on its own as well bills this as another thing: DE Kleines Theater am Weingarten's
   *  "Mystery-Vorfilm als Warm-up zu „Miles Davis – Birth of The Cool“" beside its "MILES DAVIS: BIRTH OF THE COOL". */
  def besideItsScreening(members: Seq[EvidenceNode]): Seq[(EvidenceNode, String)] = {
    // the film quoted mid-sentence, a word of the venue's running into the quote ("… als Warm-up zu „…“"): a quote heading
    // the billing ("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe") or after a break names the evening's film
    def quotedMidSentence(node: EvidenceNode): Seq[String] =
      IdentityMeasures.QuotedTitle.findAllMatchIn(node.evidence.rawTitle).toSeq
        .filter(m => node.evidence.rawTitle.take(m.start).trim.lastOption.exists(_.isLetter)).map(m => IdentityMeasures.key(m.group(1)))
    members.flatMap { node =>
      val quoted = quotedMidSentence(node).toSet
      val venues = node.listings.map(_.cinema).toSet
      Option.when(quoted.nonEmpty)(()).flatMap(_ => members.find(other => (other ne node) &&
          ListingShape.Quote.findFirstIn(other.evidence.rawTitle).isEmpty && quoted(IdentityMeasures.key(other.evidence.rawTitle)) &&
          other.listings.exists(listing => venues(listing.cinema))))
        .map(other => node -> s"no film: it quotes the film its venue bills by itself as '${other.evidence.rawTitle}'")
    }
  }
}
