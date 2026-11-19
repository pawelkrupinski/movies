package services.identity

/** The cannot-links two nodes' sources draw ([[ConstraintEdges]]): a feed's copied original title alone joining them, a
 *  reused page's copied facts denying a linked film, kept listings beside an overruled link, two feed links naming two
 *  films, a credit standing against the film the other holds. */
private[identity] final class SourcesApart(scoring: CandidateScoring, families: Families, links: TitleLinks, apart: ListingsApart) {
  import links.sanitized

  /** Do two nodes meet only through a feed's copied original title — no billed title or search form of theirs alike, every
   *  original title either carries a feed's copy ([[SourceReliability.copiesOriginalTitle]]) — while `linked`'s TMDB-keyed
   *  feed links a film that `other`, holding none yet, denies by its own facts? The copy is the linked entry's English
   *  title restated, so it joins its namesakes to the link's film, not to the venue's. DE Filmmuseum München's 95-minute
   *  "The Stranger" (Jonah Manibusan) met Cineamo's "Der Fremde" venues only by the "The Stranger" Cineamo copied from
   *  Ozon's 122-minute "L'Étranger" (1429348m), and took that film. Not where `other` already holds another film than the
   *  link's: a venue billing a film's English title, whose operator linked a namesake record (Traum-Kino's "TOGETHER
   *  APART", Andreasstadel's "Departures"), is the copy's film, and that wider form lost 5 right takes to cure CITY 46's
   *  "THE BLACK HOLE" (recording 38713274617, docs/identity-experiments.md). */
  def copiedOriginalJoinsAnotherFilm(linked: EvidenceNode, other: EvidenceNode, linkedFilm: Option[Int], otherFilm: Option[Int]): Boolean =
    scoring.generation.numbersFilmsByTmdbId && FeedLink.linked(linked.listings).exists { case (_, film) =>
      val bothTitles = Seq(linked, other)
      bothTitles.exists(_.evidence.originalTitle.isDefined) &&
        bothTitles.forall(node => node.evidence.originalTitle.isEmpty || node.listings.forall(SourceReliability.copiesOriginalTitle)) &&
        sanitized(linked.evidence.cleanTitle) != sanitized(other.evidence.cleanTitle) && apart.sharedSearchForm(linked, other).isEmpty &&
        !linkedFilm.exists(_ != film) && otherFilm.isEmpty && families.denies(other, film)
    }

  /** Does `other`, holding no film, deny the film `linked`'s TMDB-keyed feed links only by the facts of a REUSED page
   *  ([[Families.readsAsReused]]) — another record's original title, director and year copied onto it? Its title may be
   *  the linked film's word for word, but its facts are that other record's, and a cluster pooling them vetoes the film
   *  for every member. DE Bambi's "De Gaulle Zeit des Widerstands" sits on kinoheld's page of Schauder's "Vater, Sohn
   *  und der Preis des Widerstands" (1675748): joined by its title to E-Werk's, Weltspiegel's and Zeise's billings, which
   *  Cineamo links to Baudry's De Gaulle (1165366m), it denied them their film (recording 528). */
  def reusedPageDeniesLink(linked: EvidenceNode, other: EvidenceNode, linkedFilm: Option[Int], otherFilm: Option[Int]): Boolean =
    scoring.generation.numbersFilmsByTmdbId && otherFilm.isEmpty && FeedLink.linked(linked.listings).exists { case (_, film) =>
      !linkedFilm.exists(_ != film) && !FeedLink.linked(other.listings).exists(_._2 == film) && families.denies(other, film) &&
        families.readsAsReused(other)
    }

  /** Do the TMDB-keyed feeds of the two nodes link two different films, neither link overruled by the title's other
   *  sources ([[FamilyScope.overruled]])? Each operator picked its film's record: two picks are two films, and a title
   *  alone joining them hands one the other's namesake. DE Universum-City Karlsruhe's "Lauras Stern" (cineamo 903746m,
   *  the 2021 film, its 75 minutes) joined by its same title the three venues Cineamo links to the 2004 film (12697m)
   *  and took it (recording 42332112286). An overruled link is the operator's slip against the title's other sources
   *  (DE "Karla", "Momo"): it keeps apart nothing. Billed alike only: two titles meeting by an original title keep the
   *  join, the cluster's other spellings reading through it (DE "L'ÂGE D'OR" ×3 lost Buñuel's film when Studio-Kino's
   *  "Das goldene Zeitalter", linked to a 2025 namesake, was kept apart from its linked sibling). And only a pick each
   *  node's own evidence holds up -- its link's film rated no lower than the other's: DE Neues Arena's "IN THE MOOD FOR
   *  LOVE", Cineamo-linked to a 31-minute 2015 short (646676m), is Wong Kar-wai's film beside Kino im DFF's 843m. */
  def linkedApart(first: EvidenceNode, second: EvidenceNode): Boolean =
    scoring.generation.numbersFilmsByTmdbId && sanitized(first.evidence.cleanTitle) == sanitized(second.evidence.cleanTitle) &&
      ((unoverruledLink(first), unoverruledLink(second)) match {
        case (Some(one), Some(other)) => one != other && holdsUp(first, one, other) && holdsUp(second, other, one)
        case _                        => false
      })
  /** The film `node`'s TMDB-keyed feed links, unless the title's other sources overrule the link. */
  private def unoverruledLink(node: EvidenceNode): Option[Int] = {
    val scope = families.scopeOf(node)
    scope.feedLinked(node).filter(_ => scope.overruled(node).isEmpty).map(_._2)
  }
  /** Does the node's own evidence rate its link's film `own` no lower than the `others`: a pick its own scoring holds up? */
  private def holdsUp(node: EvidenceNode, own: Int, others: Int): Boolean = {
    val scored = families.scopeOf(node).of(node)
    def probability(film: Int) = scored.find(_.candidate.tmdbId == film).filterNot(_.denied).map(_.probability)
    probability(own).exists(mine => probability(others).forall(mine >= _))
  }

  /** Does `node`, taking no film alone, credit a director that the film `other` holds was not directed by (`director`
   *  `different`: no credit of the record's is the same person), its credit standing -- every listing's own statement
   *  ([[SourceReliability.Fact.Directors]] decided), or a credit naming a record of its very title by that director that
   *  the billing's own year or running time fits too (a copied credit then names the film its billing is, not a slip)?
   *  A title alone joining them would lend it a film its own credit contradicts. DE Bambi's "Les Misérables" [2026, 99′]
   *  {Fred Cavayé, kinoheld's} joined ScharfrichterKino's "Les Misérables - Die Geschichte von Jean Valjean" by a title
   *  segment and took Besnard's "Jean Valjean" (1376439), beside Cavayé's own "Les Misérables" (2026, 1241803).
   *
   *  A copied credit with no year or running time beside it is no such statement: DE Mal seh'n Kino's "THE THING WITH
   *  FEATHERS" {Anja Oelofse} carries the credit kinoheld copied from Oelofse's 24-minute short of that title, and is
   *  Southern's film the venue also bills as "Das Ding mit Federn". */
  def creditDeniesHeld(node: EvidenceNode, otherFilm: Option[Int], nodeFilm: Option[Int]): Boolean =
    nodeFilm.isEmpty && otherFilm.exists { held =>
      lazy val scored = families.scopeOf(node).of(node)
      node.listings.forall(_.directors.nonEmpty) &&
        scored.find(_.candidate.tmdbId == held).exists(_.category("director").contains("different")) &&
        (node.listings.forall(SourceReliability.decides(_, SourceReliability.Fact.Directors)) ||
          scored.exists(s => s.candidate.tmdbId != held && IdentityMeasures.sameDirector(s.measures) && s.category("title").contains("exact") &&
            (IdentityMeasures.ownAgreement(s.measures)._1("year") || IdentityMeasures.runtimeGap(s.measures).exists(_ <= Corroboration.RuntimeBand))))
    }

  /** Is `kept` a node of kept listings alone ([[Listing.kept]]) and `other` one whose feed link the present sources
   *  overrule toward another film ([[FamilyScope.overruled]])? The kept facts keep their own film's details alive; they
   *  never join a listing whose own sources say it is another film. DE "Eternity", prod-latest 41265286306: Caligari's
   *  kept kinoheld listing of Freyne's 2025 film joined the bare "Eternity" [2025] Cineamo links to it, which Zazie's and
   *  kinoheld's 2026 entries credit to Meures, and moved 18 venues onto Freyne's film. */
  def keptBesideOverruled(kept: EvidenceNode, other: EvidenceNode): Boolean =
    !Authority.may(Authority.of(kept), Authority.Act.JoinOverruled) && Authority.of(other) != Authority.Provenance.Kept &&
      families.scopeOf(other).overruled(other).isDefined
}
