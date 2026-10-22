package services.identity.agreement

import services.identity.{Answer, BilledPiece, CatalogueQuestion, DetailFacts, ProseWords,
  IdentityResolver, Listing, PosterEvidence, ResolverDecision}

import scala.collection.mutable

/** The stage's takes of a cluster the model left unmatched: the film the families agree on, a fill rule's, the venue
 *  posters' vote, the broadcast its screening days name and its catalogue ids' — each refused where a venue's synopsis
 *  names another's description, or its billing names the film as one piece beside another thing.
 *  Part of [[AgreementStage]], which mixes it in: it reads and writes the stage's own state. */
private[agreement] trait AgreementTakes { this: AgreementStage =>
  /** The listing with the year, directors, running time, original title and countries its venue's own page states where
   *  the listing does not ([[services.identity.Evidence.of]], as the model reads it): what the agreement's listing votes
   *  and title check read, never what rules a film out ([[Agreement.agreed]]). PL "The Taxidermist | Splat!FilmFest" bills no director; its page credits Paulo Nascimento.
   *  Marked as holding its page's facts (`pageFacts`), which are a catalogue's claim where the page is a listings site's
   *  catalogue entry ([[services.identity.SourceReliability.listingsSiteEntry]]: Flicks). */
  private[agreement] def asStated(listing: Listing): Listing =
    if (!venues.hasDetail(listing)) listing
    else venues.detail(listing).toOption.flatten.fold(listing)(listing.withPage)

  /** The film the first selected FILL rule takes for a cluster the families agreed on nothing for
   *  ([[services.identity.UnifiedRules.filled]] over [[services.identity.UnifiedEvidence.contenders]]: the cluster's own
   *  TMDB candidates, none denied, and the films the families took or lean to) — `Unknown` while a TMDB answer the
   *  cluster's candidates need is a gap. The venue posters' veto is read when it is taken ([[filledTake]]). */
  private[agreement] def filledOf(decision: ResolverDecision, listings: Seq[Listing], verdicts: Seq[FamilyVerdict], stated: Seq[Listing],
                       thisYear: Int, version: Long, asked: mutable.Set[(VoterFamily, String)]): Answer[Option[StoredFill]] =
    tmdb.filter(_ => rules.fill.nonEmpty).fold[Answer[Option[StoredFill]]](Answer.Known(None)) { lookups =>
      val noting = new AgreementStage.UnknownNoting(lookups)
      val nodes  = IdentityResolver.evidenceOf(listings, noting, normalizer, calibration)(_ => true)
      if (noting.unknown) Answer.Unknown
      else {
        // the `broadcast.take` signal read as the stage's own take reads the join: the same productions, the same wait
        val evidence = services.identity.UnifiedEvidence.ClusterEvidence(listings, decision, nodes, verdicts, Nil, _ => None, thisYear, stated,
          Some(measured), venueNames(listings), film => lookups.cast(film).toOption.flatten,
          productions = () => productionsOf(listings, measured, asked), undated = undatedOf(lookups, version))
        Answer.Known(rules.filled(services.identity.UnifiedEvidence.contenders(evidence)).map { case (film, rule) =>
          StoredFill(rule, film.tmdb, film.imdb, rules.explain(film, rule)) })
      }
    }

  /** The people the listings' VENUES name — each listing's own synopsis and cast, and its venue's detail page's — that the
   *  cast evidence reads ([[services.identity.CastEvidence]]): none of a listing whose facts a feed catalogue states, whose
   *  text describes the catalogue's own entry, maybe another film than the venue's. */
  private[agreement] def venueNames(listings: Seq[Listing]): services.identity.VenueNames =
    venueStated(listings).foldLeft(services.identity.VenueNames.None) { case (named, (listing, page)) =>
      named ++ listing.names ++ page.fold(services.identity.VenueNames.None)(_.names)
    }

  /** The listings whose text is their VENUE's own — none whose facts a feed catalogue states, its text describing the
   *  catalogue's entry ([[services.identity.SourceReliability]]) — each with its detail page's facts, where read. */
  private[agreement] def venueStated(listings: Seq[Listing]): Seq[(Listing, Option[DetailFacts])] =
    listings.filter(services.identity.SourceReliability.decides(_, services.identity.SourceReliability.Fact.Names))
      .map(listing => listing -> venues.detail(listing).toOption.flatten)

  /** The decision with the film a fill rule took — pending while TMDB was not asked about its IMDb id yet or the venue
   *  posters are not all hashed, and not taken when a venue poster names another candidate ([[PosterEvidence.veto]]) or
   *  the venue's synopsis another's description (`synopsis`, [[synopsisAgainst]]). */
  private[agreement] def filledTake(decision: ResolverDecision, verdict: StoredVerdict, finding: mutable.Set[String],
                         distances: Option[Int] => Answer[Seq[Map[Int, Option[Int]]]], synopsis: Int => Option[String]): Option[ResolverDecision] =
    verdict.filled.flatMap { fill =>
      tmdbFilmOf(fill.tmdb, fill.imdb, finding) match {
        case Answer.Unknown => None
        case Answer.Known(tmdbFilm) => distances(tmdbFilm).toOption.filter(found => PosterEvidence.veto(tmdbFilm, found).isEmpty &&
            tmdbFilm.forall(synopsis(_).isEmpty)).flatMap { _ =>
          val rule = Some(s"fill:${fill.rule}")
          tmdbFilm.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, film = Some(id), rule = rule))
            .orElse(fill.imdb.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, rule = rule,
              fallback = Some(ResolverDecision.Fallback(VoterFamily.Imdb.database, id, 1.0)))))
        }
      }
    }
  /** Why the film the families agree on is none of the cluster's: its billing names it as ONE PIECE beside another thing
   *  ([[BilledPiece.ofFamilies]]) — DE "Femme - Frauen heilen die Welt" is a documentary, not the 2023 "Femme". Then the
   *  families' best match is that piece's namesake, and no fill or poster is asked for a film either: what the billing
   *  names is no film the families found. */
  private[agreement] def pieceOf(verdict: StoredVerdict, listings: Seq[Listing]): Option[String] =
    verdict.agreed.flatMap { agreed =>
      val film = agreed.record.film
      BilledPiece.ofFamilies(listings, film, agreed.crossId("tmdb").flatMap(_.toIntOption))
        .map(piece => s"no film: the families agree on '${film.title}', one piece of the billing${if (piece.staging) " beside a staging" else ""} — ${piece.why}")
    }

  /** The decision with the agreed film taken — pending while TMDB was not asked about its IMDb id yet or the venue posters
   *  are not all hashed, and not taken when they VETO it ([[PosterEvidence.veto]]): a venue poster names another candidate;
   *  nor when the venue's synopsis names another's description (`synopsis`, [[synopsisAgainst]]): REFUSED, saying why. */
  private[agreement] def taken(decision: ResolverDecision, agreed: AgreedFilm, finding: mutable.Set[String],
                    distances: Option[Int] => Answer[Seq[Map[Int, Option[Int]]]], synopsis: Int => Option[String]): AgreementStage.Take = {
    val imdb = agreed.crossId("imdb")
    // read only for a film the families' take may stand on: a find only review sites' agreement waits on is none to ask
    lazy val tmdb = tmdbFilmOf(agreed.crossId("tmdb").flatMap(_.toIntOption), imdb, finding)
    val leaning = Option.when(agreed.leaning.nonEmpty)(s", ${agreed.leaning.toSeq.map(_.label).sorted.mkString(", ")} leaning to it").getOrElse("") +
      Option.when(agreed.corroborated.nonEmpty)(s", corroborated by ${agreed.corroborated.toSeq.sorted.mkString(", ")}").getOrElse("")
    val line = s"${agreed.families.toSeq.map(_.label).sorted.mkString(", ")}$leaning agree on ${agreed.record.film.titled}${imdb.fold("")(id => s" $id")}"
    val ids  = agreed.ids.map { case (family, id) => family.label -> id }
    def unlessVetoed(film: Option[Int])(take: => ResolverDecision): AgreementStage.Take = distances(film) match {
      case Answer.Known(found) =>
        if (PosterEvidence.veto(film, found).nonEmpty) AgreementStage.Take.Vetoed
        else film.flatMap(synopsis).fold[AgreementStage.Take](AgreementStage.Take.Taken(take))(AgreementStage.Take.Refused(_))
      case Answer.Unknown      => AgreementStage.Take.Pending
    }
    // a film only review sites take — their pages name no film a card stands on, and match an event's namesake (DE "André
    // Rieus Weihnachtskonzert 2026: Let it Snow": Metacritic's and RT's 2019 "Let It Snow", the model's best candidate)
    if (!agreed.families.exists(_.namesFilms)) AgreementStage.Take.Untaken
    else tmdb match {
      case Answer.Unknown           => AgreementStage.Take.Pending
      case Answer.Known(Some(film)) => unlessVetoed(Some(film))(taking(decision)(ResolverDecision.Basis.Agreed, line, film = Some(film), agreed = ids))
      case Answer.Known(None) => identityOf(agreed).fold[AgreementStage.Take](AgreementStage.Take.Untaken) { case (source, id) =>
        unlessVetoed(None)(taking(decision)(ResolverDecision.Basis.Agreed, line,
          fallback = Some(AgreementStage.fallbackOn(source, id, Some(agreed.record.film.title), agreed.record.film.year)), agreed = ids)) }
    }
  }

  /** The id a film TMDB holds no record of stands on: its IMDb id, else the own id of the first of [[identities]] that
   *  took it or whose id an agreeing record links — as `(source, id)`, the source a fallback film names. */
  private[agreement] def identityOf(agreed: AgreedFilm): Option[(String, String)] =
    (VoterFamily.Imdb +: identities).distinct.iterator.flatMap(family =>
      agreed.ids.get(family).orElse(agreed.crossId(family.database)).map(family.database -> _)).nextOption()

  /** Why the venues' own synopses refuse `film` ([[SynopsisEvidence.veto]]): they name, among the TMDB descriptions of the
   *  cluster's candidates, another's over its own. None where no venue text or no description of `film` is known: the
   *  descriptions are read as the cast is, never waited on. The venue's words are those of its listings and their detail
   *  pages, none of a listing whose facts a feed catalogue states, as [[venueNames]] reads them. */
  private[agreement] def synopsisAgainst(id: String, digest: Long, listings: Seq[Listing], film: Int): Option[String] =
    tmdb.flatMap { lookups =>
      val venue = venueStated(listings).foldLeft(ProseWords.None) { case (words, (listing, page)) =>
        words ++ listing.words ++ page.fold(ProseWords.None)(_.words)
      }
      def description(other: Int) = lookups.overview(other).toOption.flatten.filter(_.nonEmpty)
      def record(other: Int) = lookups.film(other).toOption.flatten
      Option.when(venue.nonEmpty)(()).flatMap(_ => description(film)).flatMap { own =>
        // the cluster's candidates, else what its titles' searches answered: a search not answered yet (a gap the model's
        // resolve met) leaves the candidates unread, and a namesake its other searches found is a namesake still
        val namesakes = Some(candidatesOf(id, digest, listings)).filter(_.nonEmpty).getOrElse(titledFilms(listings, answeredOnly = true))
          .distinct.filter(_ != film)
          .flatMap(other => description(other).map(other -> _))
        val people = ProseWords.ofWords(lookups.cast(film).toOption.flatten.getOrElse(Nil) ++ record(film).flatMap(_.directors).getOrElse(Nil))
        val titles = ProseWords.ofWords(listings.flatMap(listing => Seq(listing.rawTitle, listing.title, listing.cleanTitle)) ++
          (film +: namesakes.map(_._1)).flatMap(record(_).toSeq.flatMap(_.titles)))
        SynopsisEvidence.veto(venue, own, people, namesakes, titles).map { case (other, shared) =>
          def titled(of: Int) = record(of).fold(s"tmdb $of")(_.titled)
          s"no agreement on ${titled(film)}: the venue's synopsis names $shared words of ${titled(other)}'s TMDB description and none of its own"
        }
      }
    }

  /** The decision with the film its listings' catalogue ids name taken ([[Catalogue.take]]) — none while a question it
   *  needs is open, each noted to ask. */
  private[agreement] def catalogued(decision: ResolverDecision, listings: Seq[Listing], asked: mutable.Set[(VoterFamily, String)], finding: mutable.Set[String],
                         catalogueAsked: mutable.Set[CatalogueQuestion]): Option[ResolverDecision] =
    Catalogue.take(listings, listings.map(asStated), catalogue, families, tmdbOf, () => titlePicks(listings), catalogueAsked, asked, finding)
      .toOption.flatten.map { taken =>
      taken.film.fold(taking(decision)(ResolverDecision.Basis.Catalogue, taken.line,
        fallback = taken.fallback.map { case (source, id) => AgreementStage.fallbackOn(source, id, taken.title, taken.year) }))(film =>
        taking(decision)(ResolverDecision.Basis.Catalogue, taken.line, film = Some(film)))
    }

  /** The decision with the one candidate the venue posters match taken ([[PosterEvidence.vote]]). */
  private[agreement] def voted(decision: ResolverDecision, distances: Answer[Seq[Map[Int, Option[Int]]]]): Option[ResolverDecision] =
    distances.toOption.flatMap(PosterEvidence.vote).map { case (film, bits) =>
      val named = tmdb.flatMap(_.film(film).toOption.flatten).fold(s"tmdb $film")(_.titled)
      taking(decision)(ResolverDecision.Basis.Poster,
        s"the venue's poster matches $named ($bits bits), no other candidate's within ${PosterEvidence.VoteBits}", film = Some(film))
    }

  /** The decision with the one record the cluster's screening days name taken ([[Broadcast]]): of the TMDB films its own
   *  evidence reaches, none denied — none while one of their records is not known yet, nor while a record of the billed
   *  work states no day only because the store filed it with its year alone: that record is noted in `dating`, to be
   *  read again ([[wantedRecords]]). */
  private[agreement] def broadcast(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], dating: mutable.Set[Int],
                        version: Long, asked: mutable.Set[(VoterFamily, String)]): Option[ResolverDecision] =
    tmdb.filter(_ => listings.exists(!_.screenings.isEmpty)).flatMap { lookups =>
      val records = (candidatesOf(id, digest, listings) ++ uncatalogued(listings, lookups)).distinct.map(film => film -> lookups.film(film))
      Option.when(records.forall(_._2.isKnown))(records.flatMap { case (film, record) => record.toOption.flatten.map(film -> _) })
        .flatMap { known =>
          // a record of the billed work holding only its year is read again before the day decides: it might be the one
          Broadcast.take(listings, measured, known, () => productionsOf(listings, measured, asked))(undatedOf(lookups, version))
            .left.map(dating ++= _).toOption.flatten
        }
        .map(taken => taking(decision)(ResolverDecision.Basis.Broadcast, taken.line, film = Some(taken.film)))
    }
}
