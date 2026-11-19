package services.identity.agreement

import services.identity.{Answer, BilledPiece, DetailFacts, ProseWords, Question,
  Listing, PosterEvidence, ResolverDecision}

import scala.collection.mutable

/** The stage's takes of a cluster the model left unmatched: the film the families agree on, a fill rule's, the venue
 *  posters' vote, the broadcast its screening days name and its catalogue ids' — each refused where a venue's synopsis
 *  names another's description, or its billing names the film as one piece beside another thing. Each reads through
 *  `evidence` ([[AgreementEvidence]]) and the cluster's `candidates`, by the selected `rules`; a film TMDB holds no record of stands
 *  on the first of `identities` that names it, and the listings' catalogue ids are read from `catalogue`. */
private[agreement] final class AgreementTakes(evidence: AgreementEvidence, candidates: AgreementCandidates, rules: services.identity.UnifiedRules,
                                             identities: Seq[VoterFamily], catalogue: services.identity.CatalogueAnswers,
                                             tmdbOf: String => Answer[Option[Int]]) extends ClusterFill {
  import evidence.{looked, venues}
  import candidates.{candidatesOf, measured, productionsOf, titledFilms, titlePicks, undatedOf, uncatalogued}
  import AgreementTakes.taking

  /** The clusters whose agreed take the last apply left pending ([[AgreementTakes.Take.Pending]]: a venue poster not
   *  hashed, an IMDb id not found in TMDB yet), with what it asked: the model's take is served meanwhile, typed Pending
   *  ([[StageWaits.outcomeOf]]). UK "Hocus Pocus", PL "Siostry (1972)" (drift slices): agreed, then after a restart pending on the
   *  posters of the candidates it read again, the model's no film served. */
  val takeWaits = scala.collection.concurrent.TrieMap.empty[String, ClusterWait]

  /** Forget the pending takes of every cluster but those `seen`. */
  def forgetAllBut(seen: collection.Set[String]): Unit = takeWaits --= takeWaits.keySet.toSet -- seen

  /** What the cluster `id` (its sorted `listings` digesting to `digest`), a model no-match, is taken as on the families'
   *  `verdict` — the decision as the model left it while there is none. What its take asks — its posters, its finds — is
   *  kept apart, so a take pending on them names them ([[takeWaits]]), and noted in `pass`. */
  def verdictTake(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: Option[StoredVerdict],
                  pass: AgreementPass): ResolverDecision = {
    val asking = mutable.Set.empty[Question]
    takeWaits -= id
    // the venue posters' distances to the cluster's candidates and to the film the families agree on, read through the
    // apply's memo, as a correction's are: each poster decoded once an apply, not once a take
    val distances: AgreementTakes.Distances = also => candidates.posterDistances(id, digest, listings, also, asking, hashes = pass.reading.posters)
    // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
    val took = verdict.fold(decision)(v => takeOfVerdict(AgreementTakes.Unagreed(decision, id, digest, listings, v, distances, asking), pass))
    pass.asked ++= asking
    took
  }

  /** The take of the families' verdict on `cluster` ([[verdictTake]]): the film they agree on, unless a venue poster vetoes it, the venue's
   *  synopsis refuses it, or an answer it waits on is not in (pending); else the first of [[AgreementTakes.FallThrough]]
   *  to take one, and the line saying why none did. */
  private def takeOfVerdict(cluster: AgreementTakes.Unagreed, pass: AgreementPass): ResolverDecision = {
    val AgreementTakes.Unagreed(decision, id, digest, listings, v, _, asking) = cluster
    pieceOf(v, listings).fold(agreedTake(cluster) match {
      // a screen adaptation agreed for a listing naming a stage work ([[Agreement.agreed]]) yields to the relay its
      // screening days name: PL Kino Amok's bare "Manon" on the Met's broadcast day is the Met's, not Clouzot's
      case AgreementTakes.Take.Taken(agreed) if listings.exists(services.identity.ListingShape.stagesAWork) =>
        ruleTake(AgreementTakes.BroadcastTake, cluster).getOrElse(agreed)
      case AgreementTakes.Take.Taken(agreed) => agreed
      case AgreementTakes.Take.Pending       => takeWaits(id) = ClusterWait(digest, asking.toSet); decision
      case AgreementTakes.Take.Vetoed        => pass.posterVetoed += 1; fellWaiting(cluster)
      // the venue's synopsis naming a namesake's description says why, where nothing else takes one
      case AgreementTakes.Take.Refused(why)  =>
        val fell = fellWaiting(cluster)
        if (fell eq decision) taking(decision)(decision.basis, why) else fell
      // the guard that refused the film the families would agree on says why, where nothing else takes one
      case AgreementTakes.Take.Untaken       =>
        val fell = fellWaiting(cluster)
        v.refused.filter(_ => fell eq decision).fold(fell)(why => taking(decision)(decision.basis, why))
    })(line => taking(decision)(decision.basis, line))
  }

  /** What became of the film the families agree on for `cluster` ([[taken]]): none to take where they agree on none. */
  private def agreedTake(cluster: AgreementTakes.Unagreed): AgreementTakes.Take[ResolverDecision] = {
    val AgreementTakes.Unagreed(decision, id, digest, listings, v, distances, asking) = cluster
    v.agreed.fold[AgreementTakes.Take[ResolverDecision]](AgreementTakes.Take.Untaken)(taken(decision, _, asking, distances,
      film => synopsisAgainst(id, digest, listings, film)))
  }

  /** The first of the fall-through takes to take a film for `cluster` ([[fellThrough]]) — and where none did while one of
   *  them waits on a question (a find, a poster, a broadcast's record), the cluster pending on it, as an agreed take is
   *  (PL "SEVENTEEN World Tour": filled by IMDb once TMDB's find of its id was filed, read as no film before it; the
   *  property "refine each take as answers are only filed"). */
  private def fellWaiting(cluster: AgreementTakes.Unagreed): ResolverDecision = {
    val fell = fellThrough(cluster)
    if ((fell eq cluster.decision) && cluster.asking.nonEmpty) takeWaits(cluster.id) = ClusterWait(cluster.digest, cluster.asking.toSet)
    fell
  }


  /** The film the first selected FILL rule takes for a cluster the families agreed on nothing for
   *  ([[services.identity.UnifiedRules.filled]] over [[services.identity.UnifiedEvidence.contenders]]: the cluster's own
   *  TMDB candidates, none denied, and the films the families took or lean to) — `Unknown` while a TMDB answer the
   *  cluster's candidates need is a gap. The venue posters' veto is read when it is taken ([[filledTake]]). Everything it
   *  reads is noted in `receipt`, the verdict's. */
  def filledOf(decision: ResolverDecision, listings: Seq[Listing], verdicts: Seq[FamilyVerdict], stated: Seq[Listing],
                       thisYear: Int, asked: mutable.Set[Question], receipt: mutable.Map[services.identity.Dependency, Long]): Answer[Option[StoredFill]] =
    looked.filter(_ => rules.fill.nonEmpty).fold[Answer[Option[StoredFill]]](Answer.Known(None)) { lookups =>
      val noting  = new AgreementEvidence.UnknownNoting(lookups)
      val nodes  = evidence.nodesOf(listings, noting)
      if (noting.unknown) Answer.Unknown
      else {
        // the `broadcast.take` signal read as the stage's own take reads the join: the same productions, the same wait
        val cluster = services.identity.UnifiedEvidence.ClusterEvidence(listings, decision, nodes, verdicts, Nil, _ => None, thisYear, stated,
          Some(measured), venueNames(listings), film => lookups.cast(film).answered.flatten,
          productions = () => productionsOf(listings, measured, asked, Some(receipt)), undated = { film =>
            val attempt = services.identity.Dependency.Attempted(Question.Record(film))
            evidence.readNow(attempt).foreach(receipt(attempt) = _)
            undatedOf(lookups)(film)
          })
        Answer.Known(rules.filled(services.identity.UnifiedEvidence.contenders(cluster)).map { case (film, rule) =>
          StoredFill(rule, film.tmdb, film.imdb, rules.explain(film, rule)) })
      }
    }

  /** The people the listings' VENUES name — each listing's own synopsis and cast, and its venue's detail page's — that the
   *  cast evidence reads ([[services.identity.CastEvidence]]): none of a listing whose facts a feed catalogue states, whose
   *  text describes the catalogue's own entry, maybe another film than the venue's. */
  private def venueNames(listings: Seq[Listing]): services.identity.VenueNames =
    venueStated(listings).foldLeft(services.identity.VenueNames.None) { case (named, (listing, page)) =>
      named ++ listing.names ++ page.fold(services.identity.VenueNames.None)(_.names)
    }

  /** The listings whose text is their VENUE's own — none whose facts a feed catalogue states, its text describing the
   *  catalogue's entry ([[services.identity.SourceReliability]]) — each with its detail page's facts, where read. */
  private def venueStated(listings: Seq[Listing]): Seq[(Listing, Option[DetailFacts])] =
    listings.filter(services.identity.SourceReliability.decides(_, services.identity.SourceReliability.Fact.Names))
      .map(listing => listing -> venues.detail(listing).answered.flatten)

  /** The decision with the film a fill rule took — pending while TMDB was not asked about its IMDb id yet or the venue
   *  posters are not all hashed, and not taken when a venue poster names another candidate ([[PosterEvidence.veto]]) or
   *  the venue's synopsis another's description (`synopsis`, [[synopsisAgainst]]). */
  private def filledTake(decision: ResolverDecision, verdict: StoredVerdict, finding: mutable.Set[Question],
                         distances: AgreementTakes.Distances, synopsis: Int => Option[String]): Option[ResolverDecision] =
    verdict.filled.flatMap { fill =>
      // a find given up on (Failed) names no TMDB film: the fill stands on its IMDb id (fix 4: decided on, never awaited)
      evidence.tmdbFilmOf(fill.tmdb, fill.imdb, finding).failedAs(None) match {
        case Answer.Unknown => None
        case Answer.Failed  => None // failedAs leaves none
        case Answer.Known(tmdbFilm) => screened(tmdbFilm, distances, synopsis)(()) match {
          case AgreementTakes.Take.Taken(_) =>
            val rule = Some(s"fill:${fill.rule}")
            tmdbFilm.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, film = Some(id), rule = rule))
              .orElse(fill.imdb.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, rule = rule,
                fallback = Some(ResolverDecision.Fallback(VoterFamily.Imdb.database, id, 1.0)))))
          case _ => None
        }
      }
    }
  /** Why the film the families agree on is none of the cluster's: its billing names it as ONE PIECE beside another thing
   *  ([[BilledPiece.ofFamilies]]) — DE "Femme - Frauen heilen die Welt" is a documentary, not the 2023 "Femme". Then the
   *  families' best match is that piece's namesake, and no fill or poster is asked for a film either: what the billing
   *  names is no film the families found. */
  private def pieceOf(verdict: StoredVerdict, listings: Seq[Listing]): Option[String] =
    verdict.agreed.flatMap { agreed =>
      val film = agreed.record.film
      BilledPiece.ofFamilies(listings, film, agreed.crossId("tmdb").flatMap(_.toIntOption))
        .map(piece => s"no film: the families agree on '${film.title}', one piece of the billing${if (piece.staging) " beside a staging" else ""} — ${piece.why}")
    }

  /** The decision with the agreed film taken — pending while TMDB was not asked about its IMDb id yet or the venue posters
   *  are not all hashed, and not taken when they VETO it ([[PosterEvidence.veto]]): a venue poster names another candidate;
   *  nor when the venue's synopsis names another's description (`synopsis`, [[synopsisAgainst]]): REFUSED, saying why. */
  private def taken(decision: ResolverDecision, agreed: AgreedFilm, finding: mutable.Set[Question],
                    distances: AgreementTakes.Distances, synopsis: Int => Option[String]): AgreementTakes.Take[ResolverDecision] = {
    val imdb = agreed.crossId("imdb")
    // read only for a film the families' take may stand on: a find only review sites' agreement waits on is none to ask
    lazy val tmdb = evidence.tmdbFilmOf(agreed.crossId("tmdb").flatMap(_.toIntOption), imdb, finding)
    val leaning = Option.when(agreed.leaning.nonEmpty)(s", ${agreed.leaning.toSeq.map(_.label).sorted.mkString(", ")} leaning to it").getOrElse("") +
      Option.when(agreed.corroborated.nonEmpty)(s", corroborated by ${agreed.corroborated.toSeq.sorted.mkString(", ")}").getOrElse("")
    val line = s"${agreed.families.toSeq.map(_.label).sorted.mkString(", ")}$leaning agree on ${agreed.record.film.titled}${imdb.fold("")(id => s" $id")}"
    val ids  = agreed.ids.map { case (family, id) => family.label -> id }
    // a film only review sites take — their pages name no film a card stands on, and match an event's namesake (DE "André
    // Rieus Weihnachtskonzert 2026: Let it Snow": Metacritic's and RT's 2019 "Let It Snow", the model's best candidate)
    if (!agreed.families.exists(_.namesFilms)) AgreementTakes.Take.Untaken
    else tmdb.failedAs(None) match {
      case Answer.Unknown | Answer.Failed => AgreementTakes.Take.Pending // failedAs leaves no Failed
      case Answer.Known(Some(film)) => screened(Some(film), distances, synopsis)(taking(decision)(ResolverDecision.Basis.Agreed, line, film = Some(film), agreed = ids))
      case Answer.Known(None) => identityOf(agreed).fold[AgreementTakes.Take[ResolverDecision]](AgreementTakes.Take.Untaken) { case (source, id) =>
        screened(None, distances, synopsis)(taking(decision)(ResolverDecision.Basis.Agreed, line,
          fallback = Some(AgreementTakes.fallbackOn(source, id, Some(agreed.record.film.title), agreed.record.film.year)), agreed = ids)) }
    }
  }

  /** `take` of `film` as the venues' evidence lets it stand — what an agreed film and a filled one are both screened by:
   *  pending while a venue poster is not hashed, VETOED where one names another candidate ([[PosterEvidence.veto]]),
   *  REFUSED where the venue's synopsis names another's description (`synopsis`, [[synopsisAgainst]]), else taken. */
  private def screened[A](film: Option[Int], distances: AgreementTakes.Distances, synopsis: Int => Option[String])(take: => A): AgreementTakes.Take[A] =
    // a poster given up on (Failed) is no evidence for or against: as no distance found
    distances(film).failedAs(Nil) match {
      case Answer.Known(found) =>
        if (PosterEvidence.veto(film, found).nonEmpty) AgreementTakes.Take.Vetoed
        else film.flatMap(synopsis).fold[AgreementTakes.Take[A]](AgreementTakes.Take.Taken(take))(AgreementTakes.Take.Refused(_))
      case Answer.Unknown | Answer.Failed => AgreementTakes.Take.Pending // failedAs leaves no Failed
    }

  /** The id a film TMDB holds no record of stands on: its IMDb id, else the own id of the first of [[identities]] that
   *  took it or whose id an agreeing record links — as `(source, id)`, the source a fallback film names. */
  private def identityOf(agreed: AgreedFilm): Option[(String, String)] =
    (VoterFamily.Imdb +: identities).distinct.iterator.flatMap(family =>
      agreed.ids.get(family).orElse(agreed.crossId(family.database)).map(family.database -> _)).nextOption()

  /** Why the venues' own synopses refuse `film` ([[SynopsisEvidence.veto]]): they name, among the TMDB descriptions of the
   *  cluster's candidates, another's over its own. None where no venue text or no description of `film` is known: the
   *  descriptions are read as the cast is, never waited on. The venue's words are those of its listings and their detail
   *  pages, none of a listing whose facts a feed catalogue states, as [[venueNames]] reads them. */
  private def synopsisAgainst(id: String, digest: Long, listings: Seq[Listing], film: Int): Option[String] =
    looked.flatMap { lookups =>
      val venue = venueStated(listings).foldLeft(ProseWords.None) { case (words, (listing, page)) =>
        words ++ listing.words ++ page.fold(ProseWords.None)(_.words)
      }
      Option.when(venue.nonEmpty)(()).flatMap(_ => descriptionIn(lookups, film)).flatMap { own =>
        // the cluster's candidates, else what its titles' searches answered: a search not answered yet (a gap the model's
        // resolve met) leaves the candidates unread, and a namesake its other searches found is a namesake still
        val namesakes = Some(candidatesOf(id, digest, listings)).filter(_.nonEmpty).getOrElse(titledFilms(listings, answeredOnly = true))
          .distinct.filter(_ != film)
          .flatMap(other => descriptionIn(lookups, other).map(other -> _))
        val people = ProseWords.ofWords(lookups.cast(film).answered.flatten.getOrElse(Nil) ++ recordIn(lookups, film).flatMap(_.directors).getOrElse(Nil))
        val titles = ProseWords.ofWords(listings.flatMap(listing => Seq(listing.rawTitle, listing.title, listing.cleanTitle)) ++
          (film +: namesakes.map(_._1)).flatMap(recordIn(lookups, _).toSeq.flatMap(_.titles)))
        SynopsisEvidence.veto(venue, own, people, namesakes, titles).map { case (other, shared) =>
          def titled(of: Int) = recordIn(lookups, of).fold(s"tmdb $of")(_.titled)
          s"no agreement on ${titled(film)}: the venue's synopsis names $shared words of ${titled(other)}'s TMDB description and none of its own"
        }
      }
    }

  /** TMDB's description of `film` as `lookups` read it, if known and not empty. */
  private def descriptionIn(lookups: services.identity.IdentityLookups, film: Int): Option[ProseWords] =
    lookups.overview(film).answered.flatten.filter(_.nonEmpty)

  /** TMDB's record of `film` as `lookups` read it, if known. */
  private def recordIn(lookups: services.identity.IdentityLookups, film: Int): Option[services.identity.IdentityMeasures.Film] =
    lookups.film(film).answered.flatten

  /** The decision with the film its listings' catalogue ids name taken ([[Catalogue.take]]) — none while a question it
   *  needs is open, each noted to ask. */
  private def catalogued(decision: ResolverDecision, listings: Seq[Listing], asked: mutable.Set[Question]): Option[ResolverDecision] =
    Catalogue.take(listings, listings.map(evidence.asStated), catalogue, evidence.families, tmdbOf, () => titlePicks(listings), asked, evidence.attempted)
      .answered.flatten.map { taken =>
      taken.film.fold(taking(decision)(ResolverDecision.Basis.Catalogue, taken.line,
        fallback = taken.fallback.map { case (source, id) => AgreementTakes.fallbackOn(source, id, taken.title, taken.year) }))(film =>
        taking(decision)(ResolverDecision.Basis.Catalogue, taken.line, film = Some(film)))
    }

  /** The decision with the one candidate the venue posters match taken ([[PosterEvidence.vote]]). */
  private def voted(decision: ResolverDecision, distances: Answer[Seq[Map[Int, Option[Int]]]]): Option[ResolverDecision] =
    distances.answered.flatMap(PosterEvidence.vote).map { case (film, bits) =>
      val named = looked.flatMap(_.film(film).answered.flatten).fold(s"tmdb $film")(_.titled)
      taking(decision)(ResolverDecision.Basis.Poster,
        s"the venue's poster matches $named ($bits bits), no other candidate's within ${PosterEvidence.VoteBits}", film = Some(film))
    }

  /** The decision with the one record the cluster's screening days name taken ([[Broadcast]]): of the TMDB films its own
   *  evidence reaches, none denied — none while one of their records is not known yet, nor while a record of the billed
   *  work states no day only because the store filed it with its year alone: that record is noted in `asked`, to be
   *  read again. */
  private def broadcast(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing],
                        asked: mutable.Set[Question]): Option[ResolverDecision] =
    looked.filter(_ => listings.exists(!_.screenings.isEmpty)).flatMap { lookups =>
      val records = (candidatesOf(id, digest, listings) ++ uncatalogued(listings, lookups)).distinct.map(film => film -> lookups.film(film))
      // a record given up on is decided without (fix 4b); one not answered yet holds the join
      Option.when(!records.exists(_._2.isAwaited))(records.flatMap { case (film, record) => record.answered.flatten.map(film -> _) })
        .flatMap { known =>
          // a record of the billed work holding only its year is read again before the day decides: it might be the one
          Broadcast.take(listings, measured, known, () => productionsOf(listings, measured, asked))(undatedOf(lookups))
            .left.map(asked ++= _.map(Question.Record(_))).toOption.flatten
        }
        .map(taken => taking(decision)(ResolverDecision.Basis.Broadcast, taken.line, film = Some(taken.film)))
    }

  /** The takes a cluster the families' agreement took no film for is read by, by name — in [[AgreementTakes.FallThrough]]'s
   *  order, the first to take one deciding: each reads its own evidence and notes in the cluster's `asking` every question
   *  it waits on. */
  private[agreement] lazy val takes: Map[String, AgreementTakes.Unagreed => Option[ResolverDecision]] = Map(
    AgreementTakes.PosterVoteTake -> (u => voted(u.decision, u.distances(None))),
    AgreementTakes.BroadcastTake  -> (u => broadcast(u.decision, u.id, u.digest, u.listings, u.asking)),
    AgreementTakes.FillTake       -> (u => filledTake(u.decision, u.verdict, u.asking, u.distances,
      film => synopsisAgainst(u.id, u.digest, u.listings, film))),
    AgreementTakes.CatalogueTake  -> (u => catalogued(u.decision, u.listings, u.asking)))
  private def ruleTake(rule: String, untaken: AgreementTakes.Unagreed): Option[ResolverDecision] = takes(rule)(untaken)
  private[agreement] lazy val fallThrough = AgreementTakes.FallThrough.map(takes)
  /** The first of [[AgreementTakes.FallThrough]] to take a film for the cluster, else the cluster as the model left it. */
  private def fellThrough(untaken: AgreementTakes.Unagreed): ResolverDecision =
    fallThrough.iterator.flatMap(_(untaken)).nextOption().getOrElse(untaken.decision)
}

private[identity] object AgreementTakes {
  /** The takes a cluster the families agreed on no film for (or one a venue poster vetoed) is read by, IN ORDER: the
   *  first to take a film decides, and none after it is read (nor asks a question). The order is behaviour:
   *  - [[PosterVoteTake]]: the one candidate a venue poster matches within [[PosterEvidence.VoteBits]], none vetoing it;
   *  - [[BroadcastTake]]: the one record of the billed stage work broadcast on a screening day ([[agreement.Broadcast]]);
   *  - [[FillTake]]: the first selected unified fill rule's film ([[services.identity.UnifiedRules.filled]]), unless a venue
   *    poster vetoes it;
   *  - [[CatalogueTake]]: the film the listings' own catalogue ids name ([[agreement.Catalogue]]). Last though an exact id:
   *    where it names another film than a take above, the projection keeps that take, for the measure to list.
   *  A film the families agree on for a listing billing a stage work yields to [[BroadcastTake]] first (PL Kino Amok's bare
   *  "Manon" on the Met's broadcast day is the Met's, not Clouzot's). `identity-resolver.md` §21 lists this order; a spec
   *  holds the two together. */
  val PosterVoteTake = "poster-vote"
  val BroadcastTake  = "broadcast"
  val FillTake       = "fill"
  val CatalogueTake  = "catalogue"
  val FallThrough: Seq[String] = Seq(PosterVoteTake, BroadcastTake, FillTake, CatalogueTake)

  /** A model no-match the families' agreement took no film for, as the takes read it: its cluster id, listings and their
   *  digest, the families' verdict, and its venue posters' distances to its candidates (and to `also`). */
  private[agreement] final case class Unagreed(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: StoredVerdict,
                                   distances: Distances, asking: mutable.Set[Question] = mutable.Set.empty)
  /** A cluster's venue posters' distances to its candidates and to the film given (an agreed or filled one): what a take
   *  is screened by ([[AgreementTakes.screened]]). `Unknown` while a poster is not hashed. */
  private[agreement] type Distances = Option[Int] => Answer[Seq[Map[Int, Option[Int]]]]

  /** What became of the film a cluster's families agree on: taken, pending an answer, vetoed by a venue poster, refused by
   *  the venue's synopsis (and why), or none to take. */
  private[agreement] enum Take[+A] {
    case Taken(decision: A)
    case Pending
    case Vetoed
    case Refused(why: String)
    case Untaken
  }

  /** `d` taken on `basis`, `line` explaining it — with the `film`, `fallback` and `agreed` ids it takes, where they move. */
  private[agreement] def taking(d: ResolverDecision)(basis: ResolverDecision.Basis, line: String, film: Option[Int] = d.film,
                                          fallback: Option[ResolverDecision.Fallback] = d.fallback, agreed: Map[String, String] = d.agreed,
                                          rule: Option[String] = None): ResolverDecision =
    d.copy(film = film, basis = basis, explanation = d.explanation :+ line, fallback = fallback, agreed = agreed)(
      rule.fold(d.trace)(r => d.trace.copy(stage = Some(r))))

  /** A fallback film standing on `source`'s `id`: one standing on another database's id than IMDb's carries its `title`
   *  and `year`, that a link to its page is built of. */
  private[agreement] def fallbackOn(source: String, id: String, title: Option[String], year: Option[Int]): ResolverDecision.Fallback = {
    val named = source != VoterFamily.Imdb.database
    ResolverDecision.Fallback(source, id, 1.0, title.filter(_ => named), year.filter(_ => named))
  }
}
