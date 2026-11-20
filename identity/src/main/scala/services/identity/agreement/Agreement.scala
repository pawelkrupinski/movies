package services.identity.agreement

import services.titles.billing.BillingPieces

import services.identity.{Acceptance, FactRelations, Film, IdentityCalibration, IdentityLookups, IdentityMeasures, IdentityResolver, Listing, ListingShape, PosterEvidence, SourceReliability}
import services.identity.vocabulary.{Answer, Question}
import services.identity.vocabulary.toAnswer
import services.identity.SourceReliability.{Fact, Trust}
import services.titles.{TitleContainment, TitleNormalizer}

import scala.collection.mutable

object Agreement {

  /** How many families must agree: each taking the film, or leaning to it ([[leaningOf]]) beside one that takes it. A
   *  film is weighed for agreement only as a group of families' picks ([[agreed]]), so it always has a taker: leans and
   *  corroboration complete an agreement, never make one alone (PL "Pianista - Kino Konesera": IMDb takes "The
   *  Pianist", Wikidata and Filmweb lean to it). */
  val Quorum = 3
  /** The listing's own published year and director, crediting the film the takers took: a vote of the venue's own. */
  val ListingFacts = "listing"
  /** The listings' own running time — within [[RuntimeSlack]] minutes of a taker's record on every listing stating one —
   *  beside their crediting the film ([[ListingFacts]]): the venue's second vote, so one family's take its year,
   *  director and running time all credit is agreed. Replayed on the unmatched clusters (2026-10-05): DE "Pettersson und
   *  Findus Mitmachkino 2" (IMDb alone, its record undated: the three directors and 59 minutes the venues bill) right,
   *  none wrong. Never a vote alone — IMDb's take of a K-pop tour film its venues bill at its running time, with no
   *  director, is left to the rules for event films. */
  val ListingRuntime = "runtime"
  val RuntimeSlack = 5
  /** How many minutes shorter than the film a venue may state its running time before it rules the film out
   *  ([[runsLongerThanTheListing]]). */
  val RuntimeShort = 10
  /** The facts a feed copied from its catalogue's entry crediting the takers' records — one vote, however many of them
   *  agree ([[factVotes]]). */
  val FeedFacts = "feed"
  /** At least [[WidelyBilled]] venues billing the cluster's title, and the film released this year or last: a current
   *  release, not a one-off event an older namesake fits. */
  val Venues = "venues"
  val WidelyBilled = 3
  /** TMDB's own vote: the film the model's evidence leans to though no rule took it (`ResolverDecision.leaning`), else
   *  the best-ranked candidate it weighed (`ResolverDecision.candidate`) — linked to the film the takers took by a shared
   *  id or its record's facts ([[sameFilm]]), as one family's pick is to another's. PL "Siostry (1972)": IMDb and
   *  Filmweb take De Palma's "Sisters", which the model weighed best at 4.5%. */
  val ModelVote = "tmdb"
  /** A listing's own catalogue naming a taker's film by that family's id (`CatalogueId`, source the family's
   *  [[VoterFamily.database]]): the venue's identification of it. PL "Imago" lists Filmweb's 872645, Chajdas's 2023 film
   *  Filmweb and Metacritic take. */
  val Catalogue = "catalogue"

  /** The film `family` identifies `listings` as — the resolver itself over the family's answers, as the experiment ran
   *  it — or none, with every record it weighed; `Unknown` while a question it asked is not answered yet. */
  def verdict(listings: Seq[Listing], answers: FamilyAnswers, venues: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration): Answer[FamilyVerdict] = {
    val lookups    = new FamilyLookups(answers, venues,
      listings.flatMap(l => Seq(l.title, l.rawTitle, l.cleanTitle) ++ l.originalTitle ++ l.searchTitle).distinct)
    val scored     = IdentityResolver.resolveScored(listings, lookups, normalizer, calibration)
    val resolution = scored.resolution
    if (resolution.unknownQueries > 0 || resolution.unknownFilms > 0) Answer.Unknown
    else resolution.decisions.flatMap(_.film).distinct match {
      case Seq(number) =>
        (for { id <- lookups.idOf(number); answered <- answers.record(id).answered; record <- answered }
          yield FamilyVerdict(answers.family, Some(FamilyPick(answers.family, id, record)), lookups.weighed))
          .toAnswer
      case _ => // none, or the family splits the cluster: no one film
        Answer.Known(FamilyVerdict(answers.family, None, lookups.weighed, leaningOf(scored.evidence, lookups, answers)))
    }
  }

  /** The film a family's evidence LEANS to though it took none: on every listing, its best undenied candidate, at
   *  [[services.identity.Acceptance.LeanMargin]] times the runner-up's probability — the model's own lean
   *  (`Acceptance.leaning`) over the family's search. Never a pick: leans complete a quorum beside a taker, and
   *  a lean to another film is what makes a family's having weighed the film turning it down (PL "Sukienka": RT weighed
   *  "The Dress" at 6.0%, its runner-up at 2.7%; Tempo's IMDb weighed "Tempo" at 2.9% under "Old" at 33.0%). */
  private def leaningOf(nodes: Seq[IdentityResolver.NodeEvidence], lookups: FamilyLookups, answers: FamilyAnswers): Option[SourceRecord] =
    nodes.map { node =>
      val eligible = node.candidates.filterNot(_.denied).sortBy(-_.probability)
      eligible.headOption.filter(best => eligible.lift(1).forall(runnerUp => best.probability >= Acceptance.LeanMargin * runnerUp.probability)).map(_.tmdbId)
    }.distinct match {
      case Seq(Some(number)) => lookups.idOf(number).flatMap(id => answers.record(id).answered.flatten)
      case _                 => None
    }

  /** The film the families' evidence, pooled, agrees on: one family or more takes it (picks join, through any agreeing
   *  record, by a shared cross-id, else by [[equivalent]] facts), and with the families leaning to it and what
   *  corroborates it ([[ListingFacts]], [[ListingRuntime]], [[ModelVote]], [[Venues]]) it holds ≥ [[Quorum]] — that plus one for each family
   *  taking another film the listing's title names, and one for each family that weighed it and turned it down
   *  ([[turnsDown]]: PL "Okładka „Tempo”", a Finnish stage piece, is not "Tempo" (2003), which Filmweb, RT and Wikidata
   *  take and IMDb weighed and turned down for "Old"). No listing's own year or director rules out a taker's record
   *  (DE "Der kleine Maulwurf", the venues' 1968 Miler, not IMDb's and Wikidata's 2011 compilation); the listing's title
   *  names it ([[namesIt]]) — and, where leans or corroboration complete the quorum, is no other film's own
   *  ([[anothersOwnTitle]]); its billing names no namesake's director instead of its own ([[billsAnothersDirector]]); the
   *  listing bills one work ([[ListingShape.billsSeveral]]) and no stage work ([[ListingShape.stagesAWork]]); and no year its
   *  title bills dates it away from a taker's record ([[PosterEvidence.datingApart]]: DE "Weihnachten mit den Drei Herren
   *  2026", a band's Christmas show, is no 1927 "The Three-Sided Mirror" its feed's original title searched), nor does
   *  the venue's own running time run well short of it ([[runsShort]]). The
   *  experiment's one wrong without the title guard: "Akademia Polskiego Filmu: Kino żydowskie w Polsce" → "Znachor"
   *  (1937), whose year and director fit a series' episode. */
  def agreed(listings: Seq[Listing], verdicts: Seq[FamilyVerdict], modelVote: Option[SourceRecord] = None,
             thisYear: Option[Int] = None, stated: Seq[Listing] = Nil): Option[AgreedFilm] =
    weighed(listings, verdicts, modelVote, thisYear, stated).agreed

  /** [[agreed]], with why the film it would otherwise take is refused where a guard naming its reason refuses it: the
   *  venue's running time short of it ([[runsShort]]), or a year the title bills dating it away
   *  ([[PosterEvidence.datingApart]]). */
  def weighed(listings: Seq[Listing], verdicts: Seq[FamilyVerdict], modelVote: Option[SourceRecord] = None,
              thisYear: Option[Int] = None, stated: Seq[Listing] = Nil): Weighed = {
    // what the venues' own pages add to the listings votes and names films; it never rules one out — a page's year is
    // often its re-release's (PL Kinoteka "Ghost in the shell": 2026 on the page of the 1995 film)
    val asStated = if (stated.nonEmpty) stated else listings
    val picks = verdicts.flatMap(_.pick).sortBy(_.family.ordinal)
    // a family taking a film the listing's title does not name is no evidence on the listing's (US "A Night at the
    // Opera": Wikidata's "The Old Maid", by the director the venue credits)
    // nor is one only the feed's copied original title names where that copy is a near-spelling of the billing: what the
    // feed's search for the billing hit, restated, not a title of the venue's (DE Filmforum Landsberg's "Ganes", the Ladin
    // band: kinoheld copied "Ganges" and its credit from the 2019 documentary its search hit)
    val named = filmsOf(picks).filter(group => asStated.nonEmpty &&
      asStated.forall(listing => namesIt(if (copiesItsSearchHit(listing)) listing.copy(originalTitle = None) else listing, group.map(_.record.film))))
    // each group is the picks of one film, so every film weighed has a taker: a lean or a corroboration alone makes none
    named.map(group => supported(listings, asStated, group, verdicts, modelVote, thisYear)).sortBy(film => -film.support).headOption.flatMap { film =>
      val dissent = named.filterNot(_.exists(pick => film.agreed.families(pick.family))).map(_.size).sum
      val taken = film.records.take(film.agreed.families.size)
      Option.when(stands(listings, film, taken, dissent, verdicts)) {
        refusal(listings, taken, thisYear).fold(Weighed(Some(film.agreed), None))(why => Weighed(None, Some(why)))
      }
    }.getOrElse(Weighed(None, None))
  }

  /** What the families' evidence agrees on ([[weighed]]): the film, or none — and then why, where a guard naming its
   *  reason refused the film it would take. */
  final case class Weighed(agreed: Option[AgreedFilm], refused: Option[String])

  /** A film one family or more took, as the evidence stands for it: the takers, the families leaning to it, what
   *  corroborates it, and how many families weighed it and turned it down ([[turnsDown]]). */
  private final case class Supported(agreed: AgreedFilm, records: Seq[SourceRecord], turnedDown: Int) {
    /** How many families take it, lean to it or corroborate it. */
    val support: Int = agreed.families.size + agreed.leaning.size + agreed.corroborated.size
    /** Short of [[Quorum]] takers, completed by leans or corroboration. */
    def completed: Boolean = agreed.families.size < Quorum
  }

  private def supported(listings: Seq[Listing], stated: Seq[Listing], group: Seq[FamilyPick], verdicts: Seq[FamilyVerdict], modelVote: Option[SourceRecord],
                        thisYear: Option[Int]): Supported = {
    val lead    = group.head
    val records = group.map(_.record)
    // the model's film is theirs only when none of their own records holds it apart: an undated record sharing its title
    // is no link where a dated one is years off (DE "Archiv Negativ: Turksib": RT's undated page of Turin's 1929 film matched
    // TMDB's 1986 "На перевале", alternatively "Turksib", whose ids then named the 1929 film IMDb and Filmweb took)
    // takers whose records all date and credit nothing join any namesake by its title; their own ids are all that say
    // which entry they took, so the vote naming another IMDb or TMDB id is another film ([[votesAnotherEntry]])
    val voted   = modelVote.filter(vote => SameFilm.joins(records, vote) && !SameFilm.votesAnotherEntry(records, vote))
    // the takers' ids first, then those of the TMDB film the model votes for, which is the film they took
    val merged  = lead.record.copy(crossIds = (voted.toSeq ++ records).flatMap(_.crossIds).toMap ++ lead.record.crossIds)
    // the film through any record of it — the model's vote's too, which links a lean the takers' records name only in
    // another language (PL "Demony": IMDb's "Demons", by the IMDb id TMDB's "Демони" carries beside Filmweb's title)
    def isIt(record: SourceRecord) = (records ++ voted).exists(SameFilm.sameFilm(_, record))
    val leaning = verdicts.filter(verdict => verdict.pick.isEmpty && verdict.leaning.exists(isIt)).map(_.family).toSet
    val catalogued = listings.exists(_.catalogueIds.exists(id => group.exists(pick => pick.family.database == id.source && pick.id == id.id)))
    val corroborated = factVotes(stated, records) ++ Set(ModelVote).filter(_ => voted.nonEmpty) ++ Set(Catalogue).filter(_ => catalogued) ++
      Set(Venues).filter(_ => listings.map(_.venue).distinct.size >= WidelyBilled &&
        thisYear.exists(year => records.flatMap(_.film.year).maxOption.exists(_ >= year - 1)))
    val turnedDown = verdicts.count(turnsDown(_, listings, records, isIt))
    val leant = verdicts.filter(verdict => verdict.pick.isEmpty).flatMap(_.leaning).filter(isIt)
    Supported(AgreedFilm(group.map(_.family).toSet, merged, group.map(pick => pick.family -> pick.id).toMap, leaning, corroborated), records ++ leant,
      turnedDown)
  }

  /** `answer` to `question`, or -- not answered though `attempted` (it failed, or came to nothing durable) -- `Failed`:
   *  read as no evidence where it is decided on ([[Answer.failedAs]]), the take decided without it (the settle rule,
   *  docs/design/identity-questions.md step 4). THE one place an answer not given yet is decided on as if given up -- the
   *  stage's reads ([[AgreementStage.settled]]), the catalogue's ([[Catalogue]]), TMDB's finds ([[tmdbFound]])
   *  and the house productions ([[AgreementCandidates.productionsOf]]) alike: an attempt still inside its retries is
   *  read as Failed too (docs/design/identity-structural-fixes.md, fix 4; fix 1 removes it). The caller notes the
   *  question asked all the same, so it is asked on every pass and read again once answered. */
  def settledBy[A](attempted: Question => Boolean)(question: Question, answer: Answer[A]): Answer[A] =
    if (answer == Answer.Unknown && attempted(question)) Answer.Failed else answer

  /** The TMDB film TMDB finds by the IMDb id `tt`, the find noted in `finding` while it is not answered yet — `Unknown`
   *  until it was `attempted` (the settle rule, docs/design/identity-questions.md step 4), then none. */
  private[identity] def tmdbFound(tmdbOf: String => Answer[Option[Int]], tt: String, finding: mutable.Set[Question],
                                  attempted: Question => Boolean = _ => false): Answer[Option[Int]] = {
    val answer = tmdbOf(tt)
    // asked every pass; once attempted and not answered, Failed: read as TMDB holding none where it is decided on
    // (Answer.failedAs), the take decided without it
    if (answer == Answer.Unknown) finding += Question.Find(tt)
    settledBy(attempted)(Question.Find(tt), answer)
  }

  /** Did `verdict` weigh the film (`isIt`, its takers' `records`) and turn it down — take none, its evidence leaning to
   *  another film? One family's dissent, as a family taking another film is ([[agreed]]): weighed among films it favours
   *  none of is no evidence against this one (US "Spider Baby": Metacritic's best a Spider-Man film at 5.3%, its next
   *  3.0%); nor is a lean to a film the listing's own year or director rules out (DE "Überleben", Danial Miller's in
   *  2020 by the venue: Filmweb leaning to the 2022 "Survive"), or to an edition of the film itself (UK "Ken Russell's
   *  The Devils presented by Deeper Into Movies": RT leaning to its own undated "Ken Russell's The Devils: The
   *  Director's Cut" page). */
  private[identity] def turnsDown(verdict: FamilyVerdict, listings: Seq[Listing], records: Seq[SourceRecord], isIt: SourceRecord => Boolean): Boolean =
    verdict.pick.isEmpty && verdict.weighed.exists(isIt) && verdict.leaning.exists(lean =>
      !isIt(lean) && !contradictedByAnyListing(listings, lean) && !records.exists(editionOf(lean, _)))

  /** Is `edition` a record of `work` under a qualifier: the same director's, dated no earlier (or undated), its title
   *  carrying one of the work's (four letters at least) as a token run — "Ken Russell's The Devils: The Director's Cut"
   *  of "The Devils"? */
  private def editionOf(edition: SourceRecord, work: SourceRecord): Boolean = {
    val (byEdition, byWork) = (edition.film.directors.getOrElse(Nil), work.film.directors.getOrElse(Nil))
    val carried = TitleContainment.tokens(edition.film.title)
    FactRelations.samePerson(byEdition, byWork) &&
      edition.film.year.forall(year => work.film.year.forall(_ <= year)) &&
      work.film.titles.exists(title => title.length >= 4 && { val words = TitleContainment.tokens(title); words.nonEmpty && carried.containsSlice(words) })
  }

  /** What the listings' own facts vote for the takers' records: [[ListingFacts]] when every listing publishing a year
   *  and a director credits one of them — the same person directing, within a year — and one does (DE "Die Story von
   *  Joanna", Damiano's in 1975 by the venue's own page, is the 1975 film Wikidata and Filmweb took); with
   *  [[ListingRuntime]] beside it when the running times agree too, and then a record dating the film nowhere is credited
   *  by its director and running time (DE "Pettersson und Findus Mitmachkino 2"). Takers' records that credit NO director
   *  are credited instead by every dated listing's year — exactly, the slack being a matching director's — beside the
   *  running times agreeing: both votes or neither. Replayed on the unmatched clusters (2026-10-06): PL "Reporterzy
   *  wolności - film i spotkanie z twórcami" and "HER Docs - To nie jest świat dla ludzi z menopauzą", Filmweb's
   *  director-less records of the year and length the venue's page states, right; none wrong. */
  private[identity] def listingVotes(listings: Seq[Listing], records: Seq[SourceRecord]): Set[String] = {
    val runs       = runsAsTheListing(listings, records)
    val undirected = runs && records.forall(_.film.directors.forall(_.isEmpty))
    val dated      = listings.filter(listing => listing.year.isDefined && (listing.directors.nonEmpty || undirected))
    def credits(listing: Listing) =
      if (undirected) records.exists(_.film.year.exists(listing.year.contains))
      else records.exists { record =>
        (FactRelations.yearsAgree(record.film.year, listing.year) || (runs && record.film.year.isEmpty)) &&
          FactRelations.samePerson(listing.directors, record.film.directors.getOrElse(Nil))
      }
    if (dated.isEmpty || !dated.forall(credits)) Set.empty else Set(ListingFacts) ++ Option.when(runs)(ListingRuntime)
  }

  /** What the listings' facts vote for the takers' records: the venues' own ([[listingVotes]] over the listings whose
   *  venue states them), else — where no venue's facts vote — ONE vote, [[FeedFacts]], when the facts a feed copied from
   *  its catalogue's entry credit them: year, director and running time are then all that entry's one claim of which film
   *  it is, which a family finding the entry's own record only repeats (DE "To The Bone": Wikidata's record of Erin Li's
   *  2014 short, the one Filmstarts' feed copied its facts from). */
  private[identity] def factVotes(listings: Seq[Listing], records: Seq[SourceRecord]): Set[String] = {
    val own = listingVotes(listings.map(SourceReliability.stated(_, Trust.Decide)), records)
    if (own.nonEmpty) own else if (listingVotes(listings.map(SourceReliability.stated(_, Trust.Support)), records).nonEmpty) Set(FeedFacts) else Set.empty
  }

  /** Every running time a record states — one per translation fetched ([[Film.runtimes]]) — and its billed
   *  cuts' ([[FilmCuts]]): the film's IMDb number is its own, else its IMDb cross-id. */
  private def recordRuntimes(record: SourceRecord): Seq[Int] =
    record.film.runtimes.toSeq ++ services.identity.FilmCuts.of(
      if (record.film.imdbNumber != 0) record.film.imdbNumber else record.crossIds.get("imdb").fold(0)(IdentityMeasures.imdbNumber)).map(_.runtime)

  /** Does every listing stating a running time run within [[RuntimeSlack]] minutes of one of a record's ([[recordRuntimes]])
   *  — and one state it? */
  private def runsAsTheListing(listings: Seq[Listing], records: Seq[SourceRecord]): Boolean = {
    val timed = listings.filter(_.runtime.isDefined)
    timed.nonEmpty && timed.forall(listing => records.exists(record => recordRuntimes(record).exists(a => listing.runtime.exists(b => math.abs(a - b) <= RuntimeSlack))))
  }

  /** Does a listing's own year (more than one apart) or director (another person, in either script, sharing no name's
   *  stem — "Marc Donskoi" is "Mark Donskoy": [[FactRelations.otherPeople]]) rule the film out — facts their source may
   *  DECIDE on ([[SourceReliability]]: a venue's)? A feed catalogue's facts are no venue's statement: they rule nothing out
   *  on their own ([[contradictedByTheCatalogue]]). */
  private[identity] def contradictedByTheListing(listings: Seq[Listing], record: SourceRecord): Boolean =
    listings.exists(contradicts(_, record, Trust.Decide))

  /** Do the facts a feed copied from its catalogue's entry onto a listing contradict the film, as a venue's would
   *  ([[contradictedByTheListing]])? The catalogue's claim: evidence for its entry's film, weighed, never a veto. */
  private[identity] def contradictedByTheCatalogue(listings: Seq[Listing], record: SourceRecord): Boolean =
    listings.exists(contradicts(_, record, Trust.Support))

  /** Does any listing's year or director contradict the film, whoever states it: what a lean must not be to dissent, and
   *  what a fill rule's guard reads — rules that weigh nothing against it. */
  private[identity] def contradictedByAnyListing(listings: Seq[Listing], record: SourceRecord): Boolean =
    listings.exists(contradicts(_, record))

  private def contradicts(listing: Listing, record: SourceRecord): Boolean =
    yearAgainst(listing, record) || directorAgainst(listing, record)
  /** [[contradicts]] by the facts the listing's source states with `trust` alone ([[SourceReliability]]). */
  private def contradicts(listing: Listing, record: SourceRecord, trust: Trust): Boolean = {
    val source = SourceReliability.factsOf(listing)
    (source.trust(Fact.Year) == trust && yearAgainst(listing, record)) || (source.trust(Fact.Directors) == trust && directorAgainst(listing, record))
  }

  /** Does a venue state its own running time for the film more than [[RuntimeShort]] minutes SHORTER than every one the
   *  record states ([[recordRuntimes]]) — the time it states and the record's longest, if so? A slot runs the film and
   *  more (adverts, trailers, an intro), never less: DE Neue Post's "WHEN PIGS FLY" at 80 minutes is Riedmayr's 2026
   *  documentary, no 94-minute 1993 film. Longer says nothing (a relay's intervals, a double bill, a talk); nor does a
   *  record of this year or later, whose running time TMDB often holds before the cut is final (UK "Street Fighter",
   *  "Dune: Part Three - See It First Preview"), nor a silent film's, which runs at the speed it is projected (PL "Dni Kina
   *  Niemego - 100 lat temu: SYMFONIA ZMYSŁÓW"). Only a feature's running time a film can have counts
   *  ([[models.FilmRuntime.plausible]], [[SuspectTakes.FeatureMinutes]]), and only the whole film's: a part the
   *  title bills and the record's titles do not ("Shoah – Teil 1", 273 of 566 minutes), or a slot of half the film or less
   *  (a half before an intermission), screens a part of it. The agreement's guard, and the fill's `listing.runsShort`
   *  ([[services.identity.agreement.UnifiedEvidence]]); no lean's dissent reads it. */
  private[identity] def runsShort(listings: Seq[Listing], record: SourceRecord, thisYear: Option[Int]): Option[(Int, Int)] =
    if (!record.film.year.exists(released => released > ListingShape.SilentEraEnd && thisYear.forall(released < _))) None
    else {
      val runs = recordRuntimes(record)
      listings.iterator.filter(listing => SourceReliability.factsOf(listing).trust(Fact.Runtime) == Trust.Decide && !billsAPart(listing, record))
        .flatMap(_.runtime.filter(stated => models.FilmRuntime.plausible(stated) && stated >= services.identity.agreement.SuspectTakes.FeatureMinutes))
        .find(stated => runs.nonEmpty && runs.forall(stated < _ - RuntimeShort) && stated * 2 > runs.max)
        .map(_ -> runs.max)
    }

  /** Does the listing's title bill a part of a work ("Teil 1", "Part II", "część 2", "Vol. 1") that none of the record's
   *  titles bills — a screening of one part of the film the record holds whole? */
  private def billsAPart(listing: Listing, record: SourceRecord): Boolean =
    Seq(listing.rawTitle, listing.title).exists(PartBilled.findFirstIn(_).isDefined) && !record.film.titles.exists(PartBilled.findFirstIn(_).isDefined)
  private val PartBilled = """(?iU)\b(?:teil|part|parte|partie|część|czesc|cz\.|vol\.?|volume|chapter|kapitel)\s*(?:\d+|[ivx]+)\b|\b\d+\.\s*teil\b""".r
  /** Does the listing's year — more than one apart, or its billing as a silent film — rule the film out? A billing as a
   *  silent film dates it to the silent era (DE "STUMMFILM MIT TOBIAS RANK:NOSFERATU", not Eggers' 2024 film). */
  private def yearAgainst(listing: Listing, record: SourceRecord): Boolean =
    FactRelations.yearsApart(record.film.year, listing.year) ||
      ListingShape.silentBillingDenies(record.film.year, ListingShape.billsASilentFilm(listing))
  /** Does the listing credit another director than the film's own ([[FactRelations.otherPeople]])? */
  private def directorAgainst(listing: Listing, record: SourceRecord): Boolean =
    FactRelations.otherPeople(listing.directors, record.film.directors.getOrElse(Nil))

  /** Is the listing's title another film's ORIGINAL title — one a family weighed — while it is none of the agreed film's
   *  records' original titles, only a translation they file? Then the venue may well bill that film by its own name:
   *  PL "Obcy w domu" is "Hider in the House" (1989) in Polish, and the 1986 Polish film IMDb weighed beside it. */
  private[identity] def anothersOwnTitle(listings: Seq[Listing], records: Seq[SourceRecord], verdicts: Seq[FamilyVerdict]): Boolean = {
    val billed = listings.flatMap(l => Seq(l.title, l.cleanTitle)).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    def original(record: SourceRecord) = record.film.originalTitle.map(IdentityMeasures.key).filter(billed)
    val weighed = verdicts.flatMap(_.weighed)
    // any family's record of the film itself counts: Filmweb files no original title for a Polish film (PL "Imago":
    // IMDb's record of Chajdas's film is "Imago" in the original, as is the 2025 short it weighed beside it)
    val itsOwn = records ++ weighed.filter(record => records.exists(SameFilm.sameFilm(_, record)))
    itsOwn.forall(original(_).isEmpty) && weighed.exists(record => original(record).nonEmpty && !records.exists(SameFilm.sameFilm(_, record)))
  }

  /** Does a listing's billing name — beside the film's title — the director of a namesake a family weighed (a film the
   *  title names, not the film itself), and none of the film's own directors? A series' banner then says whose film it
   *  shows: DE Babylon (Mitte)'s "Hitchcock & Chabrol: Blood Relatives" is Chabrol's 1978 film, which Wikidata takes and
   *  IMDb weighed, not Noah Segan's 2022 one the four other families take. A director is named by their surname
   *  ([[Film.directorSurnames]]) where it is no word of that film's titles, which a billing of the title
   *  carries whoever it names. */
  private[identity] def billsAnothersDirector(listings: Seq[Listing], records: Seq[SourceRecord], verdicts: Seq[FamilyVerdict]): Boolean = {
    val namesakes = verdicts.flatMap(_.weighed).filterNot(weighed => records.exists(SameFilm.sameFilm(_, weighed)))
    listings.exists { listing =>
      val billed = TitleContainment.tokens(listing.rawTitle).toSet
      def credited(film: Film) = film.directorSurnames.exists(surname =>
        billed(surname) && !film.titles.exists(title => TitleContainment.tokens(title).contains(surname)))
      // the venue's own synopsis naming a director WHOLE ([[VenueNames.names]]) says whose film it describes as the billing
      // does: UK Southsea's "Unrest" synopsis names Jennifer Brea, director of the 2017 documentary, not Ipson's 2006 horror
      def described(film: Film) = IdentityMeasures.textNamesItsDirector(listing.names.names, film)
      (!records.exists(record => credited(record.film)) && namesakes.exists(namesake => credited(namesake.film) && namesIt(listing, Seq(namesake.film)))) ||
        (!records.exists(record => described(record.film)) && namesakes.exists(namesake => described(namesake.film) && namesIt(listing, Seq(namesake.film))))
    }
  }

  /** The picks as the films they name: a pick joins every film it [[joins]] — through ANY family's record, so a record
   *  that names a film only in another family's terms (Wikidata's crediting "Kukla" as Filmweb does, beside IMDb's id
   *  that IMDb's "Kukla Kesherovic" carries) joins the three, and two films a pick links are one. Each film's picks in
   *  family order. */
  private def filmsOf(picks: Seq[FamilyPick]): Seq[Seq[FamilyPick]] =
    picks.foldLeft(Vector.empty[Vector[FamilyPick]]) { (films, pick) =>
      val (joined, apart) = films.partition(film => SameFilm.joins(film.map(_.record), pick.record))
      apart :+ (joined.flatten :+ pick).sortBy(_.family.ordinal)
    }

  /** Is the listing's original title a feed's copy ([[SourceReliability.copiesOriginalTitle]]) one edit from its billing's —
   *  the record the feed's search for the billing hit, not the venue's own spelling of another title? */
  private[identity] def copiesItsSearchHit(listing: Listing): Boolean = listing.originalTitle.exists { original =>
    val (copy, billed) = (IdentityMeasures.key(original), IdentityMeasures.key(listing.cleanTitle))
    copy != billed && tools.EditDistance.within(copy, billed, 1) && SourceReliability.copiesOriginalTitle(listing)
  }

  /** Does the best supported `film` stand as the families' agreement: its support past the quorum and every dissent —
   *  the families taking others, its turned-down picks, a feed catalogue contradicting a record `taken` — and none of the
   *  listings' own billing against it (another's own title or director, a contradiction, several pieces billed, a stage
   *  work it is no screen adaptation of)? */
  private def stands(listings: Seq[Listing], film: Supported, taken: Seq[SourceRecord], dissent: Int, verdicts: Seq[FamilyVerdict]): Boolean = {
    // a feed catalogue's facts ruling the film out are its claim, one vote against it, not a veto (DE "To The Bone":
    // Filmstarts' feed credits Erin Li's 2014 short to a venue billing Noxon's 2017 feature)
    val catalogueDissent = if (taken.exists(contradictedByTheCatalogue(listings, _))) 1 else 0
    film.support >= Quorum + dissent + film.turnedDown + catalogueDissent && !(film.completed && anothersOwnTitle(listings, film.records, verdicts)) &&
      !billsAnothersDirector(listings, film.records, verdicts) &&
      !taken.exists(contradictedByTheListing(listings, _)) &&
      listings.forall(listing => !ListingShape.billsSeveralBeside(listing, film.agreed.record.film) &&
        (!ListingShape.stagesAWork(listing) || screenAdaptation(listings, film.agreed)))
  }

  /** Why an agreement that stands is refused after all: the venue running it far shorter than a record `taken` runs, or
   *  a listing billing another year than it dates by. */
  private def refusal(listings: Seq[Listing], taken: Seq[SourceRecord], thisYear: Option[Int]): Option[String] = {
    def titled(record: SourceRecord) = s"'${record.film.title}'${record.film.year.fold("")(y => s" ($y)")}"
    taken.iterator.flatMap(record => runsShort(listings, record, thisYear).map { case (stated, runs) =>
      s"no agreement on ${titled(record)}: the venue runs it ${stated}′, more than ${RuntimeShort}′ short of its ${runs}′" }).nextOption()
      .orElse(listings.iterator.flatMap(listing => taken.iterator.flatMap(record => PosterEvidence.datingApart(listing, record.film).map(year =>
        s"no agreement on ${titled(record)}: '${listing.rawTitle}' bills $year"))).nextOption())
  }

  /** The words `listing`'s normalised title keeps ([[namesIt]]): its clean title's and original title's — less what its
   *  search form drops after a "+" bolted on to the title's head. */
  private def keptWords(listing: Listing): Set[String] = {
    val original = listing.originalTitle.toSeq.flatMap(TitleContainment.tokens).toSet
    val clean    = TitleContainment.tokens(listing.cleanTitle).toSet ++ original
    // only a search form that drops what a "+" bolts on after the title's head: one dropping the head drops the film itself
    // (DE '"JAGDSZENEN AUS NIEDERBAYERN" - HEIMATFILM IM WANDEL DER ZEIT', searched as its banner), and one dropping a
    // second title keeps the film's other name out (DE "BARWY OCHRONNE / CAMOUFLAGE", searched as "BARWY OCHRONNE")
    val head = listing.searchTitle.map(_.trim).filter(search => search.nonEmpty && listing.cleanTitle.startsWith(search) &&
      listing.cleanTitle.drop(search.length).trim.startsWith("+"))
    head.map(search => clean.intersect(TitleContainment.tokens(search).toSet ++ original)).getOrElse(clean)
  }

  /** Does the listing's title name one of the films' titles — be it, or carry it whole (four letters at least)? */
  def namesIt(listing: Listing, films: Seq[Film]): Boolean = {
    val own    = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    val titles = films.flatMap(_.titles).distinct
    val raw    = TitleContainment.tokens(listing.rawTitle)
    lazy val ownWords = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(TitleContainment.tokens).toSet
    // the words the normalised title keeps: a film named only by what it drops is named by a decoration ("Lalka
    // PREMIERA", cleaned "Lalka", names no "Premiera") — nor by what its search form drops, an event bolted on after it
    // (UK "Le Moulin + Q&A", searched as "Le Moulin", names no "Q & A")
    lazy val kept = keptWords(listing)
    titles.map(IdentityMeasures.key).exists(own) ||
      titles.exists(title => title.length >= 4 && { val words = TitleContainment.tokens(title)
        words.nonEmpty && raw.containsSlice(words) && (kept.isEmpty || words.exists(kept)) }) ||
      // the film's title with a leading article the listing drops — two words after it at least (DE "Camp der
      // Verlorenen", TMDB's "Das Camp der Verlorenen"; "Devil" is not "The Devil")
      titles.exists { title => val words = TitleContainment.tokens(title); words.sizeIs >= 3 && IdentityMeasures.LeadingArticles(words.head) && ownWords(words.tail) } ||
      titles.exists(namedPieceByPiece(listing.rawTitle, _))
  }

  /** Does `billed` name `title` piece by piece — as many pieces, each of the title's a run of the billed one's words at
   *  its place, four letters at least, and one of them the whole piece? UK "Paddington Bear: Winter Warmers" names IMDb's
   *  "Paddington: Winter Warmers"; "It Follows: Chapter Two" names no "It: Chapter Two". */
  private def namedPieceByPiece(billed: String, title: String): Boolean = {
    val own   = BillingPieces.pieces(billed).map(TitleContainment.tokens)
    val its   = BillingPieces.pieces(title).map(TitleContainment.tokens)
    own.sizeIs >= 2 && own.size == its.size && own.zip(its).forall { case (o, t) => t.nonEmpty && t.mkString.length >= 4 && o.containsSlice(t) } &&
      own.zip(its).exists { case (o, t) => o == t }
  }
  /** The runs of every listing's billing the films' titles are (each as its words, articles aside). */
  def billedSpans(listings: Seq[Listing], films: Seq[Film]): Seq[Seq[String]] =
    services.identity.TitleTextMeasures.spansIn(listings.map(l => TitleContainment.tokens(l.rawTitle)), films)

  /** Does another candidate's titles name MORE of the billing than the film's — every word the film's titles name of it,
   *  and a word beside them — where the billing bills one film under two names? DE Acud Kino's "BARWY OCHRONNE /
   *  CAMOUFLAGE" names Gibalenko's 2026 "Камуфляж" only by its English title, and Zanussi's 1977 "Barwy ochronne",
   *  "Camouflage" in English, by both. A run of words, not one title: [[namedWithinAnother]] reads one title naming
   *  the film's within it. `own`, `others`: spans as [[billedSpans]] finds them. */
  def namesMoreThanIt(own: Seq[Seq[String]], others: Seq[Seq[Seq[String]]]): Boolean = {
    val named = own.flatten.toSet
    named.nonEmpty && others.exists { other => val theirs = other.flatten.toSet; named.subsetOf(theirs) && !theirs.subsetOf(named) }
  }

  /** Is the film named only WITHIN another candidate's title the listings bill whole
   *  ([[services.identity.TitleTextMeasures.namedWithinAnother]], the agreement's readers' name for it)? Over spans
   *  [[billedSpans]] found: `own` the film's, `others` every candidate's, this one's among them. */
  def namedWithinAnother(own: Seq[Seq[String]], others: Seq[Seq[Seq[String]]]): Boolean = services.identity.TitleTextMeasures.namedWithinAnother(own, others)

  /** May a listing naming a stage work show the agreed film, a SCREEN ADAPTATION of it, after all? No listing bills a
   *  house or a season ([[ListingShape.billsAHouse]]), TMDB's own evidence weighs the film best ([[ModelVote]]: the model's
   *  lean or best candidate is it) and its record is no house's season production. US "MOTHER!": Wikidata takes
   *  Aronofsky's film, Metacritic leans to it and so does the model — "Mother" is also an opera's name.
   *  "OPERA-COSI FAN TUTTE" and "ReTransmisje Met: … Così fan tutte" bill a house, so Tinto Brass's film stays no relay's. */
  private def screenAdaptation(listings: Seq[Listing], film: AgreedFilm): Boolean =
    film.corroborated(ModelVote) && !listings.exists(ListingShape.billsAHouse) && film.record.film.season.isEmpty
}
