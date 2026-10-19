package services.identity.agreement

import services.identity.{Acceptance, Answer, CandidateQuery, DetailFacts, FactRelations, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures,
  IdentityResolver, Listing, ListingShape, PosterEvidence}
import services.identity.toAnswer
import services.identity.SourceReliability
import services.identity.SourceReliability.{Fact, Trust}
import services.movies.{TitleContainment, TitleNormalizer}

import scala.collection.mutable

/**
 * The film database families a cluster TMDB matched to nothing may be identified by when several of them, each on its
 * own, identify the same film (signal-combination experiment, 2026-10-04: "≥3 independent families agree, none names a
 * different film, and the listing's title names the film" — 17 right, 0 wrong titles; every family alone added wrongs,
 * 15–45% of its gains). A family's sources mirror each other and count once. Each family's search is weighed with
 * TMDB's calibration, its search priors' spread scaled by `priorSpread` (chosen per source by cross-validation, REPORT
 * §10: 19 right, 0 wrong, against 17 / 0 with TMDB's own spread).
 */
enum VoterFamily(val label: String, val database: String, val priorSpread: Double, val searchesDirectors: Boolean, val latinTitlesOnly: Boolean,
                 val namesFilms: Boolean = true) {
  /** IMDb's own title and name search, and its records (Cinemeta and OMDb mirror it). */
  case Imdb extends VoterFamily("imdb", "imdb", 1.5, searchesDirectors = true, latinTitlesOnly = false)
  /** Wikidata's film items, and the Wikipedia articles that name them. */
  case Wiki extends VoterFamily("wiki", "wikidata", 0.5, searchesDirectors = true, latinTitlesOnly = false)
  /** Filmweb's search and film records — a voter where it indexes the country's titles (PL, DE, ES). */
  case Filmweb extends VoterFamily("filmweb", "filmweb", 0.5, searchesDirectors = false, latinTitlesOnly = false)
  case Metacritic extends VoterFamily("metacritic", "metacritic", 1.0, searchesDirectors = false, latinTitlesOnly = true, namesFilms = false)
  case RottenTomatoes extends VoterFamily("rt", "rt", 1.5, searchesDirectors = false, latinTitlesOnly = true, namesFilms = false)
}
// `database`: the name a record's cross-ids file the family's own ids under (`SourceRecord.crossIds`), and a fallback
// film's source when the family's id is the one a film stands on (`AgreementStage.identities`). `namesFilms`: its record is
// a film database's, not a review site's page — a film only review sites take is never taken (`AgreementStage`).
// What each family is asked, measured on prod's 20,310 answers (2026-10-05, agreement-question-value.md): a director
// search decided an agreed film on IMDb (10) and Wikidata (8), never on Filmweb, Metacritic or Rotten Tomatoes
// (`searchesDirectors`); Rotten Tomatoes' and Metacritic's English searches answer nothing useful for a title with no
// Latin-script word (`latinTitlesOnly`).

/** One film a family's search names: the family's own id for it, and its title and year as the search gave them. */
final case class SourceHit(id: String, title: String, originalTitle: Option[String], year: Option[Int])

/** A family's record of a film: what the identity measures read of it, and the ids other databases know it by
 *  (`"imdb" -> "tt0087843"`, `"tmdb" -> "311"`) — what links two families' films without comparing their facts. */
final case class SourceRecord(film: IdentityMeasures.Film, crossIds: Map[String, String] = Map.empty)

object SourceRecord {
  /** TMDB's record of film `tmdbId`: linked by its TMDB id, and by the IMDb id TMDB names, if any. */
  def ofTmdb(tmdbId: Int, film: IdentityMeasures.Film): SourceRecord =
    SourceRecord(film, Map("tmdb" -> tmdbId.toString) ++ IdentityMeasures.imdbId(film.imdbNumber).map("imdb" -> _))
}

/** One film a venue's programme on a family's site screens — the family's own id for it — and the days it does. */
final case class Showing(film: String, days: services.identity.ScreeningDays)

/** What a family answers, from what it keeps: `Unknown` while a question is not answered yet — a gap, never "no film". */
trait FamilyAnswers {
  def family: VoterFamily
  def titled(text: String): Answer[Seq[SourceHit]]
  /** The films a person of this name directed; `Known(Nil)` for a family with no person search. */
  def directedBy(name: String): Answer[Seq[SourceHit]]
  def record(id: String): Answer[Option[SourceRecord]]
  /** The films the family's site lists a venue screening ([[VenueListings]]): its own programme of the venue, else its
   *  town's; `Known(Nil)` for a family that lists no programmes. */
  def showing(venue: String): Answer[Seq[Showing]] = Answer.Known(Nil)
  /** Is there an answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`, `showing|<venue>`), and is it
   *  still fresh? A stale one is read all the same, and asked again; a missing one is a gap. */
  def fresh(question: String): Boolean = true
}

/** Which family answers were filed since a version of them (`<family>|<kind>|<text>`, as a verdict's reads name them):
 *  `None` when that is not known — the stage then reads a verdict's answers again to tell. */
trait AnswerChanges { def changedSince(version: Long): Option[Set[String]] }
object AnswerChanges { val Unknown: AnswerChanges = _ => None }

/** A family's identification of a cluster: its own id and record of the film it took, when it took one. */
final case class FamilyPick(family: VoterFamily, id: String, record: SourceRecord)

/** What a family made of a cluster: the film it took, if any, every film's record it weighed on the way, and the film its
 *  evidence `leaning` favours though no rule took it ([[Agreement.leaningOf]]) — a family that weighed a film and took
 *  none, leaning to another, has looked at it and turned it down. */
final case class FamilyVerdict(family: VoterFamily, pick: Option[FamilyPick], weighed: Seq[SourceRecord] = Nil,
                               leaning: Option[SourceRecord] = None)

/** The film a cluster's families agree on, with the families that named it, those whose evidence leaned to it, and what
 *  else corroborated it ([[Agreement.ListingFacts]], [[Agreement.ModelVote]]). */
final case class AgreedFilm(families: Set[VoterFamily], record: SourceRecord, ids: Map[VoterFamily, String], leaning: Set[VoterFamily] = Set.empty,
                            corroborated: Set[String] = Set.empty) {
  /** The film's id in `database` ("imdb", "tmdb"), as any agreeing family's record links it. */
  def crossId(database: String): Option[String] = record.crossIds.get(database)
}

/** A family's answers as the resolver's lookups — the family as the film database, as TMDB is to the model. Its ids
 *  are numbered as this resolve meets them (it asks its questions in one sorted order), so the resolve is a function of
 *  the answers; the venues' own detail pages come from `venues`. */
final class FamilyLookups(answers: FamilyAnswers, venues: IdentityLookups, listingTitles: Seq[String] = Nil) extends IdentityLookups {
  private val numbers = mutable.LinkedHashMap.empty[String, Int]
  private val named   = mutable.HashMap.empty[Int, SourceHit]
  /** Each numbered film's best place in a search that named it: what [[fetched]] reads. */
  private val bestRank = mutable.HashMap.empty[Int, Int]
  private val listingTokens: Set[String] = listingTitles.flatMap(TitleContainment.tokens).toSet
  private val read    = mutable.LinkedHashMap.empty[String, SourceRecord]
  /** Every film's record this resolve read, in the order it read them. */
  def weighed: Seq[SourceRecord] = read.values.toSeq
  private def hitOf(hit: SourceHit): Hit = {
    val number = numbers.getOrElseUpdate(hit.id, numbers.size + 1)
    named.getOrElseUpdate(number, hit)
    Hit(number, hit.title, hit.originalTitle, hit.year, 0.0)
  }
  /** The family's own id of a film this resolve numbered. */
  def idOf(number: Int): Option[String] = named.get(number).map(_.id)
  override def numbersFilmsByTmdbId: Boolean = false

  override def hasDetail(listing: Listing): Boolean                  = venues.hasDetail(listing)
  override def detail(listing: Listing): Answer[Option[DetailFacts]] = venues.detail(listing)
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text) if answers.family.latinTitlesOnly && !tools.LatinScript.hasLetter(text) => Answer.Known(Nil)
    case CandidateQuery.Title(text)    => answers.titled(text).map(ranked)
    case CandidateQuery.Director(_) if !answers.family.searchesDirectors => Answer.Known(Nil)
    case CandidateQuery.Director(name) => answers.directedBy(name).map(ranked)
    case _                             => Answer.Known(Nil)
  }
  private def ranked(hits: Seq[SourceHit]): Seq[Hit] = hits.zipWithIndex.map { case (hit, rank) =>
    val numbered = hitOf(hit)
    bestRank.updateWith(numbered.tmdbId)(was => Some(was.fold(rank)(_ min rank)))
    numbered
  }

  /** Is a numbered film's record worth asking for? Its search ranked it first, or its title shares a word with the
   *  listing's, or the search gave no title to judge it by — and in any case within the first [[FamilyLookups.Records]]
   *  a search returned. A hit failing that is read as no record (`film` is `None`): its question is never asked.
   *  Replayed on prod's answers (2026-10-05): ~29% fewer questions, 45% fewer record fetches, no agreed film lost —
   *  the first hit kept whatever its title (it rescues translated titles), the cap at 6 (4 lost one). */
  private def fetched(number: Int, hit: SourceHit): Boolean = bestRank.get(number).forall { rank =>
    rank < FamilyLookups.Records && (rank == 0 || {
      val titles = (Seq(hit.title) ++ hit.originalTitle).filter(_.nonEmpty)
      titles.isEmpty || titles.exists(title => TitleContainment.tokens(title).exists(listingTokens))
    })
  }

  override def film(number: Int): Answer[Option[IdentityMeasures.Film]] =
    named.get(number).filter(fetched(number, _)).fold[Answer[Option[IdentityMeasures.Film]]](Answer.Known(None)) { hit =>
      val answer = answers.record(hit.id)
      answer.toOption.flatten.foreach(record => read(hit.id) = record)
      answer.map(_.map(_.film))
    }
}

object FamilyLookups {
  /** How many of a search's films are read at most. */
  val Records = 6
}

object Agreement {

  /** How many families must agree: each taking the film, or leaning to it ([[leaningOf]]) beside one that takes it. A
   *  film is weighed for agreement only as a group of families' picks ([[agreed]]), so it always has a taker: leans and
   *  corroboration complete an agreement, never make one alone (PL "Pianista - Kino Konesera": IMDb takes "The
   *  Pianist", Wikidata and Filmweb lean to it). */
  val Quorum = 3
  /** EXPERIMENT (experiment/quorum-votes): which of the venue's own evidence counts in the quorum. 0 none (main);
   *  1 a venue poster matching the film's TMDB poster is a vote; 2 the venue naming two whole names of its cast (one
   *  candidate only) is a vote; 3 both; 4 a poster, cast or certificate CONTRADICTION is one dissenting vote. */
  val QuorumVariant: Int = 5
  /** The venue's own evidence on one film the families took, read beside the quorum ([[QuorumVariant]]). */
  final case class VenueEvidence(votes: Set[String], against: Int, dissents: Set[String] = Set.empty)
  object VenueEvidence { val Silent: VenueEvidence = VenueEvidence(Set.empty, 0) }
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
        (for { id <- lookups.idOf(number); answered <- answers.record(id).toOption; record <- answered }
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
      case Seq(Some(number)) => lookups.idOf(number).flatMap(id => answers.record(id).toOption.flatten)
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
   *  title bills dates it away from a taker's record ([[PosterEvidence.datedApart]]: DE "Weihnachten mit den Drei Herren
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
              thisYear: Option[Int] = None, stated: Seq[Listing] = Nil,
              venueEvidence: Seq[SourceRecord] => VenueEvidence = _ => VenueEvidence.Silent): Weighed = {
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
    named.map { group =>
      val film  = supported(listings, asStated, group, verdicts, modelVote, thisYear)
      val venue = venueEvidence(group.map(_.record))
      if (QuorumVariant == 5) {
        val dissent = named.filterNot(_.exists(pick => film.agreed.families(pick.family))).map(_.size).sum
        System.err.println(s"QVDIAG\t${listings.headOption.map(_.cinema).getOrElse("")}\t${listings.headOption.map(_.rawTitle).getOrElse("")}\t" +
          s"${group.head.record.crossIds.get("tmdb").getOrElse("")}\t${group.head.record.film.titled}\t${film.support}\t${Quorum + dissent + film.turnedDown}\t" +
          s"${venue.votes.toSeq.sorted.mkString(",")}\t${venue.dissents.toSeq.sorted.mkString(",")}")
        film
      } else film.copy(agreed = film.agreed.copy(corroborated = film.agreed.corroborated ++ venue.votes), venueAgainst = venue.against)
    }.sortBy(film => -film.support).headOption.flatMap { film =>
      val dissent = named.filterNot(_.exists(pick => film.agreed.families(pick.family))).map(_.size).sum
      val taken = film.records.take(film.agreed.families.size)
      // a feed catalogue's facts ruling the film out are its claim, one vote against it, not a veto (DE "To The Bone":
      // Filmstarts' feed credits Erin Li's 2014 short to a venue billing Noxon's 2017 feature)
      val catalogueDissent = if (taken.exists(contradictedByTheCatalogue(listings, _))) 1 else 0
      val stands = film.support >= Quorum + dissent + film.turnedDown + catalogueDissent + film.venueAgainst && !(film.completed && anothersOwnTitle(listings, film.records, verdicts)) &&
        !billsAnothersDirector(listings, film.records, verdicts) &&
        !taken.exists(contradictedByTheListing(listings, _)) &&
        listings.forall(listing => !ListingShape.billsSeveralBeside(listing, film.agreed.record.film) &&
          (!ListingShape.stagesAWork(listing) || screenAdaptation(listings, film.agreed)))
      Option.when(stands) {
        def titled(record: SourceRecord) = s"'${record.film.title}'${record.film.year.fold("")(y => s" ($y)")}"
        taken.iterator.flatMap(record => runsShort(listings, record, thisYear).map { case (stated, runs) =>
          s"no agreement on ${titled(record)}: the venue runs it ${stated}′, more than ${RuntimeShort}′ short of its ${runs}′" }).nextOption()
          .orElse(listings.iterator.flatMap(listing => taken.iterator.flatMap(record => PosterEvidence.datingApart(listing, record.film).map(year =>
            s"no agreement on ${titled(record)}: '${listing.rawTitle}' bills $year"))).nextOption())
          .fold(Weighed(Some(film.agreed), None))(why => Weighed(None, Some(why)))
      }
    }.getOrElse(Weighed(None, None))
  }

  /** What the families' evidence agrees on ([[weighed]]): the film, or none — and then why, where a guard naming its
   *  reason refused the film it would take. */
  final case class Weighed(agreed: Option[AgreedFilm], refused: Option[String])

  /** A film one family or more took, as the evidence stands for it: the takers, the families leaning to it, what
   *  corroborates it, and how many families weighed it and turned it down ([[turnsDown]]). */
  private final case class Supported(agreed: AgreedFilm, records: Seq[SourceRecord], turnedDown: Int, venueAgainst: Int = 0) {
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
    val voted   = modelVote.filter(vote => joins(records, vote) && !votesAnotherEntry(records, vote))
    // the takers' ids first, then those of the TMDB film the model votes for, which is the film they took
    val merged  = lead.record.copy(crossIds = (voted.toSeq ++ records).flatMap(_.crossIds).toMap ++ lead.record.crossIds)
    // the film through any record of it — the model's vote's too, which links a lean the takers' records name only in
    // another language (PL "Demony": IMDb's "Demons", by the IMDb id TMDB's "Демони" carries beside Filmweb's title)
    def isIt(record: SourceRecord) = (records ++ voted).exists(sameFilm(_, record))
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

  /** The TMDB film TMDB finds by the IMDb id `tt`, the find noted in `finding` while it is not answered yet. */
  private[identity] def tmdbFound(tmdbOf: String => Answer[Option[Int]], tt: String, finding: mutable.Set[String]): Answer[Option[Int]] = {
    val answer = tmdbOf(tt)
    if (answer == Answer.Unknown) finding += tt
    answer
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

  /** Every running time a record states — one per translation fetched ([[IdentityMeasures.Film.runtimes]]) — and its billed
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
   *  ([[services.movies.FilmRuntime.plausible]], [[SuspectTakes.FeatureMinutes]]), and only the whole film's: a part the
   *  title bills and the record's titles do not ("Shoah – Teil 1", 273 of 566 minutes), or a slot of half the film or less
   *  (a half before an intermission), screens a part of it. The agreement's guard, and the fill's `listing.runsShort`
   *  ([[services.identity.UnifiedEvidence]]); no lean's dissent reads it. */
  private[identity] def runsShort(listings: Seq[Listing], record: SourceRecord, thisYear: Option[Int]): Option[(Int, Int)] =
    if (!record.film.year.exists(released => released > ListingShape.SilentEraEnd && thisYear.forall(released < _))) None
    else {
      val runs = recordRuntimes(record)
      listings.iterator.filter(listing => SourceReliability.factsOf(listing).trust(Fact.Runtime) == Trust.Decide && !billsAPart(listing, record))
        .flatMap(_.runtime.filter(stated => services.movies.FilmRuntime.plausible(stated) && stated >= services.identity.SuspectTakes.FeatureMinutes))
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
    val itsOwn = records ++ weighed.filter(record => records.exists(sameFilm(_, record)))
    itsOwn.forall(original(_).isEmpty) && weighed.exists(record => original(record).nonEmpty && !records.exists(sameFilm(_, record)))
  }

  /** Does a listing's billing name — beside the film's title — the director of a namesake a family weighed (a film the
   *  title names, not the film itself), and none of the film's own directors? A series' banner then says whose film it
   *  shows: DE Babylon (Mitte)'s "Hitchcock & Chabrol: Blood Relatives" is Chabrol's 1978 film, which Wikidata takes and
   *  IMDb weighed, not Noah Segan's 2022 one the four other families take. A director is named by their surname
   *  ([[IdentityMeasures.Film.directorSurnames]]) where it is no word of that film's titles, which a billing of the title
   *  carries whoever it names. */
  private[identity] def billsAnothersDirector(listings: Seq[Listing], records: Seq[SourceRecord], verdicts: Seq[FamilyVerdict]): Boolean = {
    val namesakes = verdicts.flatMap(_.weighed).filterNot(weighed => records.exists(sameFilm(_, weighed)))
    listings.exists { listing =>
      val billed = TitleContainment.tokens(listing.rawTitle).toSet
      def credited(film: IdentityMeasures.Film) = film.directorSurnames.exists(surname =>
        billed(surname) && !film.titles.exists(title => TitleContainment.tokens(title).contains(surname)))
      // the venue's own synopsis naming a director WHOLE ([[VenueNames.names]]) says whose film it describes as the billing
      // does: UK Southsea's "Unrest" synopsis names Jennifer Brea, director of the 2017 documentary, not Ipson's 2006 horror
      def described(film: IdentityMeasures.Film) = film.directors.getOrElse(Nil).exists(listing.names.names)
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
      val (joined, apart) = films.partition(film => joins(film.map(_.record), pick.record))
      apart :+ (joined.flatten :+ pick).sortBy(_.family.ordinal)
    }

  /** Is `record` of the film `records` are: the [[sameFilm]] as one of them, and held [[apart]] from none? A record that
   *  dates and credits nothing is the same film as every namesake, so through it alone two films its title shares would
   *  join into one (PL "Homo sapiens?": TMDB's undated Swedish "Homo Sapiens" linking Geyrhalter's 2016 film, which IMDb,
   *  Metacritic and RT take, to Cohn and Duprat's 2025 one, which Filmweb takes — read as Geyrhalter's takers taking a
   *  current film). */
  def joins(records: Seq[SourceRecord], record: SourceRecord): Boolean =
    records.exists(sameFilm(_, record)) && !records.exists(apart(_, record))

  /** The same film by a shared cross-id, else by [[equivalent]] facts. */
  def sameFilm(a: SourceRecord, b: SourceRecord): Boolean = sharesAnId(a, b) || equivalent(a.film, b.film)

  /** Two films by their facts, no cross-id linking them ([[factsApart]]). */
  def apart(a: SourceRecord, b: SourceRecord): Boolean = !sharesAnId(a, b) && factsApart(a.film, b.film)

  private def sharesAnId(a: SourceRecord, b: SourceRecord): Boolean =
    a.crossIds.exists { case (database, id) => b.crossIds.get(database).contains(id) }

  /** Does the model's `vote` name another IMDb or TMDB id than takers whose `records` all date and credit nothing? Such a
   *  record is the same film as every namesake by its facts, so only its ids tell which entry it is (UK "Romeo and
   *  Juliet": Wikidata's undated, uncredited record of Shakespeare's play, IMDb tt2428892, joined the model's vote for
   *  Zeffirelli's 1968 film, tt0063518, by the title alone). Only the takers': a dated taker holds the film by its facts,
   *  and a bare pick joining a dated one is left to their facts (UK "Thanks for coming FFFL": Wikidata's bare
   *  tt36481185 beside IMDb's and RT's 2026 Cavalier). IMDb and TMDB ids only, which every family spells alike —
   *  Wikidata files some sites' ids in a space of its own (UK "Imamura: Black Rain": RT "22294" there, RT's own page
   *  "1035681-black_rain"). */
  private[agreement] def votesAnotherEntry(records: Seq[SourceRecord], vote: SourceRecord): Boolean =
    records.nonEmpty && records.forall(record => bare(record.film)) && records.exists(record =>
      CanonicalIds.exists(database => record.crossIds.get(database).exists(id => vote.crossIds.get(database).exists(_ != id))))
  private val CanonicalIds = Seq("imdb", "tmdb")

  /** A record that dates and credits nothing. */
  private def bare(film: IdentityMeasures.Film): Boolean = film.year.isEmpty && film.directors.forall(_.isEmpty)

  /** The same film by its facts, no cross-id linking them: years within one, no clash of Latin-script directors (one
   *  respelled in the same year and running time is none), and a
   *  shared title — or, titled in two languages, the same director the same year and a shared word of four letters. */
  def equivalent(a: IdentityMeasures.Film, b: IdentityMeasures.Film): Boolean = {
    def titles(f: IdentityMeasures.Film) = f.titles.map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    def words(f: IdentityMeasures.Film)  = f.titles.flatMap(TitleContainment.tokens).filter(_.length >= 4).toSet
    !factsApart(a, b) && ((titles(a) intersect titles(b)).nonEmpty ||
      (sameDirector(a, b) && a.year.isDefined && a.year == b.year && (words(a) intersect words(b)).nonEmpty))
  }

  /** Do the films' facts hold them apart: years more than one apart, or Latin-script directors who are other people (one
   *  respelled in the same year and running time is none)? */
  private def factsApart(a: IdentityMeasures.Film, b: IdentityMeasures.Film): Boolean = {
    val (da, db) = (a.directors.getOrElse(Nil), b.directors.getOrElse(Nil))
    // one person in two transliterations ("Andriej Konczałowski", "Andrei Konchalovsky") is no clash where the films
    // share their year and running time
    val respelled = a.year.isDefined && a.year == b.year && a.runtime.zip(b.runtime).exists { case (x, y) => math.abs(x - y) <= 2 } &&
      FactRelations.namePrefixes(da).intersect(FactRelations.namePrefixes(db)).nonEmpty
    FactRelations.yearsApart(a.year, b.year) || (da.nonEmpty && db.nonEmpty && (da ++ db).forall(tools.LatinScript.lettersOnly) && !sameDirector(a, b) && !respelled)
  }

  private def sameDirector(a: IdentityMeasures.Film, b: IdentityMeasures.Film): Boolean =
    FactRelations.samePerson(a.directors.getOrElse(Nil), b.directors.getOrElse(Nil))

  /** Is the listing's original title a feed's copy ([[SourceReliability.copiesOriginalTitle]]) one edit from its billing's —
   *  the record the feed's search for the billing hit, not the venue's own spelling of another title? */
  private[identity] def copiesItsSearchHit(listing: Listing): Boolean = listing.originalTitle.exists { original =>
    val (copy, billed) = (IdentityMeasures.key(original), IdentityMeasures.key(listing.cleanTitle))
    copy != billed && tools.EditDistance.within(copy, billed, 1) && SourceReliability.copiesOriginalTitle(listing)
  }

  /** Does the listing's title name one of the films' titles — be it, or carry it whole (four letters at least)? */
  def namesIt(listing: Listing, films: Seq[IdentityMeasures.Film]): Boolean = {
    val own    = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    val titles = films.flatMap(_.titles).distinct
    val raw    = TitleContainment.tokens(listing.rawTitle)
    lazy val ownWords = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(TitleContainment.tokens).toSet
    // the words the normalised title keeps: a film named only by what it drops is named by a decoration ("Lalka
    // PREMIERA", cleaned "Lalka", names no "Premiera") — nor by what its search form drops, an event bolted on after it
    // (UK "Le Moulin + Q&A", searched as "Le Moulin", names no "Q & A")
    lazy val kept = {
      val original = listing.originalTitle.toSeq.flatMap(TitleContainment.tokens).toSet
      val clean    = TitleContainment.tokens(listing.cleanTitle).toSet ++ original
      // only a search form that drops what a "+" bolts on after the title's head: one dropping the head drops the film itself
      // (DE '"JAGDSZENEN AUS NIEDERBAYERN" - HEIMATFILM IM WANDEL DER ZEIT', searched as its banner), and one dropping a
      // second title keeps the film's other name out (DE "BARWY OCHRONNE / CAMOUFLAGE", searched as "BARWY OCHRONNE")
      val head = listing.searchTitle.map(_.trim).filter(search => search.nonEmpty && listing.cleanTitle.startsWith(search) &&
        listing.cleanTitle.drop(search.length).trim.startsWith("+"))
      head.map(search => clean.intersect(TitleContainment.tokens(search).toSet ++ original)).getOrElse(clean)
    }
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
    val own   = IdentityMeasures.pieces(billed).map(TitleContainment.tokens)
    val its   = IdentityMeasures.pieces(title).map(TitleContainment.tokens)
    own.sizeIs >= 2 && own.size == its.size && own.zip(its).forall { case (o, t) => t.nonEmpty && t.mkString.length >= 4 && o.containsSlice(t) } &&
      own.zip(its).exists { case (o, t) => o == t }
  }
  /** The runs of every listing's billing the films' titles are (each as its words, articles aside). */
  def billedSpans(listings: Seq[Listing], films: Seq[IdentityMeasures.Film]): Seq[Seq[String]] =
    spansIn(listings.map(l => TitleContainment.tokens(l.rawTitle)), films)

  /** [[billedSpans]] of billings already read into their words: each of `billed` must hold the run. */
  def spansIn(billed: Seq[Seq[String]], films: Seq[IdentityMeasures.Film]): Seq[Seq[String]] =
    if (billed.isEmpty) Nil
    else {
      val bare = billed.map(withoutArticles)
      films.flatMap(_.titles).map(title => withoutArticles(TitleContainment.tokens(title)))
        .filter(words => words.nonEmpty && bare.forall(_.containsSlice(words))).distinct
    }

  private def withoutArticles(words: Seq[String]): Seq[String] = words.filterNot(IdentityMeasures.LeadingArticles)

  /** Does another candidate's titles name MORE of the billing than the film's — every word the film's titles name of it,
   *  and a word beside them — where the billing bills one film under two names? DE Acud Kino's "BARWY OCHRONNE /
   *  CAMOUFLAGE" names Gibalenko's 2026 "Камуфляж" only by its English title, and Zanussi's 1977 "Barwy ochronne",
   *  "Camouflage" in English, by both. A run of words, not one title: [[namedWithinAnother]] reads one title naming
   *  the film's within it. `own`, `others`: spans as [[billedSpans]] finds them. */
  def namesMoreThanIt(own: Seq[Seq[String]], others: Seq[Seq[Seq[String]]]): Boolean = {
    val named = own.flatten.toSet
    named.nonEmpty && others.exists { other => val theirs = other.flatten.toSet; named.subsetOf(theirs) && !theirs.subsetOf(named) }
  }

  /** Is the film named only WITHIN another candidate's title the listings bill whole — every run of the billing its
   *  titles are, a strictly longer run of one of `others`' titles, articles aside (a title only an article longer is the
   *  same name)? PL "Kacper i Emma – najlepsi przyjaciele …" names the 2026 "Najlepsi przyjaciele" only as part of the
   *  2013 "Kacper i Emma: Najlepsi przyjaciele"; DE CineStar's "Herr der Ringe - Rückkehr des Königs (ext.)" names the
   *  2025 short "Herr der Ringe" only as the head of "Der Herr der Ringe - Die Rückkehr des Königs", its articles dropped.
   *  Over spans [[billedSpans]] found: `own` the film's, `others` every candidate's, this one's among them. */
  def namedWithinAnother(own: Seq[Seq[String]], others: Seq[Seq[Seq[String]]]): Boolean =
    own.nonEmpty && others.exists { other =>
      val longer = other.filter(words => !own.contains(words))
      own.forall(span => longer.exists(words => words.sizeIs > span.size && words.containsSlice(span)))
    }


  /** May a listing naming a stage work show the agreed film, a SCREEN ADAPTATION of it, after all? No listing bills a
   *  house or a season ([[ListingShape.billsAHouse]]), TMDB's own evidence weighs the film best ([[ModelVote]]: the model's
   *  lean or best candidate is it) and its record is no house's season production. US "MOTHER!": Wikidata takes
   *  Aronofsky's film, Metacritic leans to it and so does the model — "Mother" is also an opera's name.
   *  "OPERA-COSI FAN TUTTE" and "ReTransmisje Met: … Così fan tutte" bill a house, so Tinto Brass's film stays no relay's. */
  private def screenAdaptation(listings: Seq[Listing], film: AgreedFilm): Boolean =
    film.corroborated(ModelVote) && !listings.exists(ListingShape.billsAHouse) && film.record.film.season.isEmpty
}
