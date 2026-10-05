package services.identity.agreement

import services.identity.{Acceptance, Answer, CandidateQuery, DetailFacts, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures, IdentityResolver,
  Listing}
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

/** What a family answers, from what it keeps: `Unknown` while a question is not answered yet — a gap, never "no film". */
trait FamilyAnswers {
  def family: VoterFamily
  def titled(text: String): Answer[Seq[SourceHit]]
  /** The films a person of this name directed; `Known(Nil)` for a family with no person search. */
  def directedBy(name: String): Answer[Seq[SourceHit]]
  def record(id: String): Answer[Option[SourceRecord]]
  /** Is there an answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`), and is it still fresh? A stale
   *  one is read all the same, and asked again; a missing one is a gap. */
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

object FamilyVerdict {
  /** A family that took `pick`. */
  def took(pick: FamilyPick): FamilyVerdict = FamilyVerdict(pick.family, Some(pick), Seq(pick.record))
}

/** The film a cluster's families agree on, with the families that named it, those whose evidence leaned to it, and what
 *  else corroborated it ([[Agreement.ListingFacts]], [[Agreement.ModelVote]]). */
final case class AgreedFilm(families: Set[VoterFamily], record: SourceRecord, ids: Map[VoterFamily, String], leaning: Set[VoterFamily] = Set.empty,
                            corroborated: Set[String] = Set.empty) {
  /** The film's id in `database` ("imdb", "tmdb"), as any agreeing family's record links it. */
  def crossId(database: String): Option[String] = Seq(record).flatMap(_.crossIds.get(database)).headOption
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

  override def hasDetail(listing: Listing): Boolean                  = venues.hasDetail(listing)
  override def detail(listing: Listing): Answer[Option[DetailFacts]] = venues.detail(listing)
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text) if answers.family.latinTitlesOnly && !FamilyLookups.hasLatinWord(text) => Answer.Known(Nil)
    case CandidateQuery.Title(text)    => answers.titled(text).mapKnown(ranked)
    case CandidateQuery.Director(_) if !answers.family.searchesDirectors => Answer.Known(Nil)
    case CandidateQuery.Director(name) => answers.directedBy(name).mapKnown(ranked)
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
      answer.mapKnown(_.map(_.film))
    }

  extension [A](answer: Answer[A]) private def mapKnown[B](f: A => B): Answer[B] = answer match {
    case Answer.Known(value) => Answer.Known(f(value))
    case Answer.Unknown      => Answer.Unknown
  }
}

object FamilyLookups {
  /** How many of a search's films are read at most. */
  val Records = 6
  /** Does `text` carry a word in Latin script? */
  def hasLatinWord(text: String): Boolean =
    text.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN)
}

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
          .fold[Answer[FamilyVerdict]](Answer.Unknown)(Answer.Known(_))
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
   *  ([[anothersOwnTitle]]); the listing bills one work ([[billsSeveral]]) and no stage work ([[stagesAWork]]). The
   *  experiment's one wrong without the title guard: "Akademia Polskiego Filmu: Kino żydowskie w Polsce" → "Znachor"
   *  (1937), whose year and director fit a series' episode. */
  def agreed(listings: Seq[Listing], verdicts: Seq[FamilyVerdict], modelVote: Option[SourceRecord] = None,
             thisYear: Option[Int] = None, stated: Seq[Listing] = Nil): Option[AgreedFilm] = {
    // what the venues' own pages add to the listings votes and names films; it never rules one out — a page's year is
    // often its re-release's (PL Kinoteka "Ghost in the shell": 2026 on the page of the 1995 film)
    val asStated = if (stated.nonEmpty) stated else listings
    val picks = verdicts.flatMap(_.pick).sortBy(_.family.ordinal)
    // a family taking a film the listing's title does not name is no evidence on the listing's (US "A Night at the
    // Opera": Wikidata's "The Old Maid", by the director the venue credits)
    val named = filmsOf(picks).filter(group => asStated.nonEmpty && asStated.forall(listing => namesIt(listing, group.map(_.record.film))))
    // each group is the picks of one film, so every film weighed has a taker: a lean or a corroboration alone makes none
    named.map(group => supported(listings, asStated, group, verdicts, modelVote, thisYear)).sortBy(film => -film.support).headOption.flatMap { film =>
      val dissent = named.filterNot(_.exists(pick => film.agreed.families(pick.family))).map(_.size).sum
      Option.when(film.support >= Quorum + dissent + film.turnedDown && !(film.completed && anothersOwnTitle(listings, film.records, verdicts)) &&
        !film.records.take(film.agreed.families.size).exists(contradictedByTheListing(listings, _)) &&
        listings.forall(listing => !billsSeveral(listing) && !stagesAWork(listing)))(film.agreed)
    }
  }

  /** A film one family or more took, as the evidence stands for it: the takers, the families leaning to it, what
   *  corroborates it, and how many families weighed it and turned it down ([[turnsDown]]). */
  private final case class Supported(agreed: AgreedFilm, records: Seq[SourceRecord], turnedDown: Int) {
    /** The records of it the takers took and the leaning families favour. */
    val support: Int = agreed.families.size + agreed.leaning.size + agreed.corroborated.size
    /** Short of [[Quorum]] takers, completed by leans or corroboration. */
    def completed: Boolean = agreed.families.size < Quorum
  }

  private def supported(listings: Seq[Listing], stated: Seq[Listing], group: Seq[FamilyPick], verdicts: Seq[FamilyVerdict], modelVote: Option[SourceRecord],
                        thisYear: Option[Int]): Supported = {
    val lead    = group.head
    val records = group.map(_.record)
    val voted   = modelVote.filter(vote => records.exists(sameFilm(_, vote)))
    // the takers' ids first, then those of the TMDB film the model votes for, which is the film they took
    val merged  = lead.record.copy(crossIds = (voted.toSeq ++ records).flatMap(_.crossIds).toMap ++ lead.record.crossIds)
    // the film through any record of it — the model's vote's too, which links a lean the takers' records name only in
    // another language (PL "Demony": IMDb's "Demons", by the IMDb id TMDB's "Демони" carries beside Filmweb's title)
    def isIt(record: SourceRecord) = (records ++ voted).exists(sameFilm(_, record))
    val leaning = verdicts.filter(verdict => verdict.pick.isEmpty && verdict.leaning.exists(isIt)).map(_.family).toSet
    val catalogued = listings.exists(_.catalogueIds.exists(id => group.exists(pick => pick.family.database == id.source && pick.id == id.id)))
    val corroborated = listingVotes(stated, records) ++ Set(ModelVote).filter(_ => voted.nonEmpty) ++ Set(Catalogue).filter(_ => catalogued) ++
      Set(Venues).filter(_ => listings.map(_.venue).distinct.size >= WidelyBilled &&
        thisYear.exists(year => records.flatMap(_.film.year).maxOption.exists(_ >= year - 1)))
    val turnedDown = verdicts.count(turnsDown(_, listings, records, isIt))
    val leant = verdicts.filter(verdict => verdict.pick.isEmpty).flatMap(_.leaning).filter(isIt)
    Supported(AgreedFilm(group.map(_.family).toSet, merged, group.map(pick => pick.family -> pick.id).toMap, leaning, corroborated), records ++ leant,
      turnedDown)
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
      !isIt(lean) && !contradictedByTheListing(listings, lean) && !records.exists(editionOf(lean, _)))

  /** Is `edition` a record of `work` under a qualifier: the same director's, dated no earlier (or undated), its title
   *  carrying one of the work's (four letters at least) as a token run — "Ken Russell's The Devils: The Director's Cut"
   *  of "The Devils"? */
  private def editionOf(edition: SourceRecord, work: SourceRecord): Boolean = {
    val (byEdition, byWork) = (edition.film.directors.getOrElse(Nil), work.film.directors.getOrElse(Nil))
    val carried = TitleContainment.tokens(edition.film.title)
    byEdition.nonEmpty && byWork.nonEmpty && IdentityMeasures.directorRelation(byEdition, byWork) == IdentityMeasures.Category("same_person") &&
      edition.film.year.forall(year => work.film.year.forall(_ <= year)) &&
      work.film.titles.exists(title => title.length >= 4 && { val words = TitleContainment.tokens(title); words.nonEmpty && carried.containsSlice(words) })
  }

  /** What the listings' own facts vote for the takers' records: [[ListingFacts]] when every listing publishing a year
   *  and a director credits one of them — the same person directing, within a year — and one does (DE "Die Story von
   *  Joanna", Damiano's in 1975 by the venue's own page, is the 1975 film Wikidata and Filmweb took); with
   *  [[ListingRuntime]] beside it when the running times agree too, and then a record dating the film nowhere is credited
   *  by its director and running time (DE "Pettersson und Findus Mitmachkino 2"). */
  private[identity] def listingVotes(listings: Seq[Listing], records: Seq[SourceRecord]): Set[String] = {
    val runs  = runsAsTheListing(listings, records)
    val dated = listings.filter(listing => listing.year.isDefined && listing.directors.nonEmpty)
    def credits(listing: Listing) = records.exists { record =>
      (record.film.year.zip(listing.year).exists { case (a, b) => math.abs(a - b) <= 1 } || (runs && record.film.year.isEmpty)) &&
        record.film.directors.exists(directors => directors.nonEmpty &&
          IdentityMeasures.directorRelation(listing.directors, directors) == IdentityMeasures.Category("same_person"))
    }
    if (dated.isEmpty || !dated.forall(credits)) Set.empty else Set(ListingFacts) ++ Option.when(runs)(ListingRuntime)
  }

  /** Does every listing stating a running time run within [[RuntimeSlack]] minutes of a record's, and one state it? */
  private def runsAsTheListing(listings: Seq[Listing], records: Seq[SourceRecord]): Boolean = {
    val timed = listings.filter(_.runtime.isDefined)
    timed.nonEmpty && timed.forall(listing => records.exists(_.film.runtime.zip(listing.runtime).exists { case (a, b) => math.abs(a - b) <= RuntimeSlack }))
  }

  /** Does a listing's own year (more than one apart) or director (another person in the same script, sharing no name's
   *  stem — "Marc Donskoi" is "Mark Donskoy") rule the film out? */
  private[identity] def contradictedByTheListing(listings: Seq[Listing], record: SourceRecord): Boolean = listings.exists { listing =>
    record.film.year.zip(listing.year).exists { case (a, b) => math.abs(a - b) > 1 } ||
      record.film.directors.exists(directors => directors.nonEmpty && listing.directors.nonEmpty &&
        IdentityMeasures.directorRelation(listing.directors, directors) == IdentityMeasures.Category("different") &&
        namePrefixes(listing.directors).intersect(namePrefixes(directors)).isEmpty)
  }

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

  /** The picks as the films they name: a pick joins every film one of whose picks is the [[sameFilm]] — through ANY
   *  family's record, so a record that names a film only in another family's terms (Wikidata's crediting "Kukla" as
   *  Filmweb does, beside IMDb's id that IMDb's "Kukla Kesherovic" carries) joins the three, and two films a pick links
   *  are one. Each film's picks in family order. */
  private def filmsOf(picks: Seq[FamilyPick]): Seq[Seq[FamilyPick]] =
    picks.foldLeft(Vector.empty[Vector[FamilyPick]]) { (films, pick) =>
      val (joined, apart) = films.partition(_.exists(member => sameFilm(member.record, pick.record)))
      apart :+ (joined.flatten :+ pick).sortBy(_.family.ordinal)
    }

  /** The same film by a shared cross-id, else by [[equivalent]] facts. */
  def sameFilm(a: SourceRecord, b: SourceRecord): Boolean =
    a.crossIds.exists { case (database, id) => b.crossIds.get(database).contains(id) } || equivalent(a.film, b.film)

  /** The same film by its facts, no cross-id linking them: years within one, no clash of Latin-script directors (one
   *  respelled in the same year and running time is none), and a
   *  shared title — or, titled in two languages, the same director the same year and a shared word of four letters. */
  def equivalent(a: IdentityMeasures.Film, b: IdentityMeasures.Film): Boolean = {
    val yearsApart = a.year.zip(b.year).exists { case (x, y) => math.abs(x - y) > 1 }
    val (da, db)   = (a.directors.getOrElse(Nil), b.directors.getOrElse(Nil))
    val sameDirector = da.nonEmpty && db.nonEmpty && IdentityMeasures.directorRelation(da, db) == IdentityMeasures.Category("same_person")
    // one person in two transliterations ("Andriej Konczałowski", "Andrei Konchalovsky") is no clash where the films
    // share their year and running time
    val respelled  = a.year.isDefined && a.year == b.year && a.runtime.zip(b.runtime).exists { case (x, y) => math.abs(x - y) <= 2 } &&
      namePrefixes(da).intersect(namePrefixes(db)).nonEmpty
    val clash      = da.nonEmpty && db.nonEmpty && latin(da ++ db) && !sameDirector && !respelled
    def titles(f: IdentityMeasures.Film) = f.titles.map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    def words(f: IdentityMeasures.Film)  = f.titles.flatMap(TitleContainment.tokens).filter(_.length >= 4).toSet
    !yearsApart && !clash && ((titles(a) intersect titles(b)).nonEmpty ||
      (sameDirector && a.year.isDefined && a.year == b.year && (words(a) intersect words(b)).nonEmpty))
  }

  /** Each name's words of four letters or more, folded to ASCII and cut to their first four. */
  private def namePrefixes(names: Seq[String]): Set[String] =
    names.flatMap(name => tools.TextNormalization.deburr(name).toLowerCase(java.util.Locale.ROOT).split("[^a-z]+")).filter(_.length >= 4).map(_.take(4)).toSet

  private def latin(names: Seq[String]): Boolean =
    names.forall(_.forall(c => !Character.isLetter(c) || Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** Does the listing's title name one of the films' titles — be it, or carry it whole (four letters at least)? */
  def namesIt(listing: Listing, films: Seq[IdentityMeasures.Film]): Boolean = {
    val own    = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    val titles = films.flatMap(_.titles).distinct
    val raw    = TitleContainment.tokens(listing.rawTitle)
    lazy val ownWords = (Seq(listing.cleanTitle, listing.rawTitle, listing.title) ++ listing.originalTitle).map(TitleContainment.tokens).toSet
    titles.map(IdentityMeasures.key).exists(own) ||
      titles.exists(title => title.length >= 4 && { val words = TitleContainment.tokens(title); words.nonEmpty && raw.containsSlice(words) }) ||
      // the film's title with a leading article the listing drops — two words after it at least (DE "Camp der
      // Verlorenen", TMDB's "Das Camp der Verlorenen"; "Devil" is not "The Devil")
      titles.exists { title => val words = TitleContainment.tokens(title); words.sizeIs >= 3 && LeadingArticles(words.head) && ownWords(words.tail) }
  }
  /** Articles a title may lead with that a venue drops: English, German, French, Spanish, Italian. */
  private val LeadingArticles = Set("the", "a", "an", "der", "die", "das", "le", "la", "les", "el", "los", "las", "il", "lo", "gli")

  /** Does the listing name a stage work ([[services.identity.StageWorks]]) — an opera or ballet a house's relay
   *  bills? The films its families find are the work's screen namesakes ("ReTransmisje Met: Così fan tutte" → Tinto
   *  Brass's 1992 "Così fan tutte", replay 2026-10-04), never the relay, whose record TMDB alone keeps. */
  def stagesAWork(listing: Listing): Boolean = {
    val titles = Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct
    // the work billed within a piece too: run into a house's word ("OPERA-COSI FAN TUTTE"), after its composer
    IdentityMeasures.stageWorksBilled(titles, IdentityMeasures.seasonYear(titles).isDefined).nonEmpty ||
      titles.map(title => IdentityMeasures.Listing(title, Some(listing.rawTitle).filter(_ != title))).exists(IdentityMeasures.stageWorks(_).nonEmpty)
  }

  private val Bill   = """(?i)double bill|double feature|podw[oó]jny seans|zestaw""".r
  private val Quoted = """[„"“][^"”„]+["”]""".r
  /** Does the listing bill several works — a "+" joining two whole works ([[IdentityMeasures.billsTwoWholeWorks]]: an
   *  event joined to the film, "11. UFF - Gala otwarcia + Demony", "… pokaz filmu + dyskusja", is none), a double bill
   *  or a set, or two quoted titles? */
  def billsSeveral(listing: Listing): Boolean =
    Bill.findFirstIn(listing.rawTitle).isDefined || Quoted.findAllIn(listing.rawTitle).size >= 2 ||
      IdentityMeasures.billsTwoWholeWorks(services.identity.Evidence.of(listing, None).measured)
}
