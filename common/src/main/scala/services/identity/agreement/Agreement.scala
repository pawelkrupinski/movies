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
enum VoterFamily(val label: String, val priorSpread: Double) {
  /** IMDb's own title and name search, and its records (Cinemeta and OMDb mirror it). */
  case Imdb extends VoterFamily("imdb", 1.5)
  /** Wikidata's film items, and the Wikipedia articles that name them. */
  case Wiki extends VoterFamily("wiki", 0.5)
  /** Filmweb's search and film records — a voter where it indexes the country's titles (PL, DE, ES). */
  case Filmweb extends VoterFamily("filmweb", 0.5)
  case Metacritic extends VoterFamily("metacritic", 1.0)
  case RottenTomatoes extends VoterFamily("rt", 1.5)
}

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
 *  else corroborated it ([[Agreement.ListingFacts]], [[Agreement.ModelLean]]). */
final case class AgreedFilm(families: Set[VoterFamily], record: SourceRecord, ids: Map[VoterFamily, String], leaning: Set[VoterFamily] = Set.empty,
                            corroborated: Set[String] = Set.empty) {
  /** The film's id in `database` ("imdb", "tmdb"), as any agreeing family's record links it. */
  def crossId(database: String): Option[String] = Seq(record).flatMap(_.crossIds.get(database)).headOption
}

/** A family's answers as the resolver's lookups — the family as the film database, as TMDB is to the model. Its ids
 *  are numbered as this resolve meets them (it asks its questions in one sorted order), so the resolve is a function of
 *  the answers; the venues' own detail pages come from `venues`. */
final class FamilyLookups(answers: FamilyAnswers, venues: IdentityLookups) extends IdentityLookups {
  private val numbers = mutable.LinkedHashMap.empty[String, Int]
  private val named   = mutable.HashMap.empty[Int, SourceHit]
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
    case CandidateQuery.Title(text)    => answers.titled(text).mapKnown(_.map(hitOf))
    case CandidateQuery.Director(name) => answers.directedBy(name).mapKnown(_.map(hitOf))
    case _                             => Answer.Known(Nil)
  }
  override def film(number: Int): Answer[Option[IdentityMeasures.Film]] =
    named.get(number).fold[Answer[Option[IdentityMeasures.Film]]](Answer.Known(None)) { hit =>
      val answer = answers.record(hit.id)
      answer.toOption.flatten.foreach(record => read(hit.id) = record)
      answer.mapKnown(_.map(_.film))
    }

  extension [A](answer: Answer[A]) private def mapKnown[B](f: A => B): Answer[B] = answer match {
    case Answer.Known(value) => Answer.Known(f(value))
    case Answer.Unknown      => Answer.Unknown
  }
}

object Agreement {

  /** How many families must agree: each taking the film, or leaning to it ([[leaningOf]]) beside at least [[Takers]]. */
  val Quorum = 3
  /** How many of the agreeing families must take the film: leans complete an agreement, never make one alone (PL
   *  "Pianista - Kino Konesera": IMDb takes "The Pianist", Wikidata and Filmweb lean to it). */
  val Takers = 1
  /** The listing's own published year and director, crediting the film the takers took: a vote of the venue's own. */
  val ListingFacts = "listing"
  /** At least [[WidelyBilled]] venues billing the cluster's title: a release, not a one-off event a namesake fits. */
  val Venues = "venues"
  val WidelyBilled = 3
  /** The TMDB film the model's own evidence leans to though no rule took it (`ResolverDecision.leaning`), linked by its
   *  IMDb id to the film the takers took. */
  val ModelLean = "tmdb"

  /** The film `family` identifies `listings` as — the resolver itself over the family's answers, as the experiment ran
   *  it — or none, with every record it weighed; `Unknown` while a question it asked is not answered yet. */
  def verdict(listings: Seq[Listing], answers: FamilyAnswers, venues: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration): Answer[FamilyVerdict] = {
    val lookups    = new FamilyLookups(answers, venues)
    val resolution = IdentityResolver.resolve(listings, lookups, normalizer, calibration)
    if (resolution.unknownQueries > 0 || resolution.unknownFilms > 0) Answer.Unknown
    else resolution.decisions.flatMap(_.film).distinct match {
      case Seq(number) =>
        (for { id <- lookups.idOf(number); answered <- answers.record(id).toOption; record <- answered }
          yield FamilyVerdict(answers.family, Some(FamilyPick(answers.family, id, record)), lookups.weighed))
          .fold[Answer[FamilyVerdict]](Answer.Unknown)(Answer.Known(_))
      case _ => // none, or the family splits the cluster: no one film
        Answer.Known(FamilyVerdict(answers.family, None, lookups.weighed, leaningOf(listings, lookups, answers, normalizer, calibration)))
    }
  }

  /** The film a family's evidence LEANS to though it took none: on every listing, its best undenied candidate, at
   *  [[services.identity.Acceptance.LeanMargin]] times the runner-up's probability — the model's own lean
   *  (`Acceptance.leaning`) over the family's search. Never a pick: leans complete a quorum beside a taker, and
   *  a lean to another film is what makes a family's having weighed the film turning it down (PL "Sukienka": RT weighed
   *  "The Dress" at 6.0%, its runner-up at 2.7%; Tempo's IMDb weighed "Tempo" at 2.9% under "Old" at 33.0%). */
  private[agreement] def leaningOf(listings: Seq[Listing], lookups: FamilyLookups, answers: FamilyAnswers, normalizer: TitleNormalizer,
                                   calibration: IdentityCalibration): Option[SourceRecord] =
    IdentityResolver.candidatesOf(listings, lookups, normalizer, calibration)(_ => true).map { node =>
      val eligible = node.candidates.filterNot(_.denied).sortBy(-_.probability)
      eligible.headOption.filter(best => eligible.lift(1).forall(runnerUp => best.probability >= Acceptance.LeanMargin * runnerUp.probability)).map(_.tmdbId)
    }.distinct match {
      case Seq(Some(number)) => lookups.idOf(number).flatMap(id => answers.record(id).toOption.flatten)
      case _                 => None
    }

  /** The film the families' evidence, pooled, agrees on: ≥ [[Takers]] families take it (picks join, through any agreeing
   *  record, by a shared cross-id, else by [[equivalent]] facts), and with the families leaning to it and what
   *  corroborates it ([[ListingFacts]], [[ModelLean]], [[Venues]]) it holds ≥ [[Quorum]] — that plus one for each family
   *  taking another film the listing's title names. No listing's own year or director rules out a taker's record
   *  (DE "Der kleine Maulwurf", the venues' 1968 Miler, not IMDb's and Wikidata's 2011 compilation); no family weighed
   *  it and took none leaning to another; the listing's title
   *  names it ([[namesIt]]) — and, where leans or corroboration complete the quorum, is no other film's own
   *  ([[anothersOwnTitle]]); the listing bills one work ([[billsSeveral]]) and no stage work ([[stagesAWork]]). The
   *  experiment's one wrong without the title guard: "Akademia Polskiego Filmu: Kino żydowskie w Polsce" → "Znachor"
   *  (1937), whose year and director fit a series' episode; the replay's without the weighed one: "Okładka „Tempo”", a
   *  Finnish dance film, → "Tempo" (2003), which IMDb's search found, weighed and turned down for "Old" while Filmweb,
   *  RT and Wikidata took it. */
  def agreed(listings: Seq[Listing], verdicts: Seq[FamilyVerdict], modelLean: Option[SourceRecord] = None): Option[AgreedFilm] = {
    val picks = verdicts.flatMap(_.pick).sortBy(_.family.ordinal)
    // a family taking a film the listing's title does not name is no evidence on the listing's (US "A Night at the
    // Opera": Wikidata's "The Old Maid", by the director the venue credits)
    val named = filmsOf(picks).filter(group => listings.nonEmpty && listings.forall(listing => namesIt(listing, group.map(_.record.film))))
    named.filter(_.size >= Takers).map(group => supported(listings, group, verdicts, modelLean)).sortBy(film => -film.support).headOption.flatMap { film =>
      val dissent = named.filterNot(_.exists(pick => film.agreed.families(pick.family))).map(_.size).sum
      Option.when(film.support >= Quorum + dissent && !film.turnedDown && !(film.completed && anothersOwnTitle(listings, film.records, verdicts)) &&
        !film.records.take(film.agreed.families.size).exists(contradictedByTheListing(listings, _)) &&
        listings.forall(listing => !billsSeveral(listing) && !stagesAWork(listing)))(film.agreed)
    }
  }

  /** A film ≥ [[Takers]] families took, as the evidence stands for it: the takers, the families leaning to it, what
   *  corroborates it, and whether a family weighed it and turned it down. */
  private final case class Supported(agreed: AgreedFilm, records: Seq[SourceRecord], turnedDown: Boolean) {
    /** The records of it the takers took and the leaning families favour. */
    val support: Int = agreed.families.size + agreed.leaning.size + agreed.corroborated.size
    /** Short of [[Quorum]] takers, completed by leans or corroboration. */
    def completed: Boolean = agreed.families.size < Quorum
  }

  private def supported(listings: Seq[Listing], group: Seq[FamilyPick], verdicts: Seq[FamilyVerdict], modelLean: Option[SourceRecord]): Supported = {
    val lead    = group.head
    val records = group.map(_.record)
    val merged  = lead.record.copy(crossIds = records.flatMap(_.crossIds).toMap ++ lead.record.crossIds)
    def isIt(record: SourceRecord) = records.exists(sameFilm(_, record))
    val leaning = verdicts.filter(verdict => verdict.pick.isEmpty && verdict.leaning.exists(isIt)).map(_.family).toSet
    val corroborated = Set(ListingFacts).filter(_ => creditedByTheListing(listings, records)) ++ Set(ModelLean).filter(_ => modelLean.exists(isIt)) ++
      Set(Venues).filter(_ => listings.map(_.venue).distinct.size >= WidelyBilled)
    // weighed and turned down for another film its evidence favours — weighed among films it favours none of is no
    // evidence against this one (US "Spider Baby": Metacritic's best a Spider-Man film at 5.3%, its next 3.0%)
    // — nor for one the listing's own year or director rules out (DE "Überleben", Danial Miller's in 2020 by the venue:
    // Filmweb leaning to the 2022 "Survive")
    val turnedDown = verdicts.exists(verdict => verdict.pick.isEmpty && verdict.weighed.exists(isIt) &&
      verdict.leaning.exists(lean => !isIt(lean) && !contradictedByTheListing(listings, lean)))
    val leant = verdicts.filter(verdict => verdict.pick.isEmpty).flatMap(_.leaning).filter(isIt)
    Supported(AgreedFilm(group.map(_.family).toSet, merged, group.map(pick => pick.family -> pick.id).toMap, leaning, corroborated), records ++ leant,
      turnedDown)
  }

  /** Do the listings credit the film themselves: every listing publishing a year and a director credits one of the
   *  takers' records — the same person directing, within a year — and one does? DE "Die Story von Joanna", Damiano's
   *  in 1975 by the venue's own page, is the 1975 film Wikidata and Filmweb took. */
  private def creditedByTheListing(listings: Seq[Listing], records: Seq[SourceRecord]): Boolean = {
    val dated = listings.filter(listing => listing.year.isDefined && listing.directors.nonEmpty)
    def credits(listing: Listing) = records.exists { record =>
      record.film.year.zip(listing.year).exists { case (a, b) => math.abs(a - b) <= 1 } &&
        record.film.directors.exists(directors => directors.nonEmpty &&
          IdentityMeasures.directorRelation(listing.directors, directors) == IdentityMeasures.Category("same_person"))
    }
    dated.nonEmpty && dated.forall(credits)
  }

  /** Does a listing's own year (more than one apart) or director (another person in the same script, sharing no name's
   *  stem — "Marc Donskoi" is "Mark Donskoy") rule the film out? */
  private def contradictedByTheListing(listings: Seq[Listing], record: SourceRecord): Boolean = listings.exists { listing =>
    record.film.year.zip(listing.year).exists { case (a, b) => math.abs(a - b) > 1 } ||
      record.film.directors.exists(directors => directors.nonEmpty && listing.directors.nonEmpty &&
        IdentityMeasures.directorRelation(listing.directors, directors) == IdentityMeasures.Category("different") &&
        namePrefixes(listing.directors).intersect(namePrefixes(directors)).isEmpty)
  }

  /** Is the listing's title another film's ORIGINAL title — one a family weighed — while it is none of the agreed film's
   *  records' original titles, only a translation they file? Then the venue may well bill that film by its own name:
   *  PL "Obcy w domu" is "Hider in the House" (1989) in Polish, and the 1986 Polish film IMDb weighed beside it. */
  private def anothersOwnTitle(listings: Seq[Listing], records: Seq[SourceRecord], verdicts: Seq[FamilyVerdict]): Boolean = {
    val billed = listings.flatMap(l => Seq(l.title, l.cleanTitle)).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    def original(record: SourceRecord) = record.film.originalTitle.map(IdentityMeasures.key).filter(billed)
    records.forall(original(_).isEmpty) &&
      verdicts.flatMap(_.weighed).exists(weighed => original(weighed).nonEmpty && !records.exists(sameFilm(_, weighed)))
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
    titles.map(IdentityMeasures.key).exists(own) ||
      titles.exists(title => title.length >= 4 && { val words = TitleContainment.tokens(title); words.nonEmpty && raw.containsSlice(words) })
  }

  /** Does the listing name a stage work ([[services.identity.StageWorks]]) — an opera or ballet a house's relay
   *  bills? The films its families find are the work's screen namesakes ("ReTransmisje Met: Così fan tutte" → Tinto
   *  Brass's 1992 "Così fan tutte", replay 2026-10-04), never the relay, whose record TMDB alone keeps. */
  def stagesAWork(listing: Listing): Boolean =
    (Seq(listing.title, listing.cleanTitle).distinct.map(title => IdentityMeasures.Listing(title, Some(listing.rawTitle).filter(_ != title))))
      .exists(IdentityMeasures.stageWorks(_).nonEmpty)

  private val Bill   = """(?i)\s\+\s|double bill|double feature|podw[oó]jny seans|zestaw""".r
  private val Quoted = """[„"“][^"”„]+["”]""".r
  /** Does the listing bill several works — a "+", a double bill or a set, or two quoted titles? */
  def billsSeveral(listing: Listing): Boolean =
    Bill.findFirstIn(listing.rawTitle).isDefined || Quoted.findAllIn(listing.rawTitle).size >= 2
}
