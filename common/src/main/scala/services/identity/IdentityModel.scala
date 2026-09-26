package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData, Tmdb}
import services.movies.{EmbeddedYear, ListingConstraints, ListingKey, ScrapeListing, TitleNormalizer}

/*
 * The identity resolver's vocabulary (docs/design/identity-resolver.md, phase 2). Everything here
 * is a value: a listing as its venue published it, the evidence it carries once its own detail page
 * is merged in, and the films an external lookup names for it. Nothing is derived from another
 * listing, and nothing is read from stored films.
 */

/** One listing exactly as its venue published it, keyed by [[ListingKey]]. `cleanTitle` is the
 *  venue's own title rules applied to the raw title (`ScrapeListing.cleanTitle`) — a function of
 *  the venue and the string, never of any other listing. */
final case class Listing(
  cinema:        Cinema,
  key:           ListingKey,
  rawTitle:      String,
  cleanTitle:    String,
  year:          Option[Int],
  directors:     Seq[String],
  runtime:       Option[Int],
  page:          Option[String],
  originalTitle: Option[String],
  countries:     Seq[String] = Nil
) {
  def venue: String = key.venue

  /** A TOTAL order over listings: the key first, then every published field, so two different
   *  listings never tie and a set of listings has exactly one sorted presentation. */
  lazy val sortKey: String =
    Seq(venue, rawTitle, page.getOrElse(""), cleanTitle, year.fold("")(_.toString), directors.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse("")).mkString("\u0000")
}

object Listing {

  /** `cm` as `cinema` lists it. Blank director names and non-positive runtimes are absent. */
  def of(cinema: Cinema, cm: CinemaMovie, normalizer: TitleNormalizer): Listing = Listing(
    cinema        = cinema,
    key           = ListingKey.of(cinema, cm),
    rawTitle      = cm.movie.rawTitle.getOrElse(cm.movie.title),
    cleanTitle    = ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)._1,
    year          = cm.movie.releaseYear,
    directors     = cm.director.map(_.trim).filter(_.nonEmpty).distinct.sorted,
    runtime       = cm.movie.runtimeMinutes.filter(_ > 0),
    page          = cm.filmUrl.map(_.trim).filter(_.nonEmpty),
    originalTitle = cm.movie.originalTitle.map(_.trim).filter(_.nonEmpty),
    countries     = cm.movie.countries.map(_.trim).filter(_.nonEmpty).distinct.sorted)

  implicit val ordering: Ordering[Listing] = Ordering.by(_.sortKey)
}

/** What a venue's own detail page adds to its listing: only the identity fields. */
final case class DetailFacts(year: Option[Int], directors: Seq[String], runtime: Option[Int], originalTitle: Option[String],
                             countries: Seq[String] = Nil)

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil) {
  lazy val key: String =
    Seq(cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000")

  /** The year the venue put in its title, when it published none as a field. */
  lazy val bracketYear: Option[Int] = EmbeddedYear.of(rawTitle, cleanTitle)

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = year.orElse(bracketYear)

  /** Whether [[statedYear]] was read from a title rather than published as a field. A bracket is
   *  often a RE-RELEASE year ("Toy Story (2026)"), a field almost never is — the calibration
   *  weighs the two separately rather than this code guessing which is which. */
  def yearFromBracket: Boolean = year.isEmpty && bracketYear.isDefined

  /** The cinema slot production would hold for this evidence — what `ListingConstraints` reads. */
  def slot: SourceData = SourceData(title = Some(cleanTitle), rawTitle = Some(rawTitle), originalTitle = originalTitle,
    director = directors, runtimeMinutes = runtime, releaseYear = year)

  def constraintEvidence: ListingConstraints.ListingEvidence =
    ListingConstraints.ListingEvidence(originalTitle, runtime, statedYear, directors)

  /** A one-slot record of this evidence on `cinema` — the row shape `ListingConstraints`' row
   *  predicates read. */
  def record(cinema: Cinema, normalizer: TitleNormalizer): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](CinemaShowing.keyFor(cinema, cleanTitle, normalizer) -> slot))
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts]): Evidence = Evidence(
    cleanTitle    = listing.cleanTitle,
    rawTitle      = listing.rawTitle,
    year          = listing.year.orElse(detail.flatMap(_.year)),
    directors     = (if (listing.directors.nonEmpty) listing.directors else detail.map(_.directors).getOrElse(Nil)).sorted,
    runtime       = listing.runtime.orElse(detail.flatMap(_.runtime).filter(_ > 0)),
    originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)),
    countries     = (if (listing.countries.nonEmpty) listing.countries else detail.map(_.countries).getOrElse(Nil)).distinct.sorted)
}

/** One film a lookup NAMED: a search result or a filmography credit. Only what the list itself
 *  carries — the film's own record is [[FilmFacts]]. */
final case class Hit(tmdbId: Int, title: String, originalTitle: Option[String], year: Option[Int], popularity: Double)

/** A film as the film database describes it — the facts a candidate is scored on. */
final case class FilmFacts(tmdbId: Int, title: Option[String], originalTitle: Option[String], year: Option[Int],
                           directors: Seq[String], runtime: Option[Int], countries: Seq[String] = Nil,
                           imdbId: Option[String] = None) {
  def slot: SourceData = SourceData(title = title, originalTitle = originalTitle, releaseYear = year,
    director = directors, runtimeMinutes = runtime)
  def record: MovieRecord = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](Tmdb -> slot))
}

/**
 * A film some listing of a family may be — every hit any of the family's queries returned, with
 * its own record when the lookup source holds one. `popularity` is the film database's own
 * ranking signal, taken as the largest any hit reported.
 */
final case class Candidate(tmdbId: Int, titles: Seq[String], year: Option[Int], directors: Seq[String],
                           runtime: Option[Int], popularity: Double, countries: Seq[String] = Nil,
                           imdbId: Option[String] = None) {
  /** The candidate as a film record — what the constraint model's listing-vs-film rules read. */
  def facts: FilmFacts = FilmFacts(tmdbId, titles.headOption, titles.lift(1), year, directors, runtime, countries, imdbId)
}

object Candidate {

  /** A candidate from every hit naming it and, when known, its own record: the record's facts
   *  win, the hits fill in (a filmography credit carries a year, a search result a popularity). */
  def of(tmdbId: Int, hits: Seq[Hit], facts: Option[FilmFacts]): Candidate = {
    val sorted = hits.sortBy(h => (h.title, h.originalTitle.getOrElse(""), h.year.getOrElse(0), -h.popularity))
    val titles = (facts.toSeq.flatMap(f => f.title.toSeq ++ f.originalTitle) ++
      sorted.flatMap(h => h.title +: h.originalTitle.toSeq)).map(_.trim).filter(_.nonEmpty).distinct
    Candidate(tmdbId, titles,
      year       = facts.flatMap(_.year).orElse(sorted.flatMap(_.year).sorted.headOption),
      directors  = facts.map(_.directors).getOrElse(Nil).sorted,
      runtime    = facts.flatMap(_.runtime).filter(_ > 0),
      popularity = if (sorted.isEmpty) 0.0 else sorted.map(_.popularity).max,
      countries  = facts.map(_.countries).getOrElse(Nil).sorted,
      imdbId     = facts.flatMap(_.imdbId))
  }
}

/** A lookup's answer: what the source KNOWS, or that it does not know. `Unknown` is not "no film":
 *  a query the observation store never recorded is a gap, and the resolver treats its would-be
 *  hits as missing evidence rather than as a definitive no-match (docs/design/identity-resolver.md
 *  §9, the gaps the recorded trees left). */
enum Answer[+A] {
  case Known(value: A)
  case Unknown

  def toOption: Option[A] = this match {
    case Known(v) => Some(v)
    case Unknown  => None
  }
  def isKnown: Boolean = this != Unknown
}

/** The questions the resolver asks. A function of one evidence: which is what makes the whole
 *  query set a function of the listing SET (assumption A1). */
enum CandidateQuery {
  /** A title search, with the year the listing states or none. */
  case Title(query: String, year: Option[Int])
  /** Every film a person of this name directed (or, with no directing credit, wrote). */
  case Director(name: String)

  def sortKey: String = this match {
    case Title(q, y)  => s"t\u0000$q\u0000${y.fold("")(_.toString)}"
    case Director(n)  => s"d\u0000$n"
  }
}

object CandidateQuery {
  implicit val ordering: Ordering[CandidateQuery] = Ordering.by(_.sortKey)
}

/**
 * The only non-pure input: the observations the resolver reads. Each answer must be a function of
 * its argument alone — no argument carries a venue set, a group or a previous answer — which is
 * what lets the resolver enumerate every question from the listing set up front.
 *
 * Minimal on purpose, and named for reconciliation with the observation store (phase 1 owns the
 * observation types; this trait is what the resolver needs of them).
 */
trait IdentityLookups {
  /** Whether the listing's venue publishes a detail page the source can answer for. */
  def hasDetail(listing: Listing): Boolean
  /** The venue's own detail page for the listing. */
  def detail(listing: Listing): Answer[Option[DetailFacts]]
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record. */
  def film(tmdbId: Int): Answer[Option[FilmFacts]]
}
