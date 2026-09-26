package services.identity

import models.{Cinema, CinemaMovie}
import services.movies.{ListingKey, ScrapeListing, TitleNormalizer}

/*
 * The identity resolver's vocabulary (docs/design/identity-resolver.md, phase 2). Everything here
 * is a value: a listing as its venue published it, the evidence it carries once its own detail page
 * is merged in, and the films an external lookup names for it. Nothing is derived from another
 * listing, and nothing is read from stored films.
 */

/** One listing exactly as its venue published it, keyed by [[ListingKey]].
 *
 *  `title` is the title the venue's client published — what the calibrated measures read
 *  (`IdentityMeasures.Listing`), exactly as the calibration read it. `cleanTitle` is that title
 *  after the venue's own title rules (`ScrapeListing.cleanTitle`): only the FAMILY keys and the
 *  title must-links read it, so a "2D PL" suffix does not keep a spelling out of its film's family. */
final case class Listing(
  cinema:        Cinema,
  key:           ListingKey,
  rawTitle:      String,
  title:         String,
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
    Seq(venue, rawTitle, page.getOrElse(""), title, cleanTitle, year.fold("")(_.toString), directors.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse("")).mkString("\u0000")
}

object Listing {

  /** `cm` as `cinema` lists it. Blank director names and non-positive runtimes are absent. */
  def of(cinema: Cinema, cm: CinemaMovie, normalizer: TitleNormalizer): Listing = Listing(
    cinema        = cinema,
    key           = ListingKey.of(cinema, cm),
    rawTitle      = cm.movie.rawTitle.getOrElse(cm.movie.title),
    title         = cm.movie.title,
    cleanTitle    = ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)._1,
    year          = cm.movie.releaseYear,
    directors     = cm.director.map(_.trim).filter(_.nonEmpty).distinct.sorted,
    runtime       = cm.movie.runtimeMinutes.filter(_ > 0),
    page          = cm.filmUrl.map(_.trim).filter(_.nonEmpty),
    originalTitle = cm.movie.originalTitle.map(_.trim).filter(_.nonEmpty),
    countries     = cm.movie.countries.map(_.trim).filter(_.nonEmpty).distinct.sorted)

  implicit val ordering: Ordering[Listing] = Ordering.by(_.sortKey)

  /** Every raw listing of `byCinema`, one per key (the smallest by the total order) —
   *  `ScrapeListing.prepare`'s per-title fold NOT applied, because the resolver reads the rows it
   *  erases ("Sinn und Sinnlichkeit" 1995 beside 2026). The resolver's listing set, wherever the
   *  listings come from: the live scrape archive (the shadow run) or a recorded corpus. */
  def corpus(byCinema: Iterable[(Cinema, Seq[CinemaMovie])], normalizer: TitleNormalizer): Seq[Listing] =
    distinct(all(byCinema, normalizer))

  /** Every listing of `byCinema`, keys not yet made unique — what [[corpus]] reduces with [[distinct]]. */
  def all(byCinema: Iterable[(Cinema, Seq[CinemaMovie])], normalizer: TitleNormalizer): Seq[Listing] =
    byCinema.toSeq.flatMap { case (cinema, films) => films.map(of(cinema, _, normalizer)) }

  /** One listing per key, the smallest by the total order, in that order. The order being total,
   *  reducing parts of a listing set first and their union after gives the same set. */
  def distinct(listings: Seq[Listing]): Seq[Listing] = listings.sorted.distinctBy(_.key)
}

/** What a venue's own detail page adds to its listing: only the identity fields. */
final case class DetailFacts(year: Option[Int], directors: Seq[String], runtime: Option[Int], originalTitle: Option[String],
                             countries: Seq[String] = Nil)

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(title: String, cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil) {
  lazy val key: String =
    Seq(title, cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000")

  /** The evidence as the calibrated measures read a listing. */
  lazy val measured: IdentityMeasures.Listing =
    IdentityMeasures.Listing(title, Some(rawTitle).filter(_ != title), originalTitle, year, runtime, directors, countries)

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = measured.statedYear
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts]): Evidence = Evidence(
    title         = listing.title,
    cleanTitle    = listing.cleanTitle,
    rawTitle      = listing.rawTitle,
    year          = listing.year.orElse(detail.flatMap(_.year)),
    directors     = (if (listing.directors.nonEmpty) listing.directors else detail.map(_.directors).getOrElse(Nil)).sorted,
    runtime       = listing.runtime.orElse(detail.flatMap(_.runtime).filter(_ > 0)),
    originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)),
    countries     = (if (listing.countries.nonEmpty) listing.countries else detail.map(_.countries).getOrElse(Nil)).distinct.sorted)
}

/** One film a lookup NAMED: a search result or a filmography credit. Only what the list itself
 *  carries — the film's own record is an `IdentityMeasures.Film`. */
final case class Hit(tmdbId: Int, title: String, originalTitle: Option[String], year: Option[Int], popularity: Double)

/** A film some listing of a family may be: its TMDB id and what TMDB says about it — its own
 *  record when the lookup source holds one (`TmdbFilmRecord`), else what the hits naming it carry. */
final case class Candidate(tmdbId: Int, film: IdentityMeasures.Film)

object Candidate {

  def of(tmdbId: Int, hits: Seq[Hit], record: Option[IdentityMeasures.Film]): Candidate = {
    val popularity = hits.map(_.popularity).maxOption
    val best = hits.sortBy(h => (-h.popularity, h.title, h.originalTitle.getOrElse(""), h.year.getOrElse(0))).headOption
    Candidate(tmdbId, record.map(f => f.copy(popularity = f.popularity.orElse(popularity))).getOrElse(
      IdentityMeasures.Film(best.fold("")(_.title), best.flatMap(_.originalTitle), Nil, best.flatMap(_.year),
        popularity = popularity)))
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
  /** A yearless title search (`IdentityMeasures.searchQueries`). */
  case Title(query: String)
  /** Every film a person of this name directed (or, with no directing credit, wrote). */
  case Director(name: String)

  def sortKey: String = this match {
    case Title(q)    => s"t\u0000$q"
    case Director(n) => s"d\u0000$n"
  }
}

object CandidateQuery {
  implicit val ordering: Ordering[CandidateQuery] = Ordering.by(_.sortKey)
}

/**
 * The only non-pure input: the observations the resolver reads. Each answer must be a function of
 * its argument alone — no argument carries a venue set, a group or a previous answer — which is
 * what lets the resolver enumerate every question from the listing set up front.
 */
trait IdentityLookups {
  /** Whether the listing's venue publishes a detail page the source can answer for. */
  def hasDetail(listing: Listing): Boolean
  /** The venue's own detail page for the listing. */
  def detail(listing: Listing): Answer[Option[DetailFacts]]
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record (`TmdbFilmRecord`). */
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]
}
