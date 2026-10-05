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
  countries:     Seq[String] = Nil,
  catalogueIds:  Seq[CatalogueId] = Nil,
  /** The title as the venue's rules ask external lookups for it (`TitleNormalizer.apiQuery`: programme
   *  prefixes, accessibility tags, "+ event" suffixes and premiere words off), when that differs from
   *  `title` — what the old pipeline searched. The card keeps the venue's own title. */
  searchTitle:   Option[String] = None,
  /** The venue's poster URL. OUTSIDE the listing's equality and hash (below): the model never reads it — only the
   *  stages after it do ([[PosterEvidence]]), from the listings as published — and a listing the model holds equal to
   *  the one published is not resolved again, so a venue re-cutting a poster URL (a CDN token, a resize) re-resolves
   *  nothing. */
  poster:        Option[String] = None
) {
  def venue: String = key.venue

  override def equals(other: Any): Boolean = other match {
    case that: Listing => (this eq that) || (key == that.key && rawTitle == that.rawTitle && title == that.title && cleanTitle == that.cleanTitle &&
      year == that.year && directors == that.directors && runtime == that.runtime && page == that.page && originalTitle == that.originalTitle &&
      countries == that.countries && catalogueIds == that.catalogueIds && searchTitle == that.searchTitle && cinema == that.cinema)
    case _ => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, mixLast}
    val fields = Array(cinema.##, key.##, rawTitle.##, title.##, cleanTitle.##, year.##, directors.##, runtime.##, page.##, originalTitle.##,
      countries.##, catalogueIds.##)
    var h = 0x4c697374
    var i = 0
    while (i < fields.length) { h = mix(h, fields(i)); i += 1 }
    finalizeHash(mixLast(h, searchTitle.##), fields.length + 1)
  }

  /** A TOTAL order over listings, as text: the key first, then every published field, so two
   *  different listings never tie and a set of listings has exactly one sorted presentation. Built on
   *  demand, never kept: [[Listing.ordering]] compares the same fields without it. */
  def sortKey: String = Listing.SortFields.map(_(this)).mkString("\u0000")
}

/** A film's id in a cinema chain's own catalogue (`CinemaMovie.externalIds`: Gatsby's "boxoffice",
 *  Flicks', Cinema City's "cc", Multikino's "mk"…): the chain lists one film under one id at every
 *  venue, a bare listing beside its credited siblings. Measured on recording 36633286921 across the
 *  five corpora: no source reuses an id for another film, and ids are global to their source. */
final case class CatalogueId(source: String, id: String) {
  /** Its family block key and must-link identity. */
  def key: String = s"c:$source:$id"
}

object CatalogueId {
  given Ordering[CatalogueId] = Ordering.by(c => (c.source, c.id))

  /** `cm`'s catalogue ids, blank ones dropped. */
  def of(cm: CinemaMovie): Seq[CatalogueId] =
    cm.externalIds.toSeq.collect { case (source, id) if source.trim.nonEmpty && id.trim.nonEmpty => CatalogueId(source.trim, id.trim) }.sorted
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
    directors     = cm.director.flatMap(credited).filter(_.nonEmpty).distinct.sorted,
    runtime       = cm.movie.runtimeMinutes.filter(_ > 0),
    page          = cm.filmUrl.map(_.trim).filter(_.nonEmpty),
    originalTitle = cm.movie.originalTitle.map(_.trim).filter(_.nonEmpty),
    countries     = cm.movie.countries.map(_.trim).filter(_.nonEmpty).distinct.sorted,
    catalogueIds  = CatalogueId.of(cm),
    searchTitle   = Some(normalizer.apiQuery(cm.movie.title).trim).filter(q => q.nonEmpty && q != cm.movie.title.trim),
    poster        = cm.posterUrl.map(_.trim).filter(_.nonEmpty))

  /** The people one director credit names — the listing's own, or its detail page's: PL venues join two in one ("Arash T. Riahi & Verena Soltiz", "Joel Crawford
   *  i Januel Mercado", "Natasha Merkulova, Aleksey Chupov"), which searched as one person found no film. Split only
   *  when every part reads as a name — two words or more, no colon — so a credit line ("… scenografia i animacje:
   *  Agata Kurzak") stays as it is. */
  private[identity] def credited(credit: String): Seq[String] = {
    val whole = credit.trim
    val parts = CreditJoin.split(whole).toSeq.map(_.trim)
    if (parts.sizeIs >= 2 && parts.forall(part => !part.contains(':') && part.split("\\s+").lengthIs >= 2)) parts else Seq(whole)
  }
  private val CreditJoin = java.util.regex.Pattern.compile("\\s+(?:&|and|i)\\s+|\\s*,\\s+")

  /** [[Listing.sortKey]]'s fields, in its order. */
  private val SortFields: IndexedSeq[Listing => String] = IndexedSeq(
    _.venue, _.rawTitle, _.page.getOrElse(""), _.title, _.cleanTitle, _.year.fold("")(_.toString),
    _.directors.mkString(","), _.runtime.fold("")(_.toString), _.originalTitle.getOrElse(""), _.catalogueIds.map(_.key).mkString(","))

  /** [[Listing.sortKey]]'s order, field by field — the same order, since the joining NUL sorts below
   *  every character — without building the key: a field is formatted only when those before it tie.
   *  Two listings the key ties — one key published twice, its countries apart — are ordered by their
   *  countries last, so the one a set keeps per key ([[distinct]]) never depends on arrival order. The
   *  countries stay out of the key itself: it names a node ([[EvidenceNode.id]]). */
  implicit val ordering: Ordering[Listing] = (a, b) => {
    var i = 0; var c = 0
    while (c == 0 && i < SortFields.size) { c = SortFields(i)(a).compareTo(SortFields(i)(b)); i += 1 }
    if (c == 0) a.countries.mkString(",").compareTo(b.countries.mkString(",")) else c
  }

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

/** What a language model, asked about a listing no rule took, says it is (`IdentityLookups.proposal`): its
 *  `category` — "film", "compilation" (a package of shorts), "event" (no film: a concert, a play, a workshop),
 *  "stage" (an opera, ballet or theatre broadcast) or "unclear" — and for a film its original title, year and
 *  directors. A PROPOSAL, never a fact: it adds one title search, and the `model-proposed` rule takes a film only
 *  when that search finds its exact title in its year and nothing the venue published contradicts it. */
final case class Proposal(category: String, originalTitle: Option[String] = None, year: Option[Int] = None,
                          directors: Seq[String] = Nil) {
  def isFilm: Boolean = category == Proposal.Film
  def notAFilm: Boolean = Proposal.NotAFilm(category)
  def key: String = Seq(category, originalTitle.getOrElse(""), year.fold("")(_.toString), directors.mkString(",")).mkString("\u0001")
}
object Proposal {
  val Film = "film"
  /** What no film record is: a package of shorts, a live event. */
  val NotAFilm: Set[String] = Set("compilation", "event")
}

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(title: String, cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil,
                          decorations: TitleDecorations = TitleDecorations.None, searchTitle: Option[String] = None,
                          proposal: Option[Proposal] = None) {
  lazy val key: String =
    Seq(title, cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000") +
      proposal.fold("")(p => "\u0000" + p.key)

  /** The evidence as the calibrated measures read a listing, its title shapes undecorated by the
   *  resolve's learned `decorations` (the same for every listing of a resolve, so not in [[key]]). */
  lazy val measured: IdentityMeasures.Listing =
    IdentityMeasures.Listing(title, Some(rawTitle).filter(_ != title), originalTitle, year, runtime, directors, countries,
      decorations = decorations, searchTitles = searchTitle.toSeq, proposal = proposal)

  /** [[measured]] with the title shapes the venue's own delimiters leave, no learned decoration nor search title
   *  stripped: what relates two LISTINGS (their families, title must-links and listing-listing
   *  measures). A learned decoration names a FILM to search for and relate to; it never links a
   *  "Horror Season 2026 Dracula" to every other venue's bare "Dracula". */
  lazy val published: IdentityMeasures.Listing = measured.copy(decorations = TitleDecorations.None, searchTitles = Nil)

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = measured.statedYear
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts], decorations: TitleDecorations = TitleDecorations.None,
         proposal: Option[Proposal] = None): Evidence = Evidence(
    title         = listing.title,
    cleanTitle    = listing.cleanTitle,
    rawTitle      = listing.rawTitle,
    year          = listing.year.orElse(detail.flatMap(_.year)),
    directors     = (if (listing.directors.nonEmpty) listing.directors
                     else detail.map(_.directors.flatMap(Listing.credited).filter(_.nonEmpty).distinct).getOrElse(Nil)).sorted,
    runtime       = listing.runtime.filter(services.movies.FilmRuntime.plausible).orElse(detail.flatMap(_.runtime).filter(services.movies.FilmRuntime.plausible)),
    originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)),
    countries     = (if (listing.countries.nonEmpty) listing.countries else detail.map(_.countries).getOrElse(Nil)).distinct.sorted,
    decorations   = decorations,
    searchTitle   = listing.searchTitle,
    proposal      = proposal)
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
 *  a query the store holds no answer to is a gap, and the resolver treats its would-be
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
  /** Every film IMDb lists under this very title, found in TMDB by its IMDb id: a path to a record
   *  TMDB's own search does not return ("Caligula: The Ultimate Cut", whose record only an IMDb id
   *  reaches). A path, never a rank: its films are scored on the listing's facts alone. */
  case Imdb(title: String)
  /** Every film IMDb lists under this very title in ANY language — its own title, its original or one of its
   *  AKAs — found in TMDB by its IMDb id: the record a local title names when TMDB carries no translation of it
   *  ("Camino dla opornych" is IMDb's Polish title of TMDB's "Compostelle", "Kuźma" IMDb's "Kuzma"). Each film
   *  found reads the title as one of its own ([[CorpusContext.titlesByImdb]]); a path, never a rank. The ones TMDB holds
   *  no record of are answered too, under their FALLBACK ids ([[FallbackIds]]): never a candidate TMDB's rules weigh,
   *  only what a cluster no TMDB film was taken for may fall back to ([[ResolverDecision.fallback]]). */
  case ImdbTitled(title: String)

  def sortKey: String = this match {
    case Title(q)      => s"t\u0000$q"
    case Director(n)   => s"d\u0000$n"
    case Imdb(t)       => s"i\u0000$t"
    case ImdbTitled(t) => s"a\u0000$t"
  }

}

object CandidateQuery {
  implicit val ordering: Ordering[CandidateQuery] = Ordering.by(_.sortKey)

  /** The query a [[CandidateQuery.sortKey]] names. */
  def fromSortKey(key: String): Option[CandidateQuery] = key.split("\u0000", 2) match {
    case Array("t", text) => Some(Title(text))
    case Array("d", name) => Some(Director(name))
    case Array("i", text) => Some(Imdb(text))
    case Array("a", text) => Some(ImdbTitled(text))
    case _                => None
  }

  /** A director credit as TMDB's person search knows it: without a trailing IMDb disambiguator —
   *  " (I)", " (II)", " (III)", … Case-sensitive and limited to well-formed numerals below C, because
   *  that is all IMDb ever writes: a lowercase "(mix)" or "(vi)", a malformed "(IIII)", or a word-like
   *  "(MIX)" is part of the credit. */
  def personName(name: String): String = ImdbDisambiguatorSuffix.replaceFirstIn(name, "").trim

  private val ImdbDisambiguatorSuffix: scala.util.matching.Regex =
    """\s+\((?=[IVXL])(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})\)$""".r
}

/**
 * The only non-pure input: the observations the resolver reads. Each answer must be a function of
 * its argument alone — no argument carries a venue set, a group or a previous answer — which is
 * what lets the resolver enumerate every question from the listing set up front.
 */
trait IdentityLookups {
  /** Whether the listing's venue publishes a detail page the source can answer for. */
  def hasDetail(listing: Listing): Boolean
  /** The questions a caller is about to ask, one by one — a source that can answer them together
   *  (in parallel, over the network) may, and serve the asks that follow from what it fetched.
   *  Every read phase begins with one, so nothing fetched for an earlier phase is served to a later. */
  def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = ()
  /** The asks a [[prefetch]] was for have been asked: what it fetched to answer them may go. */
  def prefetchAnswered(): Unit = ()
  /** Questions no listing held asks any more — a source tracking what each question read may forget them. */
  def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[services.movies.ListingKey]): Unit = ()
  /** The venue's own detail page for the listing. */
  def detail(listing: Listing): Answer[Option[DetailFacts]]
  /** What a language model proposed the listing is ([[Proposal]]) — none unless a source holds one. */
  def proposal(listing: Listing): Option[Proposal] = None
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record (`TmdbFilmRecord`). */
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]
}
