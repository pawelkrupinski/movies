package services.identity

import models.{Cinema, CinemaMovie}
import services.movies.{ListingKey, ScrapeListing, SlotFields, TitleNormalizer}

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
   *  nothing — the model holds the re-published listing in its place all the same ([[Listing.movedOutsideEquality]]). */
  poster:        Option[String] = None,
  /** The days the venue screens it. OUTSIDE the listing's equality and hash, as the poster is: a venue adding a
   *  showtime re-resolves nothing. Only the season they place a stage relay in ([[broadcastSeason]]) is the model's;
   *  the days themselves are read after it, by the broadcast take ([[agreement.Broadcast]]). */
  screenings:    ScreeningDays = ScreeningDays.None,
  /** The people the venue's own synopsis and cast field name ([[VenueNames]]): what the cast evidence reads
   *  ([[CastEvidence]]) — none for a listing whose facts a feed catalogue states ([[factsFromCatalogue]]), whose text
   *  describes the catalogue's entry. OUTSIDE the listing's equality and hash, as the poster is: the model never reads
   *  it, only the agreement stage's fill after it. */
  names:         VenueNames = VenueNames.None,
  /** Do its facts include what its page states — the listing as the agreement reads it, its page's facts merged in where
   *  it states none (`AgreementStage.asStated`)? OUTSIDE equality and hash: only the agreement builds such a listing, and
   *  only to tell whose claim the merged facts are ([[factsFromCatalogue]]). */
  pageFacts:     Boolean = false
) {
  def venue: String = key.venue

  /** Are its year, directors, running time and poster an aggregator catalogue's claim, copied by the showtimes feed that
   *  linked the screening to its entry, rather than its venue's own statement ([[CatalogueSources.feedStated]])? They are
   *  evidence FOR that entry's film, never proof that the entry is the venue's film: they never confirm the feed's own
   *  catalogue id, nor rule out another film outright. */
  def factsFromCatalogue: Boolean = CatalogueSources.feedStated(this) || (pageFacts && page.exists(CatalogueSources.catalogueEntry))

  /** The season a stage work billed with neither its season nor a year is broadcast in: the one its first screening
   *  falls in ([[ScreeningDays.season]]). A relay airs on its house's published dates — the Met's "Samson et Dalila"
   *  on 5 December 2026 — so a PL "Samson i Dalila" screening that day is the 2026/27 season's, whatever its title
   *  leaves out. `None` for anything else: a film's run moving past July re-resolves nothing. */
  lazy val broadcastSeason: Option[Int] =
    // a credited director or a running time already names the staging: UK "Royal Shakespeare Company: Macbeth" {Polly
    // Findlay} is her RSC Live record, US "Royal Opera House: Otello" [190′] the ROH's 2017 one, which the season's Met
    // records of the work would only stand beside
    if (screenings.isEmpty || year.isDefined || directors.nonEmpty || runtime.isDefined) None
    else {
      val titles = Seq(title, rawTitle)
      Option.when(IdentityMeasures.seasonYear(titles).isEmpty && IdentityMeasures.titleYearOf(titles).isEmpty &&
        IdentityMeasures.stageWorksBilled(titles, seasonNamed = false).nonEmpty)(()).flatMap(_ => screenings.season)
    }

  override def equals(other: Any): Boolean = other match {
    case that: Listing => (this eq that) || (key == that.key && rawTitle == that.rawTitle && title == that.title && cleanTitle == that.cleanTitle &&
      year == that.year && directors == that.directors && runtime == that.runtime && page == that.page && originalTitle == that.originalTitle &&
      countries == that.countries && catalogueIds == that.catalogueIds && searchTitle == that.searchTitle && cinema == that.cinema &&
      broadcastSeason == that.broadcastSeason)
    case _ => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, mixLast}
    val fields = Array(cinema.##, key.##, rawTitle.##, title.##, cleanTitle.##, year.##, directors.##, runtime.##, page.##, originalTitle.##,
      countries.##, catalogueIds.##)
    var h = 0x4c697374
    var i = 0
    while (i < fields.length) { h = mix(h, fields(i)); i += 1 }
    h = mix(h, searchTitle.##)
    finalizeHash(mixLast(h, broadcastSeason.##), fields.length + 2)
  }

  /** A TOTAL order over listings, as text: the key first, then every published field, so two
   *  different listings never tie and a set of listings has exactly one sorted presentation. Built on
   *  demand, never kept: [[Listing.ordering]] compares the same fields without it. */
  def sortKey: String = Listing.SortFields.map(_(this)).mkString("\u0000")
}

/** The days a listing screens on, each once, in order — held as epoch days: a corpus holds a listing per film and
 *  venue, and most screen on a handful of days. Or [[ScreeningDays.Unknown]]: a read of them that failed — no day,
 *  but never "screens on none" (a failed read is not data): the broadcast take waits on it. */
final class ScreeningDays private (private val epochDays: Array[Int]) {
  /** No day, and known to be none. */
  def isEmpty: Boolean = epochDays.isEmpty
  /** The days could not be read. */
  def isUnknown: Boolean = this eq ScreeningDays.Unknown
  def days: Seq[java.time.LocalDate] = if (isUnknown) Nil else epochDays.toSeq.map(day => java.time.LocalDate.ofEpochDay(day.toLong))
  def contains(day: java.time.LocalDate): Boolean = !isUnknown && java.util.Arrays.binarySearch(epochDays, day.toEpochDay.toInt) >= 0
  def first: Option[java.time.LocalDate] = days.headOption
  def last: Option[java.time.LocalDate]  = days.lastOption
  /** The performing season the first day falls in, by the year it opens: a season runs from July to June, so
   *  January 2027 is the 2026/27 season's, as August 2026 is. */
  def season: Option[Int] = first.map(day => if (day.getMonthValue >= ScreeningDays.SeasonOpens) day.getYear else day.getYear - 1)
  /** `other`'s days beside these — unknown when either is. */
  def ++(other: ScreeningDays): ScreeningDays =
    if (isUnknown || other.isUnknown) ScreeningDays.Unknown
    else if (other.isEmpty) this else if (isEmpty) other else new ScreeningDays((epochDays ++ other.epochDays).distinct.sorted)
  override def equals(other: Any): Boolean = other match {
    case that: ScreeningDays => java.util.Arrays.equals(epochDays, that.epochDays)
    case _ => false
  }
  override def hashCode: Int = java.util.Arrays.hashCode(epochDays)
  override def toString: String = if (isUnknown) "ScreeningDays(unknown)" else days.mkString("ScreeningDays(", ", ", ")")
}

object ScreeningDays {
  val None: ScreeningDays = new ScreeningDays(Array.emptyIntArray)
  /** The day standing for "not read" where days travel as showtimes (`services.scrapes.LeanListing.unread`): no venue
   *  screens in year 1. */
  val UnreadDay: java.time.LocalDate = java.time.LocalDate.of(1, 1, 1)
  /** Days that could not be read. */
  val Unknown: ScreeningDays = new ScreeningDays(Array(UnreadDay.toEpochDay.toInt))
  /** The month a performing season opens in. */
  val SeasonOpens = 7
  def of(days: Iterable[java.time.LocalDate]): ScreeningDays =
    if (days.isEmpty) None
    else if (days.exists(_ == UnreadDay)) Unknown
    else new ScreeningDays(days.iterator.map(_.toEpochDay.toInt).toArray.distinct.sorted)
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
  /** Does `now` publish what the stages after the model read differently from `was` — its poster, its screening days, the
   *  names its venue's text gives — though the two are equal ([[Listing.equals]] leaves those out)? The model then holds
   *  `now` without resolving it again, and the agreement reads it afresh ([[agreement.AgreementStage]]). Days count only
   *  where both sides know them: a lean read leaves out the days of a film billing no stage work
   *  (`services.scrapes.LeanListing.leanFilm`), which beside the model's object holding the scrape's days moved nothing —
   *  read as moved, every venue read again looked re-published, on every tick. */
  def movedOutsideEquality(was: Listing, now: Listing): Boolean =
    was.poster != now.poster || was.names != now.names ||
      (knowsDays(was.screenings) && knowsDays(now.screenings) && was.screenings != now.screenings)
  private def knowsDays(days: ScreeningDays): Boolean = !days.isEmpty && !days.isUnknown


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
    // as the card serves it ([[services.movies.CinemaSlotBuilder]]): absolute against its page, escaped — a site-relative
    // `img src` (PL Kino Roma's "/app/assets/movie/…") is a link no poster fetch takes
    poster        = SlotFields.url(cm.posterUrl, SlotFields.url(cm.filmUrl, None)),
    screenings    = ScreeningDays.of(cm.showtimes.map(_.dateTime.toLocalDate)),
    names         = venueNames(cm))

  /** What `cm`'s own synopsis and cast field name — its matching-only excerpt where the venue cut the synopsis (MSI) — none
   *  where a feed copied them from its catalogue entry. */
  private def venueNames(cm: CinemaMovie): VenueNames = {
    val text = cm.synopsis.orElse(cm.synopsisExcerpt)
    if ((text.isEmpty && cm.cast.isEmpty) || CatalogueSources.feedStated(CatalogueId.of(cm), cm.filmUrl.map(_.trim))) VenueNames.None
    else VenueNames.of(text, cm.cast)
  }

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

/** What a venue's own detail page adds to its listing: the identity fields the model reads, and the page's synopsis and
 *  cast, which only the cast evidence reads ([[names]]) — the page's own strings, as the venue page index holds them,
 *  never copied. */
final case class DetailFacts(year: Option[Int], directors: Seq[String], runtime: Option[Int], originalTitle: Option[String],
                             countries: Seq[String] = Nil, synopsis: Option[String] = None, cast: Seq[String] = Nil) {
  /** The people the page's synopsis and cast name: worked out when the cast evidence asks, never held. */
  def names: VenueNames = if (synopsis.isEmpty && cast.isEmpty) VenueNames.None else VenueNames.of(synopsis, cast)
}

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(title: String, cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil,
                          decorations: TitleDecorations = TitleDecorations.None, searchTitle: Option[String] = None,
                          /** The season a stage relay billing neither its season nor a year screens in ([[Listing.broadcastSeason]]):
                           *  what its title is searched with, never a fact it states. */
                          broadcastSeason: Option[Int] = None) {
  lazy val key: String =
    Seq(title, cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000") +
      broadcastSeason.fold("")(season => s"\u0000broadcast:$season")

  /** The evidence as the calibrated measures read a listing, its title shapes undecorated by the
   *  resolve's learned `decorations` (the same for every listing of a resolve, so not in [[key]]). */
  lazy val measured: IdentityMeasures.Listing =
    IdentityMeasures.Listing(title, Some(rawTitle).filter(_ != title), originalTitle, year, runtime, directors, countries,
      decorations = decorations, searchTitles = searchTitle.toSeq, broadcastSeason = broadcastSeason)

  /** [[measured]] with the title shapes the venue's own delimiters leave, no learned decoration nor search title
   *  stripped: what relates two LISTINGS (their families, title must-links and listing-listing
   *  measures). A learned decoration names a FILM to search for and relate to; it never links a
   *  "Horror Season 2026 Dracula" to every other venue's bare "Dracula". */
  lazy val published: IdentityMeasures.Listing = measured.copy(decorations = TitleDecorations.None, searchTitles = Nil, broadcastSeason = None)

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = measured.statedYear
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts], decorations: TitleDecorations = TitleDecorations.None): Evidence = Evidence(
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
    // a year the detail page states dates the relay as a year the venue's own field does
    broadcastSeason = listing.broadcastSeason.filter(_ => detail.flatMap(_.year).isEmpty))
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
    // A record that carries its popularity is the candidate's film itself: a copy restating it was a second Film per
    // recorded film beside the corpus's `records` (30k on worker-uk, 2026-10-07).
    Candidate(tmdbId, record.map(f => if (f.popularity.isDefined) f else f.copy(popularity = popularity)).getOrElse(
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
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record (`TmdbFilmRecord`). */
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]
  /** The day a film was released, as its record states it: `Unknown` while the record is unknown — or, in a store, while
   *  it was filed before records kept the whole day and holds only the year TMDB dated it in
   *  ([[agreement.Broadcast]] reads the day; nothing else does). */
  def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    case Answer.Known(record) => Answer.Known(record.flatMap(_.released))
    case Answer.Unknown       => Answer.Unknown
  }
  /** A film's top-billed cast as TMDB credits it ([[TmdbFilmRecord.cast]]): what the cast evidence reads
   *  ([[CastEvidence]]), never the model. Read apart from its record ([[film]]), as the release day is: every candidate
   *  of every family holds a record, and only an unmatched cluster's fill reads a cast. `Known(None)` where the source
   *  does not know it — a record filed before records kept the cast — which the evidence reads as nothing to go on. */
  def cast(tmdbId: Int): Answer[Option[Seq[String]]] = Answer.Known(None)
}
