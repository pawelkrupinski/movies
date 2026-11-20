package services.identity

import models.SlotFields

import models.{Cinema, CinemaMovie, ListingKey}
import services.movies.ScrapeListing
import services.titles.{TitleNormalizer, TitleSeason}

import scala.annotation.unused
import services.identity.vocabulary.{Answer, CandidateQuery, CatalogueId, DetailFacts, Hit, ListedCast, ProseWords, ScreeningDays, TitleDecorations, VenueNames}

/*
 * The identity resolver's vocabulary (docs/design/identity-resolver.md, phase 2). Everything here
 * is a value: a listing as its venue published it, the evidence it carries once its own detail page
 * is merged in, and the films an external lookup names for it. Nothing is derived from another
 * listing, and nothing is read from stored films.
 */

/** One listing exactly as its venue published it, keyed by [[ListingKey]].
 *
 *  `title` is the title the venue's client published — what the calibrated measures read
 *  (`MeasuredListing`), exactly as the calibration read it. `cleanTitle` is that title
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
   *  showtime re-resolves nothing. Only the season they place a stage relay in ([[broadcastSeason]]) is the model's, and
   *  the days of a relay whose air day decides its record ([[airDays]], in the equality: a venue adding the air day of
   *  UK Flicks' stale-season "Royal Ballet & Opera 2024-25: Romeo and Juliet" must resolve it again, or the incremental
   *  model keeps a take a whole resolve no longer makes); every other listing's days are read after it, by the
   *  broadcast take ([[agreement.Broadcast]]). */
  screenings:    ScreeningDays = ScreeningDays.None,
  /** The people the venue's own synopsis and cast field name ([[VenueNames]]): what the cast evidence reads
   *  ([[CastEvidence]]) — none for a listing whose facts a feed catalogue states ([[factsFromCatalogue]]), whose text
   *  describes the catalogue's entry. OUTSIDE the listing's equality and hash, as the poster is: the model never reads
   *  it, only the agreement stage's fill after it. */
  names:         VenueNames = VenueNames.None,
  /** The words the venue's own synopsis capitalises ([[ProseWords.ofProse]]): what the synopsis evidence reads against the
   *  TMDB descriptions of the films the agreement takes ([[agreement.SynopsisEvidence]]) — none for a listing whose facts a
   *  feed catalogue states, as for [[names]]. OUTSIDE the listing's equality and hash, as the poster is. */
  words:         ProseWords = ProseWords.None,
  /** Do its facts include what its page states — the listing as the agreement reads it, its page's facts merged in where
   *  it states none (`AgreementStage.asStated`)? OUTSIDE equality and hash: only the agreement builds such a listing, and
   *  only to tell whose claim the merged facts are ([[factsFromCatalogue]]). */
  pageFacts:     Boolean = false,
  /** The title its broadcaster's banner spelled by its proper name, searched beside [[searchTitle]] where the venue's
   *  rules ask that ([[Listing.searchTitlesOf]]): US "NT Live: Hamlet + Q&A" searches "NT Live: Hamlet" and "National
   *  Theatre Live: Hamlet". */
  properTitle:   Option[String] = None,
  /** The age certificate the venue bills it with (its `ageRating`, from its feed or peeled off its title at intake,
   *  `services.titles.TitleFacts`): which cut of which film it screens, as the fill's guard reads it ([[Certificates.below]]).
   *  The model weighs it too (the `certificate` measure, [[IdentityMeasures.certificateRelation]]): inside equality and hash. */
  certificate:   Option[String] = None,
  /** The venue's own cast FIELD, as the cast mismatch reads it ([[ListedCast]]) — none for a
   *  listing whose facts a feed catalogue states, as for [[names]]. The model reads it ([[CastEvidence.mismatch]]): inside
   *  equality and hash. */
  listedCast:    ListedCast = ListedCast.None,
  /** The latest year its film can be from, where its source dates an event the film cannot precede and not the film
   *  ([[models.Movie.releaseYearUpperBound]]: Cineamo's German premiere of an older film). It weighs only against a film
   *  released after it (`IdentityMeasures.yearBound`), never for one. Inside equality and hash. */
  yearUpperBound: Option[Int] = None,
  /** The year of the performance its venue's own synopsis says a stage relay screens ("the indelible 2010 Met
   *  performance", [[StageRelays.performanceYear]]): what dates an ENCORE's relay, whose billed year is the encore's
   *  ([[RelayStage.of]]). Inside equality and hash. */
  performanceYear: Option[Int] = None,
  /** Is it KEPT for its facts alone ([[ListingRetention]]): no venue bills it now, a present listing bills its title?
   *  An input fact, inside equality and hash: its facts keep its film's details alive, but it never counts toward
   *  overruling a present listing's feed link, nor joins a present listing whose link the present sources overrule
   *  toward another film ([[FeedLink]], [[ConstraintEdges.KeptOverruledApart]]). */
  kept: Boolean = false
) {
  def venue: String = key.venue

  /** The listing as its venue's own detail `page` states it: the page's facts merged in where the listing states none
   *  ([[Evidence.of]]), marked as the page's ([[pageFacts]]) — what the agreement stage and the suspect-take audit read. */
  def withPage(page: DetailFacts): Listing = {
    val stated = Evidence.of(this, Some(page))
    copy(year = stated.year, directors = stated.directors, runtime = stated.runtime, originalTitle = stated.originalTitle,
      countries = stated.countries, pageFacts = true)
  }

  /** Are its facts another source's claim than its venue's — a feed's, a listings site's entry's ([[SourceReliability]],
   *  which says how far each fact of it is trusted)? */
  def factsFromCatalogue: Boolean = SourceReliability.factsOf(this) ne SourceReliability.Venue

  /** The season a stage work billed with neither its season nor a year is broadcast in: the one its first screening
   *  falls in ([[ScreeningDays.season]]). A relay airs on its house's published dates — the Met's "Samson et Dalila"
   *  on 5 December 2026 — so a PL "Samson i Dalila" screening that day is the 2026/27 season's, whatever its title
   *  leaves out. `None` for anything else: a film's run moving past July re-resolves nothing. */
  lazy val broadcastSeason: Option[Int] =
    // a credited director or a running time already names the staging: UK "Royal Shakespeare Company: Macbeth" {Polly
    // Findlay} is her RSC Live record, US "Royal Opera House: Otello" [190′] the ROH's 2017 one, which the season's Met
    // records of the work would only stand beside
    screenedSeason.filter(_ => year.isEmpty && directors.isEmpty && runtime.isEmpty)
  /** The season a stage work billed with no season and no year of its own screens in, whatever else it states: what tells
   *  apart a house's records of the work's SEASONS ([[Acceptance.seasonRecord]]), where a credit or a running time names
   *  no season — UK "Royal Ballet and Opera: Tosca" {Oliver Mears} [195′] ×123, screening on 5 May 2027, is the house's
   *  2026/27 relay of his production, not its 2025/26 one, which alone credits him. Searched with its work only where the
   *  billing names its house, or the whole title is the work and states a running time at most
   *  ([[SearchMeasures.stageWorkQueries]]); [[broadcastSeason]] is searched wherever it is set.
   *
   *  So is the season of one stating a year BEFORE that season opens: a relay screening in a later season than its year
   *  dates the staging's premiere by it, not the broadcast — UK Aldeburgh's "The Royal Ballet: Romeo & Juliet" [2025],
   *  screening on 25 May 2027, the day the RBO's 2026/27 relay of it airs ([[StageRelays.screenedSeasonRecord]]).
   *  A year within or after the season is the venue's dating of the broadcast, and stays the only one read. */
  lazy val screenedSeason: Option[Int] =
    if (screenings.isEmpty) None
    else {
      val titles = Seq(title, rawTitle)
      Option.when(TitleSeason.of(titles, screenings.season).isEmpty && IdentityMeasures.titleYearOf(titles).isEmpty &&
        StageRelays.stageWorksBilled(titles, seasonNamed = false).nonEmpty)(()).flatMap(_ => screenings.season)
        .filter(season => year.forall(_ < season))
    }

  /** Its screening days where its stated year is more than [[Listing.StagingPredatesSeason]] before the season it
   *  screens in ([[screenedSeason]]): a year that old dates the staging only when a screening falls on the season
   *  record's air day ([[StageRelays.screenedSeasonRecord]]) — the Met's 2014 "Macbeth" revival relayed on 17 October
   *  2026 — and an encore of an old capture, on no air day, keeps its year. So are those of a relay whose title names a
   *  season further back than a season's record is encored ([[namesAStaleSeason]]). Empty for every other listing. */
  lazy val airDays: Seq[java.time.LocalDate] =
    screenedSeason.filter(season => year.exists(stated => season - stated > Listing.StagingPredatesSeason)).orElse(titleDatesItsStaging)
      .orElse(namesAStaleSeason).fold(Seq.empty[java.time.LocalDate])(_ => screenings.days)
  /** The year its first screening falls in, where its title bills an anniversary ("The Host: 20th Anniversary", screening
   *  in 2026): what dates the film that many years before when it states no year ([[IdentityMeasures.anniversaryFilmYear]]).
   *  None for every other listing, so no other listing's evidence moves with its days. */
  lazy val anniversaryScreeningYear: Option[Int] =
    screenings.firstYear.filter(_ => IdentityMeasures.anniversaryOf(Seq(title, rawTitle) ++ originalTitle).isDefined)

  /** The season it screens in, when it states no year and its title brackets one before it beside a stage work: that year
   *  may date the staging, not the broadcast (UK Ilkley's "The Royal Ballet: The Nutcracker (2025)", screening on the
   *  2026/27 relay's air day), or the performance an old capture screens ("MetOpera: Carmen (2009)") — its days tell
   *  ([[RelayStage.Billed.airedOn]]). */
  private def titleDatesItsStaging: Option[Int] = {
    val titles = Seq(title, rawTitle)
    screenings.season.filter(season => year.isEmpty && IdentityMeasures.titleYearOf(titles).exists(_ < season) &&
      StageRelays.stageWorksBilled(titles, seasonNamed = false).nonEmpty)
  }
  /** The season it screens in, when its title bills a stage work under a season more than [[Listing.SeasonEncoreLag]]
   *  before it: a season's record is encored the next season at most, so a venue billing an older one may screen its
   *  house's capture of it, or the new season's relay under the page it copied — its days tell
   *  ([[RelayStage.Billed.airedOn]]). UK Flicks' "Royal Ballet & Opera 2024-25: Romeo and Juliet" ×16, screening on 25
   *  May 2027, the day the house's 2026/27 relay of it airs, is that relay; DE "Royal Opera House 2017/18: Alice im
   *  Wunderland", on no air day, stays its season's capture. */
  private[identity] lazy val namesAStaleSeason: Option[Int] = {
    val titles = Seq(title, rawTitle)
    screenings.season.filter(season => TitleSeason.of(titles, Some(season)).exists(named => season - named > Listing.SeasonEncoreLag) &&
      StageRelays.stageWorksBilled(titles, seasonNamed = true).nonEmpty)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Listing => (this eq that) || (sameTitles(that) && sameFacts(that) && sameDating(that))
    case _ => false
  }
  // [[equals]] in three parts, compared in this order: its names first, which tell most listings apart, then the facts it
  // states, then the years and days that date it (lazy, built on demand)
  private def sameTitles(that: Listing): Boolean =
    key == that.key && rawTitle == that.rawTitle && title == that.title && cleanTitle == that.cleanTitle && cinema == that.cinema &&
      originalTitle == that.originalTitle && searchTitle == that.searchTitle && properTitle == that.properTitle
  private def sameFacts(that: Listing): Boolean =
    year == that.year && directors == that.directors && runtime == that.runtime && page == that.page && countries == that.countries &&
      catalogueIds == that.catalogueIds && listedCast == that.listedCast && certificate == that.certificate && kept == that.kept
  private def sameDating(that: Listing): Boolean =
    yearUpperBound == that.yearUpperBound && performanceYear == that.performanceYear && screenedSeason == that.screenedSeason &&
      anniversaryScreeningYear == that.anniversaryScreeningYear && airDays == that.airDays
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, mixLast}
    val fields = Array(cinema.##, key.##, rawTitle.##, title.##, cleanTitle.##, year.##, directors.##, runtime.##, page.##, originalTitle.##,
      countries.##, catalogueIds.##, kept.##)
    var h = 0x4c697374
    var i = 0
    while (i < fields.length) { h = mix(h, fields(i)); i += 1 }
    h = mix(h, searchTitle.##)
    h = mix(h, properTitle.##)
    h = mix(h, listedCast.##)
    h = mix(h, certificate.##)
    h = mix(h, yearUpperBound.##)
    h = mix(h, performanceYear.##)
    finalizeHash(mixLast(h, screenedSeason.##), fields.length + 7)
  }

  /** A TOTAL order over listings, as text: the key first, then every published field, so two
   *  different listings never tie and a set of listings has exactly one sorted presentation. Built on
   *  demand, never kept: [[Listing.ordering]] compares the same fields without it. */
  def sortKey: String = Listing.SortFields.map(_(this)).mkString("\u0000")
}

object Listing {
  /** How many seasons before the one it screens in a relay's stated year dates its staging with no further test
   *  ([[Listing.airDays]]): UK Aldeburgh's "The Royal Ballet: Romeo & Juliet" [2025] screening in the 2026/27 season. */
  val StagingPredatesSeason = 1
  /** How many seasons after its own a season's record is still encored ([[Listing.airDays]]): the next one. */
  val SeasonEncoreLag = 1

  /** Does `now` publish what the stages after the model read differently from `was` — its poster, its screening days, the
   *  names and words its venue's text gives — though the two are equal ([[Listing.equals]] leaves those out)? The model then holds
   *  `now` without resolving it again, and the agreement reads it afresh ([[agreement.AgreementStage]]). Days count only
   *  where both sides know them: a lean read leaves out the days of a film billing no stage work
   *  (`models.LeanListing.leanFilm`), which beside the model's object holding the scrape's days moved nothing —
   *  read as moved, every venue read again looked re-published, on every tick. */
  def movedOutsideEquality(was: Listing, now: Listing): Boolean =
    was.poster != now.poster || was.names != now.names || was.words != now.words ||
      (knowsDays(was.screenings) && knowsDays(now.screenings) && was.screenings != now.screenings)
  private def knowsDays(days: ScreeningDays): Boolean = !days.isEmpty && !days.isUnknown

  /** `now`, re-published equal to the `held` listing but moved outside the equality ([[movedOutsideEquality]]): to be held
   *  in its place under `held`'s own key object, the one the model's families name. None where `now` is `held` itself, or
   *  differs from it, or moved nothing. */
  def republishedAs(held: Listing, now: Listing): Option[Listing] =
    Option.when((held ne now) && held == now && movedOutsideEquality(held, now))(now.copy(key = held.key))

  /** `l` as the MODEL reads it: every field inside its equality ([[Listing.equals]]), none of the stage's alone (its
   *  poster, days, names, words, page facts): what a family's model entry digests ([[FamilyIndex.modelDigest]]). */
  def asTheModelReads(l: Listing): Listing =
    l.copy(poster = None, screenings = ScreeningDays.None, names = VenueNames.None, words = ProseWords.None, pageFacts = false)

  /** `l`'s facts alone: its screenings, names and words (what re-publishes it daily, and most of its weight) off. */
  def factsAlone(l: Listing): Listing = l.copy(screenings = ScreeningDays.None, names = VenueNames.None, words = ProseWords.None)

  /** The facts `scraped`'s title bills, in its own fields and off its title, as intake gives the slot
   *  (`ScrapeListing.prepare`). */
  private def withFacts(cinema: Cinema, scraped: CinemaMovie, normalizer: TitleNormalizer): CinemaMovie =
    ScrapeListing.withTitleFacts(scraped, normalizer.titleFacts(cinema, ScrapeListing.billedTitle(scraped)))

  /** The key [[of]] gives `scraped`: read with the facts its title bills, so a credit the title names ("… reż. Andrzej
   *  Wajda", "Alpha (dir. Ducournau)") is in it. Everything that looks a venue's row up by a listing's key keys the row
   *  by this — keyed by the bare row, such a listing's film was drafted with no slot at its venue (record runs 408,
   *  469). */
  def keyOf(cinema: Cinema, scraped: CinemaMovie, normalizer: TitleNormalizer): ListingKey =
    ListingKey.of(cinema, withFacts(cinema, scraped, normalizer))

  /** `scraped` as `cinema` lists it, the facts its title bills read into its fields ([[services.titles.TitleFacts]]). Blank
   *  director names and non-positive runtimes are absent. Its title as every title is read
   *  ([[services.titles.TitleText.tidy]]), those facts off it: a box office filing a film library-style ("Tribute von Panem - Mockingjay Teil 2 (WA: 2026),
   *  Die") is searched and measured as "Die Tribute von Panem - …", as its family key already reads it — searched as filed,
   *  only its pieces found films, and it leaned to "Catching Fire". The raw title stays as published. */
  def of(cinema: Cinema, scraped: CinemaMovie, normalizer: TitleNormalizer): Listing = {
    val cm    = withFacts(cinema, scraped, normalizer)
    val title = normalizer.factFree(cm.movie.title)
    val (searchTitle, properTitle) = searchTitlesOf(title, normalizer)
    Listing(
      cinema        = cinema,
      key           = ListingKey.of(cinema, cm),
      rawTitle      = cm.movie.rawTitle.getOrElse(cm.movie.title),
      title         = title,
      cleanTitle    = ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)._1,
      year          = cm.movie.releaseYear,
      directors     = cm.director.flatMap(credited).filter(_.nonEmpty).distinct.sorted,
      runtime       = cm.movie.runtimeMinutes.filter(_ > 0),
      page          = cm.filmUrl.map(_.trim).filter(_.nonEmpty),
      originalTitle = cm.movie.originalTitle.map(_.trim).filter(_.nonEmpty),
      countries     = cm.movie.countries.map(_.trim).filter(_.nonEmpty).distinct.sorted,
      catalogueIds  = CatalogueId.of(cinema, cm),
      searchTitle   = searchTitle,
      // as the card serves it ([[services.movies.CinemaSlotBuilder]]): absolute against its page, escaped — a site-relative
      // `img src` (PL Kino Roma's "/app/assets/movie/…") is a link no poster fetch takes
      poster        = SlotFields.url(cm.posterUrl, SlotFields.url(cm.filmUrl, None)),
      screenings    = ScreeningDays.of(cm.showtimes.map(_.dateTime.toLocalDate)),
      names         = venueNames(cinema, cm),
      words         = venueWords(cinema, cm),
      properTitle   = properTitle,
      certificate   = cm.ageRating.map(_.trim).filter(_.nonEmpty),
      listedCast    = if (cm.cast.isEmpty || !namesAreVenues(cinema, cm)) ListedCast.None else ListedCast.of(cm.cast),
      yearUpperBound = cm.movie.releaseYearUpperBound,
      performanceYear = synopsisOf(cm).filter(_ => namesAreVenues(cinema, cm)).flatMap(StageRelays.performanceYear))
  }

  /** The titles external lookups are asked for besides the venue's own, if any: the one its rules ask
   *  (`TitleNormalizer.apiQuery`: programme prefixes, accessibility tags, "+ event" suffixes off), else a broadcaster's
   *  banner spelled by its proper name (`TitleNormalizer.properNamed`) — US "NT Live: Hamlet" is TMDB's "National Theatre
   *  Live: Hamlet" (2026), which neither the venue's spelling nor the bare work reaches; and, beside a query the rules
   *  ask, that query's proper name ("NT Live: Hamlet + Q&A" asks "NT Live: Hamlet" and "National Theatre Live: Hamlet"). */
  private[identity] def searchTitlesOf(title: String, normalizer: TitleNormalizer): (Option[String], Option[String]) = {
    def own(query: String) = Some(query.trim).filter(q => q.nonEmpty && q != title.trim)
    // the venue's own dash after the banner the name replaced is no part of the work: "NT Live - Hay Fever"
    def proper(query: String) = own(BannerDash.replaceFirstIn(normalizer.properNamed(query), ": "))
    own(normalizer.apiQuery(title)) match {
      case Some(query) => (Some(query), proper(query).filter(_ != query))
      case None        => (proper(title), None)
    }
  }
  private val BannerDash = """:\s*[-–—]\s*""".r

  /** What `cm`'s own synopsis and cast field name — none where a feed copied them from its catalogue entry. */
  private def venueNames(cinema: Cinema, cm: CinemaMovie): VenueNames = {
    val text = synopsisOf(cm)
    if ((text.isEmpty && cm.cast.isEmpty) || !namesAreVenues(cinema, cm)) VenueNames.None
    else VenueNames.of(text, cm.cast)
  }

  /** The words `cm`'s own synopsis capitalises — none where a feed copied it from its catalogue entry, as for [[venueNames]]. */
  private def venueWords(cinema: Cinema, cm: CinemaMovie): ProseWords =
    synopsisOf(cm).filter(_ => namesAreVenues(cinema, cm)).fold(ProseWords.None)(text => ProseWords.ofProse(Seq(text)))

  /** `cm`'s synopsis — its matching-only excerpt where the venue cut the synopsis (MSI). */
  private def synopsisOf(cm: CinemaMovie): Option[String] = cm.synopsis.orElse(cm.synopsisExcerpt)

  /** Are the names `cm`'s text gives the venue's own, not a feed's copy of its catalogue entry ([[SourceReliability]])? */
  private def namesAreVenues(cinema: Cinema, cm: CinemaMovie): Boolean =
    SourceReliability.factsOf(CatalogueId.of(cinema, cm), cm.filmUrl.map(_.trim), pageFacts = false).decides(SourceReliability.Fact.Names)

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

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(title: String, cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil,
                          decorations: TitleDecorations = TitleDecorations.None, searchTitle: Option[String] = None,
                          /** [[Listing.properTitle]], searched beside `searchTitle`. */
                          properTitle: Option[String] = None,
                          /** The season a stage relay billing neither its season nor a year screens in ([[Listing.broadcastSeason]]):
                           *  what its title is searched with, never a fact it states. */
                          broadcastSeason: Option[Int] = None,
                          /** The season its screenings fall in, whatever it states ([[Listing.screenedSeason]]). */
                          screenedSeason: Option[Int] = None,
                          /** The season its first screening falls in, unconditionally: the seasons its title can name
                           *  ([[services.titles.TitleSeason.near]]), never a fact it states. */
                          screeningsSeason: Option[Int] = None,
                          /** Its screening days, where the air-day test reads them ([[Listing.airDays]]); else empty. */
                          airDays: Seq[java.time.LocalDate] = Nil,
                          /** The venue's certificate ([[Listing.certificate]]): what the weighted `certificate` measure reads. */
                          certificate: Option[String] = None,
                          /** [[Listing.yearUpperBound]]: weighs against a film released after it, never a year it states. */
                          yearUpperBound: Option[Int] = None,
                          /** [[Listing.performanceYear]]: what dates an encore's relay. */
                          performanceYear: Option[Int] = None,
                          /** [[Listing.anniversaryScreeningYear]]: what dates the film of an anniversary it bills, never a year it states. */
                          anniversaryScreeningYear: Option[Int] = None) {
  lazy val key: String =
    Seq(title, cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000") +
      broadcastSeason.fold("")(season => s"\u0000broadcast:$season") +
      screenedSeason.filterNot(broadcastSeason.contains).fold("")(season => s"\u0000screened:$season") +
      // only where a rule reads it -- it bounds what the title names, or the billing is a stage work's, whose season rules
      // read it ([[StageRelays.readsItsScreeningsSeason]]) and no season above keys it yet -- so every other listing keeps
      // its key, and a film's run past July re-resolves nothing
      screeningsSeason.filter(season => TitleSeason.of(Seq(title, rawTitle)) != TitleSeason.of(Seq(title, rawTitle), screeningsSeason) ||
        (!broadcastSeason.contains(season) && !screenedSeason.contains(season) && StageRelays.readsItsScreeningsSeason(measured)))
        .fold("")(season => s"\u0000screenings:$season") +
      (if (airDays.isEmpty) "" else airDays.mkString("\u0000aired:", ",", "")) +
      // only where a venue bills one, so every other listing keeps its key
      certificate.fold("")(c => s"\u0000certificate:$c") +
      // only where a source bounds the year, so every other listing keeps its key
      yearUpperBound.fold("")(bound => s"\u0000yearUpTo:$bound") +
      // only where a synopsis names the performance, so every other listing keeps its key
      performanceYear.fold("")(year => s"\u0000performed:$year") +
      // only where its title bills an anniversary, so every other listing keeps its key
      anniversaryScreeningYear.fold("")(year => s"\u0000anniversaryScreened:$year")

  /** The evidence as the calibrated measures read a listing, its title shapes undecorated by the
   *  resolve's learned `decorations` (the same for every listing of a resolve, so not in [[key]]). */
  lazy val measured: MeasuredListing =
    MeasuredListing(title, Some(rawTitle).filter(_ != title), originalTitle, year, runtime, directors, countries,
      decorations = decorations, searchTitles = searchTitle.toSeq ++ properTitle, broadcastSeason = broadcastSeason, screenedSeason = screenedSeason,
      screeningsSeason = screeningsSeason, airDays = airDays, certificate = certificate, yearUpperBound = yearUpperBound,
      performanceYear = performanceYear, anniversaryScreeningYear = anniversaryScreeningYear)

  /** [[measured]] with the title shapes the venue's own delimiters leave, no learned decoration nor search title
   *  stripped: what relates two LISTINGS (their families, title must-links and listing-listing
   *  measures). A learned decoration names a FILM to search for and relate to; it never links a
   *  "Horror Season 2026 Dracula" to every other venue's bare "Dracula". */
  lazy val published: MeasuredListing =
    measured.copy(decorations = TitleDecorations.None, searchTitles = Nil, broadcastSeason = None, screenedSeason = None)

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = measured.statedYear

  /** The evidence with its stated facts set aside — its titles alone: what a node whose facts a feed copied from an
   *  overruled link is read as ([[FeedLink]]). */
  def titlesOnly: Evidence = copy(year = None, directors = Nil, runtime = None, originalTitle = None, countries = Nil, certificate = None,
    yearUpperBound = None)
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts], decorations: TitleDecorations = TitleDecorations.None): Evidence = Evidence(
    title         = listing.title,
    cleanTitle    = listing.cleanTitle,
    rawTitle      = listing.rawTitle,
    year          = listing.year.orElse(detail.flatMap(_.year)),
    directors     = (if (listing.directors.nonEmpty) listing.directors
                     else detail.map(_.directors.flatMap(Listing.credited).filter(_.nonEmpty).distinct).getOrElse(Nil)).sorted,
    runtime       = listing.runtime.filter(models.FilmRuntime.plausible).orElse(detail.flatMap(_.runtime).filter(models.FilmRuntime.plausible)),
    originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)),
    countries     = (if (listing.countries.nonEmpty) listing.countries else detail.map(_.countries).getOrElse(Nil)).distinct.sorted,
    decorations   = decorations,
    searchTitle   = listing.searchTitle,
    properTitle   = listing.properTitle,
    // a year the detail page states dates the relay as a year the venue's own field does
    broadcastSeason = listing.broadcastSeason.filter(_ => detail.flatMap(_.year).isEmpty),
    screenedSeason  = listing.screenedSeason.filter(_ => detail.flatMap(_.year).isEmpty),
    screeningsSeason = listing.screenings.season,
    // ...but for a season named too far back, whose page is as old as its title (UK Flicks' "Royal Ballet & Opera
    // 2024-25: Romeo and Juliet", its page dated 2025, screening on the 2026/27 relay's air day)
    airDays         = listing.airDays.filter(_ => detail.flatMap(_.year).isEmpty || listing.namesAStaleSeason.isDefined),
    certificate     = listing.certificate.map(_.trim).filter(_.nonEmpty),
    yearUpperBound  = listing.yearUpperBound,
    performanceYear = listing.performanceYear,
    anniversaryScreeningYear = listing.anniversaryScreeningYear)
}

/** A film some listing of a family may be: its TMDB id and what TMDB says about it — its own
 *  record when the lookup source holds one (`TmdbFilmRecord`), else what the hits naming it carry. `awaitingRecord`: its
 *  record is not answered yet (a record given up on is not awaited: decided on its hits), so the hits alone carry it and nothing its record states (a running time, credits, where it
 *  was released) can deny it — a family takes a film only where its pool with and without it agree ([[FamilyScope]]). */
final case class Candidate(tmdbId: Int, film: Film, awaitingRecord: Boolean = false) {
  /** Is it a fallback source's film ([[FallbackIds]]), which only a no-match's fallback scores, never TMDB's? */
  def isFallback: Boolean = FallbackIds.isFallback(tmdbId)
  /** A fallback source's film as that source names it: the source's label and the IMDb id it stands for. */
  def fallbackRecord: Option[(String, String)] =
    FallbackIds.unapply(tmdbId).flatMap { case (source, _) => FallbackIds.imdbId(tmdbId).map(source.label -> _) }
}

object Candidate {

  def of(tmdbId: Int, hits: Seq[Hit], record: Answer[Option[Film]]): Candidate = {
    val popularity = hits.map(_.popularity).maxOption
    val best = Hit.best(hits)
    // A record that carries its popularity is the candidate's film itself: a copy restating it was a second Film per
    // recorded film beside the corpus's `records` (30k on worker-uk, 2026-10-07).
    Candidate(tmdbId, record.answered.flatten.map(f => if (f.popularity.isDefined) f else f.copy(popularity = popularity)).getOrElse(
      Film(best.fold("")(_.title), best.flatMap(_.originalTitle), Nil, best.flatMap(_.year),
        popularity = popularity)), awaitingRecord = record.isAwaited)
  }
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
  def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[models.ListingKey]): Unit = ()
  /** The venue's own detail page for the listing. */
  def detail(listing: Listing): Answer[Option[DetailFacts]]
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record (`TmdbFilmRecord`). */
  def film(tmdbId: Int): Answer[Option[Film]]
  /** Does this source number its films by their TMDB ids — the number a TMDB-keyed feed's link names ([[FeedLink]])? A
   *  film-database family's answers number theirs as one resolve meets them (`agreement.FamilyLookups`): there a link's
   *  TMDB id names none of them, and read as a film number it denied the very film the link meant. */
  def numbersFilmsByTmdbId: Boolean = true
  /** The day a film was released, as its record states it: `Unknown` while the record is unknown — or, in a store, while
   *  it was filed before records kept the whole day and holds only the year TMDB dated it in
   *  ([[agreement.Broadcast]] reads the day; nothing else does). */
  def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId).map(_.flatMap(_.released))
  /** A film's top-billed cast as TMDB credits it ([[TmdbFilmRecord.cast]]): what the cast evidence reads
   *  ([[CastEvidence]]), never the model. Read apart from its record ([[film]]), as the release day is: every candidate
   *  of every family holds a record, and only an unmatched cluster's fill reads a cast. `Known(None)` where the source
   *  does not know it — a record filed before records kept the cast — which the evidence reads as nothing to go on. */
  def cast(@unused tmdbId: Int): Answer[Option[Seq[String]]] = Answer.Known(None)
  /** The words a film's TMDB descriptions capitalise — its localized and English overviews ([[TmdbFilmRecord.overview]]):
   *  what the synopsis evidence reads ([[agreement.SynopsisEvidence]]), never the model. Read apart from its record, as
   *  the cast is. `Known(None)` where the source does not know it — a record filed before records kept the overview. */
  def overview(@unused tmdbId: Int): Answer[Option[ProseWords]] = Answer.Known(None)
}
