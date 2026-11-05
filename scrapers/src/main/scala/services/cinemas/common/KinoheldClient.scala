package services.cinemas.common

import models._
import play.api.Logging
import play.api.libs.json._
import services.cinemas.common.ScraperParse.jsonText
import services.cinemas.pl.NonMovieEventClassifier
import tools.{HttpFetch, HttpRead, TextNormalization}

import java.time.{LocalDate, LocalDateTime, OffsetDateTime, ZoneId}
import java.util.Locale
import scala.util.Try

/** A venue as kinoheld.de knows it: the id its GraphQL API filters shows by, and the
 *  path of its public programme page (`/kino/<city>/<venue>`), which the API never
 *  asks for but a visitor and /uptime link to. */
final case class KinoheldVenue(cinemaId: String, path: String)

/**
 * One German venue's programme from kinoheld.de — the ticketing aggregator nearly
 * every independent German cinema and several chains (UCI, Cineplex, Kinopolis)
 * sell through, and so the one source that lists the venues Filmstarts does not:
 * whole cities of them (Bochum, Aachen, Solingen, Siegen…), harvested by
 * `data/germany/scripts/harvest_kinoheld.py`.
 *
 * The site's own GraphQL endpoint answers unauthenticated:
 *
 *   POST https://next-live.kinoheld.de/graphql
 *     programShows(cinemaIds:[id], excludeExpired:true, first:100, page:n)
 *       → { paginatorInfo { hasMorePages }, data: [show…] }
 *
 * one show per screening, its `movie` the film kinoheld LINKED it to. The pages are
 * walked until `hasMorePages` is false, never a fixed window: a multiplex lists half
 * a year ahead (UCI Ruhr Park: 468 shows to June 2027 on 2026-10-11), which the
 * `shows(limit:)` field — capped at 500 — would one day cut short.
 *
 * Two titles come with a show: the VENUE's own `name` for it ("Forgotten Island",
 * "SNEAK PREVIEW KW42/2026") and the linked film's catalogue `title` ("Die vergessene
 * Insel"). The catalogue title is the German release title Filmstarts also lists,
 * the same at every venue, so it is the one used — but only while the two still
 * name the same film. A link is kinoheld's guess, and it guesses wrong: Casablanca
 * Bochum's travel talk "Nordische Inseln" is linked to "Life Itself". A name sharing
 * no word with the film's title or original title keeps the venue's own name and
 * none of the linked film's facts ([[linkTrusted]]).
 *
 * The linked film's facts — year, runtime, director, cast, poster — are kinoheld's
 * CATALOGUE claim, not the venue's, so `filmUrl` is the catalogue's film page
 * (`/film/<slug>`), which `SourceReliability.Kinoheld` names as a feed's.
 */
class KinoheldClient(
  http: HttpFetch,
  venue: KinoheldVenue,
  override val cinema: Cinema,
  /** The day the horizon is measured from, asked per scrape — today in Germany. */
  today: => LocalDate
) extends CinemaScraper with Logging {
  import KinoheldClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl, SiteUrl)

  override def sourceUrl: Option[String] = Some(SiteUrl + venue.path)

  /** The venue's id names its programme; the page path is the site's decoration. */
  override def sourceKey: Option[String] = Some(s"kinoheld.de/cinema/${venue.cinemaId}")

  def fetch(): Seq[CinemaMovie] = {
    val referenceDay = today
    val shows = readAllPages(latestYear = referenceDay.getYear + 1).filterNot(_.dateTime.toLocalDate.isBefore(referenceDay))
    shows.flatMap(show => show.renamedEntry.map(_ -> show.film.title)).distinct.foreach { (entry, title) =>
      logger.info(s"kinoheld entry $entry renamed: ${cinema.displayName} bills \"$title\" on another film's entry; its facts left out")
    }
    shows.flatMap(show => show.guessedEntry.map(_ -> show.film.title)).distinct.foreach { (entry, title) =>
      logger.info(s"kinoheld entry $entry guessed: ${cinema.displayName}'s \"$title\" carries the facts of a film named with part of it; left out")
    }
    toMovies(shows, cinema)
  }

  /** Every page of the programme. The first page failing fails the scrape — the source
   *  answered nothing; a later one failing keeps what came before it and marks the
   *  listing incomplete, so the cache does not prune the films only that page held. */
  private def readAllPages(latestYear: Int): Seq[Show] = {
    val shows = Seq.newBuilder[Show]
    var page  = 1
    var more  = true
    while (more) {
      val read = Try(parsePage(HttpRead.postPage(http, ApiUrl, requestBody(venue.cinemaId, page)), latestYear))
      if (page == 1) read.failed.foreach(throw _)
      read.failed.foreach(ListingReads.pageFailed)
      read.foreach(p => shows ++= p.shows)
      // An empty page ends the walk whatever it claims: a paginator that never says
      // "no more" must not spin the scrape forever.
      more = read.toOption.exists(p => p.hasMorePages && p.shows.nonEmpty)
      page += 1
    }
    shows.result()
  }
}

object KinoheldClient {
  val ApiUrl:  String = "https://next-live.kinoheld.de/graphql"
  val SiteUrl: String = "https://www.kinoheld.de"
  val Zone:    ZoneId = TimeZones.Germany

  /** The catalogue's film pages — a feed's claim of which film a show is. */
  val FilmPagePrefix: String = s"$SiteUrl/film/"

  private val Query: String =
    """query Programme($cinema: ID!, $page: Int!) {
      |  programShows(cinemaIds: [$cinema], excludeExpired: true, first: 100, page: $page) {
      |    paginatorInfo { hasMorePages }
      |    data {
      |      name beginning urlSlug deeplink
      |      flags { category code }
      |      subtitleLanguage { isocode }
      |      auditorium { name }
      |      cinema { urlSlug city { urlSlug } }
      |      movie {
      |        id title titleOriginal urlSlug duration productionYear description
      |        contentRating { minimumAge }
      |        productionCountries { name }
      |        genres { name }
      |        directors { name }
      |        actors { name }
      |        thumbnailImage { url }
      |        trailers { url }
      |      }
      |    }
      |  }
      |}""".stripMargin

  /** The POST body for one page of a venue's programme. Deterministic — the recorded
   *  fixtures are keyed by its hash. */
  def requestBody(cinemaId: String, page: Int): String =
    Json.obj("query" -> Query, "variables" -> Json.obj("cinema" -> cinemaId, "page" -> page)).toString

  /** One page of a programme: its shows and whether another page follows. */
  final case class Page(shows: Seq[Show], hasMorePages: Boolean)

  /** One screening, the film it shows already decided ([[linkTrusted]]); `renamedEntry` the catalogue id of a linked
   *  entry its venue renamed for this show ([[entryRenamed]]), `guessedEntry` that of an event's entry kinoheld filled
   *  with another film's facts ([[entryGuessed]]), whose facts it does not carry. */
  final case class Show(film: Film, dateTime: LocalDateTime, bookingUrl: Option[String],
                        room: Option[String], format: List[String], renamedEntry: Option[String] = None,
                        guessedEntry: Option[String] = None)

  /** The film a show screens: kinoheld's linked film when the link is trusted (`id` its
   *  catalogue id), else only the venue's own name for it (`id` None). */
  final case class Film(
    id:             Option[String],
    title:          String,
    originalTitle:  Option[String] = None,
    slug:           Option[String] = None,
    runtimeMinutes: Option[Int]    = None,
    year:           Option[Int]    = None,
    countries:      Seq[String]    = Nil,
    genres:         Seq[String]    = Nil,
    director:       Seq[String]    = Nil,
    cast:           Seq[String]    = Nil,
    synopsis:       Option[String] = None,
    posterUrl:      Option[String] = None,
    trailerUrl:     Option[String] = None,
    ageRating:      Option[String] = None
  )

  /** Parse one `programShows` response. A response without the programme — GraphQL
   *  `errors`, a changed schema — throws rather than reading as a venue with nothing on.
   *  `latestYear` is the latest production year a screened film can have: the catalogue's
   *  `productionYear` is hand-typed, and files "Hirten" (2025) under 2040 — a year past it
   *  reads as none. */
  def parsePage(body: String, latestYear: Int): Page = {
    val js       = Json.parse(body)
    val program  = (js \ "data" \ "programShows").toOption.filter(_.isInstanceOf[JsObject]).getOrElse(
      throw new IllegalStateException(s"kinoheld answered no programShows: ${(js \ "errors").toOption.getOrElse(js).toString.take(300)}"))
    Page(
      shows        = (program \ "data").as[Seq[JsValue]].flatMap(parseShow(_, latestYear)),
      hasMorePages = (program \ "paginatorInfo" \ "hasMorePages").asOpt[Boolean].getOrElse(false)
    )
  }

  private def parseShow(js: JsValue, latestYear: Int): Option[Show] =
    for {
      name     <- (js \ "name").asOpt[String].map(_.trim)
      dateTime <- (js \ "beginning").asOpt[String].flatMap(s => Try(OffsetDateTime.parse(s)).toOption)
                    .map(_.atZoneSameInstant(Zone).toLocalDateTime)
      (film, leftOut) <- filmOf(name, js \ "movie", latestYear)
    } yield Show(
      film       = film,
      dateTime   = dateTime,
      bookingUrl = jsonText(js \ "deeplink").orElse(showPage(js)),
      room       = jsonText(js \ "auditorium" \ "name"),
      format     = formatTokens((js \ "flags").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { f =>
                     for (c <- (f \ "category").asOpt[String]; k <- (f \ "code").asOpt[String]) yield c -> k
                   }, jsonText(js \ "subtitleLanguage" \ "isocode")),
      renamedEntry = leftOut.collect { case (id, Renamed) => id },
      guessedEntry = leftOut.collect { case (id, Guessed) => id }
    )

  /** The show's own page on kinoheld, which books it: for a venue that sells through
   *  kinoheld itself, the API gives no `deeplink`. */
  private def showPage(js: JsValue): Option[String] =
    for {
      city   <- jsonText(js \ "cinema" \ "city" \ "urlSlug")
      cinema <- jsonText(js \ "cinema" \ "urlSlug")
      show   <- jsonText(js \ "urlSlug")
    } yield s"$SiteUrl/kino/$city/$cinema/vorstellung/$show"

  /** Why a linked entry's facts were left out. */
  private sealed trait LeftOut
  private case object Renamed extends LeftOut
  private case object Guessed extends LeftOut

  /** The film a show screens, and the id of the linked entry when its facts are left out, with why: its venue renamed
   *  it ([[entryRenamed]]), or kinoheld guessed them for an event ([[entryGuessed]]). A show the venue named with
   *  nothing is no show. */
  private def filmOf(showName: String, movie: JsLookupResult, latestYear: Int): Option[(Film, Option[(String, LeftOut)])] = {
    val linked = for {
      id    <- (movie \ "id").asOpt[String]
      title <- jsonText(movie \ "title")
    } yield (id, title, jsonText(movie \ "titleOriginal"))
    lazy val venueOwn = Option(showName).filter(_.nonEmpty).map(name => Film(id = None, title = name))
    linked match {
      case Some((id, title, original)) if linkTrusted(showName, title, original) &&
          entryRenamed(title, original, jsonText(movie \ "urlSlug"), jsonText(movie \ "thumbnailImage" \ "url")) =>
        venueOwn.map(_ -> Some(id -> Renamed))
      case Some((id, title, original)) if linkTrusted(showName, title, original) &&
          entryGuessed(title, original, jsonText(movie \ "productionYear").flatMap(y => Try(y.take(4).toInt).toOption),
            (movie \ "duration").asOpt[Int].filter(_ > 0), names(movie \ "productionCountries")) =>
        venueOwn.map(_ -> Some(id -> Guessed))
      case Some((id, title, original)) if linkTrusted(showName, title, original) =>
        Some(Film(
          id             = Some(id),
          title          = title,
          originalTitle  = original.filter(_ != title),
          slug           = jsonText(movie \ "urlSlug"),
          runtimeMinutes = (movie \ "duration").asOpt[Int].filter(_ > 0),
          year           = jsonText(movie \ "productionYear").flatMap(y => Try(y.take(4).toInt).toOption).filter(_ <= latestYear),
          countries      = names(movie \ "productionCountries"),
          genres         = names(movie \ "genres"),
          director       = names(movie \ "directors"),
          cast           = names(movie \ "actors"),
          synopsis       = jsonText(movie \ "description").map(TextNormalization.stripHtmlKeepingParagraphs).filter(_.nonEmpty),
          posterUrl      = jsonText(movie \ "thumbnailImage" \ "url"),
          trailerUrl     = (movie \ "trailers").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(t => jsonText(t \ "url")).headOption,
          ageRating      = (movie \ "contentRating" \ "minimumAge").asOpt[Int].flatMap(age => AgeRating.normalize(s"FSK $age"))
        ) -> None)
      case _ => venueOwn.map(_ -> None)
    }
  }

  /** Words too common in titles to tell two films apart. */
  private val CommonWords = Set("der", "die", "das", "und", "the", "and", "von", "ein", "eine", "for", "of")

  /** Does the venue's name for a show still name the film kinoheld linked it to? It does
   *  when the two share a word — "Forgotten Island" shares one with its linked film's
   *  original title — or when the venue named the show only with a placeholder
   *  (`KINOHELD_DUMMY_…`), which names nothing. "Nordische Inseln" and "Life Itself" share
   *  none: a wrong link, not the same film. Nor is a link from a mystery screening ever
   *  right: "Sneak_nov" names no film, and the word it shares with the 8-minute short
   *  "Sneak Over" kinoheld linked it to is the sneak's own — the show keeps the venue's
   *  name, which [[toMovies]] leaves out. */
  def linkTrusted(showName: String, title: String, original: Option[String]): Boolean =
    showName.startsWith("KINOHELD_DUMMY") || !NonMovieEventClassifier.isMysteryScreening(showName) && {
      val named = words(showName)
      (words(title) ++ original.toSeq.flatMap(words)).exists(named)
    }

  /** Has a venue renamed the entry for another show, which keeps the facts of the film it was made for? kinoheld lets
   *  a venue reuse an old entry for its next event: the title changes, and the slug, poster, original title, credit,
   *  runtime and synopsis stay the old film's (prod 2026-10-21: entry 60643, made for "Hodder rettet die Welt", titled
   *  "Über Unterbiberger - Vom Woid in die Welt" at Filmforum Landsberg; 74283, made for "Rock Around the Clock",
   *  "Tyketto + Special Guest" at Kaminwerk; 931, "Wie in alten Zeiten", "Guido Cantz - Komische Zeiten"). The venue's
   *  name matches the new title, so [[linkTrusted]] passes; the facts would put the old film's card on the show and
   *  name that film to the resolver.
   *
   *  It is renamed when the entry states an original title its title shares no word with, and its slug and poster —
   *  each named from the title the entry was made under — share less than half their words with it. An original title
   *  alone tells nothing: a German title translates one ("Mit leiser Stimme", "À voix basse"), and its poster is
   *  named for it; nor does an entry made under its original title ("Schach dem Teufel", slugged `beat-the-devil`,
   *  "Nur geträumt" `juste-une-illusion`), then given its German one. Without an original title the entry names no
   *  other film: the Filmmuseum bills Fellini's "E la nave va" on the entry made as "Fellinis Schiff der Träume". A
   *  venue's own entry, slugged by a hash, names nothing. On the DE prod mirror of 2026-10-21 it named 17 entries (19
   *  listings): 15 a concert, reading, event or other film on an old film's entry, "Mit offenen Armen" unsure, and one
   *  the venue retitled to its film's German title ("Wie weit noch", slugged `nein-doch-wie`), whose facts still find
   *  it. The facts dropped from those recorded on 2026-10-15 and 10-20 cured 3 wrong takes (Guido Cantz → "Wie in alten
   *  Zeiten", HG Butzko → "Erlöse uns von dem Bösen", Com Felipe Quartett → "Glücklich wie Lazzaro") and gained 4
   *  right ones the copied credit had vetoed (Cinecitta's "Über Unterbiberger"), moving no right take. */
  def entryRenamed(title: String, original: Option[String], slug: Option[String], posterUrl: Option[String]): Boolean = {
    val billed = spellings(title)
    def sharesHalf(name: String) = {
      val named  = spellings(name)
      val shared = billed.count(_._2.exists(named.values.flatten.toSet))
      billed.nonEmpty && named.nonEmpty && shared * 2 >= math.min(billed.size, named.size)
    }
    val names = slug.filterNot(VenueSlug.matches).map(_.replace('-', ' ')).toSeq ++
      posterUrl.flatMap(url => PosterStem.findFirstMatchIn(url.substring(url.lastIndexOf('/') + 1))).map(_.group(1).replace('-', ' '))
    val originalWords = original.map(spellings(_).values.flatten.toSet).getOrElse(Set.empty)
    val slugWords     = slug.filterNot(VenueSlug.matches).map(s => spellings(s.replace('-', ' '))).getOrElse(Map.empty)
    def slugsTheOriginal = slugWords.nonEmpty && slugWords.values.forall(_.exists(originalWords))
    billed.nonEmpty && slugWords.nonEmpty && originalWords.nonEmpty &&
      originalWords.intersect(billed.values.flatten.toSet).isEmpty && !slugsTheOriginal && !names.exists(sharesHalf)
  }

  /** Did kinoheld fill an event's entry with the facts of a film named with part of its title? A venue's event gets a
   *  catalogue entry of its own name, which kinoheld fills from a film database by that name: an original title, credit,
   *  cast and genres, but no year, runtime or production country (prod 2026-10-22: "Ping Pong Session"
   *  carries Ping Pong (2025)'s original title and director, "Helmut Schleich" Helmut's, "Sunday Night Jazz" Sunday
   *  Night's, "ICONIC MOVIE" Iconic's). A copy that backs a judged-wrong film twice as often as a right one (kinoheld's,
   *  2026-10-21) is mostly these.
   *
   *  It is guessed when the entry states no year, runtime or production country (a distributor's entry not yet dated
   *  states one: "Minions & Monster", Minions 3), and an original title at least half of whose words its title holds,
   *  beside a word of its own: the name of a film the venue's longer billing contains. Not when
   *  the title is the original's (a festival film's own entry), nor the original's shortened ("Cloud", "Cloud クラウド"),
   *  nor translates it ("Red Rocks", "Les Roches Rouges"), nor bills it as one part of a strand's or a translation's
   *  billing ("FRAUENWELTEN 2026: Killing Anna", "Viva a Vida - Auf das Leben"). On the DE prod mirror of 2026-10-22
   *  it named 46 entries (47 of 3,081 kinoheld listings), 45 an event on a guessed film's facts and one a documentary
   *  ("30 Jahre mit Kardeş Türküler") whose facts were its own; over the 1,083 venues' live programmes, 104 entries, 3
   *  of them a film's own. */
  def entryGuessed(title: String, original: Option[String], year: Option[Int], runtime: Option[Int],
                   countries: Seq[String] = Nil): Boolean =
    year.isEmpty && runtime.isEmpty && countries.isEmpty && original.exists { original =>
      val billed = words(title)
      val named  = words(original)
      def partBills = {
        val parts = BillingParts.split(title).filter(_.trim.nonEmpty)
        parts.length > 1 && parts.exists(part => named.subsetOf(words(part)))
      }
      named.nonEmpty && named.intersect(billed).size * 2 >= named.size && !billed.subsetOf(named) && !partBills
    }

  /** Where a billing joins its parts: a strand's colon, a pipe or slash between two languages' titles, a spaced dash,
   *  a bracket. */
  private val BillingParts = """\s*(?::|\||/|\s[-–—]\s|\(|\))\s*""".r

  /** A venue's own entry's slug — a hash, a bare number — names no title. */
  private val VenueSlug = "[0-9a-f]{24}|\\d+".r
  /** A catalogue poster's file name: the entry's title as it was slugged, its id, a version. */
  private val PosterStem = """^(.+?)-\d+(?:-\d+)?\.v\d+\.\w+$""".r

  /** Each counted word of `s` ([[words]]), with its spellings: deburred, and transliterated as kinoheld's slugs are
   *  ("Über" as "ueber", "Außer" as "ausser"). A trailing hash a slug carries is no word. */
  private def spellings(s: String): Map[String, Set[String]] = {
    val lower = s.toLowerCase(Locale.ROOT)
    def split(t: String) = t.split("[^\\p{L}\\p{N}]+").toSeq
    val plain = split(TextNormalization.deburr(lower))
    val translit = split(TextNormalization.deburr(lower.replace("ä", "ae").replace("ö", "oe").replace("ü", "ue").replace("ß", "ss")))
    if (plain.size != translit.size) words(s).map(w => w -> Set(w)).toMap
    else plain.zip(translit).collect {
      case (w, t) if w.length >= 3 && !CommonWords(w) && !(w.length >= 10 && w.exists(_.isDigit) && w.forall(c => c.isDigit || c <= 'f')) => w -> Set(w, t)
    }.toMap
  }

  /** The counted words of a title: deburred, lower-cased, three letters or more, no [[CommonWords]]. */
  private[common] def words(s: String): Set[String] =
    TextNormalization.deburr(s.toLowerCase(Locale.ROOT))
      .split("[^\\p{L}\\p{N}]+").filter(w => w.length >= 3 && !CommonWords(w)).toSet

  /** kinoheld's TECHNOLOGY flags, as the tokens Filmstarts emits for the same formats. */
  private val TechnologyTokens: Map[String, String] = Map(
    "3d"              -> "3D",
    "imax"            -> "IMAX",
    "atmos"           -> "ATMOS",
    "dbox"            -> "DBOX",
    "4dx"             -> "4DX",
    "screenx"         -> "SCREENX",
    "infinity-vision" -> "INFINITY"
  )

  /** Screen-format tokens, then the language version, for one show — the same tokens
   *  [[WebediaMarket.Germany]] gives the same versions. The subtitle language is the
   *  plainest statement of a subtitled print (`de` → OmU, `en` → OmeU); a venue that
   *  sets only a LANGUAGE flag says it with `omu`/`omdu`/`subtitled`, `omeu` or
   *  `original`/`ov`. A German film or a dub carries neither — the unmarked default.
   *  Event and miscellaneous flags ("Sneak", "Kinderkino") describe the slot, not
   *  the print, and give no token. */
  def formatTokens(flags: Seq[(String, String)], subtitleLanguage: Option[String]): List[String] = {
    val technology = flags.collect { case ("TECHNOLOGY", code) => code }.flatMap(TechnologyTokens.get).distinct.toList
    val language   = flags.collect { case ("LANGUAGE", code) => code }.toSet
    val version =
      if (subtitleLanguage.contains("en") || language("omeu")) List("OmeU")
      else if (subtitleLanguage.isDefined || language.exists(Set("omu", "omdu", "subtitled"))) List("OmU")
      else if (language("original") || language("ov")) List("OV")
      else Nil
    technology ++ version
  }

  /** Merge a venue's shows into one row per film — kinoheld's film id where the link is
   *  trusted, the venue's own name otherwise — showtimes de-duplicated and in time order.
   *  A venue's mystery screening ("Sneak Preview") names no film and is left out, as is a
   *  reading, gig or day pass it sells beside its films — `NonMovieEventClassifier.isNonScreening`,
   *  as [[KinoprogrammClient]] leaves them out. */
  private[common] def toMovies(shows: Seq[Show], cinema: Cinema): Seq[CinemaMovie] =
    SlotsToMovies.fold(shows.filterNot(show => NonMovieEventClassifier.isNonScreening(show.film.title)),
      (show: Show) => show.film.id.getOrElse("name:" + show.film.title),
      (s: Show) => Showtime(s.dateTime, s.bookingUrl, s.room, s.format), SlotsToMovies.byTimeBookingAndFormat) {
      (_, same, showtimes) =>
        val film = same.head.film
        CinemaMovie(
          movie       = Movie(film.title, runtimeMinutes = film.runtimeMinutes, releaseYear = film.year,
                              countries = film.countries, genres = film.genres, originalTitle = film.originalTitle),
          cinema      = cinema,
          posterUrl   = film.posterUrl,
          filmUrl     = film.slug.map(FilmPagePrefix + _),
          synopsis    = film.synopsis,
          cast        = film.cast,
          director    = film.director,
          showtimes   = showtimes,
          trailerUrl  = film.trailerUrl,
          ageRating   = film.ageRating
        )
    }


  private def names(js: JsLookupResult): Seq[String] =
    js.asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(n => jsonText(n \ "name")).distinct
}
