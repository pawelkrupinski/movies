package services.cinemas.common

import models._
import play.api.libs.json._
import services.cinemas.pl.NonMovieEventClassifier
import tools.{HttpFetch, HttpRead, TextNormalization}

import java.text.Normalizer
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
 * (`/film/<slug>`), which `CatalogueSources.FeedPages` names as a feed's.
 */
class KinoheldClient(
  http: HttpFetch,
  venue: KinoheldVenue,
  override val cinema: Cinema,
  /** The day the horizon is measured from, asked per scrape — today in Germany. */
  today: => LocalDate
) extends CinemaScraper {
  import KinoheldClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl, SiteUrl)

  override def sourceUrl: Option[String] = Some(SiteUrl + venue.path)

  /** The venue's id names its programme; the page path is the site's decoration. */
  override def sourceKey: Option[String] = Some(s"kinoheld.de/cinema/${venue.cinemaId}")

  def fetch(): Seq[CinemaMovie] = {
    val referenceDay = today
    val shows = readAllPages(latestYear = referenceDay.getYear + 1).filterNot(_.dateTime.toLocalDate.isBefore(referenceDay))
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

  /** One screening, the film it shows already decided ([[linkTrusted]]). */
  final case class Show(film: Film, dateTime: LocalDateTime, bookingUrl: Option[String],
                        room: Option[String], format: List[String])

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
      film     <- filmOf(name, js \ "movie", latestYear)
    } yield Show(
      film       = film,
      dateTime   = dateTime,
      bookingUrl = text(js \ "deeplink").orElse(showPage(js)),
      room       = text(js \ "auditorium" \ "name"),
      format     = formatTokens((js \ "flags").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { f =>
                     for (c <- (f \ "category").asOpt[String]; k <- (f \ "code").asOpt[String]) yield c -> k
                   }, text(js \ "subtitleLanguage" \ "isocode"))
    )

  /** The show's own page on kinoheld, which books it: for a venue that sells through
   *  kinoheld itself, the API gives no `deeplink`. */
  private def showPage(js: JsValue): Option[String] =
    for {
      city   <- text(js \ "cinema" \ "city" \ "urlSlug")
      cinema <- text(js \ "cinema" \ "urlSlug")
      show   <- text(js \ "urlSlug")
    } yield s"$SiteUrl/kino/$city/$cinema/vorstellung/$show"

  private def filmOf(showName: String, movie: JsLookupResult, latestYear: Int): Option[Film] = {
    val linked = for {
      id    <- (movie \ "id").asOpt[String]
      title <- text(movie \ "title")
    } yield (id, title, text(movie \ "titleOriginal"))
    linked match {
      case Some((id, title, original)) if linkTrusted(showName, title, original) =>
        Some(Film(
          id             = Some(id),
          title          = title,
          originalTitle  = original.filter(_ != title),
          slug           = text(movie \ "urlSlug"),
          runtimeMinutes = (movie \ "duration").asOpt[Int].filter(_ > 0),
          year           = text(movie \ "productionYear").flatMap(y => Try(y.take(4).toInt).toOption).filter(_ <= latestYear),
          countries      = names(movie \ "productionCountries"),
          genres         = names(movie \ "genres"),
          director       = names(movie \ "directors"),
          cast           = names(movie \ "actors"),
          synopsis       = text(movie \ "description").map(TextNormalization.stripHtmlKeepingParagraphs).filter(_.nonEmpty),
          posterUrl      = text(movie \ "thumbnailImage" \ "url"),
          trailerUrl     = (movie \ "trailers").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(t => text(t \ "url")).headOption,
          ageRating      = (movie \ "contentRating" \ "minimumAge").asOpt[Int].flatMap(age => AgeRating.normalize(s"FSK $age"))
        ))
      case _ => Option(showName).filter(_.nonEmpty).map(name => Film(id = None, title = name))
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

  private def words(s: String): Set[String] =
    Normalizer.normalize(s.toLowerCase(Locale.ROOT), Normalizer.Form.NFKD)
      .replaceAll("\\p{M}", "")
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
    shows.filterNot(show => NonMovieEventClassifier.isNonScreening(show.film.title))
      .groupBy(show => show.film.id.getOrElse("name:" + show.film.title)).toSeq.map { case (_, same) =>
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
          showtimes   = same.map(s => Showtime(s.dateTime, s.bookingUrl, s.room, s.format))
                          .distinctBy(s => (s.dateTime, s.bookingUrl, s.format)).sortBy(_.dateTime),
          trailerUrl  = film.trailerUrl,
          ageRating   = film.ageRating
        )
      }.sortBy(_.movie.title)

  private def text(js: JsLookupResult): Option[String] = js.asOpt[String].map(_.trim).filter(_.nonEmpty)

  private def names(js: JsLookupResult): Seq[String] =
    js.asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(n => text(n \ "name")).distinct
}
