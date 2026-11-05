package services.cinemas.uk

import models._
import play.api.libs.json._
import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import services.cinemas.common.ScraperParse.jsonText
import services.cinemas.pl.OnlyMovieEventsFilter
import services.movies.BbfcCertificate
import tools.Whitespace.collapsedSpaces
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.time.{LocalDate, LocalDateTime}
import java.util.Locale
import java.util.regex.Pattern
import scala.util.Try
import scala.util.matching.Regex

/**
 * Spektrix — the box office most UK arts centres sell through (the ICA, Broadway
 * Nottingham, the Brewery Kendal, …). Flicks lists several of them with few or no times,
 * while Spektrix publishes every client's programme as public, unauthenticated JSON
 * keyed by the client's name:
 *
 *   1. `GET https://system.spektrix.com/<client>/api/v3/events?instanceStart_from=<today>`
 *      → the events with a performance from today on: `id`, `name`, `duration`,
 *        `imageUrl`, `webUrl`, `description`, and the client's own `attribute_*` fields.
 *   2. `GET https://system.spektrix.com/<client>/api/v3/instances?startFrom=<today>`
 *      → every performance from today on: `start` (venue wall-clock), `event.id`,
 *        `cancelled`, and the client's own `attribute_*` fields (the screen, often).
 *
 * Two requests are the venue's whole scrape, every date it sells. Without the date
 * filters both endpoints return the client's entire history (the ICA's is 3.4 MB of
 * events and an instance list that does not finish in 40 s).
 *
 * WHICH EVENTS ARE FILMS. An arts centre sells its cinema beside gigs, talks and
 * exhibition time-slots through the same client, and Spektrix has no standard field
 * saying which is which: each client files events under attributes of its own naming —
 * `attribute_Category: "Films"` (ICA), `attribute_EventType: "Cinema"` (Gala Durham),
 * `attribute_Genre1: "Screen"` (Lowther Pavilion), a boolean `attribute_Film: true`
 * (Broadway). So the rule is the box office's own filing, read generically: an event is
 * a film when one of its `attribute_*` values names film, cinema or screen, or a boolean
 * attribute so named is true. A client that files nothing that way lists nothing here —
 * with no way to tell its films from its gigs, the venue is better served by its
 * Flicks fallback than by a guess. The [[OnlyMovieEventsFilter]] then drops a stray live
 * event or ticket product filed among the films.
 *
 * WHICH VENUE. Some clients sell several buildings (Gala Durham also sells the Consett
 * Empire; Leeds Heritage Theatres sells Hyde Park Picture House beside two theatres).
 * `venue` names the building as the client's own venue/site/location attribute spells
 * it, on the performance or on its event; `None` keeps every building.
 *
 * Credits, year, countries, certificate, original title and running time are read when the client
 * keeps them in attributes named so (`attribute_DirectedBy`, `attribute_FilmDirector`,
 * `attribute_YearOfRelease`, `attribute_CountryOfOrigin`, `attribute_Country`, `attribute_Certificate`,
 * `attribute_AgeCertificate`, `attribute_OriginalLanguageTitle`, `attribute_RunningTime`,
 * `attribute_FilmDuration`, …).
 *
 * RUNNING TIME. The event's own `duration` is the performance's slot, not the film: Broadway
 * books Die Hard (132 min) for `duration: 152` with `attribute_FilmDuration: "132"`, the
 * twenty minutes of ads and trailers added on; Lighthouse Poole sets `duration: 0` and states
 * the film as text, `attribute_RunningTime: "1h 33m (plus ads and trailers)"`. So the film's
 * running-time attribute wins where it reads as the film's alone, and `duration` is the
 * fallback. A figure that includes an interval ("1hr 31mins (incl interval)"), is not settled
 * ("3h TBC") or does not read as a time at all states nothing: the slot `duration`, or no
 * runtime, stands rather than a guess.
 *
 * @param client the Spektrix client name, the first path segment of its API.
 */
class SpektrixClient(
  http:   HttpFetch,
  client: String,
  override val cinema: Cinema,
  today:  => LocalDate,
  venue:  Option[String] = None
) extends CinemaScraper with OnlyMovieEventsFilter {

  import SpektrixClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceKey: Option[String] =
    Some(s"${CinemaScraper.urlKey(BaseUrl)}/$client${venue.fold("")(v => s"#$v")}")

  override protected def venueEventMarkers: Seq[Regex] = TicketProducts

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val day = today
    parse(array(eventsUrl(client, day)), array(instancesUrl(client, day)), cinema, day, venue)
  }

  private def array(url: String): JsArray =
    HttpRead.jsonArray(http, url)(ReadOutcome.Answered(_)).required
}

object SpektrixClient {

  val BaseUrl = "https://system.spektrix.com"

  def eventsUrl(client: String, from: LocalDate): String =
    s"$BaseUrl/$client/api/v3/events?instanceStart_from=$from"

  def instancesUrl(client: String, from: LocalDate): String =
    s"$BaseUrl/$client/api/v3/instances?startFrom=$from"

  /** A word that files an event as a film: "Film", "Films", "Cinema", "Screen",
   *  "Screenings", "Movies". Word-bounded, so "Filmmaker Talks" is not one. */
  private val FilmWord = """(?i)\b(films?|cinema|screens?|screenings?|movies?)\b""".r

  /** The same, inside a camel-cased boolean attribute's name: `attribute_Film`,
   *  `attribute_EventCinema` (Broadway's opera and ballet relays). */
  private val FilmKey = """(?i)(film|cinema|screen|movie)""".r

  /** Attributes naming the building a performance is in: `attribute_Venue`,
   *  `attribute_Location`, `attribute_WebsiteLocation`, `attribute_Site`, `attribute_Sitename`
   *  — "site" only as the key's own word, so not `attribute_Website`,
   *  `attribute_TheatreWebsiteGenre` or `attribute_EDISiteNumber`. */
  private val VenueAttribute = """(?i)attribute_(\w*(?:venue|location)|site(?:name)?)""".r

  /** Instance attributes naming the screen a performance is in. */
  private val RoomAttribute = """(?i)attribute_(screen\w*|auditorium|room)""".r
  private val PlainVenueAttribute = """(?i)attribute_venue""".r

  private val DirectorAttribute = """(?i)attribute_(film)?(director|directedby)""".r
  private val CastAttribute     = """(?i)attribute_(film)?cast(details)?""".r
  private val CountryAttribute  = """(?i)attribute_(film)?country(oforigin)?""".r
  private val YearAttribute     = """(?i)attribute_(film)?year(ofproduction|ofrelease)?""".r
  private val RatingAttribute   =
    """(?i)attribute_(film)?(certificate|certification|classification|rating|agerating|agecertificate|certificateageguidance)""".r
  private val OriginalTitleAttribute = """(?i)attribute_(film)?original(language)?title""".r
  /** The film's own running time: `attribute_RunningTime` (Lighthouse Poole), `attribute_FilmDuration` (Broadway,
   *  the Hippodrome), `attribute_Duration` (the ICA). Not Campus West's `attribute_ApproxRunningMinutes`, which is
   *  the slot again and says it is approximate. */
  private val RuntimeAttribute  = """(?i)attribute_(film)?(runningtime|duration)""".r

  /** A running time as a box office writes it: "106", "140 minutes", "2h", "1h 33m", "1hr 50 mins", then at most
   *  one parenthesised note ("(plus ads and trailers)", "(+short film, Q+A)"). */
  private val RunningTimeText =
    """(?i)\s*(?:(\d{1,2})\s*(?:h|hrs?|hours?)\.?(?:\s*(\d{1,2})\s*(?:m|mins?|minutes?)\.?)?|(\d{1,3})\s*(?:m|mins?|minutes?)?\.?)\s*(?:\(([^()]*)\))?\s*""".r

  /** A note saying the figure is not the film's alone, or not settled: "(incl interval)", "(including interval)". */
  private val NotTheFilmAlone = """(?i)\b(incl\w*|tbc|tba|approx\w*)\b""".r
  private val Bbfc              = s"""(?i)(?<!\\w)${BbfcCertificate.alternation(BbfcCertificate.Public)}(?!\\w)""".r

  /** What a box office files under its films that is not a screening: a bundle of
   *  tickets, a voucher, a membership, a season pass, a seat dedication, the hire of
   *  captioning glasses ("WatchWord Glasses", 48 "performances" at Old Market Hall), and
   *  a test listing named for its venue ("Test Walker Cinema"; one billed as a test event,
   *  "Website Test Event 2", the shared classifier drops). Matched on the lower-cased title. */
  private val TicketProducts: Seq[Regex] = Seq(
    """\bbundle(s)?\b""".r, """\bgift\s+(card|voucher)""".r, """\bmembership""".r, """\bseason\s+pass""".r,
    """\bname\s+a\s+seat\b""".r, """\bglasses\b""".r, """^test\b""".r
  )

  private case class Slot(event: JsObject, title: String, showtime: Showtime)

  def parse(events: JsArray, instances: JsArray, cinema: Cinema, today: LocalDate,
            venue: Option[String] = None): Seq[CinemaMovie] = {
    val byId = filmsOf(events.value.toSeq.collect { case o: JsObject => o })
      .flatMap(e => (e \ "id").asOpt[String].map(_ -> e)).toMap

    val slots = for {
      instance <- instances.value.toSeq.collect { case o: JsObject => o }
      if !(instance \ "cancelled").asOpt[Boolean].contains(true)
      event    <- (instance \ "event" \ "id").asOpt[String].flatMap(byId.get).toSeq
      if venue.forall(isIn(_, instance, event))
      title    <- jsonText(event \ "name").map(titleOf(_, event)).filter(_.nonEmpty).toSeq
      start    <- (instance \ "start").asOpt[String].flatMap(s => Try(LocalDateTime.parse(s)).toOption).toSeq
      if !start.toLocalDate.isBefore(today)
    } yield Slot(event, title, Showtime(dateTime = start, bookingUrl = webUrl(event), room = roomOf(instance, venue)))

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val event = group.head.event
      CinemaMovie(
        movie = Movie(
          title          = title,
          rawTitle       = jsonText(event \ "name").filterNot(_ == title),
          runtimeMinutes = attribute(event, RuntimeAttribute).flatMap(runtimeIn)
                             .orElse((event \ "duration").asOpt[Int].filter(_ > 0)),
          releaseYear    = attribute(event, YearAttribute).flatMap(ScraperParse.yearIn),
          countries      = attribute(event, CountryAttribute).toSeq.flatMap(ScraperParse.listItems),
          originalTitle  = attribute(event, OriginalTitleAttribute).map(_.collapsedSpaces)
                             .filterNot(_.equalsIgnoreCase(title))
        ),
        cinema      = cinema,
        posterUrl   = jsonText(event \ "imageUrl"),
        filmUrl     = webUrl(event),
        synopsis    = jsonText(event \ "description").flatMap(synopsisOf),
        cast        = names(attribute(event, CastAttribute)),
        director    = names(attribute(event, DirectorAttribute)),
        showtimes   = showtimes,
        externalIds = (event \ "id").asOpt[String].map("spektrix" -> _).toMap,
        ageRating   = attributes(event, RatingAttribute).flatMap(Bbfc.findFirstIn).headOption.map(_.toUpperCase(Locale.ROOT))
      )
    }
  }

  /** The events the box office files as films — none, when it files nothing that way
   *  (see the class doc). */
  def filmsOf(events: Seq[JsObject]): Seq[JsObject] = events.filter(filedAsFilm)

  private def filedAsFilm(event: JsObject): Boolean =
    event.fields.exists {
      case (key, JsString(value)) => key.startsWith("attribute_") && FilmWord.findFirstIn(value).isDefined
      case (key, JsTrue)          => key.startsWith("attribute_") && FilmKey.findFirstIn(key.stripPrefix("attribute_")).isDefined
      case _                      => false
    }

  /** Whether the performance — or, when it names none, its event — is in `venue`. */
  private def isIn(venue: String, instance: JsObject, event: JsObject): Boolean =
    Some(venuesNamed(instance)).filter(_.nonEmpty).getOrElse(venuesNamed(event)).exists(_.equalsIgnoreCase(venue))

  private def venuesNamed(o: JsObject): Seq[String] = o.fields.collect {
    case (VenueAttribute(_), JsString(value)) if value.trim.nonEmpty => value.trim
  }.toSeq

  /** The event's name without the building tag a multi-venue box office bills it under:
   *  Gala Durham lists "Verity (15) (Gala)" with `attribute_Sitename: "Gala"`. Left on,
   *  the tag is noise in the title — and "(Gala)" reads to the live-event filter as a
   *  gala night, which dropped every Gala film. */
  private def titleOf(name: String, event: JsObject): String =
    venuesNamed(event).foldLeft(name) { (title, site) =>
      title.replaceAll(s"(?i)\\s*\\(${Pattern.quote(site)}\\)", "")
    }.collapsedSpaces

  /** A box office's facts sheet heading a description, one "Label: value" line each — DCA's festival films open
   *  "Director: … / Country: … / Year: … / Duration: … / Certificate: … / Distributor: … / Pricing: … / Ads &
   *  trailers: No" (The Everest Dream, 2026-10-13) before the film's own words. */
  private val FactLine =
    """(?iu)^\s*(?:director|directed\s+by|country|year|duration|running\s+time|runtime|certificate|cert|distributor|""" +
      """pricing|price|ads\s*&\s*trailers|language|subtitles|cast|starring|format)\s*:.*$"""

  /** A "Synopsis:" heading on a line of its own: a relay's description lists its "Cast:" and "Creatives:" first and
   *  heads the story so (Taunton Brewhouse's RBO Live - Carmen, 2026-10-13). */
  private val SynopsisHeading = """(?iu)^[\s ]*synopsis[\s ]*:[\s ]*$"""

  /** The description's own words: what follows its "Synopsis:" heading when it has one, else the description without
   *  the facts sheet heading it; none when nothing is left. */
  private def synopsisOf(description: String): Option[String] = {
    val lines = description.linesIterator.toSeq
    val heading = lines.indexWhere(_.matches(SynopsisHeading))
    val sheet   = lines.takeWhile(line => line.replace(' ', ' ').trim.isEmpty || line.matches(FactLine))
    if (heading < 0 && !sheet.exists(_.matches(FactLine))) Some(description)
    else Some(lines.drop(if (heading >= 0) heading + 1 else sheet.size).mkString("\n").replace(' ', ' ').trim)
      .filter(_.nonEmpty)
  }

  private def attributes(o: JsObject, key: Regex): Seq[String] =
    o.fields.collect { case (key(_*), JsString(value)) if value.trim.nonEmpty => value.trim }.toSeq

  private def attribute(o: JsObject, key: Regex): Option[String] = attributes(o, key).headOption

  /** The minutes a running-time attribute states for the film alone; None when it states anything else (see the
   *  class doc). */
  def runtimeIn(text: String): Option[Int] = text match {
    case RunningTimeText(hours, minutes, bare, note) if !Option(note).exists(NotTheFilmAlone.findFirstIn(_).isDefined) =>
      Option(bare).map(_.toInt)
        .orElse(Option(hours).map(h => h.toInt * 60 + Option(minutes).fold(0)(_.toInt)))
        .filter(m => m > 0 && m <= 600)
    case _ => None
  }

  private def names(value: Option[String]): Seq[String] =
    value.toSeq.flatMap(_.split("[,;]")).map(_.trim).filter(_.nonEmpty)

  private def webUrl(event: JsObject): Option[String] =
    jsonText(event \ "webUrl").filter(_.toLowerCase(Locale.ROOT).startsWith("http"))

  /** The screen, else the performance's venue attribute when that names a room inside
   *  the building ("Cinema 2" at the ICA) rather than the building itself. */
  private def roomOf(instance: JsObject, venue: Option[String]): Option[String] =
    attribute(instance, RoomAttribute).orElse(
      attribute(instance, PlainVenueAttribute).filterNot(v => venue.exists(_.equalsIgnoreCase(v))))
}
