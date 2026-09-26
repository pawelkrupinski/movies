package services.cinemas.pl

import services.cinemas.common.ScraperParse
import services.movies.TitleNormalizer
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import play.api.libs.json._
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDateTime, OffsetDateTime}
import scala.util.Try

/**
 * Generic client for cinemas ticketed through the VisualSoft ticketing platform
 * — branded `systembiletowy.pl` when a venue takes the vendor's subdomain (e.g.
 * `shd.systembiletowy.pl` for the Suchedniów cultural centre's Kino Kuźnica),
 * but the SAME software is also white-labelled onto venues' own domains
 * (`bilety.kino.bochnia.pl`, `bilety.kinomikro.pl`, …).
 *
 * Every instance serves its whole upcoming programme as JSON at
 * `service.php/repertoire/list.json` — the feed the instance's own front end
 * and the venues' own websites read. It replaced scraping the instance
 * homepage, whose markup came in four skins that drifted independently
 * (Bochnia's titles moved from `h3` to `h2` on ~2026-09-21 and the venue read
 * empty for five days); the feed has one shape everywhere. Each record is one
 * screening: `title`, an ISO `date` with the zone offset, and `id`.
 *
 * `advanced=1` adds a `url` (the `kup-bilet` booking link), an `image`, an
 * `event` (`description`, `category`) and a `location` (`institution_name`).
 * Older instances (Farys, Kino Orzeł in 2026-09) don't ship the advanced
 * template and answer `{"error":"The template … does not exist …"}`; for those
 * the plain feed is fetched instead, and the booking link is the
 * `index.php/repertoire.html?id=<id>` page their homepage links, keyed by the same id.
 *
 * Two optional scopes, for instances that sell more than one venue's events:
 *   - `institution` keeps only records whose `location.institution_name`
 *     matches — Kino Mikro and Mikro Bronowice share one instance; Oświęcim's
 *     culture centre sells its concerts next to Nasze Kino's screenings.
 *   - `filmGroups` keeps only records whose `event.category` is one of them —
 *     BCKino (Bytom) tags films "BCKino" beside "Warsztaty"/"BECEK CZYTA"; Kino
 *     Frajda's are "Imprezy SDK" beside Chorzów's own "Imprezy ChCK" — and
 *     peels the "<group> – " prefix those venues glue onto a title.
 * Both need the advanced feed. Left empty (the default) they change nothing.
 *
 * One instance per venue, captured by its `baseUrl` + `cinema` (+ scopes), so
 * adding a VisualSoft-hosted cinema is a catalog line, not a new client (OCP).
 */
class SystemBiletowyClient(http: HttpFetch, baseUrl: String, override val cinema: Cinema,
                           titles: TitleNormalizer, filmGroups: Set[String] = Set.empty,
                           institution: Option[String] = None)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)
  // Two venues on one instance share its URL; the institution tells them apart.
  override def sourceKey: Option[String] =
    super.sourceKey.map(key => institution.fold(key)(name => s"$key#$name"))

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    SystemBiletowyClient.parse(feed(), cinema, baseUrl, titles, filmGroups, institution)

  private def feed(): String = {
    val advanced = http.get(SystemBiletowyClient.advancedFeedUrl(baseUrl))
    if (SystemBiletowyClient.lacksAdvancedTemplate(advanced)) http.get(SystemBiletowyClient.basicFeedUrl(baseUrl))
    else advanced
  }
}

object SystemBiletowyClient {

  // `limit` is honoured and the default page is small; 1000 is ~5× the busiest
  // instance's whole programme (Kino PCKul, 206 in 2026-09).
  def advancedFeedUrl(baseUrl: String): String = s"$baseUrl/service.php/repertoire/list.json?limit=1000&advanced=1"
  def basicFeedUrl(baseUrl: String): String    = s"$baseUrl/service.php/repertoire/list.json?limit=1000"

  // A body that isn't JSON at all throws here — a failed scrape, not an empty one.
  private[pl] def lacksAdvancedTemplate(json: String): Boolean = {
    val parsed = Json.parse(json)
    (parsed \ "repertoires").isEmpty && (parsed \ "error").isDefined
  }

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], format: List[String],
                             poster: Option[String], director: Seq[String])

  // Kino Orzeł's (and Nowa Sarzyna's) boilerplate "-Film"/"- Film" word ahead
  // of the format tag ("…-Film 2D", "… - Film 2D dubbing") — not a real
  // format/version word, so it's peeled before FormatTags sees the title.
  // Restricted (via the lookahead) to where it's immediately followed by a real
  // format/version word, so a title that legitimately contains "Film" is never
  // touched.
  private val FilmBoilerplate =
    """(?i)[-–—]\s*Film\b(?=\s+(?:2D|3D|IMAX|4DX|dolby|atmos|dubbing|dubb|dub|napisy|nap|lektor|lek)\b)""".r

  def parse(json: String, cinema: Cinema, baseUrl: String, titles: TitleNormalizer,
            filmGroups: Set[String] = Set.empty, institution: Option[String] = None): Seq[CinemaMovie] = {
    val records = (Json.parse(json) \ "repertoires").toOption.toSeq.flatMap {
      case o: JsObject => o.values.toSeq
      case a: JsArray  => a.value.toSeq
      case _           => Seq.empty
    }
    def absolute(path: String) = if (path.startsWith("http")) path else baseUrl + path

    val slots = records.flatMap { r =>
      val group = (r \ "event" \ "category").asOpt[String].getOrElse("")
      for {
        rawTitle <- (r \ "title").asOpt[String]
        if institution.forall(name => (r \ "location" \ "institution_name").asOpt[String].contains(name))
        if filmGroups.isEmpty || filmGroups.contains(group)
        peeled    = FilmBoilerplate.replaceFirstIn(stripGroupPrefix(rawTitle.trim, group), "")
        title     = titles.cinemaClean(cinema.slug, cleanTitle(peeled))
        if title.nonEmpty
        dateTime <- (r \ "date").asOpt[String].flatMap(d => Try(OffsetDateTime.parse(d).toLocalDateTime).toOption)
      } yield RawSlot(
        title    = title,
        dateTime = dateTime,
        booking  = (r \ "url").asOpt[String].filter(_.nonEmpty).map(absolute)
                     .orElse((r \ "id").toOption.map {
                       case JsString(id) => id
                       case id           => id.toString
                     }.map(id => s"$baseUrl/index.php/repertoire.html?id=$id")),
        format   = ScraperParse.extractFormatTags(peeled)._2,
        poster   = (r \ "image").asOpt[String].filter(_.nonEmpty).map(absolute),
        director = (r \ "event" \ "description").asOpt[String].map(parseDirector).getOrElse(Seq.empty)
      )
    }.distinctBy(s => (s.title, s.dateTime, s.booking))

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val sorted = group.sortBy(_.dateTime)
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = sorted.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = sorted.map(_.director).find(_.nonEmpty).getOrElse(Seq.empty),
        showtimes = showtimes
      )
    }
  }

  /** Drop the trailing `dubbing`/`napisy`/… version tag (so the same film's
   *  dubbed and subtitled screenings merge into one row) and sentence-case the
   *  result. Tag stripping is shared with the other portal clients. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))

  /** Peel a "<group> – "/"<group> - " prefix a mixed-category venue's own
   *  listing glues onto its title ("BCKino – Kandydaci śmierci" → "Kandydaci
   *  śmierci"), case-insensitively. A no-op when `group` is empty. */
  private def stripGroupPrefix(title: String, group: String): String =
    if (group.isEmpty) title
    else title.replaceFirst("(?i)^" + java.util.regex.Pattern.quote(group) + """\s*[-–—]\s*""", "")

  // `event.description` is an HTML blob whose director line reads either
  // `<div>Reżyseria George Sluizer</div>` (no colon) or `<br>Reżyseria:
  // Federico Fellini <br>` (colon). Once Jsoup flattens the markup to text the
  // `<div>`/`<br>` boundaries collapse to spaces, so the value is bounded by
  // the next field label rather than by markup. Capture everything after the
  // `Reżyseria` marker up to the next known label, a `|` separator
  // (`Reżyseria: Sam Raimi | Produkcja: USA, 1987`), or the end of text. The
  // label's end is `(?!\p{L})`, not `\b`: Java's `\b` is ASCII-only, so it sees
  // no boundary after the `ą` of "Występują".
  private val FieldLabels = "Obsada|Występują|Scenariusz|Muzyka|Zdjęcia|Gatunek|Produkcja|Czas|Dystrybutor"
  private val DirectorPat =
    ("""(?i)Reżyseria\s*:?\s*(.+?)\s*(?=(?:""" + FieldLabels + """)(?!\p{L})|\||$)""").r

  /** Pull the director name(s) out of a record's `event.description` HTML.
   *  Splits the captured value on `,`/`;`, trims, and drops empties. Empty when
   *  the blob carries no `Reżyseria` marker. */
  private[cinemas] def parseDirector(descriptionHtml: String): Seq[String] = {
    val text = Jsoup.parse(descriptionHtml).text()
    DirectorPat.findFirstMatchIn(text).map(_.group(1)).toSeq.flatMap { captured =>
      captured.split("[,;]").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    }
  }
}
