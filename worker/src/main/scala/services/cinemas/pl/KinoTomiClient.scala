package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{AgeRating, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Tomi (Pabianice). Its own ticketing site renders the WHOLE programme —
 * every scheduled day, weeks ahead — server-side on one page, `/repertuarr`:
 * one `div.seat-plan-row[data-date=YYYY-MM-DD]` section per day, each an `li`
 * per film:
 *   - `.movie-name img`          → poster
 *   - `.movie-name a.name`       → the title as the anchor's own text, the
 *     version ("- Dubbing" / "- Napisy" / "- Polski") in a nested `<span>`, and
 *     the `/film/<slug>` detail page as its href
 *   - `.movie-schedule a.item[href*=zamowienie/]` → one per screening: the
 *     `HH:MM` and the seat-selection booking link. Other `a.item`s there are
 *     programme badges ("Niedzielne Poranki") and are skipped.
 *
 * Because the version lives in its own element, a title that itself contains
 * " - " ("Avengers: Koniec gry - wersja rozszerzona") needs no splitting: the
 * dubbed and subtitled billings of one film land on the same film, each
 * showing badged by its own version. "Polski" (a Polish-language film) maps to
 * no badge.
 *
 * The `/film/<slug>` detail page carries the director, runtime, genres, cast,
 * age rating, synopsis and trailer, so it is deferred to [[DetailEnricher]] with
 * TMDB resolution waiting for it. Its "Rok" field is NOT the production year —
 * it is the Polish (re-)release year (Avengers: Endgame, 2019, and Asterix &
 * Obelix: Mission Cleopatra, 2002, both read "Rok: 2026") — so it is never
 * emitted as a year hint.
 */
class KinoTomiClient(http: HttpFetch, override val cinema: Cinema = KinoTomi) extends CinemaScraper with DetailEnricher {

  import KinoTomiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = parse(http.get(RepertoireUrl), cinema)

  override val detailGroup: String = "kino-tomi"

  /** A durable 404/410 escapes (see [[DetailFetchOutcome]]); a loaded page is a
   *  detail even when it parses to nothing, so it is stamped, not retried. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(parseDetail)
}

object KinoTomiClient {

  val BaseUrl       = "https://kinotomi.pl"
  val RepertoireUrl = s"$BaseUrl/repertuarr"

  private val YouTubeId  = """[?&]v=([\w-]{11})""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    format:   List[String],
    filmUrl:  Option[String],
    poster:   Option[String]
  )

  private[pl] def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("div.seat-plan-row[data-date]").asScala.toSeq.flatMap { day =>
      Try(LocalDate.parse(day.attr("data-date").trim)).toOption.toSeq
        .flatMap(date => day.select("ul.seat-plan-wrapper > li").asScala.toSeq.flatMap(filmRow(_, date)))
    }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def filmRow(row: Element, date: LocalDate): Seq[RawSlot] =
    Option(row.selectFirst(".movie-name a.name")).toSeq.flatMap { link =>
      val title   = link.ownText.trim
      val format  = Option(link.selectFirst("span")).map(s => ScraperParse.formatTokensIn(s.text)).getOrElse(Nil)
      val filmUrl = Option(link.attr("abs:href")).filter(_.nonEmpty)
      val poster  = Option(row.selectFirst(".movie-name img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)
      if (title.isEmpty) Seq.empty
      else row.select(".movie-schedule a.item[href*='zamowienie/']").asScala.toSeq.flatMap { a =>
        ScraperParse.parseHHmm(a.text).map { time =>
          RawSlot(title, LocalDateTime.of(date, time), Option(a.attr("abs:href")).filter(_.nonEmpty), format, filmUrl, poster)
        }
      }
    }

  private[pl] def parseDetail(html: String): FilmDetail = {
    val doc = Jsoup.parse(html, BaseUrl)
    val about = Option(doc.selectFirst(".movie-details .tab-area .tab-item .item"))
    // The fact list is `<li>Label: value</li>`, except the cast, whose value is
    // the `<li>` after a bare "Obsada:".
    val items = about.toSeq.flatMap(_.select("ul > li").asScala.toSeq.map(_.text.trim))
    val facts = items.flatMap { item =>
      item.split(":", 2) match {
        case Array(label, value) => Some(label.trim.toLowerCase -> value.trim)
        case _                   => None
      }
    }.toMap
    val castLine = items.indexWhere(_.toLowerCase.startsWith("obsada")) match {
      case -1 => None
      case i  => facts.get("obsada").filter(_.nonEmpty).orElse(items.lift(i + 1))
    }
    def names(value: Option[String]): Seq[String] =
      value.toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).distinct

    val synopsis = about.map(ScraperParse.cleanSynopsis(_, "ul")).filter(_.nonEmpty)
    val runtime = doc.select(".duration-area .item").asScala
      .find(_.selectFirst(".fa-clock") != null).flatMap(i => ScraperParse.hoursMinutesRuntime(i.text))
    val trailer = Option(doc.selectFirst("a.video-popup[href]"))
      .flatMap(a => YouTubeId.findFirstMatchIn(a.attr("href")))
      .flatMap(m => ScraperParse.canonicalTrailer(s"https://www.youtube.com/watch?v=${m.group(1)}"))

    FilmDetail(
      synopsis       = synopsis,
      cast           = names(castLine),
      director       = names(facts.get("reżyser")),
      runtimeMinutes = runtime,
      genres         = doc.select(".details-banner .movie-tags span").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty),
      posterUrl      = Option(doc.selectFirst(".details-banner-thumb > img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty),
      trailerUrl     = trailer,
      ageRating      = AgeRating.normalize(facts.get("ograniczenia"))
    )
  }
}
