package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino SCK — Stargardzkie Centrum Kultury (Stargard). Its repertoire at
 * `sck.stargard.pl/repertuar-kina/` is a WordPress (Avada/Fusion) page with no
 * JSON-LD and no JSON API — plain server-rendered HTML. Previously scraped via
 * biletyna.pl, which had stopped carrying the film programme.
 *
 * The page lists one `div.movie-wrapper` per film, each self-contained:
 *   - `.movie-title`                     → the (already clean) title
 *   - `.movie-tags span.movie-tag`       → loose format words ("2d", "dubbing",
 *                                          "napisy") we map to `Showtime.format`
 *                                          display tokens
 *   - `.movie-poster img`                → poster URL (the real src hides in
 *                                          `data-orig-src`; `src` is a lazyload
 *                                          SVG placeholder)
 *   - `.movie-seances a[data-seance_link]` → one booking anchor per screening.
 *     `data-seance_link` carries the FULL "YYYY-MM-DD HH:MM:SS" datetime (so no
 *     year inference is needed) and the anchor `href` is the rezerwacje.sck
 *     booking link. The anchor's visible text is the HH:MM plus a tooltip span;
 *     we read the date+time off the attribute, not the text.
 *
 * Wrappers with no `data-seance_link` are "coming soon" announcements (a
 * `.movie-date` "od DD.MM.YYYY" with no bookable screenings) — they yield no
 * showtimes and are dropped by [[SlotsToMovies.fold]].
 *
 * The page is purely film screenings (no concerts/kabaret/theatre), so
 * [[OnlyMovieEventsFilter]] is not mixed in.
 */
class SckStargardClient(http: HttpFetch, override val cinema: Cinema = KinoSCK)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(SckStargardClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(SckStargardClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    SckStargardClient.parse(http.get(SckStargardClient.RepertoireUrl), cinema)
}

object SckStargardClient {

  val BaseUrl       = "https://sck.stargard.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar-kina/"

  // The full datetime baked into each booking anchor: "2026-06-23 12:00:00".
  private val SeanceDateTime = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    poster:   Option[String],
    format:   List[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.movie-wrapper").asScala.toSeq.flatMap(parseFilm)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, format = s.format)) {
      (_, group, showtimes) =>
        val head = group.head
        CinemaMovie(
          movie     = Movie(head.title),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        )
    }
  }

  /** One film's slots: its title/poster/format plus one [[RawSlot]] per
   *  bookable `data-seance_link` anchor. A wrapper with no such anchor (a
   *  "coming soon" announcement) yields nothing. */
  private def parseFilm(wrapper: Element): Seq[RawSlot] = {
    val title = Option(wrapper.selectFirst(".movie-title")).map(_.text.trim).filter(_.nonEmpty)
    title.toSeq.flatMap { filmTitle =>
      val poster = Option(wrapper.selectFirst(".movie-poster img"))
        .map(img => Option(img.attr("abs:data-orig-src")).filter(_.nonEmpty).getOrElse(img.attr("abs:src")))
        .filter(_.nonEmpty)
        .filterNot(_.startsWith("data:")) // skip the lazyload SVG placeholder

      val format = wrapper.select(".movie-tags span.movie-tag").asScala.toSeq
        .flatMap(tag => ScraperParse.FormatToken.get(tag.text.trim.toLowerCase))
        .distinct
        .toList

      wrapper.select(".movie-seances a[data-seance_link]").asScala.toSeq.flatMap { anchor =>
        Try(LocalDateTime.parse(anchor.attr("data-seance_link").trim, SeanceDateTime)).toOption.map { dt =>
          RawSlot(
            title    = filmTitle,
            dateTime = dt,
            booking  = Option(anchor.attr("abs:href")).filter(_.nonEmpty),
            poster   = poster,
            format   = format
          )
        }
      }
    }
  }
}
