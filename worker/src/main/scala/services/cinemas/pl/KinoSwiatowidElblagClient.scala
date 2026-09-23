package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import services.movies.FormatTags
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ScrapeHorizon}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Światowid (Elbląg — not to be confused with [[KinoSwiatowid]], its
 * Katowice namesake). Its own site at `kino.swiatowid.elblag.pl/repertuar`
 * renders one `div.movie-card` per film, but ONLY for whichever day the
 * `?dzien=YYYY-MM-DD` query names — a genuinely per-day filter, confirmed by
 * probing six consecutive days and finding zero booking-link slugs shared
 * across any two `dzien` values. So this is fetched one request per day,
 * walking forward for as long as the programme lasts (see [[ScrapeHorizon]]),
 * the same shape [[KinoMikroClient]] uses for its JSON feed.
 *
 * The showtime's booking link (`bilet.swiatowid.elblag.pl/index.php/kup-bilet/
 * <slug>-<date>-<time>[-N]`) is NOT a reliable date source — its embedded date
 * is frozen at whenever that recurring "event" was created in the booking
 * system (visualTicket) and does not move with the actual screening, while
 * the trailing numeric suffix is an unrelated session-instance counter. Two
 * different bookable instances of the same nominal slug, requested on two
 * different `dzien` days, were confirmed (by opening the booking page itself)
 * to land on the ACTUAL date being browsed at the time — so every showtime's
 * real date is the `dzien` value its `<li>` was read under, paired with the
 * bare `HH:MM` text next to it; the slug is kept only as the booking link.
 *
 * Per film-card:
 *   - `h3.movie-card__title`                    → title
 *   - `div.movie-card__thumbnail a[href]`        → the venue's own film page
 *   - `div.movie-card__thumbnail img[src]`       → poster
 *   - first `p.movie-card__description`          → "<genres> - <runtime> min - <age>"
 *   - second `p.movie-card__description`         → language version ("napisy"/"dubbing"),
 *     fed through [[FormatTags]] the way [[KinoSlezaClient]] does
 *   - `ul.movie-card__showtimes a[href]`          → one booking link per `HH:MM`
 *
 * Room isn't exposed on this listing at all (confirmed only present on the
 * booking site's own per-showtime confirmation page, which we don't follow —
 * that would multiply the request count ~10x over the per-day fetch this
 * already needs), so [[Showtime.room]] is left unset.
 *
 * The venue also runs "The Met Opera" / discussion-club cycles alongside its
 * ordinary film programme, so non-film rows (should one ever appear on this
 * listing) are dropped by [[NonMovieEventClassifier]] — applied in
 * [[reduceChunks]] rather than via the [[OnlyMovieEventsFilter]] mixin, since
 * that trait's `fetch()` is `final` and collides with
 * [[ChunkedCinemaScraper]]'s own final `fetch()`.
 */
class KinoSwiatowidElblagClient(
  http:                HttpFetch,
  override val cinema: Cinema     = KinoSwiatowidElblag,
  today:               LocalDate  = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends ChunkedCinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSwiatowidElblagClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoSwiatowidElblagClient.RepertoireUrl)

  def planChunks(): Seq[String] =
    ScrapeHorizon.liveDays(today) { day =>
      KinoSwiatowidElblagClient.parseDay(http.get(KinoSwiatowidElblagClient.dayUrl(day)), day, cinema).nonEmpty
    }.map(_.toString).grouped(KinoSwiatowidElblagClient.DaysPerChunk).map(_.mkString(",")).toSeq

  def fetchChunk(key: String): Seq[CinemaMovie] =
    key.split(",").toSeq.flatMap { d =>
      val day = LocalDate.parse(d)
      KinoSwiatowidElblagClient.parseDay(http.get(KinoSwiatowidElblagClient.dayUrl(day)), day, cinema)
    }

  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    super.reduceChunks(chunks).filterNot(cm => NonMovieEventClassifier.isLiveEvent(cm.movie.title))
}

object KinoSwiatowidElblagClient {

  /** Days per chunk task, so a wider window costs chunk tasks in weeks not days. */
  val DaysPerChunk = 7

  val BaseUrl       = "http://kino.swiatowid.elblag.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar"

  def dayUrl(date: LocalDate): String = s"$RepertoireUrl?dzien=$date"

  // "dramat , kryminał - 122 min - 15+" (Jsoup collapses the source's
  // multi-line comma list to single spaces) — genres before the dash-min
  // marker, runtime, then the trailing age tag.
  private val MetaPat = """^(.*?)-\s*(\d+)\s*min\s*-\s*(.+)$""".r

  private[cinemas] def parseDay(html: String, day: LocalDate, cinema: Cinema): Seq[CinemaMovie] =
    Jsoup.parse(html, BaseUrl).select("div.movie-card").asScala.toSeq.flatMap(parseMovieCard(_, day, cinema))

  private def parseMovieCard(card: Element, day: LocalDate, cinema: Cinema): Option[CinemaMovie] = {
    val descriptions = card.select("p.movie-card__description").asScala.toSeq
    val (genres, runtime) = descriptions.headOption.map(_.text.trim).flatMap(metaOf).getOrElse((Seq.empty, None))
    val format = descriptions.lift(1).map(_.text).toSeq.flatMap(FormatTags.formatTokensIn).distinct.toList

    val showtimes = card.select("ul.movie-card__showtimes a[href]").asScala.toSeq.flatMap { link =>
      ScraperParse.parseHHmm(link.text.trim).map { time =>
        Showtime(LocalDateTime.of(day, time), Some(link.attr("abs:href")).filter(_.nonEmpty), format = format)
      }
    }

    for {
      title <- Option(card.selectFirst("h3.movie-card__title")).map(_.text.trim).filter(_.nonEmpty)
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title, runtimeMinutes = runtime, genres = genres),
      cinema    = cinema,
      posterUrl = Option(card.selectFirst("div.movie-card__thumbnail img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty),
      filmUrl   = Option(card.selectFirst("div.movie-card__thumbnail a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = showtimes.sortBy(_.dateTime)
    )
  }

  /** Genres and runtime out of the "dramat , kryminał - 122 min - 15+" line.
    * Silently yields nothing (not a thrown error) for the rare card whose
    * description doesn't carry a runtime — the film still gets a `CinemaMovie`
    * with empty genres, just not runtime-less parsing noise. */
  private def metaOf(text: String): Option[(Seq[String], Option[Int])] =
    text match {
      case MetaPat(genresPart, runtimeDigits, _) =>
        val genres = genresPart.split(",").map(_.trim).filter(_.nonEmpty).toSeq
        Some((genres, Try(runtimeDigits.toInt).toOption))
      case _ => None
    }
}
