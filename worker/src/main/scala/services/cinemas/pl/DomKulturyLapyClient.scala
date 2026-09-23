package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._

/**
 * Dom Kultury w Łapach. Its `dklapy.pl/kino/` page is a WordPress (Gutenberg)
 * blog listing that embeds the FULL body of its recent "Premiery kinowe"
 * posts inline — no need to follow a post's own permalink for the schedule.
 * Each screening is one `div.wp-block-columns` whose first
 * `div.wp-block-column` carries the title (`h2.wp-block-heading`) and the
 * date/time (`h5.wp-block-heading`, "D miesiąca godzina HH:MM" — no year,
 * the venue only ever posts about the upcoming week or two), and whose
 * second column carries two plain `<p>`s (the homepage embed strips the
 * `wp-block-paragraph` class the post carries on its own permalink page): a
 * "Xg. Ym.; Kraj Rok; gatunek, gatunek" metadata line, then the synopsis.
 *
 * The page also embeds an unrelated "Film Konesera" (free "film of the
 * connoisseur") series whose cards use `h5`, not `h2`, for a deliberately
 * WITHHELD title ("Tytuł filmu dostępny w domu kultury") — selecting on
 * `h2.wp-block-heading` for the title excludes that series without a
 * dedicated non-film filter; it never has a real title to show anyway.
 */
class DomKulturyLapyClient(http: HttpFetch, override val cinema: Cinema = DomKulturyLapy,
                 today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(DomKulturyLapyClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(DomKulturyLapyClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    DomKulturyLapyClient.parse(http.get(DomKulturyLapyClient.RepertoireUrl), cinema, today)
}

object DomKulturyLapyClient {

  val BaseUrl       = "https://dklapy.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  // "1g. 52m." / "2g. 42m." runtime, hours always present, minutes optional.
  private val RuntimePat = """(\d+)g\.\s*(?:(\d+)m\.)?""".r

  private case class RawSlot(
    title:      String,
    dateTime:   LocalDateTime,
    runtime:    Option[Int],
    countries:  Seq[String],
    year:       Option[Int],
    genres:     Seq[String],
    synopsis:   String,
    filmUrl:    Option[String]
  )

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("div.wp-block-columns").asScala.toSeq.flatMap(parseBlock(_, today))

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, group, showtimes) =>
      val first = group.head
      CinemaMovie(
        movie     = Movie(
          title          = title,
          runtimeMinutes = first.runtime,
          releaseYear    = first.year,
          countries      = first.countries,
          genres         = first.genres
        ),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = first.filmUrl,
        synopsis  = Some(first.synopsis).filter(_.nonEmpty),
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseBlock(columns: Element, today: LocalDate): Option[RawSlot] = {
    val cols = columns.select("> div.wp-block-column").asScala.toSeq
    for {
      first        <- cols.headOption
      second       <- cols.lift(1)
      titleElement <- Option(first.selectFirst("h2.wp-block-heading"))
      title         = titleElement.text.trim
      if title.nonEmpty
      dateElement  <- Option(first.selectFirst("h5.wp-block-heading"))
      dayMonth     <- ScraperParse.parseDayMonth(dateElement.text)
      time         <- ScraperParse.parseHHmm(dateElement.text)
      date         <- ScraperParse.upcomingDate(dayMonth, today)
      // The homepage embeds a post's body WITHOUT the `wp-block-paragraph`
      // class WordPress adds when the post renders on its own permalink page
      // (verified against both: the standalone post page has it, the
      // `/kino/` embed doesn't) — select on the bare tag.
      paragraphs    = second.select("> p").asScala.toSeq
      metaText     <- paragraphs.headOption.map(_.text.trim)
      synopsis     <- paragraphs.lift(1).map(_.text.trim)
    } yield {
      val parts      = metaText.split(";").map(_.trim)
      val runtime    = parts.headOption.flatMap(RuntimePat.findFirstMatchIn).map { m =>
        m.group(1).toInt * 60 + Option(m.group(2)).map(_.toInt).getOrElse(0)
      }
      val (countries, year) = parts.lift(1).map(ScraperParse.productionMeta).getOrElse((Nil, None))
      val genres     = parts.lift(2).map(_.split(",").toSeq.map(_.trim).filter(_.nonEmpty)).getOrElse(Seq.empty)
      val filmUrl    = Option(columns.closest("div.post")).flatMap(post => Option(post.selectFirst("h4 a[href]")))
        .map(_.attr("abs:href")).filter(_.nonEmpty)
      RawSlot(title, LocalDateTime.of(date, time), runtime, countries, year, genres, synopsis, filmUrl)
    }
  }
}
