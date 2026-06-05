package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Nowe Horyzonty (Wrocław) — the largest arthouse cinema in Poland. The
 * `program.s` page lists every film with its screenings inline (each slot is a
 * `bilet.s?eventId=` booking link carrying the date + time); the per-film
 * `op.s?id=` page carries runtime / year / countries / genres / director /
 * synopsis. Dates are partly relative ("dzisiaj"/"jutro"), so `today` is
 * injected to keep the fixture replay deterministic.
 */
class NoweHoryzontyClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper {

  val cinema: Cinema = KinoNoweHoryzonty

  private val BaseUrl       = "https://www.kinonh.pl"
  private val RepertoireUrl = s"$BaseUrl/program.s"
  private val FilmIdPat     = """op\.s\?id=(\d+)""".r
  private val EventIdPat     = """eventId=(\d+)""".r

  private case class RawSlot(filmId: String, title: String, poster: Option[String], eventId: String, dateTime: LocalDateTime, bookingUrl: String)

  def fetch(): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(http.get(RepertoireUrl))

    // The listing holds every bookable slot as a `seansapla.mala` anchor
    // (date in span.d, time in span.cz, booking in href). Walk up from each
    // slot to the enclosing film card to recover the film id / title / poster,
    // so both card and single-row layouts are captured uniformly.
    val slots = doc.select("a.seansapla.mala").asScala.toSeq.flatMap { a =>
      val href = a.attr("href")
      for {
        eventId <- EventIdPat.findFirstMatchIn(href).map(_.group(1))
        card    <- a.parents().asScala.find(p => p.selectFirst("a[href^=\"op.s?id=\"]") != null)
        link     = card.selectFirst("a[href^=\"op.s?id=\"]")
        filmId  <- FilmIdPat.findFirstMatchIn(link.attr("href")).map(_.group(1))
        title    = link.text.trim if title.nonEmpty
        date    <- NoweHoryzontyClient.parseDate(Option(a.selectFirst("span.d")).map(_.text.trim).getOrElse(""), today)
        time    <- ScraperParse.parseHHmm(Option(a.selectFirst("span.cz")).map(_.text.trim).getOrElse(""))
      } yield RawSlot(filmId, title, posterOf(card), eventId, date.atTime(time),
                      if (href.startsWith("http")) href else s"$BaseUrl/$href")
    }

    val byFilm = slots.groupBy(_.filmId)
    val pages: Map[String, String] = {
      val pending = byFilm.keys.toSeq.map(id => id -> http.getAsync(s"$BaseUrl/op.s?id=$id"))
      pending.flatMap { case (id, f) => Try(f.join()).toOption.map(id -> _) }.toMap
    }

    byFilm.toSeq.flatMap { case (filmId, group) =>
      val primary    = group.head
      val showtimes  = group.distinctBy(_.eventId).sortBy(_.dateTime)
                         .map(s => Showtime(s.dateTime, Some(s.bookingUrl), None, Nil))
      val d = pages.get(filmId).map(NoweHoryzontyClient.parseDetail).getOrElse(NoweHoryzontyClient.Detail.empty)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(
          title          = primary.title,
          runtimeMinutes = d.runtimeMinutes,
          releaseYear    = d.year,
          originalTitle  = d.originalTitle,
          countries      = d.countries,
          genres         = d.genres
        ),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption.orElse(d.poster),
        filmUrl   = Some(s"$BaseUrl/op.s?id=$filmId"),
        synopsis  = d.synopsis,
        cast      = Seq.empty,
        director  = d.director,
        showtimes = showtimes
      ))
    }
  }

  private def posterOf(block: Element): Option[String] =
    Option(block.selectFirst("span.ilustr")).map(_.attr("style"))
      .flatMap(ScraperParse.cssUrl)
      .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
}

object NoweHoryzontyClient {

  // Polish month abbreviations as they appear in span.d ("12 cze").
  private val Months = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )
  private val DayMonthPat = """(\d{1,2})\s+([a-ząćęłńóśźż]{3})""".r

  /** "dzisiaj"/"jutro"/"DD mmm" → an absolute date, resolved against `today`.
   *  Bare DD-mmm carry no year; take `today`'s year, rolling to next year when
   *  the month is already behind us (a December→January wrap). */
  def parseDate(raw: String, today: LocalDate): Option[LocalDate] = {
    val s = raw.toLowerCase
    if (s.contains("dzisiaj") || s.contains("dziś")) Some(today)
    else if (s.contains("jutro")) Some(today.plusDays(1))
    else DayMonthPat.findFirstMatchIn(s).flatMap { m =>
      Months.get(m.group(2)).flatMap { mon =>
        val year = if (mon < today.getMonthValue) today.getYear + 1 else today.getYear
        Try(LocalDate.of(year, mon, m.group(1).toInt)).toOption
      }
    }
  }

  final case class Detail(
    runtimeMinutes: Option[Int],
    year:           Option[Int],
    originalTitle:  Option[String],
    countries:      Seq[String],
    genres:         Seq[String],
    director:       Seq[String],
    synopsis:       Option[String],
    poster:         Option[String]
  )
  object Detail { val empty: Detail = Detail(None, None, None, Seq.empty, Seq.empty, Seq.empty, None, None) }

  private val RuntimePat = """(\d+)""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  private def crrow(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("div.crrow").asScala.find(_.text.toLowerCase.contains(label))
      .map(_.text.replaceFirst(s"(?i)^[^:]*:\\s*", "").trim).filter(_.nonEmpty)

  /** Parse the op.s film page for metadata. Selectors mirror the page's
   *  `czas:` / `produkcja:` / `gatunek:` credit rows plus the synopsis block. */
  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    val runtime = crrow(doc, "czas").flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt))
    val prod    = crrow(doc, "produkcja")
    val year    = prod.flatMap(s => YearPat.findFirstMatchIn(s).map(_.group(1).toInt))
    val countries = prod.map(s => YearPat.replaceAllIn(s, "")).map(_.trim.stripSuffix(","))
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val genreRaw = crrow(doc, "gatunek")
      .orElse(Option(doc.selectFirst("h4:contains(gatunek)")).map(_.text))
      .map(_.replaceFirst("(?i)^[^:]*:\\s*", "").trim)
      // The genre run is sometimes followed by an age-rating clause in the same
      // element ("Dramat, Kryminał Kategoria Wiekowa: 16+") — drop it.
      .map(_.split("(?i)kategoria").head.trim).filter(_.nonEmpty)
    val genres  = genreRaw.toSeq.flatMap(_.split("[,/]").map(_.trim).filter(_.nonEmpty))
                    .map(tools.TextNormalization.titleCaseIfAllLower)
    val director = Option(doc.selectFirst("h4:contains(reż.) a")).map(_.text.trim)
                    .filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val original = Option(doc.selectFirst("h4.tytulorg")).map(_.text.trim).filter(_.nonEmpty)
    val synopsis = Option(doc.selectFirst("div.txt.wciecia.opisf p")).map(_.text.trim).filter(_.length > 20)
    val poster   = Option(doc.selectFirst("div.plakat img[src]")).map(_.attr("src"))
                    .filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else s"https://www.kinonh.pl/${u.stripPrefix("/")}")
    Detail(runtime, year, original, countries, genres, director, synopsis, poster)
  }
}
