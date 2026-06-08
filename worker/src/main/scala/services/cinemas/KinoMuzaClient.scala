package services.cinemas

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters.*
import scala.util.Try

class KinoMuzaClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper {

  val cinema: Cinema = KinoMuza
  private val RepertoireUrl = "https://www.kinomuza.pl/repertuar/"

  private def parseDate(ddMM: String): Option[LocalDate] =
    Try {
      val parts     = ddMM.trim.split("\\.")
      val candidate = LocalDate.of(today.getYear, parts(1).toInt, parts(0).toInt)
      // Only roll forward when the date is *substantially* in the past —
      // matches `KinoBulgarskaClient`'s rule. A 2-day-old listing (the
      // site still shows yesterday's screening, the snapshot test runs
      // 2 days after its capture date, …) is this year, not next; only
      // a year-boundary January-from-December case justifies rolling
      // forward. 6 months is the sweet spot between the two.
      if (candidate.isBefore(today.minusMonths(6))) candidate.plusYears(1) else candidate
    }.toOption

  // 5-min scrape returns just the listing — title, director, runtime, year,
  // country, poster, showtimes. Per-film detail-page synopses used to be
  // fetched inline (two-phase fetch + parallel workers) but the 80+ extra
  // requests every tick tripped Muza's burst limiter and synopses came
  // back empty for the whole tick. A separate
  // `KinoMuzaSynopsisRefresher` walks unresolved rows once, slowly, off
  // the scrape tick; this `fetch()` is back to a single repertuar-page
  // request.
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl))

  // Muza's detail pages render the synopsis in the first `paragraph`-classed
  // column of the film header — `div.col-lg-7.paragraph`. A second
  // `.paragraph` block appears further down for "Treści wrażliwe" (sensitive
  // content); scoping to the col-lg-7 variant keeps that out. Multiple
  // `<p>` children get joined with a blank line so original paragraph
  // boundaries survive the homepage card formatter.
  //
  // Public so `KinoMuzaSynopsisRefresher` can drive its per-row fetches
  // through the same parser without re-running the listing loop.
  def parseSynopsis(document: Document): Option[String] = {
    val paragraphs = document.select("div.col-lg-7.paragraph > p").asScala
      .map(_.text().trim)
      .filter(_.nonEmpty)
      .toSeq
    if (paragraphs.isEmpty) None else Some(paragraphs.mkString("\n\n"))
  }

  /** Higher-fidelity portrait poster from a Muza detail page (the listing
   *  page only carries a smaller landscape thumbnail). The detail page
   *  template renders the poster as
   *  `<img class="lazyload img-fuild" data-src=".../<title>-NNNx800.png"
   *  alt="<title> - okładka">`. The typo class `img-fuild` (sic; Muza's
   *  template, not ours) is unique to the poster slot, distinguishing it
   *  from the screenshot stills further down (`lazyload` without
   *  `img-fuild`) and the mobile-only banner above (eagerly loaded with
   *  `src`, no `data-src`). Returns `None` when the slot is absent.
   *  Public so `KinoMuzaSynopsisRefresher` can drive its per-row poster
   *  upgrade through the same parser. */
  def parsePoster(document: Document): Option[String] =
    Option(document.selectFirst("img.img-fuild[data-src]"))
      .map(_.attr("data-src"))
      .filter(_.nonEmpty)

  /** Trailer URL from a Muza detail page. Muza embeds the YouTube trailer
   *  via `<iframe class="embed-responsive-item" src="https://www.youtube
   *  .com/embed/<id>?…">`. Returns the canonical `watch?v=<id>` form; the
   *  view layer reshapes back to `/embed/` at render time via
   *  `TrailerEmbed.embedUrlFor`. Public so the refresher can drive the
   *  per-row fetch through the same parser. */
  def parseTrailer(document: Document): Option[String] =
    Option(document.selectFirst("iframe.embed-responsive-item")).map(_.attr("src")).filter(_.nonEmpty)
      .flatMap(ScraperParse.canonicalTrailer)

  private val RuntimePat = """(\d+)’""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  // Strip the "| najlepsze z najgorszych" series tag Muza appends to films
  // featured in that recurring programme — same film, regular title in the
  // rest of the corpus.
  private val SeriesSuffix = """(?i)\s*\|\s*najlepsze\s+z\s+najgorszych\s*$""".r

  private def cleanTitle(raw: String): String =
    SeriesSuffix.replaceFirstIn(raw, "").trim

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc      = Jsoup.parse(html)
    val previews = doc.select("#movies .preview").asScala

    previews.flatMap { preview =>
      val title    = Option(preview.selectFirst(".preview-title")).map(_.text().trim).map(cleanTitle).filter(_.nonEmpty)
      val filmUrl  = Option(preview.selectFirst("a[href*=/movie/]")).map(_.attr("href"))
      val infoHtml  = Option(preview.selectFirst(".f1-bold p")).map(_.html()).getOrElse("")
      val infoText  = Option(preview.selectFirst(".f1-bold p")).map(_.text()).getOrElse("")
      // Info block is <br>-separated: line 0 = "reż. <names>", line 1 = country/-ies,
      // then year and runtime (also picked up by the regexes from the flat text).
      val infoLines = infoHtml.split("(?i)<br\\s*/?>").toSeq
                              .map(line => Jsoup.parse(line).text().trim)
                              .filter(_.nonEmpty)
      val director  = infoLines.headOption
        .map(_.replaceFirst("(?i)^\\s*reż\\.\\s*", "").trim)
        .filter(_.nonEmpty)
        .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      // Line 1, if present, is the country list. Skip it if it instead carries a
      // year/runtime token (some entries omit the country line entirely).
      // Split on "," for co-productions ("Polska, Niemcy" → two entries).
      val countries = infoLines.lift(1)
        .filterNot(_.matches(".*\\b(?:19|20)\\d{2}\\b.*"))
        .filterNot(_.matches(".*\\d+\\s*[\\u2019'].*"))
        .filter(_.nonEmpty)
        .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      val runtimeMinutes = RuntimePat.findFirstMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption)
      val releaseYear    = YearPat.findAllMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption).toSeq.headOption

      title.map { title =>
        val showtimes = preview.select(".table-row").asScala.flatMap { row =>
          val dateOpt = Option(row.selectFirst(".day")).flatMap(el => parseDate(el.text()))
          val items   = row.select(".ticket-list-item").asScala

          dateOpt.toSeq.flatMap { date =>
            items.flatMap { item =>
              val timeText   = Option(item.selectFirst(".ticket-hour")).map(_.text().trim)
              val bookingUrl = Option(item.selectFirst("a.ticket-buy")).map(_.attr("href"))
              val room       = Option(item.selectFirst("span.text-gold"))
                                 .map(_.parent().text().trim)
                                 .filter(_.nonEmpty)
              timeText.flatMap { t =>
                Try {
                  val parts = t.split(":")
                  Showtime(LocalDateTime.of(date, LocalTime.of(parts(0).toInt, parts(1).toInt)),
                           bookingUrl, room)
                }.toOption
              }
            }
          }
        }.toSeq.distinctBy(_.dateTime)

        CinemaMovie(
          movie     = Movie(title, runtimeMinutes, releaseYear, countries = countries),
          cinema    = KinoMuza,
          // The listing page only carries a landscape-cropped thumbnail; the
          // higher-fidelity portrait poster lives on each film's detail page
          // and is pulled by `KinoMuzaSynopsisRefresher` on the
          // `CinemaMovieAdded` event. Leaving the listing's posterUrl as None
          // means the merge in `MovieCache.recordCinemaScrape` can't clobber
          // the refresher's portrait on subsequent ticks.
          posterUrl = None,
          filmUrl   = filmUrl,
          synopsis  = None,
          cast      = Seq.empty,
          director  = director,
          showtimes = showtimes.sortBy(_.dateTime)
        )
      }
    }.toSeq.filter(_.showtimes.nonEmpty)
  }
}

