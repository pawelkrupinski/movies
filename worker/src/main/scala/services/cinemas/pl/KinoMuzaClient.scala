package services.cinemas.pl

import org.jsoup.nodes.Document
import models.*
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters.*
import scala.util.Try

class KinoMuzaClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")))
  extends CinemaScraper with DetailEnricher with OnlyMovieEventsFilter {

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

  // The listing scrape returns just the listing — title, director, runtime,
  // year, country, showtimes (no synopsis/poster/trailer). Fetching all 80+
  // detail pages inline every tick tripped Muza's burst limiter and synopses
  // came back empty for the whole tick, so per-film detail is DEFERRED: this
  // `fetch()` is a single repertuar-page request, and each film's synopsis /
  // poster / trailer is filled later by a deduped `EnrichDetails` queue task
  // (the `DetailEnricher.fetchFilmDetail` below). Unlike most deferred-detail
  // cinemas this detail is display-only — the listing already carries the TMDB
  // hints (director/year) — so `defersTmdbResolution = false` keeps TMDB
  // resolution immediate rather than waiting on the detail fetch.
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl))

  // Muza's detail pages render the synopsis in the first `paragraph`-classed
  // column of the film header — `div.col-lg-7.paragraph`. A second
  // `.paragraph` block appears further down for "Treści wrażliwe" (sensitive
  // content); scoping to the col-lg-7 variant keeps that out. Multiple
  // `<p>` children get joined with a blank line so original paragraph
  // boundaries survive the homepage card formatter.
  //
  // Used by `fetchFilmDetail`; public so the client spec can exercise it
  // against a detail-page document directly.
  def parseSynopsis(document: Document): Option[String] = {
    // Drop paragraphs that are organiser/event links (an `<a>` to a festival,
    // Instagram/Facebook, ticket page) and strip any plain-text URL from the
    // rest, so only prose paragraphs survive.
    val paragraphs = document.select("div.col-lg-7.paragraph > p").asScala
      .filter(p => Option(p.selectFirst("a")).isEmpty)
      .map(p => ScraperParse.stripUrls(p.text().trim))
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
   *  Used by `fetchFilmDetail`; public for the client spec. */
  def parsePoster(document: Document): Option[String] =
    Option(document.selectFirst("img.img-fuild[data-src]"))
      .map(_.attr("data-src"))
      .filter(_.nonEmpty)

  /** Trailer URL from a Muza detail page. Muza embeds the YouTube trailer
   *  via `<iframe class="embed-responsive-item" src="https://www.youtube
   *  .com/embed/<id>?…">`. Returns the canonical `watch?v=<id>` form; the
   *  view layer reshapes back to `/embed/` at render time via
   *  `TrailerEmbed.embedUrlFor`. Used by `fetchFilmDetail`; public for the
   *  client spec. */
  def parseTrailer(document: Document): Option[String] =
    Option(document.selectFirst("iframe.embed-responsive-item")).map(_.attr("src")).filter(_.nonEmpty)
      .flatMap(ScraperParse.canonicalTrailer)

  override val detailGroup: String = "kino-muza"

  /** KinoMuza's deferred detail is display-only (synopsis / poster / trailer);
   *  its TMDB hints (director / year) already come from the listing scrape, so
   *  resolution must NOT wait on this fetch — see the class comment. */
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail fetch — the `EnrichDetails` task calls this with
   *  the listing's `filmUrl`. One detail-page request covers synopsis, poster,
   *  and trailer (Muza ships all three on the same page). `None` on a fetch
   *  failure so the task stays stale and is retried rather than recording an
   *  empty result as fresh; a page that simply lacks a synopsis still returns
   *  `Some` (with whatever fields it does carry), marking the row fresh so it
   *  isn't refetched until the freshness window lapses. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.get(ref)).toOption.map { html =>
      val document = Jsoup.parse(html)
      FilmDetail(
        synopsis   = parseSynopsis(document),
        posterUrl  = parsePoster(document),
        trailerUrl = parseTrailer(document)
      )
    }

  private val RuntimePat = """(\d+)’""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  // The "| najlepsze z najgorszych" series-tag strip now lives in the editable
  // "kino-muza" rules (see TitleRules).
  private def cleanTitle(raw: String): String =
    services.movies.TitleNormalizer.cinemaClean("kino-muza", raw)

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val document      = Jsoup.parse(html)
    val previews = document.select("#movies .preview").asScala

    previews.flatMap { preview =>
      val rawTitle = Option(preview.selectFirst(".preview-title")).map(_.text().trim).filter(_.nonEmpty)
      val title    = rawTitle.map(cleanTitle).filter(_.nonEmpty)
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
          val dateOpt = Option(row.selectFirst(".day")).flatMap(element => parseDate(element.text()))
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
          movie     = Movie(title, runtimeMinutes, releaseYear, countries = countries, rawTitle = rawTitle),
          cinema    = KinoMuza,
          // The listing page only carries a landscape-cropped thumbnail; the
          // higher-fidelity portrait poster lives on each film's detail page
          // and is pulled by the deferred `fetchFilmDetail`. Leaving the
          // listing's posterUrl as None means the merge in
          // `MovieCache.recordCinemaScrape` can't clobber the detail-page
          // portrait on subsequent ticks.
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
