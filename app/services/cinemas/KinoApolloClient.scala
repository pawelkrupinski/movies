package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{DaemonExecutors, HttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Apollo (kinoapollo.pl/kino) — a small Poznań cinema running on WordPress +
 * Elementor + Jet Engine. There's no public JSON/REST endpoint, so we scrape the
 * single repertoire page. Each screening is rendered twice (desktop & mobile
 * variants are both in the DOM); we deduplicate by ticketing URL.
 *
 * Two-phase fetch: the repertoire page yields one row per (film, showtime) with
 * title, time, poster, and the per-film detail-page URL. The detail page is
 * then fetched per unique film for the metadata the listing doesn't expose —
 * runtime (`Czas trwania: NN min.`), synopsis (text-editor body paragraphs),
 * director (`Reżyseria: <a>name</a>`, modern films only), cast (`Obsada:` for
 * modern films / `obsada/cast:` for the Wajda cycle layout). Without the
 * detail-page step every Apollo row had `runtimeMinutes = None` and the row
 * couldn't go into `requireRuntime` in the integration spec.
 *
 * Layout (within one event block on the listing):
 *   1. Day heading (`dd.mm.yyyy` + a Polish weekday) precedes the block
 *   2. Time link  — `<a href=".../event/view/id/N"><span>HH:MM</span>`
 *   3. Title      — `itemprop="name"` div with the film name
 *   4. "Kup bilet" button (same booking URL as the time link)
 *   5. Synopsis   — a `jet-listing-dynamic-field__content` block of paragraphs
 *   6. "Czytaj opis" link — `<a href="kinoapollo.pl/kino/<slug>/">` → detail page
 *   7. Poster     — `<img>` to a WordPress media URL
 */
class KinoApolloClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoApollo
  // Production redirects from /kino to /kino/ — request the canonical-shaped URL
  // directly. The FakeHttpFetch can't traverse a trailing slash to a file.
  private val PageUrl = "https://kinoapollo.pl/kino"
  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  // Every Jet-Engine dynamic link to the ticketing site has a label span. The
  // time link's label is `HH:MM`; the same event also renders a "Kup bilet"
  // button pointing to the same URL. The film title sits *between* those two
  // anchors inside the event's card — that's our anchor pair for the title.
  private val EventLinkPat = """<a href="(https://bilety\.kinoapollo\.pl/event/view/id/\d+)"[^>]*>\s*<span class="jet-listing-dynamic-link__label">([^<]+)</span>""".r
  // `itemprop="name"` wrapper for the film title.
  // (?s) so `.*?` crosses newlines — the wrapper and the inner content div sit
  // on separate lines.
  private val TitlePat     = """(?s)itemprop="name"[^>]*>.*?jet-listing-dynamic-field__content[^>]*>([^<]+)</div>""".r
  // Day heading content (just the date — the weekday text is in a sibling field).
  private val DatePat      = """jet-listing-dynamic-field__content[^>]*>(\d{1,2}\.\d{1,2}\.\d{4})</div>""".r
  // WordPress media poster URL.
  private val PosterPat    = """https://kinoapollo\.pl/wp-content/uploads/\d{4}/\d{2}/[^\s'"\)]+\.(?:jpg|jpeg|png)""".r
  private val TimeOnlyPat  = """^\d{1,2}:\d{2}$""".r
  // Per-event detail-page anchor — the "Czytaj opis" button next to each
  // screening. Excludes feed / page archive URLs which use the same /kino/
  // prefix but don't correspond to a film. Slug never contains a slash.
  private val DetailLinkPat = """<a href="(https://kinoapollo\.pl/kino/[^"/]+/)"[^>]*>\s*<span class="jet-listing-dynamic-link__label">Czytaj opis</span>""".r

  // ── Detail-page patterns ──────────────────────────────────────────────────
  //
  // Two runtime formats: modern films use `Czas trwania: NN min.`; Wajda
  // cycle uses `Czas trwania: /running time: NN'` (apostrophe / U+2019 minute
  // mark instead of "min"). Both share the same span prefix.
  private val RuntimePat   = """Czas trwania:\s*</span>[^0-9]*(\d+)\s*(?:min|['’])""".r
  // Apollo's `Producent:` field carries two different things depending on
  // page layout:
  //   - Modern films (Drzewo Magii): `Producent:</span> Francja, USA, … | 2026`
  //   - Wajda cycle: `Producent:</span> Zespół Autorów Filmowych „Kadr"`
  // The second form is a *studio*, not countries. Resolve by splitting on
  // "," and only accepting the line as countries when every part is in
  // `CountryNames.Polish`.
  private val ProducentPat = """Producent:\s*</span>\s*([^<|]+)""".r
  // Director — only the modern-film layout exposes a structured `Reżyseria:`
  // line; Wajda cycle pages don't carry per-film director info (it's the
  // cycle's namesake). Anchor on `Reżyseria:` followed by an `<a>` wrapper.
  private val DirectorPat  = """Reżyseria:\s*<a[^>]*>([^<]+)""".r
  // Cast — two layouts:
  //   - Modern films:  `Obsada: <a>name1, name2, …</a>`
  //   - Wajda cycle:   `<strong>obsada/cast:</strong> name1, name2, …`
  // Both end at the first subsequent `<` (closing tag, line break, …).
  private val CastPat      = """(?:>\s*Obsada:\s*<a[^>]*>|<strong>obsada/cast:</strong>\s*)([^<]+)""".r
  // YouTube trailer URL — Apollo's video widget embeds the trailer via an
  // Elementor button block whose `data-settings="..."` attribute contains
  // a JSON object with `youtube_url`. The whole attribute is HTML-escaped
  // (`&quot;` for `"`) and the URL's slashes are JSON-escaped (`\/`).
  // Captured form: `youtube_url&quot;:&quot;http:\/\/youtube.com\/...&quot;`.
  private val YouTubeUrlPat = """youtube_url&quot;:&quot;([^"&]+)&quot;""".r

  def fetch(): Seq[CinemaMovie] = {
    val movies = parseHtml(http.get(PageUrl))
    val urls   = movies.flatMap(_.filmUrl).distinct
    if (urls.isEmpty) movies
    else {
      val metas = fetchDetails(urls)
      movies.map { m =>
        val meta = m.filmUrl.flatMap(metas.get).getOrElse(EmptyDetailMeta)
        m.copy(
          movie      = m.movie.copy(runtimeMinutes = meta.runtime, countries = meta.countries),
          synopsis   = meta.synopsis.orElse(m.synopsis),
          director   = meta.director.orElse(m.director),
          cast       = meta.cast.orElse(m.cast),
          trailerUrl = meta.trailerUrl.orElse(m.trailerUrl)
        )
      }
    }
  }

  case class DetailMeta(
    runtime:    Option[Int],
    synopsis:   Option[String],
    director:   Option[String],
    cast:       Option[String],
    countries:  Seq[String],
    trailerUrl: Option[String]
  )

  private val EmptyDetailMeta = DetailMeta(None, None, None, None, Seq.empty, None)

  private def fetchDetails(urls: Seq[String]): Map[String, DetailMeta] = {
    val ec = DaemonExecutors.virtualThreadEC("kino-apollo-details")
    try {
      val futures = urls.map(url => Future(url -> fetchDetail(url))(ec))
      Await.result(Future.sequence(futures)(implicitly, ec), 1.minute).toMap
    } finally ec.shutdown()
  }

  private def fetchDetail(detailUrl: String): DetailMeta =
    Try(http.get(detailUrl)).toOption.map(parseDetail).getOrElse(EmptyDetailMeta)

  def parseDetail(html: String): DetailMeta = {
    val doc = Jsoup.parse(html)
    // The page's own metadata sits inside its first text-editor widget. Apollo
    // detail pages also render "related screenings" listings further down via
    // a `jet-listing-grid` widget — those repeat every other current film's
    // metadata, including `Reżyseria: …` and `Obsada: …` for unrelated films.
    // Scoping director / cast / synopsis extraction to the page's first
    // text-editor body keeps that bleed out of the result. (Runtime is in a
    // sibling heading widget, not the text-editor — scan the full page for
    // that one.)
    val firstEditorHtml = Option(doc.select("div.elementor-widget-text-editor div.elementor-widget-container").first())
      .map(_.html()).getOrElse("")
    DetailMeta(
      runtime    = parseRuntime(html),
      synopsis   = parseSynopsis(html),
      director   = DirectorPat.findFirstMatchIn(firstEditorHtml).map(_.group(1).trim).filter(_.nonEmpty),
      cast       = CastPat.findFirstMatchIn(firstEditorHtml).map(_.group(1).trim.stripSuffix(",").trim).filter(_.nonEmpty),
      countries  = parseCountries(html),
      trailerUrl = parseTrailer(html)
    )
  }

  /** YouTube trailer URL from the Elementor button block. Returns the
   *  canonical `youtube.com/watch?v=ID` form when the captured URL parses
   *  as a YouTube video; otherwise None. */
  def parseTrailer(html: String): Option[String] =
    YouTubeUrlPat.findFirstMatchIn(html)
      .map(_.group(1).replace("\\/", "/"))
      .flatMap(u => services.movies.TrailerEmbed.youTubeId(u)
        .map(id => s"https://www.youtube.com/watch?v=$id"))

  // Producent: in the page header (a sibling heading widget — not inside the
  // first text-editor). Split on `,` and only commit to the values as
  // countries when every part is a recognised Polish country name; otherwise
  // the line is a studio ("Zespół Autorów Filmowych „Kadr"") and we leave
  // countries empty.
  def parseCountries(html: String): Seq[String] =
    ProducentPat.findFirstMatchIn(html).map { m =>
      m.group(1).trim.split(",").map(_.trim).filter(_.nonEmpty).toSeq
    }.filter(_.forall(CountryNames.isPolish)).getOrElse(Seq.empty)

  def parseRuntime(html: String): Option[Int] =
    RuntimePat.findFirstMatchIn(html).flatMap(m => Try(m.group(1).toInt).toOption)
      .filter(n => n >= 30 && n <= 300)

  // Take the page's first text-editor widget body (where Apollo puts the
  // synopsis), pull its paragraphs in order, and drop any paragraph that
  // contains one of the structured-metadata markers — those land in
  // `director` / `cast` instead. The remaining paragraphs are joined with a
  // blank-line separator so the original paragraph boundaries survive the
  // formatter on the homepage card.
  private def parseSynopsis(html: String): Option[String] = {
    val doc = Jsoup.parse(html)
    val firstEditor = doc.select("div.elementor-widget-text-editor div.elementor-widget-container").first()
    if (firstEditor == null) None
    else {
      val paragraphs = firstEditor.select("p").iterator().asScala
        .map(_.text().trim)
        .filter(_.nonEmpty)
        .filterNot(isMetadataParagraph)
        .toList
      Some(paragraphs.mkString("\n\n")).filter(_.nonEmpty)
    }
  }

  // Markers that identify a "metadata" paragraph — the structured credits
  // block (modern-film layout) or the bilingual scenariusz/obsada block
  // (Wajda cycle layout). Those land in `director` / `cast` via their own
  // regexes; including them in `synopsis` would duplicate the values and
  // pollute the body text.
  private val MetadataMarkers = Seq(
    "Reżyseria:", "Obsada:", "Oryginalny tytuł:",
    "scenariusz/written by:", "obsada/cast:"
  )

  private def isMetadataParagraph(text: String): Boolean =
    MetadataMarkers.exists(text.contains)

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val links   = EventLinkPat.findAllMatchIn(html)
      .map(m => LinkOccurrence(m.start, m.group(1), m.group(2))).toList
    val titles  = TitlePat.findAllMatchIn(html).map(m => (m.start, m.group(1).trim)).toList
    val dates   = DatePat.findAllMatchIn(html).map(m => (m.start, m.group(1))).toList
    val details = DetailLinkPat.findAllMatchIn(html).map(m => (m.start, m.group(1))).toList

    // Each event renders four anchors on the page (desktop time + desktop "Kup
    // bilet" + same pair for mobile). Group by URL, take the first pair: the
    // title that falls between the time anchor and its "Kup bilet" sibling
    // belongs to that event. The "Czytaj opis" detail link sits just past the
    // "Kup bilet" button inside the same card.
    val events = links.groupBy(_.url).toSeq.flatMap { case (url, occurrences) =>
      val sorted = occurrences.sortBy(_.offset)
      for {
        timeOcc <- sorted.find(o => TimeOnlyPat.matches(o.label))
        kupOcc  <- sorted.find(o => o.offset > timeOcc.offset && !TimeOnlyPat.matches(o.label))
        date    <- dates.takeWhile { case (o, _) => o <= timeOcc.offset }.lastOption.map(_._2)
        title   <- titles.find { case (o, _) => o > timeOcc.offset && o < kupOcc.offset }.map(_._2)
        dt      <- Try(LocalDateTime.of(LocalDate.parse(date, DateFmt), LocalTime.parse(timeOcc.label))).toOption
      } yield {
        val cardEnd = sorted.lastOption.map(_.offset + 200).getOrElse(kupOcc.offset)
        val poster  = pickPoster(PosterPat.findAllIn(html.substring(timeOcc.offset, cardEnd)).toSeq)
        val detail  = details.find { case (o, _) => o > kupOcc.offset }.map(_._2)
        ScreeningRow(url, dt, cleanTitle(title), poster, detail)
      }
    }

    events
      .groupBy(_.title)
      .toSeq
      .map { case (title, rows) =>
        val sorted = rows.sortBy(_.dateTime)
        CinemaMovie(
          movie     = Movie(title),
          cinema    = KinoApollo,
          posterUrl = sorted.flatMap(_.posterUrl).headOption,
          filmUrl   = pickFilmUrl(sorted.flatMap(_.detailUrl)),
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = sorted.map(r => Showtime(r.dateTime, Some(r.bookingUrl)))
        )
      }
      .sortBy(_.movie.title)
  }

  // Detail-URL slugs for the decorated variants (pre-premiere /
  // children's-day) point at separate WordPress pages from the canonical
  // film. After `cleanTitle` merges them into one Movie, prefer the
  // detail URL that ISN'T one of these variants so `filmUrl` stays the
  // canonical slug. Mirrors the title-cleanup tokens above.
  private val NonCanonicalSlugTokens = Seq("-seans-przedpremierowy", "dzien-dziecka-w-apollo-")

  private def pickFilmUrl(candidates: Seq[String]): Option[String] =
    candidates.find(url => !NonCanonicalSlugTokens.exists(url.contains))
      .orElse(candidates.headOption)

  // Strip event/promo decoration so the same film with a "seans przedpremierowy"
  // (pre-premiere) screening or a "DZIEŃ DZIECKA W APOLLO - ..." (children's-day
  // banner) screening collapses into the same Movie as the regular run. Without
  // this, those decorated rows can't be enriched — TMDB's title search returns
  // nothing for the decorated string and the row stays at tmdbId=None.
  def cleanTitle(title: String): String =
    title
      .stripPrefix("DZIEŃ DZIECKA W APOLLO - ")
      .stripSuffix(" - seans przedpremierowy")

  // WordPress generates many size variants for each poster (e.g.
  // `..._plakat-200x300.jpg`, `..._plakat-683x1024.jpg`, `..._plakat-scaled.jpg`,
  // `..._plakat.jpg`). Pick the largest by height: scaled/no-suffix beat sized,
  // sized are ranked by height number, anything else gets 0.
  private val SizedVariant = """-(\d+)x(\d+)\.[A-Za-z]+$""".r
  private val FullVariant  = """(?:-scaled)?\.[A-Za-z]+$""".r

  private def pickPoster(urls: Seq[String]): Option[String] =
    if (urls.isEmpty) None
    else Some(urls.distinct.maxBy(posterRank))

  private def posterRank(url: String): Int =
    SizedVariant.findFirstMatchIn(url).map(_.group(2).toInt).getOrElse {
      if (url.contains("-scaled.")) 99999  // "-scaled" is the original full-size
      else if (FullVariant.findFirstMatchIn(url).isDefined) 99998  // unsuffixed original
      else 0
    }

  private case class LinkOccurrence(offset: Int, url: String, label: String)

  private case class ScreeningRow(
    bookingUrl: String,
    dateTime:   LocalDateTime,
    title:      String,
    posterUrl:  Option[String],
    detailUrl:  Option[String]
  )
}
