package services.cinemas.common

import tools.PersonName
import models._
import org.jsoup.parser.Parser
import org.jsoup.nodes.Element

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._

/** One day's sessions fragment of a Flicks venue (`<market base>/cinema/sessions/<slug>/<YYYY-MM-DD>/`, see
 *  [[FlicksClient]]) read into its session slots: a `cinema-times__article` card per film, its sessions under it. */
object FlicksDayPage extends SlimmedPage {

  private[common] val SlugPat    = """/movie/([^/?#]+)""".r
  private val DigitsPat  = """(\d+)""".r
  private val OptTimePat = raw"""${ScraperParse.ClockParts}:\d{2}""".r
  private val AmPmPat    = raw"""(?i)${ScraperParse.ClockParts}\s*(am|pm)""".r
  // Keys lifted from a session button's `data-eventjson` blob (jsoup returns it
  // entity-decoded, so we match against real quotes). `content_cast`,
  // `content_genre` and `content_director` are comma-separated lists; `content_awards` has no model
  // home and is ignored. The age rating (`content_rating` in the blob) is read
  // instead off the film card's `.cinema__movie-classification` element, where
  // it renders cleanly as the BBFC label.
  private val ContentId    = """"content_id"\s*:\s*"(\d+)"""".r
  private val ContentCast  = """"content_cast"\s*:\s*"([^"]*)"""".r
  private val ContentGenre = """"content_genre"\s*:\s*"([^"]*)"""".r
  private val ContentDirector = """"content_director"\s*:\s*"([^"]*)"""".r

  /** A film's directors: EVERY name the blob's comma-separated `content_director` lists, in its
   *  order — the card's `.cinema__director span` renders only the first ("A Night at the Opera":
   *  card "Edmund Goulding", blob "edmund goulding,sam wood"), and matching on that uncredited first
   *  name alone picked Goulding's "The Old Maid". The blob lowercases names, so a name the card
   *  renders keeps the card's own spelling and the rest are capitalised; a card with no blob keeps
   *  its one name. */
  private[common] def directorsOf(card: Option[String], blob: Seq[String]): Seq[String] =
    if (blob.isEmpty) card.toSeq
    else blob.map(name => card.filter(_.equalsIgnoreCase(name)).getOrElse(PersonName.capitalized(name))).distinct

  /** Split one of the comma-separated `data-eventjson` list values into trimmed,
   *  non-blank entries (`"a, b ,"` → `List("a", "b")`). */
  private def commaList(value: String): List[String] =
    value.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList

  /** One session slot off the fragment: the film's card (its slug, a stable id, title and metadata, constant across
   *  a film's sessions) and the single screening. */
  final case class RawFlicksSlot(card: FilmCard, dateTime: LocalDateTime, booking: Option[String], format: List[String]) {
    export card.*
  }

  /** Parse one day's sessions fragment for the given calendar date. Pure +
   *  public so the spec feeds it the recorded HTML directly. */
  def parseDay(html: String, date: LocalDate, market: FlicksMarket): Seq[RawFlicksSlot] =
    parseDocument(Parser.htmlParser().parseInput(slimmedReader(html), market.baseUrl), date, market)

  /** [[parseDay]] over an already-parsed fragment — the spec parses the raw page with it too. */
  private[common] def parseDocument(doc: org.jsoup.nodes.Document, date: LocalDate, market: FlicksMarket): Seq[RawFlicksSlot] =
    doc.select("article.cinema-times__article").asScala.toSeq.flatMap { article =>
      filmCardOf(article, market).toSeq.flatMap(card => sessionsOf(article, card, date))
    }

  /** What a film card states once for all its sessions. */
  final case class FilmCard(slug: String, title: String, runtimeMinutes: Option[Int], posterUrl: Option[String],
                            directors: Seq[String], contentId: Option[String], cast: Seq[String],
                            genres: Seq[String], trailerUrl: Option[String], ageRating: Option[String])

  private def filmCardOf(article: Element, market: FlicksMarket): Option[FilmCard] =
    for {
      slug  <- firstMovieSlug(article)
      title <- Option(article.selectFirst("h3.cinema-times__movie-title")).map(_.text.trim).filter(_.nonEmpty)
    } yield {
      // Every session button in a film's card carries the same `data-eventjson`
      // blob; read the first non-empty one once and lift id/cast/genre/director from it.
      val eventJson = article.select(".times-calendar-times__button").asScala.iterator
        .map(_.attr("data-eventjson")).find(_.nonEmpty).getOrElse("")
      def blobList(key: scala.util.matching.Regex): List[String] =
        key.findFirstMatchIn(eventJson).map(_.group(1)).map(commaList).getOrElse(Nil)
      FilmCard(
        slug, title,
        runtimeMinutes = Option(article.selectFirst(".cinema__movie-duration"))
          .map(_.text).flatMap(s => DigitsPat.findFirstIn(s)).map(_.toInt).filter(_ > 0),
        // Flicks shows a placeholder for a film it has no poster for, under a URL whose `?t=` changes
        // on every request: kept, every scrape rewrote the slot for nothing. It is no poster.
        posterUrl = Option(article.selectFirst(".cinema-times__image img")).map(_.attr("src"))
          .filter(src => src.nonEmpty && !src.contains(PlaceholderPoster)),
        directors = directorsOf(
          Option(article.selectFirst(".cinema__director span")).map(_.text.trim).filter(_.nonEmpty), blobList(ContentDirector)),
        contentId = ContentId.findFirstMatchIn(eventJson).map(_.group(1)),
        // Flicks lowercases every cast name in the blob ("christoph waltz"), so
        // the list is capitalised HERE rather than left for the write boundary:
        // this client's output is also what the scrape archive and the fixture
        // harness record, and a name stored lowercase renders lowercase.
        cast      = PersonName.capitalizedAll(blobList(ContentCast)),
        genres    = blobList(ContentGenre),
        trailerUrl = Option(article.selectFirst(""".cinema__trailer-wrap a[href^="/trailer/"]"""))
          .map(_.attr("href")).filter(_.nonEmpty)
          .map(h => if (h.startsWith("http")) h else s"${market.baseUrl}$h"),
        // The BBFC label ("U"/"PG"/"12A") the card renders in its own element.
        ageRating = Option(article.selectFirst(".cinema__movie-classification")).map(_.text).flatMap(AgeRating.normalize)
      )
    }

  private def sessionsOf(article: Element, card: FilmCard, date: LocalDate): Seq[RawFlicksSlot] =
    article.select("li.times-calendar-times__el").asScala.toSeq.flatMap { li =>
      // Tag-agnostic: a bookable session is an `<a …>`, an unbookable one the
      // same button as a `<span>`. Keying on the `<a>` silently dropped every
      // showtime at a venue with no booking deep-links (see the spec).
      val button = Option(li.selectFirst(".times-calendar-times__button"))
      button.flatMap(parseTime).map { time =>
        // A `<span>` button has no href, so this stays None — an unbookable
        // screening is still a screening.
        val booking = button.map(_.attr("href")).filter(_.nonEmpty)
        val label   = Option(li.selectFirst("span.times-calendar-times__el__label span")).map(_.text.trim).filter(_.nonEmpty)
        RawFlicksSlot(card, LocalDateTime.of(date, time), booking, label.toList)
      }
    }

  /** The ranges of `html` [[parseDay]] reads, in order, flattened: without every `data-eventjson` of a film card after
   *  its first non-empty one (each session button carries the same blob, entity-escaped — a third of a busy venue's
   *  day), and inline `<svg>` icons (a fifth). Parsing was 7% of a US worker's CPU (JFR, 2026-10-01). */
  protected def keptRanges(html: String): Array[Int] = new KeptRanges(html).result()

  /** One pass over `html` for [[keptRanges]]: the next `<article`, `<svg` and `data-eventjson` ahead, and what is kept
   *  up to the nearest. */
  private final class KeptRanges(html: String) {
    private val ranges        = Array.newBuilder[Int]
    private var at            = 0
    private var keptEventJson = false
    private var article       = next(ArticleOpen)
    private var svg           = next(SvgOpen)
    private var eventJson     = next(EventJsonAttr)

    def result(): Array[Int] = {
      while (at < html.length) step(math.min(article, math.min(svg, eventJson)))
      ranges.result()
    }

    private def next(token: String): Int = { val i = html.indexOf(token, at); if (i < 0) Int.MaxValue else i }

    private def keep(from: Int, until: Int): Unit = if (until > from) { ranges += from; ranges += until }

    private def step(first: Int): Unit =
      if (first == Int.MaxValue) { keep(at, html.length); at = html.length }
      else if (first == article) passArticle()
      else if (first == svg) dropSvg()
      else passEventJson()

    // A film card opens: its first blob is kept again.
    private def passArticle(): Unit = {
      keep(at, article + ArticleOpen.length); at = article + ArticleOpen.length
      keptEventJson = false
      article = next(ArticleOpen)
    }

    private def dropSvg(): Unit = {
      val close = html.indexOf(SvgClose, svg)
      keep(at, if (close < 0) html.length else svg)
      at = if (close < 0) html.length else close + SvgClose.length
      // Re-found only when the icon swallowed them: a scan from here to a far `<article` per
      // icon was most of this pass's cost.
      if (article < at) article = next(ArticleOpen)
      if (eventJson < at) eventJson = next(EventJsonAttr)
      svg = next(SvgOpen)
    }

    // A card's first non-empty blob is kept; every later one is cut.
    private def passEventJson(): Unit = {
      val valueStart = eventJson + EventJsonAttr.length
      val valueEnd   = html.indexOf('"', valueStart)
      val end        = if (valueEnd < 0) html.length else valueEnd + 1
      if (keptEventJson) keep(at, eventJson)
      else { keep(at, end); keptEventJson = valueEnd > valueStart }
      at = end
      eventJson = next(EventJsonAttr)
    }
  }

  /** What [[FlicksClient.parseChunkPage]] makes of a day page — pinned against the recorded pages by
   *  `FlicksPageParserVersionSpec`, which fails when the parse changes and this does not. */
  val PageParserVersion = 2

  /** The code a day page's parse runs through beyond this file — the encoder of its slice, the name
   *  helper, the models it builds — whose change the recorded pages may not exercise. */
  private[common] val ParseClasses: Seq[Class[?]] = Seq(classOf[FlicksClient], FlicksClient.getClass, getClass, CinemaMovieJson.getClass,
    PersonName.getClass, classOf[CinemaMovie], classOf[Movie], classOf[Showtime], classOf[org.jsoup.parser.Parser])

  /** The memo's name for this parse: the hand version, and a fingerprint of the classes it runs through,
   *  so a change to any of them re-parses every remembered page whether or not the version was bumped. */
  lazy val PageParser: String = s"$PageParserVersion:${tools.Digest.classesHex(ParseClasses)}"

  /** The path of the image Flicks shows for a film it has no poster for. */
  private[common] val PlaceholderPoster = "/images/others/not_available/"
  private val ArticleOpen   = "<article"
  private val SvgOpen       = "<svg"
  private val SvgClose      = "</svg>"
  private val EventJsonAttr = " data-eventjson=\""

  private def firstMovieSlug(article: Element): Option[String] =
    article.select("""a[href*="/movie/"]""").asScala.iterator
      .flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1))).nextOption()

  /** A session's start time: the 24h time trailing `data-optlabel`
   *  ("Odeon Cinema Norwich-10:10:00"), which some sessions omit — then the
   *  visible "10:10 am" text. */
  private def parseTime(button: Element): Option[LocalTime] = {
    val fromOptLabel = OptTimePat.findAllMatchIn(button.attr("data-optlabel")).toSeq.lastOption
      .flatMap(m => ScraperParse.clockAt(m, 1))
    fromOptLabel.orElse {
      val text = Option(button.selectFirst("span.times-calendar-times__el__time")).map(_.text).getOrElse("")
      parseAmPm(text)
    }
  }

  /** "10:10 am" / "7:05 pm" → `LocalTime`. */
  private def parseAmPm(text: String): Option[LocalTime] =
    AmPmPat.findFirstMatchIn(text).flatMap(ScraperParse.meridiemClockAt(_, 1, 3))
}
