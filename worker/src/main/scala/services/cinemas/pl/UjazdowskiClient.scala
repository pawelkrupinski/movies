package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScrapeHorizon}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino U-jazdowski (CSW Zamek Ujazdowski, Warszawa). The repertoire page lists
 * the day timestamps (`ut`, midnight epoch seconds) in its nav; each day's
 * `week.ajax?ut=N` returns that day's screening cards (title, time, a
 * director/country/year/runtime meta line, poster) linking to the film page.
 * The per-film page adds the synopsis. The date comes from the `ut`, so the
 * replay is deterministic. Booking is the film page (no stable deep-link).
 *
 * The nav only enumerates a short rolling window (it stopped one day after
 * "today" while `week.ajax` served far more), so advance days were silently
 * dropped. We union the nav `ut`s with a walk forward from today that follows
 * the programme for as long as it lasts — measured 2026-08-05, `week.ajax` was
 * still returning screenings 25 days out, four times the week we used to ask
 * for. See [[ScrapeHorizon.liveDays]].
 */
class UjazdowskiClient(
  http:  HttpFetch,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with DetailEnricher {


  val cinema: Cinema = Ujazdowski
  override val detailGroup: String = "ujazdowski"

  private val BaseUrl    = "https://u-jazdowski.pl"
  private val ListingUrl = s"$BaseUrl/kino/repertuar"
  private val UtPat      = """ut=(\d+)""".r
  private val SlugPat    = """/kino/repertuar/([a-z0-9-]+)""".r
  private val WarsawZone = ZoneId.of("Europe/Warsaw")

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, meta: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = fetchBare()

  private def fetchBare(): Seq[CinemaMovie] = {
    val main   = http.get(ListingUrl)
    val navUts = UtPat.findAllMatchIn(main).map(_.group(1)).toSeq

    // Each day is read once, whether it arrived from the nav or from the walk
    // below — the walk has to parse a day to know whether the programme goes on,
    // so caching here is what keeps that from costing a second fetch.
    val byUt = scala.collection.mutable.LinkedHashMap.empty[String, Seq[RawSlot]]
    def slotsFor(ut: String): Seq[RawSlot] = byUt.getOrElseUpdate(ut,
      (for {
        date <- Try(Instant.ofEpochSecond(ut.toLong).atZone(WarsawZone).toLocalDate).toOption
        html <- Try(http.get(s"$ListingUrl/week.ajax?ut=$ut")).toOption
      } yield parseDay(html, date)).getOrElse(Seq.empty))

    navUts.foreach(slotsFor)
    // `ut` is the day's midnight-Warsaw epoch (DST-aware). Walk forward from
    // today for as long as the programme runs; a missing day 404s → no slots,
    // and enough of those in a row ends the walk.
    ScrapeHorizon.liveDays(today) { day =>
      slotsFor(day.atStartOfDay(WarsawZone).toEpochSecond.toString).nonEmpty
    }
    val slots = byUt.values.toSeq.flatten

    slots.groupBy(_.slug).toSeq.flatMap { case (slug, group) =>
      val primary    = group.head
      val detailUrl  = s"$BaseUrl/kino/repertuar/$slug"
      val showtimes  = group.map(s => Showtime(s.dateTime, Some(detailUrl), None, Nil))
                         .distinctBy(_.dateTime).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val meta = UjazdowskiClient.parseMeta(primary.meta.getOrElse(""))
        Some(CinemaMovie(
          movie     = Movie(title = primary.title, runtimeMinutes = meta.runtime, releaseYear = meta.year,
                            countries = meta.countries),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = Some(detailUrl),
          synopsis  = None,
          cast      = Seq.empty,
          director  = meta.director,
          showtimes = showtimes
        ))
      }
    }
  }

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's film-page URL. Only the synopsis + bracketed original title come
   *  from the detail page (the meta line on the listing supplies everything
   *  else). None on fetch failure so the task stays stale and is retried.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(Jsoup.parse).map { document =>
      FilmDetail(
        // Some descriptions embed a source/related link as plain-text URL; strip
        // it so the synopsis stays prose-only. cleanSynopsis also keeps the
        // <p>/<br> paragraph structure (ScraperParse.blockText).
        synopsis      = Option(document.selectFirst("div.body.max-w")).map(ScraperParse.cleanSynopsis(_)).filter(_.length > 20),
        originalTitle = UjazdowskiClient.originalTitleOf(document)
      )
    }

  private def parseDay(html: String, date: java.time.LocalDate): Seq[RawSlot] =
    Jsoup.parse(html).select("a.event-list-day-box").asScala.toSeq.flatMap { a =>
      val slug  = SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1))
      val title = Option(a.selectFirst(".title em")).orElse(Option(a.selectFirst(".title"))).map(_.text.trim).filter(_.nonEmpty)
      val time  = Option(a.selectFirst(".hours")).map(_.text.trim)
                    .flatMap(ScraperParse.parseHHmm)
      for { s <- slug; t <- title; tm <- time } yield {
        val meta   = Option(a.selectFirst(".fs-20.max-w")).map(_.text.trim).filter(_.nonEmpty)
        val poster = Option(a.selectFirst("img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                       .map(u => if (u.startsWith("http")) u else BaseUrl + u)
        RawSlot(s, t, date.atTime(tm), meta, poster)
      }
    }
}

object UjazdowskiClient {

  // "[Original Title], reż. Director, Country1/ Country2 YEAR, RUNTIME'"
  private val MetaPat = """reż\.\s*(.+?),\s*(.+?)\s+((?:19|20)\d{2}),\s*(\d+)['’]""".r
  // The film page's header renders that meta with the original title bracketed
  // (`<i class="finterp">[</i><em>…</em>…`), so jsoup's text reads "[Orig], …".
  // Polish films omit the bracket, so this is `None` for them.
  private val OrigTitlePat = """^\s*\[(.+?)\]""".r

  /** The bracketed original title from the film-page header meta, e.g.
   *  "[Da hong deng long gao gao gua], reż. …" → "Da hong deng long gao gao gua". */
  def originalTitleOf(document: org.jsoup.nodes.Document): Option[String] =
    Option(document.selectFirst("div.event-content-header div.fs-20.max-w")).map(_.text.trim)
      .flatMap(s => OrigTitlePat.findFirstMatchIn(s).map(_.group(1).trim)).filter(_.nonEmpty)

  final case class Meta(director: Seq[String], countries: Seq[String], year: Option[Int], runtime: Option[Int])

  def parseMeta(s: String): Meta =
    MetaPat.findFirstMatchIn(s) match {
      case Some(m) => Meta(
        director  = m.group(1).split(",").map(_.trim).filter(_.nonEmpty).toSeq,
        countries = m.group(2).split("[,/]").map(_.trim).filter(_.nonEmpty).toSeq,
        year      = Some(m.group(3).toInt),
        runtime   = Some(m.group(4).toInt)
      )
      case None => Meta(Seq.empty, Seq.empty, None, None)
    }
}
