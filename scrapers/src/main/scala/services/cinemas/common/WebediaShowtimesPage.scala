package services.cinemas.common

import java.time.LocalDateTime
import java.util.Locale

import scala.util.Try

import models.*
import play.api.libs.json.*
import services.cinemas.common.ScraperParse.jsonText

/** One `/_/showtimes/theater-<id>/d-<date>/p-<n>/` JSON page of a Webedia site ([[WebediaShowtimesClient]]) read
 *  into its films, their showtimes and each showtime's format tokens. */
object WebediaShowtimesPage {

  /** One parsed page: its films plus the response's total page count (so the
   *  caller knows whether to fetch p-2…). Pure so a spec can feed fixture bytes. */
  case class Page(films: Seq[RawWebediaFilm], totalPages: Int)

  /** The fields this client needs off one `results[]` element. */
  case class RawWebediaFilm(
    internalId:     Long,
    title:          String,
    originalTitle:  Option[String],
    year:           Option[Int],
    runtimeMinutes: Option[Int],
    genres:         Seq[String],
    director:       Seq[String],
    posterUrl:      Option[String],
    synopsis:       Option[String],
    ageRating:      Option[String],
    showtimes:      Seq[Showtime]
  )

  /** Parse one `/_/showtimes/theater-<id>/d-<date>/p-<n>/` response. Pure +
   *  public so the spec feeds it the recorded JSON directly. Takes the market
   *  because two things inside a result are language-shaped rather than
   *  structural: the `runtime` string's unit words and the version tokens. */
  def parsePage(json: String, market: WebediaMarket): Page = {
    // A body that is not JSON throws: as JsNull it read as a day with no films.
    val js = Json.parse(json)
    val totalPages = (js \ "pagination" \ "totalPages").asOpt[Int].getOrElse(1)
    val films = (js \ "results").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(parseResult(_, market))
    Page(films, totalPages)
  }

  private def parseResult(js: JsValue, market: WebediaMarket): Option[RawWebediaFilm] = {
    val movie = js \ "movie"
    for {
      id    <- (movie \ "internalId").asOpt[Long]
      title <- jsonText(movie \ "title")
    } yield RawWebediaFilm(
      internalId     = id,
      title          = title,
      originalTitle  = jsonText(movie \ "originalTitle"),
      year           = (movie \ "data" \ "productionYear").asOpt[Int],
      runtimeMinutes = (movie \ "runtime").asOpt[String].flatMap(parseRuntime(_, market)),
      genres         = (movie \ "genres").asOpt[Seq[JsValue]].getOrElse(Nil)
        .flatMap(g => jsonText(g \ "translate")),
      director       = parseDirectors(movie \ "credits"),
      posterUrl      = jsonText(movie \ "poster" \ "url"),
      // `synopsisFull` is HTML (`<p class="bo-p">…</p>` per paragraph), not prose —
      // flatten it, keeping the paragraph breaks, or the tags render as visible text.
      synopsis       = (movie \ "synopsisFull").asOpt[String]
        .map(tools.TextNormalization.stripHtmlKeepingParagraphs).filter(_.nonEmpty),
      // German FSK certificate off the first `releases[]` element that carries one
      // (a film's earliest releases can lack a certificate while a later one has
      // it). Kept verbatim — German "FSK 6", Spanish "+12" — because the local
      // spelling IS the recognisable label; `normalize` only drops blank/placeholder codes.
      ageRating      = AgeRating.normalize(
        (movie \ "releases").asOpt[Seq[JsValue]].getOrElse(Nil)
          .flatMap(r => (r \ "certificate" \ "code").asOpt[String]).headOption
          .map(market.certificateLabel)),
      showtimes      = parseShowtimes(js \ "showtimes", market)
    )
  }

  /** Director names off `credits[]` — every credit whose `position.name` is
   *  DIRECTOR, "First Last" (either part may be absent). */
  private def parseDirectors(credits: JsLookupResult): Seq[String] =
    credits.asOpt[Seq[JsValue]].getOrElse(Nil).collect {
      case c if (c \ "position" \ "name").asOpt[String].contains("DIRECTOR") =>
        val first = (c \ "person" \ "firstName").asOpt[String].getOrElse("")
        val last  = (c \ "person" \ "lastName").asOpt[String].getOrElse("")
        s"$first $last".trim
    }.filter(_.nonEmpty).distinct

  /** Flatten the version-bucketed `showtimes` object into a flat screening list.
   *  Each bucket (`original`/`dubbed`/`local`, each optionally `_st`/`_sme`) is
   *  an array; the bucket KEY plus the screening's tags become
   *  [[Showtime.format]] tokens. The key is carried into `formatTokens` because
   *  it is the only reliable statement of the language version: the majority of
   *  dubbed screenings carry no `Localization.*` tag at all (probed 2026-09-02
   *  over 120 venues per market: 337 of 850 slots in Spain, 723 of 914 in
   *  Germany), so a tags-only reading can name the original versions but never
   *  the dubbed ones. */
  private def parseShowtimes(js: JsLookupResult, market: WebediaMarket): Seq[Showtime] =
    js.asOpt[JsObject].map(_.fields.toSeq).getOrElse(Seq.empty).flatMap { case (bucketKey, bucket) =>
      bucket.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty).flatMap { s =>
        (s \ "startsAt").asOpt[String].flatMap(parseLocalDateTime).map { dt =>
          val booking = (s \ "data" \ "ticketing").asOpt[Seq[JsValue]].getOrElse(Nil)
            .flatMap(t => (t \ "urls").asOpt[Seq[String]].getOrElse(Nil))
            .headOption.map(WebediaBoxOffice.cleanBookingUrl)
          Showtime(dt, booking, None,
            formatTokens(bucketKey, (s \ "tags").asOpt[Seq[String]].getOrElse(Nil), market))
        }
      }
    }

  /** Screen-format tokens, in a fixed order, for the namespaced `tags` a
   *  screening carries — the clean vocabulary, rather than the noisier
   *  `projection`/`experience`/`sound` fields that spell 3D as `F_3D` and leave
   *  the language version encoded only in the bucket.
   *
   *  The needles are matched as SUBSTRINGS of a lower-cased tag, which is what
   *  lets one `format.projection.4de` row cover both `4DE` and `4DE3D`. The two
   *  BASELINE tags every screening carries — `Format.Projection.Digital` and
   *  `Format.Sound.DolbyDigital` — are deliberately absent: a token every slot
   *  in the country shares tells a visitor nothing.
   *
   *  Probed 2026-09-02 over 120 venues per market; the list is the union of
   *  what both sites actually emit (Spain: Imax/Laser/4k/3d/4DE/4DE3D,
   *  DolbyAtmos, VIP, InfinityVision (Ocine Girona, 2026-10-04) — Germany: 2D/3d/4k, DBox/DolbyAtmos/PLF, Premium), plus
   *  the 4DX and Dolby projection rows the family has always spelled this way. */
  private val ScreenTokens: List[(String, String)] = List(
    "format.projection.2d"             -> "2D",
    "format.projection.3d"             -> "3D",
    "format.projection.4de"            -> "4DE",
    "format.projection.4dx"            -> "4DX",
    "format.projection.4k"             -> "4K",
    "format.projection.imax"           -> "IMAX",
    "format.projection.laser"          -> "LASER",
    "format.projection.dolby"          -> "DOLBY",
    "auditorium.experience.dolbyatmos" -> "ATMOS",
    "auditorium.experience.screenx"    -> "SCREENX",
    "auditorium.experience.infinityvision" -> "INFINITY",
    "auditorium.experience.dbox"       -> "DBOX",
    "auditorium.experience.plf"        -> "PLF",
    "showtime.experience.premium"      -> "PREMIUM",
    "showtime.service.vip"             -> "VIP",
  )

  /** Format + language-version tokens for one screening: the non-baseline screen
   *  formats above, then the language token(s) from [[versionTokens]].
   *
   *  Public, and taking the bucket key + raw tag list rather than the screening
   *  object, so a spec can pin a COMBINATION the recorded captures do not hold —
   *  the same reason [[GatsbyBoxOfficeParser.formatTokens]] is public. A single
   *  day's capture of one venue is usually all-dubbed-digital, so most branches
   *  would otherwise ship untested. */
  def formatTokens(bucketKey: String, rawTags: Seq[String], market: WebediaMarket): List[String] = {
    val tags = rawTags.map(_.toLowerCase(Locale.ROOT))
    val screen = ScreenTokens.collect { case (needle, token) if tags.exists(_.contains(needle)) => token }
    (screen ++ versionTokens(bucketKey, tags, market)).distinct
  }

  /** The language-version token a screening earns — one, or none; two only for a print
   *  subtitled in both the market's language and English.
   *
   *  The BUCKET decides which of the three versions this is, not the tags: a
   *  `local` screening is routinely tagged `Localization.Version.Original` (it
   *  is, literally — a Spanish film in Spanish), and reading that tag is what
   *  used to put a `VO` badge on domestic films for whom their own language is
   *  the unmarked default. So `local` yields nothing, and only the tags of an
   *  `original` bucket are consulted — for the SUBTITLE language, which is the
   *  one thing the bucket key doesn't carry. `dubbed` names the audio language
   *  when it isn't the market's own (a Catalan dub in Spain).
   *
   *  Tags are the fallback for an unrecognised bucket only, so a future key the
   *  site adds still resolves rather than silently going unmarked. */
  private def versionTokens(bucketKey: String, tags: Seq[String], market: WebediaMarket): List[String] = {
    val key        = bucketKey.toLowerCase(Locale.ROOT)
    val isLocal    = key.startsWith("local")
    val isDubbed   = key.startsWith("dubbed")
    val isOriginal = key.startsWith("original") ||
      (!isLocal && !isDubbed && tags.exists(_.contains("localization.version.original")))
    if (isLocal)         Nil
    else if (isOriginal) originalVersionTokens(tags.filter(_.contains("localization.subtitle.")), market)
    else if (isDubbed)   List(dubbedToken(tags, market))
    else                 Nil
  }

  private def originalVersionTokens(subtitles: Seq[String], market: WebediaMarket): List[String] = {
    val english = subtitles.exists(_.endsWith("english"))
    // A print subtitled in English AND another language is both versions, and badged as both:
    // the market's own subtitled token is the one its "subtitles" filter reads.
    val other   = subtitles.exists(!_.endsWith("english"))
    (english, other) match {
      case (true, true)   => List(market.subtitledToken, market.englishSubtitledToken)
      case (true, false)  => List(market.englishSubtitledToken)
      case (false, true)  => List(market.subtitledToken)
      case (false, false) => List(market.originalVersionToken)
    }
  }

  private def dubbedToken(tags: Seq[String], market: WebediaMarket): String =
    market.dubbedLanguageTokens.collectFirst {
      case (language, token) if tags.exists(_.contains(s"localization.language.$language")) => token
    }.getOrElse(market.dubbedToken)

  private def parseLocalDateTime(s: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(s.trim)).toOption

  /** German "1 Std. 56 Min." → 116, Spanish "1h 56min" → 116, "56 Min." → 56.
   *  `None` when neither part is present.
   *
   *  The unit words come from the market, and the match is CASE-SENSITIVE on
   *  purpose. Germany's "Min" and Spain's "min" are the same letters, so a
   *  case-insensitive match would let the German market read a Spanish
   *  "1h 56min" as 56 minutes — an hour short, silently, and plausible enough
   *  that nothing downstream would flag it. Case-sensitivity makes a market
   *  applied to the wrong payload produce NOTHING rather than something wrong. */
  private def parseRuntime(s: String, market: WebediaMarket): Option[Int] = {
    def part(marker: String): Int =
      s"(\\d+)\\s*${java.util.regex.Pattern.quote(marker)}".r
        .findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    Some(part(market.hourMarker) * 60 + part(market.minuteMarker)).filter(_ > 0)
  }
}
