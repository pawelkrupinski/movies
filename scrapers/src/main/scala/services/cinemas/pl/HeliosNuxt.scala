package services.cinemas.pl

import services.cinemas.common.SlotsToMovies

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.time.LocalDateTime
import scala.util.Try
import services.titles.TitleNormalizer

/** Per-cinema Helios configuration: which `Cinema` the scrape feeds, the page
 *  URL slug (`<citySlug>/<cinemaSlug>`) whose `/repertuar` carries the NUXT
 *  blob, and the REST `cinemaId` UUID. `baseUrl` is shared by the film /
 *  event page links and the NUXT parse; the booking host is chain-global. */
final case class HeliosCinema(cinema: Cinema, citySlug: String, cinemaSlug: String, sourceId: String) {
  val baseUrl: String = s"https://helios.pl/$citySlug/$cinemaSlug"
  val pageUrl: String = s"$baseUrl/repertuar"
}

// Helios renders its repertoire page as Nuxt 2 hydration: the entire payload is
// an IIFE assigned to `window.__NUXT__`, with shared string/number values
// hoisted to parameter names like `a, b, …, $$`. We pull out the movie list,
// the per-day screenings, and emit `CinemaMovie` rows.
//
// Centralised here so HeliosClient can focus on REST fetching, REST↔NUXT
// reconciliation, and the post-processing pipeline.
object HeliosNuxt {

  val BookingBase = "https://bilety.helios.pl/screen"

  // Strip event/promo suffixes so that "Diabeł ubiera się u Prady 2 - KNT"
  // collapses to the canonical "Diabeł ubiera się u Prady 2".
  //
  // The audio-version + screen-code tail Helios bakes into an event title
  // ("… - dubbing - Event projekt", "Michael - AF") is decoration only — the
  // 2D/3D dimension and DUB/ORG/NAP/LEK version it implies is already parsed
  // into `Showtime.format` from each screening's `release`/`printRelease`, so
  // stripping it from the title loses nothing while letting the row enrich off
  // the clean film name and the dubbing/napisy variants collapse into one row.
  // The event/format suffix peeling now lives in the editable "helios" rules
  // (TitleRules, same order); this delegates so the Nuxt+REST dedup
  // grouping/matching still collapses decorated variants onto the bare film.
  // On top of those rules we peel any trailing screen-format/version tag the
  // rules don't cover ("Babystar - 2D NAP"); the 2D/NAP it carries is already in
  // Showtime.format (see above), so stripping it from the title loses nothing.
  // Anchored on the separator that precedes the tag, so a bare in-title "3D"
  // ("Billie Eilish … The Tour Live in 3D") and trailing whitespace are left
  // alone — only a clearly-delimited format tail is removed. Deliberately NOT the
  // shared extractFormatTags: Helios's varied non-format suffixes ("UROCZYSTA
  // POLSKA PREMIERA") would partial-strip (only the trailing version word drops),
  // leaving a worse title — so a "- 2D/DUB - Pokaz specjalny"-style label after
  // the format is left in place rather than risk that.
  // Collapse the irregular internal whitespace Helios bakes into some raw titles
  // ("Odyseja -  Salon Kultury Helios" — a DOUBLED space after the dash) to a
  // single space FIRST: the "helios" strand/format tags are single-space literals
  // ("- Salon Kultury Helios$"), so a doubled space slipped every one of them and
  // the whole chain served the decorated title as its own row instead of merging
  // onto the bare film. A double space is never meaningful in a film title, so
  // this loses nothing.
  def cleanTitle(title: String, titles: TitleNormalizer): String = {
    val collapsed = InteriorWhitespace.replaceAllIn(title, " ")
    val cleaned   = titles.cinemaClean("helios", collapsed)
    if (FormatTail.findFirstMatchIn(cleaned).isDefined) FormatTail.replaceFirstIn(cleaned, "").trim
    else cleaned
  }

  private val InteriorWhitespace = """ {2,}""".r

  private val FormatTail =
    """(?i)\s+[-–—|/]\s*(?:(?:2D|3D|IMAX|4DX|DUBBING|DUBB|DUB|NAPISY|NAP|LEKTOR|LEK)\b[\s/]*)+$""".r

  def buildMovies(html: String, config: HeliosCinema, titles: TitleNormalizer): Seq[CinemaMovie] = {
    val parsed   = parseNuxtPage(html, config.baseUrl)
    val nuxtRows = parsed.showtimesByMovie.toSeq.flatMap { case (movieId, slots) =>
      parsed.movies.get(movieId).map(movie => movie -> slots)
    }

    def showtimeOf(slot: (LocalDateTime, String, List[String])) = slot match {
      case (dateTime, screeningId, format) =>
        Showtime(
          dateTime   = dateTime,
          bookingUrl = Some(s"$BookingBase/$screeningId?cinemaId=${config.sourceId}").filter(_ => screeningId.nonEmpty),
          room       = None,
          format     = format
        )
    }
    SlotsToMovies.foldBlocks(nuxtRows, { case (m, _) => cleanTitle(m.title, titles) }, _._2.map(showtimeOf),
      distinctBy = SlotsToMovies.byWholeShowtime, order = SlotsToMovies.MovieOrder.asGrouped) { (title, rows, showtimes) =>
        val movies = rows.map(_._1)
        val movie  = movies.find(_.runtimeMinutes.nonEmpty).getOrElse(movies.head)
        // When both a regular film and an event share the same cleaned title (e.g. "Diabeł" + "Diabeł - KNT"),
        // prefer the /filmy/ URL — it points to the canonical movie page rather than a specific event.
        val urls   = movies.flatMap(_.filmUrl)
        // Keep the raw titleOriginal (e.g. "The Pianist (re-release)") as a TMDB
        // resolution hint — downstream de-decoration strips the suffix itself.
        val originalTitle = movies.flatMap(_.originalTitle).map(_.trim).find(_.nonEmpty)
        Some(CinemaMovie(
          movie = Movie(title = title, runtimeMinutes = movie.runtimeMinutes, releaseYear = None,
            rawTitle = movies.map(_.title).headOption, originalTitle = originalTitle),
          cinema    = config.cinema,
          posterUrl = movies.flatMap(_.posterUrl).headOption,
          filmUrl   = urls.find(_.contains("/filmy/")).orElse(urls.headOption),
          showtimes = showtimes
        ))
    }
  }

  // ── NUXT IIFE parser ──────────────────────────────────────────────────────

  private case class NuxtPage(
    movies:           Map[String, NuxtMovie],
    showtimesByMovie: Map[String, Seq[(LocalDateTime, String, List[String])]]
  )

  private case class NuxtMovie(
    title:          String,
    slug:           String,
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    runtimeMinutes: Option[Int],
    originalTitle:  Option[String]
  )

  // Bundles the three things every internal parser needs: the slice of the IIFE
  // body that contains film metadata, the slice that contains screenings, and
  // the resolver that turns a parameter-name token into its string value.
  private case class NuxtContext(
    movieBody:      String,
    screeningsBody: String,
    resolve:        String => Option[String],
    baseUrl:        String
  )

  // Reused patterns. Hoisted so the structural anchors live in one place.
  private val ScreeningsStart   = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r
  private val MovieGroupStart   = """([em]\d+):\{screenings:\[""".r
  private val DayMarker         = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
  private val EmbeddedMovieBlk  = """movie:\{(.*?)\},moviePrint:""".r
  private val EventEntryAnchor  = """,name:([\w$]+),slug:([\w$]+),""".r
  private val ScreeningEntry    = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r
  private val PrintRelease      = """printRelease:([^,}]+)""".r
  // Top-level film: ,id:N,sourceId:X,title:Y,titleOriginal:Z,slug:W in order.
  private val TopLevelMovie     = """,id:(\d{3,}|[\w$]+),sourceId:(?:"[^"]+"|[\w$]+),title:(?:"([^"]+)"|([\w$]+)),titleOriginal:(?:"([^"]*)"|([\w$]+)),slug:(?:"([^"]+)"|([\w$]+))""".r

  private def parseNuxtPage(html: String, baseUrl: String): NuxtPage = {
    val empty = NuxtPage(Map.empty, Map.empty)
    (for {
      iife <- extractIifeBody(html)
      context  <- splitBody(iife._2, makeResolver(iife._1), baseUrl)
    } yield NuxtPage(
      movies           = parseNuxtMovies(context),
      showtimesByMovie = parseNuxtShowtimes(context).groupMap(_._1)(_._2)
    )).getOrElse(empty)
  }

  // Locate the `(function(parameters){body}(values))` IIFE and return (parameterMap, body).
  private def extractIifeBody(html: String): Option[(Map[String, JsValue], String)] = {
    val index = html.lastIndexOf("window.__NUXT__")
    if (index < 0) return None
    val script      = html.substring(index)
    val parametersStart = script.indexOf("(function(") + "(function(".length
    val parametersEnd   = script.indexOf("){", parametersStart)
    val bodyEnd     = script.indexOf("}(")
    if (parametersStart < "(function(".length || parametersEnd < 0 || bodyEnd < 0) return None

    val parameterNames = script.substring(parametersStart, parametersEnd).split(",").toSeq
    val valuesRaw  = script.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val parameterMap = Try {
      val clean = valuesRaw.substring(0, valuesEnd)
        .replaceAll("""Array\(\d+\)""", "null")
        .replace("undefined", "null")
      parameterNames.zip(Json.parse("[" + clean + "]").as[JsArray].value).toMap
    }.getOrElse(Map.empty[String, JsValue])

    Some((parameterMap, script.substring(parametersEnd + 2, bodyEnd)))
  }

  // A resolver turns a token — either a `"literal"` or a single-identifier parameter
  // reference — into its underlying string. `/` is decoded everywhere.
  private def makeResolver(parameterMap: Map[String, JsValue]): String => Option[String] = { token =>
    val trimmedToken = token.trim
    val raw =
      if (trimmedToken.startsWith("\"")) Some(trimmedToken.stripPrefix("\"").stripSuffix("\""))
      else parameterMap.get(trimmedToken).flatMap {
        case JsString(s) => Some(s)
        case n: JsNumber => n.value.toBigIntExact.map(_.toString)
        case _           => None
      }
    raw.map(_.replace("\\u002F", "/"))
  }

  // The IIFE body has two regions back-to-back: film metadata first, then a
  // per-day screenings map. The screenings region starts at `screenings:{"YYYY-…`.
  private def splitBody(body: String, resolve: String => Option[String], baseUrl: String): Option[NuxtContext] =
    ScreeningsStart.findFirstMatchIn(body).map(m => NuxtContext(
      movieBody      = body.substring(0, m.start),
      screeningsBody = body.substring(m.start + "screenings:".length),
      resolve        = resolve,
      baseUrl        = baseUrl
    ))

  private def parseNuxtMovies(context: NuxtContext): Map[String, NuxtMovie] = {
    val normal   = parseNormalNuxtMovies(context)
    val embedded = parseEmbeddedScreeningNuxtMovies(context)
    val events   = parseEventNuxtMovies(context)
    // When an event is tied to a parent film (e.g. "Pianista w Helios RePlay"
    // → the embedded "Pianista" movie block), prefer the parent film's
    // poster/runtime/titleOriginal. The event entry itself carries no
    // titleOriginal, so this is the only place a RePlay/event row gets one.
    // Sports and concert-only events have no embedded block, so we fall back to
    // the poster/runtime extracted directly from the event entry.
    val enrichedEvents = events.map { case (id, ev) =>
      id -> embedded.get(id).map(em => ev.copy(
        posterUrl      = em.posterUrl.orElse(ev.posterUrl),
        runtimeMinutes = em.runtimeMinutes.orElse(ev.runtimeMinutes),
        originalTitle  = em.originalTitle.orElse(ev.originalTitle)
      )).getOrElse(ev)
    }
    normal ++ embedded ++ enrichedEvents
  }

  private def parseEmbeddedScreeningNuxtMovies(context: NuxtContext): Map[String, NuxtMovie] =
    MovieGroupStart.findAllMatchIn(context.screeningsBody).flatMap { gm =>
      val movieId = gm.group(1)
      val array     = bracketedArrayContent(context.screeningsBody, gm.end)
      EmbeddedMovieBlk.findFirstMatchIn(array)
        .flatMap(m => parseNuxtEmbeddedMovieBlock(m.group(1), context.resolve, context.baseUrl).map(movieId -> _))
    }.toMap

  private def parseNuxtEmbeddedMovieBlock(block: String, resolve: String => Option[String], baseUrl: String): Option[NuxtMovie] =
    for {
      title <- nuxtField(block, "title", resolve)
      slug  <- nuxtField(block, "slug", resolve)
    } yield NuxtMovie(
      title          = title,
      slug           = slug,
      posterUrl      = nuxtPoster(block, resolve),
      filmUrl        = nuxtDigits(block, "id", resolve).map(id => s"$baseUrl/filmy/$slug-$id")
                         .orElse(Some(s"$baseUrl/filmy/$slug")),
      runtimeMinutes = nuxtRuntime(block, resolve),
      originalTitle  = nuxtField(block, "titleOriginal", resolve)
    )

  private def parseNormalNuxtMovies(context: NuxtContext): Map[String, NuxtMovie] =
    TopLevelMovie.findAllMatchIn(context.movieBody).flatMap { m =>
      val numericId = Some(m.group(1)).flatMap(id =>
        if (id.forall(_.isDigit)) Some(id) else context.resolve(id).filter(_.forall(_.isDigit)))
      val title     = Option(m.group(2)).orElse(context.resolve(m.group(3)))
      // titleOriginal may be a "literal" (group 4) or a reference token (group 5).
      val original  = Option(m.group(4)).orElse(Option(m.group(5)).flatMap(context.resolve))
      val slug      = Option(m.group(6)).orElse(context.resolve(m.group(7)))
      for {
        nid <- numericId
        t   <- title
        s   <- slug
      } yield {
        val nearby = context.movieBody.substring(m.start, math.min(m.start + 1500, context.movieBody.length))
        s"m$nid" -> NuxtMovie(t, s, nuxtPoster(nearby, context.resolve), Some(s"${context.baseUrl}/filmy/$s-$nid"), nuxtRuntime(nearby, context.resolve), original)
      }
    }.toMap

  private def parseEventNuxtMovies(context: NuxtContext): Map[String, NuxtMovie] = {
    val eventIds = MovieGroupStart.findAllMatchIn(context.screeningsBody)
                     .map(_.group(1)).filter(_.startsWith("e")).toSet
    eventIds.flatMap { eventId =>
      val numericId = eventId.stripPrefix("e")
      findEventMetaInMovieBody(eventId, context)
        .map(movie => eventId -> movie.copy(filmUrl = Some(s"${context.baseUrl}/wydarzenie/${movie.slug}-$numericId")))
    }.toMap
  }

  private def findEventMetaInMovieBody(eventId: String, context: NuxtContext): Option[NuxtMovie] =
    s"""_id:"$eventId"""".r.findFirstMatchIn(context.movieBody).flatMap { m =>
      val before = context.movieBody.substring(math.max(0, m.start - 1000), m.start)
      EventEntryAnchor.findAllMatchIn(before).toSeq.lastOption.flatMap { nm =>
        for {
          title <- context.resolve(nm.group(1))
          slug  <- context.resolve(nm.group(2))
        } yield {
          // The event entry continues from the name/slug anchor through to _id —
          // pull poster and runtime out of that slice the same way film entries do.
          val entryBlock = before.substring(nm.start)
          NuxtMovie(
            title          = title,
            slug           = slug,
            posterUrl      = nuxtPoster(entryBlock, context.resolve),
            filmUrl        = None,
            runtimeMinutes = nuxtRuntime(entryBlock, context.resolve),
            originalTitle  = None
          )
        }
      }
    }

  private def parseNuxtShowtimes(context: NuxtContext): Seq[(String, (LocalDateTime, String, List[String]))] =
    dayBlocks(context.screeningsBody).flatMap { dayBlock =>
      MovieGroupStart.findAllMatchIn(dayBlock).flatMap { gm =>
        val movieId = gm.group(1)
        val array     = bracketedArrayContent(dayBlock, gm.end)
        ScreeningEntry.findAllMatchIn(array).flatMap { sm =>
          for {
            timeStr <- context.resolve(sm.group(1)) if timeStr.length == 19
            sid     <- resolveSourceId(sm.group(2), context.resolve)
          } yield {
            // printRelease lives inside `moviePrint:{…}`. For regular film entries it's
            // a few fields after sourceId; for event entries it's deeper because of the
            // embedded `event:{…}` and `screeningMovies:[{…}]` blocks. Every Helios entry
            // we've seen carries one, so the first match in a generous lookahead window
            // is always the current entry's release.
            val tail   = array.substring(sm.end, math.min(sm.end + 2000, array.length))
            val format = PrintRelease.findFirstMatchIn(tail)
              .flatMap(m => context.resolve(m.group(1)))
              .map(_.split("/").toList.filter(_.nonEmpty))
              .getOrElse(Nil)
            movieId -> (LocalDateTime.parse(timeStr.replace(' ', 'T')), sid, format)
          }
        }
      }
    }

  // Slice `screeningsBody` between each `"YYYY-MM-DD":{ … }` day marker.
  private def dayBlocks(screeningsBody: String): Seq[String] = {
    val cuts = DayMarker.findAllMatchIn(screeningsBody).map(_.end).toSeq
    val ends = cuts.drop(1) :+ screeningsBody.length
    cuts.lazyZip(ends).map((s, e) => screeningsBody.substring(s, e)).toSeq
  }

  private def resolveSourceId(raw: String, resolve: String => Option[String]): Option[String] =
    if (raw.startsWith("\"")) Some(raw.stripPrefix("\"").stripSuffix("\""))
    else resolve(raw)

  // ── NUXT field extraction helpers ─────────────────────────────────────────
  //
  // NUXT IIFE values come in two flavours: literal `"strings"` or single-token
  // parameter references (parameter names may include `$`, so plain `\w` isn't enough).

  private def nuxtField(block: String, field: String, resolve: String => Option[String]): Option[String] =
    s"""$field:(?:"([^"]+)"|([\\w$$]+))""".r.findFirstMatchIn(block)
      .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))

  private def nuxtDigits(block: String, field: String, resolve: String => Option[String]): Option[String] =
    s"""$field:(\\d{3,}|[\\w$$]+)""".r.findFirstMatchIn(block)
      .flatMap(m => Some(m.group(1)).flatMap(s =>
        if (s.forall(_.isDigit)) Some(s) else resolve(s).filter(_.forall(_.isDigit))))

  private def nuxtPoster(block: String, resolve: String => Option[String]): Option[String] =
    """posterPhoto:\{[^}]*?url:(?:"([^"]+)"|([\w$]+))""".r.findFirstMatchIn(block)
      .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
      .map(_.replace("\\u002F", "/"))

  private def nuxtRuntime(block: String, resolve: String => Option[String]): Option[Int] =
    """duration:([^,}]+)""".r.findFirstMatchIn(block)
      .flatMap(m => parseInt(m.group(1)).orElse(resolve(m.group(1)).flatMap(parseInt)))

  private def bracketedArrayContent(text: String, start: Int): String = {
    var depth = 1; var pos = start
    while (pos < text.length && depth > 0) {
      if (text(pos) == '[') depth += 1 else if (text(pos) == ']') depth -= 1
      pos += 1
    }
    text.substring(start, math.max(start, pos - 1))
  }

  private def parseInt(s: String): Option[Int] = Try(s.trim.toInt).toOption
}
