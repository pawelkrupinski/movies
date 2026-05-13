package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._

import java.time.LocalDateTime
import scala.util.Try

// Helios renders its repertoire page as Nuxt 2 hydration: the entire payload is
// an IIFE assigned to `window.__NUXT__`, with shared string/number values
// hoisted to parameter names like `a, b, …, $$`. We pull out the movie list,
// the per-day screenings, and emit `CinemaMovie` rows.
//
// Centralised here so HeliosClient can focus on REST fetching, REST↔NUXT
// reconciliation, and the post-processing pipeline.
object HeliosNuxt {

  val BaseUrl        = "https://helios.pl/poznan/kino-helios"
  val BookingBase    = "https://bilety.helios.pl/screen"
  val CinemaSourceId = "815face9-2a1d-4c62-9b2f-a361574b79a2"

  // Strip event/promo suffixes so that "Diabeł ubiera się u Prady 2 - KNT"
  // collapses to the canonical "Diabeł ubiera się u Prady 2".
  def cleanTitle(title: String): String =
    Seq(" w Helios RePlay", " w Helios Anime", " w Helios na Scenie", " w HnS",
        " - Salon Kultury Helios", " - KNTJ", " - KNT")
      .foldLeft(title)((t, suffix) => t.stripSuffix(suffix))

  def buildMovies(html: String): Seq[CinemaMovie] = {
    val parsed   = parseNuxtPage(html)
    val nuxtRows = parsed.showtimesByMovie.toSeq.flatMap { case (movieId, slots) =>
      parsed.movies.get(movieId).map(movie => movie -> slots)
    }

    nuxtRows
      .groupBy { case (m, _) => cleanTitle(m.title) }
      .toSeq
      .map { case (title, rows) =>
        val movies = rows.map(_._1)
        val movie  = movies.find(_.runtimeMinutes.nonEmpty).getOrElse(movies.head)
        val slots  = rows.flatMap(_._2).distinct.sortBy(_._1)
        // When both a regular film and an event share the same cleaned title (e.g. "Diabeł" + "Diabeł - KNT"),
        // prefer the /filmy/ URL — it points to the canonical movie page rather than a specific event.
        val urls   = movies.flatMap(_.filmUrl)
        CinemaMovie(
          movie = Movie(title = title, runtimeMinutes = movie.runtimeMinutes,
                        releaseYear = None, premierePl = None, premiereWorld = None),
          cinema    = Helios,
          posterUrl = movies.flatMap(_.posterUrl).headOption,
          filmUrl   = urls.find(_.contains("/filmy/")).orElse(urls.headOption),
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = slots.map { case (dateTime, screeningId) =>
            Showtime(
              dateTime   = dateTime,
              bookingUrl = Some(s"$BookingBase/$screeningId?cinemaId=$CinemaSourceId").filter(_ => screeningId.nonEmpty),
              room       = None,
              format     = None
            )
          }.distinct
        )
      }
      .filter(_.showtimes.nonEmpty)
  }

  // ── NUXT IIFE parser ──────────────────────────────────────────────────────

  private case class NuxtPage(
    movies:           Map[String, NuxtMovie],
    showtimesByMovie: Map[String, Seq[(LocalDateTime, String)]]
  )

  private case class NuxtMovie(
    title:          String,
    slug:           String,
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    runtimeMinutes: Option[Int]
  )

  // Bundles the three things every internal parser needs: the slice of the IIFE
  // body that contains film metadata, the slice that contains screenings, and
  // the resolver that turns a param-name token into its string value.
  private case class NuxtCtx(
    movieBody:      String,
    screeningsBody: String,
    resolve:        String => Option[String]
  )

  // Reused patterns. Hoisted so the structural anchors live in one place.
  private val ScreeningsStart   = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r
  private val MovieGroupStart   = """([em]\d+):\{screenings:\[""".r
  private val DayMarker         = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
  private val EmbeddedMovieBlk  = """movie:\{(.*?)\},moviePrint:""".r
  private val EventEntryAnchor  = """,name:([\w$]+),slug:([\w$]+),""".r
  private val ScreeningEntry    = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r
  // Top-level film: ,id:N,sourceId:X,title:Y,titleOriginal:Z,slug:W in order.
  private val TopLevelMovie     = """,id:(\d{3,}|[\w$]+),sourceId:(?:"[^"]+"|[\w$]+),title:(?:"([^"]+)"|([\w$]+)),titleOriginal:(?:"[^"]*"|[\w$]+),slug:(?:"([^"]+)"|([\w$]+))""".r

  private def parseNuxtPage(html: String): NuxtPage = {
    val empty = NuxtPage(Map.empty, Map.empty)
    (for {
      iife <- extractIifeBody(html)
      ctx  <- splitBody(iife._2, makeResolver(iife._1))
    } yield NuxtPage(
      movies           = parseNuxtMovies(ctx),
      showtimesByMovie = parseNuxtShowtimes(ctx).groupMap(_._1)(_._2)
    )).getOrElse(empty)
  }

  // Locate the `(function(params){body}(values))` IIFE and return (paramMap, body).
  private def extractIifeBody(html: String): Option[(Map[String, JsValue], String)] = {
    val idx = html.lastIndexOf("window.__NUXT__")
    if (idx < 0) return None
    val script      = html.substring(idx)
    val paramsStart = script.indexOf("(function(") + "(function(".length
    val paramsEnd   = script.indexOf("){", paramsStart)
    val bodyEnd     = script.indexOf("}(")
    if (paramsStart < "(function(".length || paramsEnd < 0 || bodyEnd < 0) return None

    val paramNames = script.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw  = script.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val paramMap = Try {
      val clean = valuesRaw.substring(0, valuesEnd)
        .replaceAll("""Array\(\d+\)""", "null")
        .replace("undefined", "null")
      paramNames.zip(Json.parse("[" + clean + "]").as[JsArray].value).toMap
    }.getOrElse(Map.empty[String, JsValue])

    Some((paramMap, script.substring(paramsEnd + 2, bodyEnd)))
  }

  // A resolver turns a token — either a `"literal"` or a single-identifier param
  // reference — into its underlying string. `/` is decoded everywhere.
  private def makeResolver(paramMap: Map[String, JsValue]): String => Option[String] = { token =>
    val t = token.trim
    val raw =
      if (t.startsWith("\"")) Some(t.stripPrefix("\"").stripSuffix("\""))
      else paramMap.get(t).flatMap {
        case JsString(s) => Some(s)
        case n: JsNumber => n.value.toBigIntExact.map(_.toString)
        case _           => None
      }
    raw.map(_.replace("\\u002F", "/"))
  }

  // The IIFE body has two regions back-to-back: film metadata first, then a
  // per-day screenings map. The screenings region starts at `screenings:{"YYYY-…`.
  private def splitBody(body: String, resolve: String => Option[String]): Option[NuxtCtx] =
    ScreeningsStart.findFirstMatchIn(body).map(m => NuxtCtx(
      movieBody      = body.substring(0, m.start),
      screeningsBody = body.substring(m.start + "screenings:".length),
      resolve        = resolve
    ))

  private def parseNuxtMovies(ctx: NuxtCtx): Map[String, NuxtMovie] = {
    val normal   = parseNormalNuxtMovies(ctx)
    val embedded = parseEmbeddedScreeningNuxtMovies(ctx)
    val events   = parseEventNuxtMovies(ctx)
    // Events have a title/slug pair but no poster/runtime in the movie body;
    // pull those from the embedded film block keyed by the same event id.
    val enrichedEvents = events.map { case (id, ev) =>
      id -> embedded.get(id).map(em => ev.copy(
        posterUrl      = ev.posterUrl.orElse(em.posterUrl),
        runtimeMinutes = ev.runtimeMinutes.orElse(em.runtimeMinutes)
      )).getOrElse(ev)
    }
    normal ++ embedded ++ enrichedEvents
  }

  private def parseEmbeddedScreeningNuxtMovies(ctx: NuxtCtx): Map[String, NuxtMovie] =
    MovieGroupStart.findAllMatchIn(ctx.screeningsBody).flatMap { gm =>
      val movieId = gm.group(1)
      val arr     = bracketedArrayContent(ctx.screeningsBody, gm.end)
      EmbeddedMovieBlk.findFirstMatchIn(arr)
        .flatMap(m => parseNuxtEmbeddedMovieBlock(m.group(1), ctx.resolve).map(movieId -> _))
    }.toMap

  private def parseNuxtEmbeddedMovieBlock(block: String, resolve: String => Option[String]): Option[NuxtMovie] =
    for {
      title <- nuxtField(block, "title", resolve)
      slug  <- nuxtField(block, "slug", resolve)
    } yield NuxtMovie(
      title          = title,
      slug           = slug,
      posterUrl      = nuxtPoster(block, resolve),
      filmUrl        = nuxtDigits(block, "id", resolve).map(id => s"$BaseUrl/filmy/$slug-$id")
                         .orElse(Some(s"$BaseUrl/filmy/$slug")),
      runtimeMinutes = nuxtRuntime(block, resolve)
    )

  private def parseNormalNuxtMovies(ctx: NuxtCtx): Map[String, NuxtMovie] =
    TopLevelMovie.findAllMatchIn(ctx.movieBody).flatMap { m =>
      val numericId = Some(m.group(1)).flatMap(id =>
        if (id.forall(_.isDigit)) Some(id) else ctx.resolve(id).filter(_.forall(_.isDigit)))
      val title     = Option(m.group(2)).orElse(ctx.resolve(m.group(3)))
      val slug      = Option(m.group(4)).orElse(ctx.resolve(m.group(5)))
      for {
        nid <- numericId
        t   <- title
        s   <- slug
      } yield {
        val nearby = ctx.movieBody.substring(m.start, math.min(m.start + 1500, ctx.movieBody.length))
        s"m$nid" -> NuxtMovie(t, s, nuxtPoster(nearby, ctx.resolve), Some(s"$BaseUrl/filmy/$s-$nid"), nuxtRuntime(nearby, ctx.resolve))
      }
    }.toMap

  private def parseEventNuxtMovies(ctx: NuxtCtx): Map[String, NuxtMovie] = {
    val eventIds = MovieGroupStart.findAllMatchIn(ctx.screeningsBody)
                     .map(_.group(1)).filter(_.startsWith("e")).toSet
    eventIds.flatMap { eventId =>
      val numericId = eventId.stripPrefix("e")
      findEventMetaInMovieBody(eventId, ctx)
        .map(movie => eventId -> movie.copy(filmUrl = Some(s"$BaseUrl/wydarzenie/${movie.slug}-$numericId")))
    }.toMap
  }

  private def findEventMetaInMovieBody(eventId: String, ctx: NuxtCtx): Option[NuxtMovie] =
    s"""_id:"$eventId"""".r.findFirstMatchIn(ctx.movieBody).flatMap { m =>
      val before = ctx.movieBody.substring(math.max(0, m.start - 1000), m.start)
      EventEntryAnchor.findAllMatchIn(before).toSeq.lastOption.flatMap { nm =>
        for {
          title <- ctx.resolve(nm.group(1))
          slug  <- ctx.resolve(nm.group(2))
        } yield NuxtMovie(title, slug, posterUrl = None, filmUrl = None, runtimeMinutes = None)
      }
    }

  private def parseNuxtShowtimes(ctx: NuxtCtx): Seq[(String, (LocalDateTime, String))] =
    dayBlocks(ctx.screeningsBody).flatMap { dayBlock =>
      MovieGroupStart.findAllMatchIn(dayBlock).flatMap { gm =>
        val movieId = gm.group(1)
        val arr     = bracketedArrayContent(dayBlock, gm.end)
        ScreeningEntry.findAllMatchIn(arr).flatMap { sm =>
          for {
            timeStr <- ctx.resolve(sm.group(1)) if timeStr.length == 19
            sid     <- resolveSourceId(sm.group(2), ctx.resolve)
          } yield movieId -> (LocalDateTime.parse(timeStr.replace(' ', 'T')), sid)
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
  // param references (param names may include `$`, so plain `\w` isn't enough).

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
