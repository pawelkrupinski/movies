package services.cinemas

import models.{Cinema, CinemaMovie, Helios, HeliosStarachowice, HeliosKrosno, HeliosTczew, HeliosZory, HeliosLubin, HeliosOstrowWlkp, HeliosKedzierzynKozle, HeliosAlejaBielany, HeliosAlfa, HeliosBiala, HeliosBielskoBiala, HeliosBlueCity, HeliosBydgoszcz, HeliosDabrowaGornicza, HeliosForum, HeliosGorzow, HeliosJeleniaGora, HeliosJurowiecka, HeliosKalisz, HeliosKatowice, HeliosKielce, HeliosKonin, HeliosKoszalin, HeliosLegnica, HeliosLodz, HeliosMagnolia, HeliosMetropolia, HeliosNowySacz, HeliosOlsztyn, HeliosOpoleKarolinka, HeliosOpoleSolaris, HeliosOutletPark, HeliosPlock, HeliosPrzemysl, HeliosRadom, HeliosRiviera, HeliosRzeszow, HeliosSosnowiec, HeliosSzczecin, Movie, Showtime}
import play.api.libs.json._

import java.time.LocalDateTime
import scala.util.Try

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

  // The Helios venues this app scrapes, each keyed by its REST source UUID
  // (verified live against restapi.helios.pl) and page slug.
  val Poznan       = HeliosCinema(Helios,             "poznan",   "kino-helios",                "815face9-2a1d-4c62-9b2f-a361574b79a2")
  val Magnolia     = HeliosCinema(HeliosMagnolia,     "wroclaw",  "kino-helios-magnolia",       "c21c6e3b-874c-4432-b44e-a155ec9102cd")
  val AlejaBielany = HeliosCinema(HeliosAlejaBielany, "wroclaw",  "kino-helios-aleja-bielany",  "7582cee7-2815-4897-8715-e90a5b99d2e4")
  val BlueCity     = HeliosCinema(HeliosBlueCity,     "warszawa", "kino-helios-blue-city",      "4ca060df-c4f2-4157-8905-bf46527aae58")
  val Metropolia   = HeliosCinema(HeliosMetropolia,   "gdansk",   "kino-helios-metropolia",     "d09e2607-693f-479f-8deb-59a18add40eb")
  val Forum        = HeliosCinema(HeliosForum,        "gdansk",   "kino-helios-forum",          "60cfe883-55aa-4e28-be7f-233f589a34cb")
  val Riviera      = HeliosCinema(HeliosRiviera,      "gdynia",   "kino-helios",                "0bdb21cd-cf7d-4efb-829f-d1913a666b83")
  val Lodz         = HeliosCinema(HeliosLodz,         "lodz",     "kino-helios",                "46055d88-5f34-44a0-9584-b041caa71e26")
  val Alfa         = HeliosCinema(HeliosAlfa,         "bialystok", "kino-helios-alfa",          "3e616682-1048-46d8-954d-a2c8de8e7ee4")
  val Biala        = HeliosCinema(HeliosBiala,        "bialystok", "kino-helios-biala",         "d516a0dd-b0ab-4e15-85b2-0286eaf6dde6")
  val Jurowiecka   = HeliosCinema(HeliosJurowiecka,   "bialystok", "kino-helios-jurowiecka",    "5c699a45-d219-4f4e-90f4-4aac0ca44a88")
  val Bydgoszcz    = HeliosCinema(HeliosBydgoszcz,    "bydgoszcz","kino-helios",                "8acb28a0-2e4b-4beb-a13a-bb97e17085cc")
  // Katowice's REST sourceId is KATOWICE Libero (b661164e…). The old value
  // 6f3ef265… is in fact Olsztyn Galeria Aura — a copy-paste that silently fed
  // Katowice's room/format/synopsis enrichment (and restOnly films) from Olsztyn.
  val Katowice     = HeliosCinema(HeliosKatowice,     "katowice", "kino-helios",                "b661164e-04d4-41af-bdca-0f9a745c3898")
  val Szczecin     = HeliosCinema(HeliosSzczecin,     "szczecin", "kino-helios-chr-kupiec",     "3e60a454-c438-4c9b-b24f-7c6feff3b5ed")
  val SzczecinOutletPark = HeliosCinema(HeliosOutletPark, "szczecin", "helios-outlet-park",     "b898bbed-7ab6-428f-a57b-26d6b6e6a04b")
  val Radom        = HeliosCinema(HeliosRadom,        "radom",    "kino-helios-radom",          "287fc8f4-7bee-49b4-afcd-b22b52e520b9")
  val Sosnowiec    = HeliosCinema(HeliosSosnowiec,    "sosnowiec","kino-helios-sosnowiec",      "880f2a3a-e9e8-447c-95ca-72c49ca38a1e")
  val Kielce       = HeliosCinema(HeliosKielce,       "kielce",   "kino-helios",                "d01162e6-31a4-41b9-97cd-1ef7707522fe")
  val Rzeszow      = HeliosCinema(HeliosRzeszow,      "rzeszow",  "kino-helios-galeria",        "409eac78-9718-4fa3-b590-d1e1fb60892d")

  // New mid-size-city venues. citySlug/cinemaSlug verified to render a NUXT
  // repertoire page; sourceId verified against restapi.helios.pl/api/cinema.
  val Olsztyn      = HeliosCinema(HeliosOlsztyn,      "olsztyn",  "kino-helios",                "6f3ef265-ef99-48e9-84f4-ed862badb756")
  val BielskoBiala = HeliosCinema(HeliosBielskoBiala, "bielsko-biala", "kino-helios",           "cb932ba4-3dfd-4946-b3fd-1687603f78f7")
  val OpoleKarolinka = HeliosCinema(HeliosOpoleKarolinka, "opole", "kino-helios-karolinka",     "ec0ba789-c70b-42e7-8adf-8c9595888672")
  val OpoleSolaris = HeliosCinema(HeliosOpoleSolaris, "opole",    "kino-helios-solaris",        "d01e6010-d098-4899-951b-a7b7208df75a")
  val Gorzow       = HeliosCinema(HeliosGorzow,       "gorzow-wielkopolski", "kino-helios",     "6f51c391-4d6d-4aeb-bf78-6c61b676865f")
  val Koszalin     = HeliosCinema(HeliosKoszalin,     "koszalin", "kino-helios-galeria-emka",   "4b58d474-bf12-405f-b76d-f1f88cae2b07")
  val Kalisz       = HeliosCinema(HeliosKalisz,       "kalisz",   "kino-helios",                "99d1a365-3dad-4c9b-a9e9-6e6177630708")
  val Legnica      = HeliosCinema(HeliosLegnica,      "legnica",  "kino-helios",                "34b23726-7c53-483b-a0e5-13dfca6075ba")
  val Plock        = HeliosCinema(HeliosPlock,        "plock",    "kino-helios",                "4623d4a3-d0be-49d8-82d8-ef16a67a848c")
  val DabrowaGornicza = HeliosCinema(HeliosDabrowaGornicza, "dabrowa-gornicza", "kino-helios",  "d61fc6d9-bac1-44f0-a34a-00868e694c9b")
  val NowySacz     = HeliosCinema(HeliosNowySacz,     "nowy-sacz", "kino-helios",               "223cd021-5821-481a-9d04-31511fb44042")
  val JeleniaGora  = HeliosCinema(HeliosJeleniaGora,  "jelenia-gora", "kino-helios",            "88c70097-4f24-49b5-bd35-c752e3543d4e")
  val Przemysl     = HeliosCinema(HeliosPrzemysl,     "przemysl", "kino-helios",                "19a14011-0e44-43cd-9d12-8e8a16e9d744")
  val Konin        = HeliosCinema(HeliosKonin,        "konin",    "kino-helios",                "0fe6ba65-dc83-49c1-8f01-bef8675b9927")
  // Moved off Filmweb onto the native Helios REST feed.
  val Starachowice    = HeliosCinema(HeliosStarachowice,    "starachowice",       "kino-helios", "9906f18b-ed73-41d8-a35e-bfcda930b2d3")
  val Krosno          = HeliosCinema(HeliosKrosno,          "krosno",             "kino-helios", "be832e9d-5b79-47ef-a0b0-1b465349ce89")
  val Tczew           = HeliosCinema(HeliosTczew,           "tczew",              "kino-helios", "58fbf23b-c605-4185-b1e9-6a3c5d7878ce")
  val Zory            = HeliosCinema(HeliosZory,            "zory",               "kino-helios", "d116bcc0-8675-41d8-b0b0-3e7155df9ffc")
  val Lubin           = HeliosCinema(HeliosLubin,           "lubin",              "kino-helios", "71192dbe-94ef-43c0-9079-ae7c9747a2f4")
  val OstrowWielkopolski = HeliosCinema(HeliosOstrowWlkp,   "ostrow-wielkopolski", "kino-helios", "1a433348-f89c-4b18-bfa4-4332e3bbf0c8")
  val KedzierzynKozle = HeliosCinema(HeliosKedzierzynKozle, "kedzierzyn-kozle",   "kino-helios", "cf760fa9-32ba-4541-ae07-ef14532a911d")

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
  // (TitleRuleDefaults, same order); this delegates so the Nuxt+REST dedup
  // grouping/matching still collapses decorated variants onto the bare film.
  def cleanTitle(title: String): String =
    services.movies.TitleNormalizer.cinemaClean("helios", title)

  def buildMovies(html: String, config: HeliosCinema = Poznan): Seq[CinemaMovie] = {
    val parsed   = parseNuxtPage(html, config.baseUrl)
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
          movie = Movie(title = title, runtimeMinutes = movie.runtimeMinutes, releaseYear = None,
            rawTitle = movies.map(_.title).headOption),
          cinema    = config.cinema,
          posterUrl = movies.flatMap(_.posterUrl).headOption,
          filmUrl   = urls.find(_.contains("/filmy/")).orElse(urls.headOption),
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = slots.map { case (dateTime, screeningId, format) =>
            Showtime(
              dateTime   = dateTime,
              bookingUrl = Some(s"$BookingBase/$screeningId?cinemaId=${config.sourceId}").filter(_ => screeningId.nonEmpty),
              room       = None,
              format     = format
            )
          }.distinct
        )
      }
      .filter(_.showtimes.nonEmpty)
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
    runtimeMinutes: Option[Int]
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
  private val TopLevelMovie     = """,id:(\d{3,}|[\w$]+),sourceId:(?:"[^"]+"|[\w$]+),title:(?:"([^"]+)"|([\w$]+)),titleOriginal:(?:"[^"]*"|[\w$]+),slug:(?:"([^"]+)"|([\w$]+))""".r

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
    // When an event is tied to a parent film (e.g. "Billie Eilish - … Live in 3D"
    // → the base Tour entry), prefer the parent film's poster/runtime. Sports
    // and concert-only events have no embedded block, so we fall back to the
    // poster/runtime extracted directly from the event entry.
    val enrichedEvents = events.map { case (id, ev) =>
      id -> embedded.get(id).map(em => ev.copy(
        posterUrl      = em.posterUrl.orElse(ev.posterUrl),
        runtimeMinutes = em.runtimeMinutes.orElse(ev.runtimeMinutes)
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
      runtimeMinutes = nuxtRuntime(block, resolve)
    )

  private def parseNormalNuxtMovies(context: NuxtContext): Map[String, NuxtMovie] =
    TopLevelMovie.findAllMatchIn(context.movieBody).flatMap { m =>
      val numericId = Some(m.group(1)).flatMap(id =>
        if (id.forall(_.isDigit)) Some(id) else context.resolve(id).filter(_.forall(_.isDigit)))
      val title     = Option(m.group(2)).orElse(context.resolve(m.group(3)))
      val slug      = Option(m.group(4)).orElse(context.resolve(m.group(5)))
      for {
        nid <- numericId
        t   <- title
        s   <- slug
      } yield {
        val nearby = context.movieBody.substring(m.start, math.min(m.start + 1500, context.movieBody.length))
        s"m$nid" -> NuxtMovie(t, s, nuxtPoster(nearby, context.resolve), Some(s"${context.baseUrl}/filmy/$s-$nid"), nuxtRuntime(nearby, context.resolve))
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
            runtimeMinutes = nuxtRuntime(entryBlock, context.resolve)
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
