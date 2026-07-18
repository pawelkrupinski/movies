package services.cinemas.pl

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}
import play.api.libs.json._
import services.movies.TrailerEmbed

import java.time.LocalDateTime

/**
 * Pure JSON → `CinemaMovie` transformation for the Multikino API response.
 * No I/O: callers fetch the body (via `HttpFetch`) and hand it here. Keeping
 * parsing separate from transport lets the parsing be unit-tested against
 * recorded fixtures without any HTTP stubbing.
 */
object MultikinoParser {
  private val BaseUrl = "https://www.multikino.pl"

  /** `cinema` defaults to Poznań's Multikino (Stary Browar) so the existing
   *  parser specs and fixture stay byte-identical; the multi-city clients pass
   *  their own Multikino venue. */
  def parse(json: String, cinema: Cinema = Multikino): Seq[CinemaMovie] =
    (Json.parse(json) \ "result").as[JsArray].value.map(parseFilm(_, cinema)).toSeq

  /** Strip cycle decoration so a decorated screening merges onto the same row
   *  — and enriches off the same clean title — as the regular run: "Kino na
   *  obcasach: X" (ladies' programme), "Kolekcja Mamoru Hosody: X" (anime
   *  retrospective). The underlying films are widely-released titles other
   *  cinemas list bare; leaving the prefix on produces a Multikino-only
   *  variant TMDB / Filmweb queries can't find. Public for direct unit tests. */
  // Cycle-decoration stripping now lives in the editable "multikino" rules (see
  // TitleRules); this delegates so it stays unit-testable here.
  def cleanTitle(filmTitle: String): String =
    services.movies.TitleNormalizer.cinemaClean("multikino", filmTitle)

  private def parseFilm(film: JsValue, cinema: Cinema): CinemaMovie = {
    val rawFilmTitle = (film \ "filmTitle").as[String]
    // Casing is applied centrally at the scrape choke point
    // (`TitleNormalizer.recase`), not here — the Multikino API ships ALL-CAPS.
    val title        = cleanTitle(rawFilmTitle)
    val multikinoId = (film \ "filmId").asOpt[String].filter(_.nonEmpty)
    val mxcId       = (film \ "movieXchangeCode").asOpt[String].filter(_.nonEmpty)
    val sessions    = (film \ "showingGroups").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                        .flatMap(g => (g \ "sessions").as[JsArray].value)
    CinemaMovie(
      movie       = Movie(
        title          = title,
        rawTitle       = Some(rawFilmTitle),
        runtimeMinutes = (film \ "runningTime").asOpt[Int],
        // `releaseDate` from Multikino's API is the Polish theatrical (re-)
        // release date, not the film's production year — e.g. "Zawieście
        // czerwone latarnie" (Raise the Red Lantern, 1991) comes back with
        // releaseDate=2026-06-18. Using that for `releaseYear` poisons TMDB
        // resolution: year-scoped search excludes the actual film, and the
        // year-less fallback picks whatever now shares the Polish title
        // (often a famous English-language film with the same translation).
        // We have no production-year field in this API, so leave it None
        // and let TMDB rank by title alone.
        releaseYear    = None,
        // English/international release title — non-empty on ~5% of films
        // (Cirque du Soleil, opera/concert documents, English-language imports
        // distributed under a Polish title). Empty on Polish releases and
        // most blockbusters where the Polish title IS the only known
        // identifier. Useful as a TMDB-search fallback for niche titles.
        originalTitle  = (film \ "originalTitle").asOpt[String].filter(_.nonEmpty),
        // Multikino's `genres` field is schema-present but empty across the
        // current Poznań catalog (`"genres": []`). Extract anyway so a future
        // upstream backfill flows through automatically — the merge layer
        // demotes Multikino under TMDB/Filmweb so an empty list here is
        // harmless when richer sources have data.
        genres         = (film \ "genres").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
                           .flatMap(g => (g \ "name").asOpt[String].orElse(g.asOpt[String]))
                           .filter(_.nonEmpty)
      ),
      cinema      = cinema,
      posterUrl   = (film \ "posterImageSrc").asOpt[String].filter(_.nonEmpty),
      filmUrl     = absoluteUrl((film \ "filmUrl").asOpt[String]),
      synopsis    = (film \ "synopsisShort").asOpt[String].filter(_.nonEmpty).map(tools.TextNormalization.stripHtml),
      cast        = (film \ "cast").asOpt[String].filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director    = (film \ "director").asOpt[String].filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      showtimes   = sessions.flatMap(parseSession).toSeq,
      externalIds = (multikinoId.map("mk" -> _) ++ mxcId.map("mxc" -> _)).toMap,
      // Multikino's API exposes `trailers: [<url>]` on films that have one
      // (alongside `hasTrailer` which sometimes stays true even after the
      // array empties out — only the array entries are usable). Take the
      // first entry; only commit when it parses as a YouTube URL we can
      // embed.
      trailerUrl  = (film \ "trailers").asOpt[JsArray]
                      .flatMap(_.value.headOption).flatMap(_.asOpt[String])
                      .filter(_.nonEmpty)
                      .flatMap(u => TrailerEmbed.youTubeId(u)
                        .map(id => s"https://www.youtube.com/watch?v=$id"))
    )
  }

  private def parseSession(session: JsValue): Option[Showtime] =
    (session \ "startTime").asOpt[String].map { startTime =>
      val attrs = (session \ "attributes").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                    .flatMap(a => (a \ "name").asOpt[String]).toSet
      Showtime(
        dateTime   = LocalDateTime.parse(startTime),
        bookingUrl = absoluteUrl((session \ "bookingUrl").asOpt[String]),
        room       = (session \ "screenName").asOpt[String].filter(_.nonEmpty),
        format     = parseFormat(attrs)
      )
    }

  // Multikino tags Language attributes as DUBBING / NAPISY / JĘZYK ORYGINALNY.
  // Normalise to DUB / NAP tokens (other clients use the same); original-language
  // screenings get no language tag (matches Cinema City's behaviour).
  private def parseFormat(attrs: Set[String]): List[String] = List(
    attrs.find(n => n == "2D" || n == "3D"),
    if (attrs.contains("DUBBING"))     Some("DUB")
    else if (attrs.contains("NAPISY")) Some("NAP")
    else None
  ).flatten

  private def absoluteUrl(url: Option[String]): Option[String] =
    url.filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else BaseUrl + u)
}
