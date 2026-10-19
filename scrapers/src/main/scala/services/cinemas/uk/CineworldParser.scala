package services.cinemas.uk

import play.api.libs.json._
import services.cinemas.common.{FilmDetail, GatsbyBoxOfficeParser}

import scala.util.Try

/**
 * Pure JSON → [[FilmDetail]] transformation for Cineworld's per-film detail
 * endpoint (`/api/gatsby-source-boxofficeapi/movies?ids=<id>`, see
 * [[CineworldClient]]). No I/O: the client fetches the body and hands it here,
 * so the parse is unit-testable against recorded fixtures with no HTTP
 * stubbing.
 *
 * The listing scrape itself (schedule + chain-wide film catalogue) is NOT
 * parsed here — `CineworldClient` composes [[services.cinemas.common.GatsbyBoxOfficeClient]]
 * for that, since it is the identical platform Showcase/Everyman already run.
 * This file exists only for the ONE thing that platform's shared parser
 * doesn't cover: Cineworld's own site (unlike Showcase/Everyman) exposes
 * synopsis/cast/director/certificate, just off this separate runtime
 * endpoint rather than the static catalogue query.
 */
object CineworldParser {


  /** One film's detail off a `movies?ids=<id>` response — a JSON ARRAY with at
   *  most one element (the platform silently omits an id it doesn't recognise
   *  rather than erroring, so an empty array is a normal, LOADED "not found"
   *  answer). `None` only for a body that fails to parse at all, or that
   *  parses to something other than an array — a genuine fetch/format
   *  failure, not a valid "no match" response.
   *
   *  An empty array yields `Some(FilmDetail())` (all fields empty), not
   *  `None`: `DetailFetchOutcome` never stamps a `None`/`Failed` result, so
   *  `DetailReaper` would otherwise re-enqueue that same unrecognised id on
   *  every tick forever, pinning "Cineworld Enrichment" red — the exact
   *  loaded-but-empty livelock `DetailEnricherDurableFailureSpec` guards every
   *  other deferred-detail client against (see its class doc); a well-formed
   *  `[]` is this platform's shape of "loaded but carries no fields", not an
   *  unreadable body. */
  def parseMovieDetail(json: String): Option[FilmDetail] =
    Try(Json.parse(json)).toOption
      .flatMap(_.asOpt[Seq[JsValue]])
      .map(_.headOption.fold(FilmDetail())(toFilmDetail))

  private def toFilmDetail(m: JsValue): FilmDetail = {
    val details = GatsbyBoxOfficeParser.detailsOf(m)
    FilmDetail(
      synopsis       = details.synopsis,
      cast           = details.cast,
      director       = details.directors,
      runtimeMinutes = details.runtimeMinutes,
      ageRating      = details.certificate.filter(GatsbyBoxOfficeParser.BbfcCertificates))
  }
}
