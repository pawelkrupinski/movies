package services.cinemas.uk

import play.api.libs.json._
import services.cinemas.common.FilmDetail

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

  /** The BBFC certificates the `movies` endpoint's `certificate` field can
   *  legitimately hold. Whitelisted rather than passed through verbatim, so a
   *  future vendor value we don't recognise (a rating-pending placeholder, a
   *  non-UK certificate on a rare import) drops instead of leaking onto a
   *  card — the same discipline the old `attributeIds` whitelist kept. */
  private val BbfcCertificates: Set[String] = Set("U", "PG", "12A", "12", "15", "18")

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

  private def toFilmDetail(m: JsValue): FilmDetail = FilmDetail(
    synopsis       = (m \ "synopsis").asOpt[String].map(_.trim).filter(_.nonEmpty),
    cast           = (m \ "casting").asOpt[Seq[String]].getOrElse(Seq.empty)
                        .map(_.trim).filter(_.nonEmpty),
    // `direction`/`coDirection` are separate arrays on the wire (a credited
    // co-director is a distinct field, not folded into `direction`) but the
    // app has one `director` list — concatenate.
    director       = ((m \ "direction").asOpt[Seq[String]].getOrElse(Seq.empty) ++
                       (m \ "coDirection").asOpt[Seq[String]].getOrElse(Seq.empty))
                        .map(_.trim).filter(_.nonEmpty),
    // The wire carries seconds ("runtime": 7500); the app wants whole minutes.
    runtimeMinutes = (m \ "runtime").asOpt[Int].filter(_ > 0).map(_ / 60),
    ageRating      = (m \ "certificate").asOpt[String].map(_.trim.toUpperCase)
                        .filter(BbfcCertificates.contains)
  )
}
