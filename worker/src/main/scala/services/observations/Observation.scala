package services.observations

import models.{Cinema, CinemaMovie}
import services.movies.ListingKey
import tools.{HttpStatusException, RedactedUrl}

import java.net.URI
import java.time.Instant
import java.util.Base64
import scala.util.Try

/*
 * OBSERVATIONS — the identity program's evidence (docs/design/identity-resolver.md, "Phase 1:
 * observations"). Two kinds, both immutable and timestamped:
 *
 *  - a LISTING observation: what one venue published about one film, keyed by `ListingKey`;
 *  - a LOOKUP observation: what an external service answered to one request, keyed by the
 *    canonical request (`LookupQuery`) — the identity resolver's: TMDB searches, details,
 *    credits and filmographies, and a venue's per-film detail page.
 *
 * Nothing here is derived from another observation, and nothing serving reads them.
 */

/** One external request, canonically: the method, the credential-masked URL and, where the body
 *  is what distinguishes two calls (IMDb's GraphQL POST), a fingerprint of it. The SAME key the
 *  recorded enrichment trees and the remembered-verdict cache use, so an observation, a fixture
 *  and a cached verdict of one request are one key. */
final case class LookupQuery(key: String) {

  /** The service asked, for reporting — derived from the key, never a per-source table. */
  def host: String = key.split(' ') match {
    case Array(_, url, _*) => Try(Option(new URI(url).getHost)).toOption.flatten.getOrElse("")
    case _                 => ""
  }

  /** Whether this is evidence the identity resolver reads: a venue's detail, or a request under
   *  TMDB's API — the one external client `TmdbIdentityLookups` asks, and so the one the capture
   *  observes (`identityLookupFetch`). What the unscoped capture filed from every other client —
   *  rating pages above all — is not, and `PurgeNonIdentityObservations` removes it. */
  def isIdentityEvidence: Boolean = key.split(' ') match {
    case Array(LookupQuery.DetailMethod, _*) => true
    case Array(_, url, _*)                   => url.startsWith(s"${clients.TmdbClient.ApiBase}/")
    case _                                   => false
  }
}

object LookupQuery {
  def of(method: String, url: String, body: Option[String] = None): LookupQuery = {
    val masked = RedactedUrl(url)
    LookupQuery(body.fold(s"$method $masked")(text => s"$method $masked ${Integer.toHexString(text.hashCode)}"))
  }

  /** A venue's per-film detail answer, which reaches the resolver PARSED (`DetailEnricher`)
   *  rather than as one HTTP body: some venues assemble a detail from several requests. */
  def venueDetail(venue: String, page: String): LookupQuery = LookupQuery(s"$DetailMethod $page $venue")

  private[observations] val DetailMethod = "DETAIL"
}

/** What a request came back with: a body, raw bytes, or a failure (with its HTTP status when it
 *  had one). */
sealed trait LookupAnswer {

  /** Whether this answer says something about the REQUEST rather than about the moment: a body,
   *  or a failure whose status describes the URL (404, 410). A timeout, a 5xx or a 429 is a
   *  failed read, and a failed read is not data — it never replaces a definitive answer. */
  def definitive: Boolean
}

object LookupAnswer {
  final case class Body(text: String) extends LookupAnswer { def definitive = true }

  final case class Bytes(base64: String) extends LookupAnswer {
    def definitive = true
    def bytes: Array[Byte] = Base64.getDecoder.decode(base64)
  }

  final case class Failed(status: Option[Int], method: String, message: String) extends LookupAnswer {
    def definitive: Boolean = status.exists(HttpStatusException.isDurable)
  }

  def ofBytes(bytes: Array[Byte]): Bytes = Bytes(Base64.getEncoder.encodeToString(bytes))

  /** What to keep of a failure. Status-bearing failures keep their code; anything else (a
   *  timeout, a reset socket) keeps its class name so a puzzling miss can be diagnosed later. */
  def failureOf(failure: Throwable, method: String): Failed = failure match {
    case status: HttpStatusException => Failed(Some(status.code), status.method, status.getMessage)
    case other                       => Failed(None, method, s"${other.getClass.getName}: ${other.getMessage}")
  }
}

/** One service answer, as fetched. `fetchedAt` is when this answer was first seen;
 *  `lastFetchedAt` when the same answer was last seen again. */
final case class LookupObservation(query: LookupQuery, answer: LookupAnswer, fetchedAt: Instant, lastFetchedAt: Instant)

/** One listing, as its venue published it. The listing's EVIDENCE — title, raw title, year,
 *  directors, runtime, page, original title and the rest of what the venue says about the film —
 *  without its showtimes: those change every day and are not evidence of which film it is
 *  (`cinema_scrapes` keeps the current ones; phase 4 keys them by `ListingKey`). */
final case class ListingObservation(key: ListingKey, cinema: Cinema, listing: CinemaMovie, observedAt: Instant,
                                    lastObservedAt: Instant)

object ListingObservation {

  /** The evidence half of a scraped listing. */
  def evidence(listing: CinemaMovie): CinemaMovie = listing.copy(showtimes = Nil)
}
