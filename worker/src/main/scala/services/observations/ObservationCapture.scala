package services.observations

import models.{Cinema, Source}
import play.api.Logging
import play.api.libs.json._
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.scrapes.{ArchivedScrape, BarrenAttempt, ScrapeArchiveRepository, ScrapeAttempt, SuccessfulScrape}
import tools.HttpFetch

import java.time.Instant
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/*
 * SHADOW CAPTURE — one decorator per seam, each generic over everything that passes through it:
 *
 *  - `ObservingHttpFetch`: every identity lookup (`identityLookupFetch`, the fetch under the TMDB
 *    client the resolver's `TmdbIdentityLookups` asks — never a rating page);
 *  - `ObservingDetailEnricher`: every venue's per-film detail;
 *  - `ObservingScrapeArchive`: every scraped listing.
 *
 * Each is INVISIBLE to the pipeline: the caller gets the same value, or the very exception, it
 * would have got without it, and a store that fails to write never fails the call it observes.
 * `ObservationCaptureSpec` and `ObservationCaptureEndToEndSpec` (the whole corpus, byte-identical
 * with capture on and off) hold them to that.
 */
private[observations] object Capture extends Logging {

  /** Run a store write, and never let it fail the observed call — a lost observation is only a
   *  gap the gate will report, a failed scrape or lookup would change what the pipeline does. */
  def safely(what: => String)(write: => Unit): Unit =
    try write
    catch { case NonFatal(e) => logger.warn(s"observation not recorded ($what): $e") }

  /** Evaluate `call`, hand its outcome to `observe`, and return or rethrow it untouched. */
  def observed[A](call: => A)(observe: Try[A] => Unit): A = {
    val outcome = Try(call)
    observe(outcome)
    outcome.get
  }
}

/** Every request through `underlying`, kept as a lookup observation. */
final class ObservingHttpFetch(underlying: HttpFetch, store: ObservationStore) extends HttpFetch {
  import Capture._

  override def get(url: String): String =
    observe("GET", url, None)(underlying.get(url))(LookupAnswer.Body.apply)

  // Headers are constant per client (a user agent, TMDB's bearer), so they are not part of the
  // query — the same rule the recorded trees and the verdict cache key by.
  override def get(url: String, headers: Map[String, String]): String =
    observe("GET", url, None)(underlying.get(url, headers))(LookupAnswer.Body.apply)

  override def getBytes(url: String): Array[Byte] =
    observe("BYTES", url, None)(underlying.getBytes(url))(LookupAnswer.ofBytes)

  override def post(url: String, body: String, contentType: String): String =
    observe("POST", url, Some(body))(underlying.post(url, body, contentType))(LookupAnswer.Body.apply)

  private def observe[A](method: String, url: String, body: Option[String])(call: => A)(encode: A => LookupAnswer): A =
    observed(call) { outcome =>
      val query = LookupQuery.of(method, url, body)
      safely(query.key)(store.observeLookup(query, outcome match {
        case Success(value)   => encode(value)
        case Failure(failure) => LookupAnswer.failureOf(failure, method)
      }))
    }
}

/** A venue's detail enricher, every detail it fetches kept as a lookup observation of
 *  `(venue, page)`. Everything else is the wrapped enricher's. */
final class ObservingDetailEnricher(underlying: DetailEnricher, store: ObservationStore) extends DetailEnricher {
  import Capture._

  override def cinema: Cinema                             = underlying.cinema
  override def detailGroup: String                        = underlying.detailGroup
  override def detailTarget: Source                       = underlying.detailTarget
  override def enrichmentServiceOverride: Option[String] = underlying.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = underlying.defersTmdbResolution

  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    observed(underlying.fetchFilmDetail(ref)) { outcome =>
      val query = LookupQuery.venueDetail(cinema.displayName, ref)
      safely(query.key)(store.observeLookup(query, outcome match {
        case Success(detail)  => LookupAnswer.Body(Json.stringify(Json.toJson(detail)(using ObservingDetailEnricher.optionFormat)))
        case Failure(failure) => LookupAnswer.failureOf(failure, "DETAIL")
      }))
    }
}

object ObservingDetailEnricher {
  private implicit val detailFormat: Format[FilmDetail] = Json.format[FilmDetail]
  private val optionFormat: Format[Option[FilmDetail]] = Format.optionWithNull[FilmDetail]

  /** The observed detail of `page` at `cinema`: `Right` the venue's answer (a detail, or none),
   *  `Left` the failure it answered with; `None` when it was never observed. */
  def detail(store: ObservationStore, cinema: Cinema, page: String): Option[Either[LookupAnswer.Failed, Option[FilmDetail]]] =
    store.lookup(LookupQuery.venueDetail(cinema.displayName, page)).map(_.answer).collect {
      case LookupAnswer.Body(json) => Right(Json.parse(json).as[Option[FilmDetail]](using optionFormat))
      case failed: LookupAnswer.Failed => Left(failed)
    }
}

/** The scrape archive, every listing of a scrape with content kept as a listing observation.
 *  Every read and every archiving rule is the wrapped archive's. */
final class ObservingScrapeArchive(underlying: ScrapeArchiveRepository, store: ObservationStore)
  extends ScrapeArchiveRepository {
  import Capture._

  override def enabled: Boolean = underlying.enabled

  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = {
    underlying.record(ScrapeAttempt(cinema, city, scrape.at, scrape.listingComplete, scrape.films))
    scrape.films.foreach(film => safely(s"${cinema.displayName} ${film.movie.title}")(store.observeListing(cinema, film)))
  }

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    underlying.record(ScrapeAttempt(cinema, city, attempt.at, listingComplete = true, films = Nil, error = attempt.error))

  override def find(cinema: Cinema): Option[ArchivedScrape]     = underlying.find(cinema)
  override def findAll(): Seq[ArchivedScrape]                   = underlying.findAll()
  override def lastContentAt(): Map[String, Option[Instant]]    = underlying.lastContentAt()
  override def close(): Unit                                    = underlying.close()
}
