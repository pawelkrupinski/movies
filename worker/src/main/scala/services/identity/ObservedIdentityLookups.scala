package services.identity

import clients.TmdbClient
import models.{Cinema, Source}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.observations.{LookupAnswer, LookupQuery, ObservationStore, ObservingDetailEnricher}
import tools.{HttpFetch, HttpStatusException}

import java.util.concurrent.atomic.AtomicLong
import scala.util.control.NoStackTrace

/*
 * The shadow resolver's lookup source in production (docs/design/identity-resolver.md §8, "it
 * issues NO new lookups in prod"): the very `TmdbIdentityLookups` the offline harness resolves
 * with, over a TMDB client and venue details answered ONLY from the observation store. There is
 * no network beneath it — not a fetch that is switched off, but none at all — so the shadow run
 * cannot reach an external service however it is configured.
 *
 * A question the store holds no live DEFINITIVE answer to (never observed, or observed only as a
 * transient failure — a failed read is not data) is a GAP: counted, and turned by
 * `TmdbIdentityLookups` into `Answer.Unknown`, never into "no film". A definitive failure (404,
 * 410) is answered as the service answered it. Every read renews what it read (§9a retention):
 * evidence a live listing still needs stays as long as it is needed.
 */

/** The questions one resolve found unanswered, counted by KIND — the method, host and path with
 *  ids and query stripped (`GET api.themoviedb.org/3/search/movie`, `DETAIL`) — so a tick says
 *  which service its Unknowns wait on. */
final class ObservationGaps {
  private val count  = new AtomicLong()
  private val kinds  = new java.util.concurrent.ConcurrentHashMap[String, AtomicLong]()
  def record(query: LookupQuery): Unit = {
    count.incrementAndGet()
    kinds.computeIfAbsent(ObservationGaps.kindOf(query), _ => new AtomicLong()).incrementAndGet()
    ()
  }
  def total: Long = count.get()
  def byKind: Map[String, Long] = { import scala.jdk.CollectionConverters._; kinds.asScala.view.mapValues(_.get).toMap }
}

object ObservationGaps {
  def kindOf(query: LookupQuery): String = query.key.split(' ') match {
    case Array("DETAIL", _*)    => "DETAIL"
    case Array(method, url, _*) =>
      val u = scala.util.Try(new java.net.URI(url)).toOption
      s"$method ${u.flatMap(x => Option(x.getHost)).getOrElse("")}${u.flatMap(x => Option(x.getPath)).getOrElse("").replaceAll("/\\d{2,}", "/{id}")}"
    case _ => query.key
  }
}

/** Thrown for a gap: the caller sees a failed call, and the gap count says it was not a failure. */
final class ObservationGap(query: String) extends RuntimeException(s"not observed: $query") with NoStackTrace

/** Every request answered from the store's lookup observations, keyed exactly as
 *  `ObservingHttpFetch` filed them. */
final class ObservedHttpFetch(store: ObservationStore, gaps: ObservationGaps) extends HttpFetch {

  override def get(url: String): String = text("GET", url, None)
  // Headers are constant per client and not part of the key (as `ObservingHttpFetch` files it).
  override def get(url: String, headers: Map[String, String]): String = text("GET", url, None)
  override def post(url: String, body: String, contentType: String): String = text("POST", url, Some(body))
  override def getBytes(url: String): Array[Byte] = answer("BYTES", url, None) match {
    case b: LookupAnswer.Bytes => b.bytes
    case _                     => gap(LookupQuery.of("BYTES", url))
  }

  private def text(method: String, url: String, body: Option[String]): String = answer(method, url, body) match {
    case LookupAnswer.Body(value) => value
    case _                        => gap(LookupQuery.of(method, url, body))
  }

  /** The live definitive answer; a definitive failure is thrown as the service threw it. */
  private def answer(method: String, url: String, body: Option[String]): LookupAnswer = {
    val query = LookupQuery.of(method, url, body)
    store.lookup(query).map(_.answer).filter(_.definitive) match {
      case Some(LookupAnswer.Failed(Some(status), failedMethod, _)) => throw new HttpStatusException(status, failedMethod, url, None)
      case Some(answer)                                             => answer
      case None                                                     => gap(query)
    }
  }

  private def gap(query: LookupQuery): Nothing = { gaps.record(query); throw new ObservationGap(query.key) }
}

/** A venue's detail answered from the store's `DETAIL <page> <venue>` observations. Everything
 *  but the fetch is the wrapped enricher's; the wrapped enricher's own fetch is never called. */
final class ObservedDetailEnricher(underlying: DetailEnricher, store: ObservationStore, gaps: ObservationGaps)
    extends DetailEnricher {

  override def cinema: Cinema                             = underlying.cinema
  override def detailGroup: String                        = underlying.detailGroup
  override def detailTarget: Source                       = underlying.detailTarget
  override def enrichmentServiceOverride: Option[String] = underlying.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = underlying.defersTmdbResolution

  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    ObservingDetailEnricher.detail(store, cinema, ref) match {
      case Some(Right(detail)) => detail
      // The venue answered with a failure: the offline source's call would have thrown too.
      case Some(Left(failed))  => throw new IllegalStateException(s"${cinema.displayName} detail $ref: ${failed.message}")
      case None                =>
        val query = LookupQuery.venueDetail(cinema.displayName, ref)
        gaps.record(query); throw new ObservationGap(query.key)
    }
}

object ObservedIdentityLookups {

  /** The resolver's lookups over `store` alone, with a fresh gap count — one per resolve. `tmdb`
   *  builds the deployment's TMDB client (key, language) over a fetch, the one the pipeline's
   *  observed client was built by, so this one asks exactly the requests that client filed. No
   *  answer here is transient, so the client never waits to retry. */
  def over(store: ObservationStore, tmdb: HttpFetch => TmdbClient, enrichers: Seq[DetailEnricher]): (IdentityLookups, ObservationGaps) = {
    val gaps = new ObservationGaps
    (new TmdbIdentityLookups(tmdb(new ObservedHttpFetch(store, gaps)), enrichers.map(new ObservedDetailEnricher(_, store, gaps)),
      () => gaps.total), gaps)
  }
}
