package services.identity

import clients.TmdbClient
import models.{Cinema, Source}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.observations.ObservationStore
import tools.HttpFetch

/*
 * The identity projection's lookups in a cut-over country (docs/design/identity-resolver.md §8,
 * phase 5): the very `TmdbIdentityLookups` the shadow run resolves with, answered from the
 * observation store FIRST and from the live service only for a question the store holds no live
 * definitive answer to. The live side is the pipeline's own (the observed `lookupFetch`, the
 * observed detail enrichers), so an answer asked live is filed and the next projection reads it
 * from the store: a resolve asks each question once per observation lifetime, not once per tick.
 * A no-match is simply such an answer, re-asked when it expires (§2.1).
 */

/** `observed`, and `live` for what it has not observed. */
final class ObservedFirstHttpFetch(store: ObservationStore, live: HttpFetch) extends HttpFetch {
  private val observed = new ObservedHttpFetch(store, new ObservationGaps)

  private def firstObserved[A](read: HttpFetch => A): A =
    try read(observed) catch { case _: ObservationGap => read(live) }

  override def get(url: String): String = firstObserved(_.get(url))
  override def get(url: String, headers: Map[String, String]): String = firstObserved(_.get(url, headers))
  override def post(url: String, body: String, contentType: String): String = firstObserved(_.post(url, body, contentType))
  override def getBytes(url: String): Array[Byte] = firstObserved(_.getBytes(url))
}

/** `live`'s detail, answered from the store when it has observed it. */
final class ObservedFirstDetailEnricher(live: DetailEnricher, store: ObservationStore) extends DetailEnricher {
  private val observed = new ObservedDetailEnricher(live, store, new ObservationGaps)

  override def cinema: Cinema                             = live.cinema
  override def detailGroup: String                        = live.detailGroup
  override def detailTarget: Source                       = live.detailTarget
  override def enrichmentServiceOverride: Option[String] = live.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = live.defersTmdbResolution

  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    try observed.fetchFilmDetail(ref) catch { case _: ObservationGap => live.fetchFilmDetail(ref) }
}

object CutoverIdentityLookups {

  /** The projection's lookups: observed first when a store is wired, else the live source alone. */
  def over(store: Option[ObservationStore], tmdb: HttpFetch => TmdbClient, live: HttpFetch, enrichers: Seq[DetailEnricher]): IdentityLookups =
    store match {
      case Some(s) => new TmdbIdentityLookups(tmdb(new ObservedFirstHttpFetch(s, live)), enrichers.map(new ObservedFirstDetailEnricher(_, s)))
      case None    => new TmdbIdentityLookups(tmdb(live), enrichers)
    }
}
