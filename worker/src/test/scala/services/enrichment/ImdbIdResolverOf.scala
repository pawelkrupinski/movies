package services.enrichment

import models.MovieRecord
import services.movies.{LatestTitleYear, MovieCache}
import services.resolution.ResolutionCache

import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.duration.FiniteDuration

/** The IMDb id resolver as the composition root (`ResolutionWiring`) builds it — its ladder's rungs, the recovery over
 *  the cache, the async resolver over that — for the specs, each piece reachable: the bus listener and the drain, a
 *  resolution on the spec's thread (`resolveSync`), and the ladder alone (`lookupId`, a row that need not be cached). */
final class ImdbIdResolverOf private (val resolver: ImdbIdResolver, recovery: ImdbIdRecovery, ladder: ImdbIdLadder)(using LatestTitleYear) {
  def onImdbIdMissing: PartialFunction[services.events.DomainEvent, Unit] = resolver.onImdbIdMissing
  def drain(): Unit = resolver.drain()
  def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit = recovery.resolve(title, year, searchTitle)
  def lookupId(searchTitle: String, year: Option[Int], record: MovieRecord = MovieRecord()): Option[String] =
    ladder.lookupId(ImdbIdQuery.of(searchTitle, year, record))
}

object ImdbIdResolverOf {
  def apply(cache: MovieCache, imdb: ImdbClient, executionContext: ExecutionContextExecutorService,
            imdbIdCache: ResolutionCache = ResolutionCache.passthrough, wikidata: Option[WikidataClient] = None,
            letterboxdIdResolver: Option[LetterboxdIdResolver] = None, omdb: Option[OMDbClient] = None,
            cinemeta: Option[CinemetaClient] = None, retries: ScheduledExecutorService, retriesFailures: Boolean = true,
            random: () => Double = () => scala.util.Random.nextDouble(), stopwatch: tools.Stopwatch,
            sleep: FiniteDuration => Unit)(using LatestTitleYear): ImdbIdResolverOf = {
    val ladder   = new ImdbIdLadder(new ImdbSuggestions(imdb, imdbIdCache, cache.normalizer),
      new ImdbCrossReferences(wikidata, letterboxdIdResolver, omdb, cinemeta))
    val recovery = new ImdbIdRecovery(cache, ladder)
    new ImdbIdResolverOf(new ImdbIdResolver(recovery, executionContext, retries, retriesFailures, random, stopwatch, sleep), recovery, ladder)
  }
}
