package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import clients.TmdbClient
import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import integration.ProjectedMongoCorpus
import services.resolution.TmdbAttempt
import tools.{Env, Eventually, GetOnlyHttpFetch}

import java.time.{Instant, LocalDateTime}

/**
 * The prod path of the 2026-09-23 serving gap, end to end: `UnresolvedTmdbReaper`'s
 * `retryResolve` on a no-match row, TMDB answering "no match" again, the real cache and
 * the real projector on the real `movies` cursor.
 *
 * In prod the card was retired (`stream-row-unready`) the moment the retry dropped the
 * row's remembered miss, and came back only at the next 30-minute prune — Met Opera
 * events gone from ~200 US city pages several times a day. Asserts the card never leaves.
 *
 * Requires MONGODB_URI.
 */
class RetryResolveServingIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway(Env.fromProcess())

  private object NoMatchTmdb extends GetOnlyHttpFetch {
    override def get(url: String): String = """{"results":[]}"""
  }

  "a no-match row re-tried by the reaper" should "keep its card while TMDB answers no match again" in
    ProjectedMongoCorpus.withCorpus("retry_resolve_serving") { corpus =>
      import corpus._
      val title      = "__retry-resolve-serving__"
      val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
      val service    = new MovieService(cache, new InProcessEventBus(), new TmdbClient(http = NoMatchTmdb, apiKey = Some("stub")))
      val key        = cache.keyOf(title, None)
      val when       = LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)
      val removals   = new java.util.concurrent.atomic.AtomicInteger(0)

      cache.start()
      val projecting = repository.watchUpserts(projector.onMovieUpsert)
      // Every row the stream shows unready is a card the projector retires.
      val unreadySeen = repository.watchUpserts(r => if (!r.record.readyToProject && r.cacheKey(titleNormalizer) == key) removals.incrementAndGet())
      projecting should not be empty
      try {
        cache.put(key, MovieRecord(
          tmdbAttempt = Some(TmdbAttempt("fingerprint-of-the-first-miss", Instant.parse("2026-09-22T10:00:00Z"))),
          data = Map[Source, SourceData](Multikino -> SourceData(title = Some(title), showtimes = Seq(Showtime(when, None))))))
        val id = cache.idOf(key).getOrElse(fail("the seeded row has no id")).value
        def served: Boolean = readModel.findAllMovieIds().contains(id)
        withClue("the no-match row never reached the read model, so nothing below tests what it claims: ") {
          Eventually.poll(30000)(served) shouldBe true
        }

        service.retryResolve(title, None)
        service.drain()
        withClue("premise — TMDB was asked again and concluded the row as a fresh miss: ") {
          cache.get(key).flatMap(_.tmdbAttempt).map(_.at) should not be Some(Instant.parse("2026-09-22T10:00:00Z"))
        }
        withClue("the retry concluded as a miss again, but the row's card is not served: ") {
          Eventually.poll(30000)(served) shouldBe true
        }
        withClue("the retry made the row unready on the change stream, which retires its card: ") {
          removals.get() shouldBe 0
        }
      } finally {
        service.stop(); cache.stop()
        unreadySeen.foreach(_.close()); projecting.foreach(_.close())
      }
    }
}
