package services.identity

import org.bson.{BsonDocument, BsonInt64}
import play.api.Logging
import services.observations.{LookupAnswer, ObservationStore}
import tools.HttpStatusException

import scala.util.{Failure, Success}

/**
 * Fills the normalized TMDB store once from the raw answers the observation store already holds —
 * TMDB's and IMDb's, streamed a page at a time — so the first model to read the store finds what the
 * raw capture gathered instead of asking TMDB for all of it again. Marked done in the store; every
 * later answer arrives through the normalizer as it is fetched.
 */
final class TmdbStoreBackfill(observations: ObservationStore, normalizer: TmdbNormalizer, docs: TmdbDocuments,
                              clock: java.time.Clock) extends Logging {
  import TmdbStoreBackfill._

  def ensure(): Unit = if (docs.get(TmdbKind.Query, Seq(Marker)).isEmpty) {
    val started = System.nanoTime()
    var count   = 0L
    Prefixes.foreach(prefix => observations.eachCurrentLookup(prefix) { page =>
      page.foreach { o =>
        val url = o.query.key.split(' ') match { case Array(_, u, _*) => u; case _ => "" }
        o.answer match {
          case LookupAnswer.Body(body)                  => normalizer.filed("GET", url, Success(body)); count += 1
          case LookupAnswer.Failed(Some(404), method, _) => normalizer.filed("GET", url, Failure(new HttpStatusException(404, method, url, None))); count += 1
          case _                                        => ()
        }
      }
    })
    docs.put(TmdbKind.Query, Seq(Marker -> new BsonDocument("at", BsonInt64(clock.millis())).append("answers", BsonInt64(count))))
    logger.info(f"identity store: backfilled from $count%d observed answers in ${(System.nanoTime() - started) / 1e9}%.0fs")
  }
}

object TmdbStoreBackfill {
  val Marker   = "meta|backfilled"
  val Prefixes = Seq(s"GET ${clients.TmdbClient.ApiBase}/", s"GET ${services.enrichment.ImdbClient.SuggestionBase}/")
}
