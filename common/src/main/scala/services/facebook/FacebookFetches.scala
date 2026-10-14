package services.facebook

import com.mongodb.client.model.{BulkWriteOptions, UpdateOneModel, UpdateOptions}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Projections, Updates}
import services.{MongoTtlIndex, TtlIndexMismatches}

import java.time.Instant
import java.util.Date
import scala.concurrent.Await
import scala.concurrent.duration.*

/**
 * THE FILM PAGES FACEBOOK HAS FETCHED, and when — the web records each fetch by Facebook's
 * crawler, the worker's re-scrapes read them.
 *
 * Facebook holds a preview only of a page it has fetched: one somebody shared, or one the app
 * asked it to re-scrape. A re-scrape of any other page refreshes nothing — the first share
 * fetches the page as it is then — and CREATES a preview that every later card change then has
 * to re-scrape. On 2026-10-12 Germany's first week queued ~4,100 city pages for re-scrape while
 * Facebook had fetched 30-140 of its film pages a day, so the fleet's re-scrapes ask only for the
 * pages Facebook fetched within [[FacebookFetches.PreviewLifetime]].
 *
 * Keyed by the page's URL as [[models.CityPath.filmUrl]] builds it, on both sides.
 */
trait FacebookFetches {
  /** Record each page as fetched at its instant; a page recorded later keeps the later fetch. */
  def record(fetched: Map[String, Instant]): Unit
  /** Those of `urls` Facebook fetched at or after `since`. */
  def fetchedSince(urls: Seq[String], since: Instant): Set[String]
}

object FacebookFetches {
  /** The collection, in the fleet database ([[services.FleetDatabase]]). */
  val Collection = "facebook_fetches"

  /** How long Facebook keeps a page's preview before fetching the page again by itself, which
   *  is also how long a fetch is kept: a page Facebook last fetched before that has no preview
   *  worth refreshing. */
  val PreviewLifetime: FiniteDuration = 30.days

  /** Facebook's link-preview crawler, by its user agent. */
  def isFacebookCrawler(userAgent: String): Boolean =
    userAgent.contains("facebookexternalhit") || userAgent.contains("Facebot")
}

/** [[FacebookFetches]] in one collection of the fleet database: a document per page (`_id` its
 *  URL) holding the latest fetch, expired [[FacebookFetches.PreviewLifetime]] after it. */
class MongoFacebookFetches(collection: MongoCollection[Document]) extends FacebookFetches {
  import MongoFacebookFetches.*

  private def date(at: Instant) = new Date(at.toEpochMilli)

  def record(fetched: Map[String, Instant]): Unit =
    if (fetched.nonEmpty) {
      val upserts = fetched.toSeq.map { case (url, at) =>
        new UpdateOneModel[Document](Filters.eq("_id", url), Updates.max(FetchedAt, date(at)), new UpdateOptions().upsert(true))
      }
      Await.result(collection.bulkWrite(upserts, new BulkWriteOptions().ordered(false)).toFuture(), Timeout)
      ()
    }

  def fetchedSince(urls: Seq[String], since: Instant): Set[String] =
    urls.distinct.grouped(LookupBatch).flatMap { batch =>
      Await.result(collection.find(Filters.and(Filters.in("_id", batch*), Filters.gte(FetchedAt, date(since))))
        .projection(Projections.include("_id")).batchSize(tools.MongoReplies.Default).toFuture(), Timeout).flatMap(_.get("_id").map(_.asString().getValue))
    }.toSet

  /** Expire each page [[FacebookFetches.PreviewLifetime]] after its latest fetch. Asked of the
   *  worker's wiring, which owns the fleet database's indexes; the web only writes. */
  def ensureExpiry(mismatches: TtlIndexMismatches): Unit =
    MongoTtlIndex.reconcile(collection, FetchedAt, FacebookFetches.PreviewLifetime.toSeconds, "facebook fetches", mismatches)
}

object MongoFacebookFetches {
  private val FetchedAt   = "fetchedAt"
  private val Timeout     = 10.seconds
  /** URLs per lookup: a film shows in at most a few hundred cities. */
  private val LookupBatch = 500
}
