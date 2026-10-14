package services.sharecards

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument, UpdateOptions}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Indexes, Sorts, Updates}
import play.api.Logging
import services.MongoErrors
import services.facebook.FacebookFetches
import tools.{DaemonExecutors, LogThrottle}

import java.time.{Clock, Instant}
import java.util.Date
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/**
 * THE FLEET'S FACEBOOK RE-SCRAPES — one queue and one request quota shared by every country's
 * worker, because every one of them asks Facebook as the same app, and Facebook limits the app.
 *
 * A re-scrape used to be a per-country task asking for all of a film's city pages back to back:
 * spaced 10 s apart per FILM, so a film in 265 US cities was 265 requests in two minutes, and a
 * failure re-sent all of them. On 2026-09-25 one such film (Doctor Who: The Satan Pit) drew 668
 * HTTP 403s in two minutes and left DE and PL refused for the next hour.
 *
 * Now a film is one entry ([[RescrapeTarget.FilmPages]]); the drain expands it into one entry per
 * page ([[RescrapeTarget.Page]]), and each page is sent on its own slot of the fleet-wide quota
 * ([[FacebookRescrapeDrain.Spacing]] apart on average, whichever country sends). An entry already
 * waiting absorbs a second request for the same film or page, and waits for the later of the two. A page that fails retries alone; a rate
 * limit pushes the quota's next slot out for the whole fleet.
 */
sealed trait RescrapeTarget { def key: String }
object RescrapeTarget {
  /** A film whose pages are to be re-scraped — expanded into [[Page]]s when due. */
  final case class FilmPages(country: String, filmId: String) extends RescrapeTarget { val key = s"film|$country|$filmId" }
  /** One page URL. */
  final case class Page(url: String) extends RescrapeTarget { val key = s"page|$url" }
}

/** The two kinds of waiting entry, as the store filters on them. */
sealed abstract class RescrapeKind(val name: String)
object RescrapeKind {
  case object Film extends RescrapeKind("film")
  case object Page extends RescrapeKind("page")
  def of(target: RescrapeTarget): RescrapeKind = target match {
    case _: RescrapeTarget.FilmPages => Film
    case _: RescrapeTarget.Page      => Page
  }
}

/** A waiting re-scrape. `attempts` counts claims — what [[FacebookRescrapeStore.complete]] and
 *  [[FacebookRescrapeStore.retry]] match on, so a claim whose lease ran out and was claimed again
 *  cannot settle the newer claim's entry. */
final case class RescrapeEntry(country: String, target: RescrapeTarget, notBefore: Instant, attempts: Int = 0) {
  def key: String = target.key
  def kind: RescrapeKind = RescrapeKind.of(target)
}

/** Where the fleet's re-scrapes wait, and the quota they share. Storage only: what to do with an
 *  entry is [[FacebookRescrapeDrain]]'s. */
trait FacebookRescrapeStore {
  /** Add each entry unless one with its key is waiting already — that one is then due no sooner
   *  than either asked (a second request names a newer card, which needs its own wait); how many
   *  were added. */
  def add(entries: Seq[RescrapeEntry]): Int
  /** True when the country has an entry of `kind` due at `now`. */
  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean
  /** The country's oldest-due entry of `kind`, leased: held back until `now + lease` and its
   *  `attempts` raised — the entry as claimed. */
  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry]
  /** Remove a claimed entry — unless it was claimed again since. */
  def complete(claimed: RescrapeEntry): Unit
  /** Put a claimed entry back, due at `at`; `countAttempt = false` gives the claim back. */
  def retry(claimed: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit
  /** Take the quota's next slot, true when it was due. Slots are a SCHEDULE: the one after comes
   *  `spacing` after this one, or after `now - slack` if that is later — so a taker a few seconds
   *  late does not push every later slot back, and an idle quota banks at most `slack`. */
  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean
  /** No slot before `until`, whatever was due. */
  def holdSlots(until: Instant): Unit
  /** The country's waiting pages. */
  def waitingPages(country: String): Long
}

/**
 * The store in one Mongo collection of the fleet database: an entry per key (`_id`), and the
 * quota as the one document `_id: "quota"`. `add` is an upsert that only sets on insert, so an
 * entry already waiting is untouched; `takeSlot` is one conditional upsert — a slot not yet due
 * fails the filter, and the upsert's insert then collides with the quota's `_id`.
 */
class MongoFacebookRescrapeStore(collection: MongoCollection[Document]) extends FacebookRescrapeStore with Logging {
  import MongoFacebookRescrapeStore.*

  locally {
    val thread = new Thread(() => {
      // Equality, sort, range: `claim` asks for the OLDEST due entry, so `enqueuedAt` comes before
      // the `notBefore` range — that way the index walk is already in claim order and stops at the
      // first due key. With the range first (the index's first shape), every due entry was fetched
      // and sorted in memory on every claim.
      Try(Await.result(collection.createIndex(
        Indexes.ascending("country", "kind", "enqueuedAt", "notBefore")).toFuture(), Timeout))
        .fold(e => logger.warn(s"facebook_rescrapes index creation failed: ${e.getMessage}"),
              // The range-first order it replaced, kept by nothing: dropped only once the claim order is there.
              _ => services.MongoIndex.dropSuperseded(collection, SupersededClaimIndex, "facebook_rescrapes"))
    }, "facebook-rescrapes-init")
    thread.setDaemon(true)
    thread.start()
  }

  private def await[T](f: scala.concurrent.Future[T]): T = Await.result(f, Timeout)
  private def date(at: Instant) = new Date(at.toEpochMilli)

  def add(entries: Seq[RescrapeEntry]): Int = entries.count { entry =>
    val target = entry.target match {
      case RescrapeTarget.FilmPages(_, filmId) => Updates.setOnInsert("filmId", filmId)
      case RescrapeTarget.Page(url)            => Updates.setOnInsert("url", url)
    }
    await(collection.updateOne(Filters.eq("_id", entry.key), Updates.combine(
      Updates.setOnInsert("country", entry.country), Updates.setOnInsert("kind", entry.kind.name), target,
      Updates.max("notBefore", date(entry.notBefore)), Updates.setOnInsert("enqueuedAt", date(entry.notBefore)),
      Updates.setOnInsert("attempts", 0)), new UpdateOptions().upsert(true)).toFuture()).getUpsertedId != null
  }

  private def due(country: String, kind: RescrapeKind, now: Instant) =
    Filters.and(Filters.eq("country", country), Filters.eq("kind", kind.name), Filters.lte("notBefore", date(now)))

  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean =
    await(collection.find(due(country, kind, now)).limit(1).toFuture()).nonEmpty

  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry] =
    Option(await(collection.findOneAndUpdate(due(country, kind, now),
      Updates.combine(Updates.set("notBefore", date(now.plusMillis(lease.toMillis))), Updates.inc("attempts", 1)),
      new FindOneAndUpdateOptions().sort(Sorts.ascending("enqueuedAt")).returnDocument(ReturnDocument.AFTER)).toFutureOption()).orNull)
      .map(entryOf)

  private def claimed(entry: RescrapeEntry) = Filters.and(Filters.eq("_id", entry.key), Filters.eq("attempts", entry.attempts))

  def complete(entry: RescrapeEntry): Unit = { await(collection.deleteOne(claimed(entry)).toFuture()); () }

  def retry(entry: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit = {
    val due = Updates.set("notBefore", date(at))
    await(collection.updateOne(claimed(entry), if (countAttempt) due else Updates.combine(due, Updates.inc("attempts", -1))).toFuture())
    ()
  }

  // An update PIPELINE, so the next slot is computed from the one taken, in the same write:
  // nextSlot = max(nextSlot, now - slack) + spacing. On the first insert `$nextSlot` is missing,
  // which `$max` ranks below any date.
  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean =
    Try(await(collection.updateOne(
      Filters.and(Filters.eq("_id", QuotaId), Filters.lte("nextSlot", date(now))),
      Seq(Document("$set" -> Document("nextSlot" -> Document("$add" -> BsonArray(
        Document("$max" -> BsonArray(BsonString("$nextSlot"), BsonDateTime(now.toEpochMilli - slack.toMillis))),
        BsonInt64(spacing.toMillis)))))),
      new UpdateOptions().upsert(true)).toFuture())) match {
      case Success(_)                                                            => true
      case Failure(e: MongoWriteException) if MongoErrors.isDuplicateKey(e)      => false
      case Failure(e)                                                            => throw e
    }

  def holdSlots(until: Instant): Unit = {
    await(collection.updateOne(Filters.eq("_id", QuotaId), Updates.max("nextSlot", date(until)), new UpdateOptions().upsert(true)).toFuture())
    ()
  }

  def waitingPages(country: String): Long =
    await(collection.countDocuments(Filters.and(Filters.eq("country", country), Filters.eq("kind", RescrapeKind.Page.name))).toFuture())

  private def entryOf(d: Document): RescrapeEntry = {
    def text(field: String) = d.get(field).map(_.asString().getValue).getOrElse("")
    val country = text("country")
    val target  = if (text("kind") == RescrapeKind.Film.name) RescrapeTarget.FilmPages(country, text("filmId"))
                  else RescrapeTarget.Page(text("url"))
    RescrapeEntry(country, target, d.get("notBefore").map(v => Instant.ofEpochMilli(v.asDateTime().getValue)).getOrElse(Instant.EPOCH),
      d.get("attempts").map(_.asInt32().getValue).getOrElse(0))
  }
}

object MongoFacebookRescrapeStore {
  /** The collection, in the fleet database ([[services.FleetDatabase]]). */
  val Collection = "facebook_rescrapes"
  val QuotaId    = "quota"
  private val Timeout = 10.seconds
  /** The claim index's first shape, the `notBefore` range before `enqueuedAt`'s sort. */
  private val SupersededClaimIndex = "country_1_kind_1_notBefore_1_enqueuedAt_1"
}

/** What a share-card render asks for when a film's previews on Facebook are out of date. */
trait ShareCardRescrapes {
  /** Re-scrape the film's pages, no sooner than `notBefore`. Never throws: a render must not
   *  fail because the queue could not be written. */
  def request(filmId: String, notBefore: Instant): Unit
}

object ShareCardRescrapes {
  /** No re-scrapes — the worker has no Facebook credentials, or no fleet database. */
  def disabled(metrics: ShareCardMetrics): ShareCardRescrapes =
    (_: String, _: Instant) => metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Disabled)
}

/** A country's requests into the fleet queue: one [[RescrapeTarget.FilmPages]] entry per film. */
class FacebookRescrapeQueue(store: FacebookRescrapeStore, country: String) extends ShareCardRescrapes with Logging {
  def request(filmId: String, notBefore: Instant): Unit =
    Try(store.add(Seq(RescrapeEntry(country, RescrapeTarget.FilmPages(country, filmId), notBefore)))).failed.foreach { e =>
      logger.warn(s"share card: re-scrape of $filmId not queued: ${e.getClass.getSimpleName}: ${e.getMessage}")
    }
}

/**
 * A country's side of draining the fleet queue, one [[tick]] every [[FacebookRescrapeDrain.TickEvery]]
 * on its own daemon thread — never a task: a request waiting on the quota or on Facebook holds
 * no task-worker slot, so card renders never queue behind re-scrapes.
 *
 * Each tick expands the country's due films into their pages (no quota: that is a read-model
 * read) — only the pages Facebook holds a preview of ([[FacebookFetches]]) — then, when the
 * country has a page due and the fleet's next slot has come round, sends ONE page. Sent: gone,
 * and when Facebook says the app's hourly allowance is nearly spent the fleet waits
 * [[UsageHold]] before the next. Rate-limited: the whole fleet's quota waits [[RateLimitHold]] and
 * the page goes back without spending an attempt. Unavailable (unreachable, a 5xx): the same for
 * [[UnavailableHold]]. Refused: that page alone retries, backing off, and is dropped after
 * [[MaxAttempts]].
 */
class FacebookRescrapeDrain(store: FacebookRescrapeStore, graph: FacebookGraph, pages: String => Seq[String],
                            fetches: FacebookFetches, country: String, metrics: ShareCardMetrics, clock: Clock) extends services.Stoppable with Logging {
  import FacebookRescrapeDrain.*

  def tick(): Unit = {
    expandFilms()
    sendPage()
    metrics.rescrapesWaiting(store.waitingPages(country))
  }

  private def expandFilms(): Unit =
    Iterator.continually(store.claim(country, RescrapeKind.Film, clock.instant(), Lease)).take(FilmsPerTick)
      .takeWhile(_.isDefined).flatten.foreach { film =>
        val filmId = film.target match { case RescrapeTarget.FilmPages(_, id) => id; case other => other.key }
        Try(fetched(pages(filmId))) match {
          case Success(urls) =>
            store.add(urls.map(url => RescrapeEntry(country, RescrapeTarget.Page(url), clock.instant())))
            store.complete(film)
          case Failure(e) if film.attempts >= MaxAttempts =>
            logger.warn(s"share card: re-scrape of $filmId dropped, its pages could not be read ${film.attempts} times: ${e.getMessage}")
            store.complete(film)
          case Failure(e) =>
            logger.info(s"share card: re-scrape of $filmId deferred, its pages could not be read: ${e.getMessage}")
            store.retry(film, clock.instant().plusMillis(backoff(film.attempts).toMillis), countAttempt = true)
        }
      }

  /** Those of `urls` Facebook holds a preview of — fetched within its preview's lifetime; each
   *  other one counted, not asked for. */
  private def fetched(urls: Seq[String]): Seq[String] = {
    val previewed = fetches.fetchedSince(urls, clock.instant().minusMillis(FacebookFetches.PreviewLifetime.toMillis))
    val (kept, unfetched) = urls.partition(previewed)
    if (unfetched.nonEmpty) metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Unfetched, unfetched.size)
    kept
  }

  private def urlOf(page: RescrapeEntry): String = page.target match { case RescrapeTarget.Page(u) => u; case other => other.key }

  /** The country's oldest due page Facebook holds a preview of. Each one ahead of it that Facebook
   *  never fetched — queued before re-scrapes asked only for fetched pages, or fetched last before
   *  its preview expired — is dropped on the way, up to [[UnfetchedPerSlot]] in one slot. */
  @scala.annotation.tailrec
  private def nextFetchedPage(now: Instant, dropped: Int = 0): Option[RescrapeEntry] =
    if (dropped >= UnfetchedPerSlot) None
    else store.claim(country, RescrapeKind.Page, now, Lease) match {
      case Some(page) if fetched(Seq(urlOf(page))).isEmpty => store.complete(page); nextFetchedPage(now, dropped + 1)
      case found                                           => found
    }

  private def sendPage(): Unit = {
    val now = clock.instant()
    if (store.hasDue(country, RescrapeKind.Page, now) && store.takeSlot(now, Spacing, TickEvery))
      nextFetchedPage(now).foreach { page =>
        val url   = urlOf(page)
        val reply = graph.scrape(url)
        reply.usage.foreach(paceOn)
        reply.outcome match {
          case FacebookScrape.Accepted =>
            metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Sent)
            store.complete(page)
          case FacebookScrape.RateLimited(why) =>
            metrics.rescrape(ShareCardMetrics.RescrapeOutcome.RateLimited)
            val until = clock.instant().plusMillis(RateLimitHold.toMillis)
            logger.warn(s"share card: Facebook rate-limited the re-scrape of $url ($why) — every country waits until $until")
            store.holdSlots(until)
            store.retry(page, until, countAttempt = false)
          case FacebookScrape.Unavailable(why) =>
            metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
            val until = clock.instant().plusMillis(UnavailableHold.toMillis)
            logger.info(s"share card: Facebook unavailable for the re-scrape of $url ($why) — every country waits until $until")
            store.holdSlots(until)
            store.retry(page, until, countAttempt = false)
          case FacebookScrape.Refused(why) if page.attempts >= MaxAttempts =>
            metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
            logger.warn(s"share card: Facebook re-scrape of $url dropped after ${page.attempts} attempts: $why")
            store.complete(page)
          case FacebookScrape.Refused(why) =>
            metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
            logger.info(s"share card: Facebook re-scrape of $url failed: $why")
            store.retry(page, clock.instant().plusMillis(backoff(page.attempts).toMillis), countAttempt = true)
        }
      }
  }

  /** Facebook's word on how much of the app's hourly allowance is spent: at [[UsageCeiling]] the
   *  whole fleet waits [[UsageHold]], so the next request comes after some of the hour's calls
   *  have aged out rather than into a refusal and its hour's hold. */
  private def paceOn(usage: AppUsage): Unit = {
    metrics.appUsage(usage.percent)
    if (usage.percent >= UsageCeiling) {
      val until = clock.instant().plusMillis(UsageHold.toMillis)
      logger.info(s"share card: Facebook says ${usage.percent}% of the app's hourly allowance is spent — every country waits until $until")
      store.holdSlots(until)
    }
  }

  // One tick at a time on one thread; a failed tick (the fleet database unreachable) is logged
  // at most once per ten minutes and the next tick tries again.
  private val failures = new LogThrottle(10.minutes.toNanos)
  private var scheduler = Option.empty[ScheduledExecutorService]

  def start(): Unit = synchronized {
    if (scheduler.isEmpty) {
      val s = DaemonExecutors.scheduler(s"facebook-rescrapes-$country")
      s.scheduleWithFixedDelay(() => Try(tick()).failed.foreach { e =>
        failures.admit().foreach(suppressed => logger.warn(
          s"share card: re-scrape drain failed (${e.getClass.getSimpleName}: ${e.getMessage}); $suppressed more since the last report"))
      }, TickEvery.toMillis, TickEvery.toMillis, TimeUnit.MILLISECONDS)
      scheduler = Some(s)
    }
  }

  def stop(): Unit = synchronized { scheduler.foreach(_.shutdownNow()); scheduler = None }
}

object FacebookRescrapeDrain {
  /** The fleet's pace: one request every 20 s across every country on average, never two closer
   *  than 15 s (the slot schedule's [[TickEvery]] of slack) — 180 an hour, ABOVE what Facebook
   *  allows: it refused the app (code 4) after 45-105 requests each hour on 2026-10-11/12, and each
   *  refusal held the fleet an hour. So the pace is Facebook's own word on the allowance
   *  ([[UsageCeiling]]); demand is kept under it by re-scraping only a card whose picture moved
   *  beyond its rating badges ([[ShareCardService.render]]), and only on pages Facebook fetched. */
  val Spacing: FiniteDuration = 20.seconds
  /** How often each country looks for due work. Under [[Spacing]], so one country alone keeps up
   *  with the quota. */
  val TickEvery: FiniteDuration = 5.seconds
  /** A claimed entry is held back this long — so an entry whose worker died mid-request is
   *  claimed again, and never while its request (20 s timeout) may still be running. */
  val Lease: FiniteDuration = 5.minutes
  /** How long the whole fleet waits after Facebook names its rate limit. The limit is a rolling
   *  hour; on 2026-09-25 requests were refused for about that long. */
  val RateLimitHold: FiniteDuration = 1.hour
  /** How long the whole fleet waits after Facebook could not be reached or answered a 5xx. */
  val UnavailableHold: FiniteDuration = 5.minutes
  /** Refusals (or unreadable pages) before an entry is given up. */
  val MaxAttempts: Int = 5
  /** Films expanded per tick, so a backlog of films cannot hold one tick for long. */
  val FilmsPerTick: Int = 20

  /** Pages Facebook never fetched dropped in one slot before it is given up: the backlog queued
   *  before re-scrapes asked only for fetched pages goes in a few minutes, not a slot a page. */
  val UnfetchedPerSlot: Int = 100

  /** The share of the app's hourly allowance (Facebook's `X-App-Usage`) at which the fleet stops
   *  for [[UsageHold]] rather than run on into Facebook's refusal and its hour-long hold. */
  val UsageCeiling: Int = 90
  /** How long the fleet waits once Facebook's usage reaches [[UsageCeiling]]: the allowance is a
   *  rolling hour, so a few minutes frees a few of its calls. */
  val UsageHold: FiniteDuration = 5.minutes

  /** A refused page's wait before its next attempt: 1, 2, 4, 8 minutes. */
  def backoff(attempts: Int): FiniteDuration = 1.minute * (1L << (attempts - 1).max(0).min(10))
}
