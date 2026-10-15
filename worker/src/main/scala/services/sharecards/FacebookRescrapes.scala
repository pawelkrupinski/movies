package services.sharecards

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument, UpdateOptions}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Indexes, Sorts, Updates}
import play.api.Logging
import services.MongoErrors
import services.facebook.{AppUsage, FacebookFetches, FacebookSlots, FacebookUsageBrake, MongoFacebookSlots}
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
  /** One page URL; `previewed` when Facebook fetched it within its preview's lifetime
   *  ([[services.facebook.FacebookFetches]]) — so holds a preview a card change made stale — and
   *  sent before every page it did not. One entry per URL, whichever: a page waiting as never
   *  fetched becomes previewed when it is asked for as previewed, and never goes back. */
  final case class Page(url: String, previewed: Boolean = true) extends RescrapeTarget { val key = s"page|$url" }
}

/** The kinds of waiting entry, as the store filters on them. */
sealed abstract class RescrapeKind(val name: String)
object RescrapeKind {
  case object Film extends RescrapeKind("film")
  /** A page Facebook holds a preview of. */
  case object Page extends RescrapeKind("page")
  /** A page Facebook never fetched: sent with what the allowance leaves after [[Page]]s. */
  case object UnfetchedPage extends RescrapeKind("unfetched_page")
  /** The page kinds, in the order the drain sends them. */
  val Pages: Seq[RescrapeKind] = Seq(Page, UnfetchedPage)
  def of(target: RescrapeTarget): RescrapeKind = target match {
    case _: RescrapeTarget.FilmPages                  => Film
    case RescrapeTarget.Page(_, previewed) if previewed => Page
    case _: RescrapeTarget.Page                       => UnfetchedPage
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
trait FacebookRescrapeStore extends FacebookSlots {
  /** Add each entry unless one with its key is waiting already — that one is then due no sooner
   *  than either asked (a second request names a newer card, which needs its own wait); how many
   *  were added. */
  def add(entries: Seq[RescrapeEntry]): Int
  /** True when the country has an entry of `kind` due at `now`. */
  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean
  /** The country's oldest-due entry of `kind`, leased: held back until `now + lease` and its
   *  `attempts` raised — the entry as claimed. */
  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry]
  /** Remove a claimed entry — unless it was claimed again since, or asked for again since it was
   *  claimed: a request that lands while its page is being sent names a newer card than the one
   *  Facebook fetched, so the entry waits again, due when that request asked, its attempts reset. */
  def complete(claimed: RescrapeEntry): Unit
  /** Put a claimed entry back, due at `at`; `countAttempt = false` gives the claim back. */
  def retry(claimed: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit
  /** Take the quota's next slot, true when it was due. Slots are a SCHEDULE: the one after comes
   *  `spacing` after this one, or after `now - slack` if that is later — so a taker a few seconds
   *  late does not push every later slot back, and an idle quota banks at most `slack`. */
  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean
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
      case RescrapeTarget.Page(url, _)         => Updates.setOnInsert("url", url)
    }
    await(collection.updateOne(Filters.eq("_id", entry.key), Updates.combine(
      Updates.setOnInsert("country", entry.country), kindOf(entry.kind), target,
      Updates.max("notBefore", date(entry.notBefore)), Updates.setOnInsert("enqueuedAt", date(entry.notBefore)),
      Updates.setOnInsert("attempts", 0), Updates.max(AskedAgainAt, date(entry.notBefore))),
      new UpdateOptions().upsert(true)).toFuture()).getUpsertedId != null
  }

  // A page asked for as previewed is one from then on, whatever it waited as: Facebook fetched it
  // since it was queued. Never the other way — a preview does not go away with one request.
  private def kindOf(kind: RescrapeKind) =
    if (kind == RescrapeKind.Page) Updates.set("kind", kind.name) else Updates.setOnInsert("kind", kind.name)

  private def due(country: String, kind: RescrapeKind, now: Instant) =
    Filters.and(Filters.eq("country", country), Filters.eq("kind", kind.name), Filters.lte("notBefore", date(now)))

  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean =
    await(collection.find(due(country, kind, now)).limit(1).toFuture()).nonEmpty

  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry] =
    Option(await(collection.findOneAndUpdate(due(country, kind, now),
      Updates.combine(Updates.set("notBefore", date(now.plusMillis(lease.toMillis))), Updates.inc("attempts", 1), Updates.unset(AskedAgainAt)),
      new FindOneAndUpdateOptions().sort(Sorts.ascending("enqueuedAt")).returnDocument(ReturnDocument.AFTER)).toFutureOption()).orNull)
      .map(entryOf)

  private def claimed(entry: RescrapeEntry) = Filters.and(Filters.eq("_id", entry.key), Filters.eq("attempts", entry.attempts))

  // `askedAgainAt` is the latest due time asked for since the entry was last claimed (set by every
  // `add`, cleared by `claim`): present at completion, the entry waits again instead of going.
  def complete(entry: RescrapeEntry): Unit =
    if (await(collection.deleteOne(Filters.and(claimed(entry), Filters.exists(AskedAgainAt, false))).toFuture()).getDeletedCount == 0) {
      await(collection.updateOne(Filters.and(claimed(entry), Filters.exists(AskedAgainAt)), Seq(
        Document("$set" -> Document("notBefore" -> s"$$$AskedAgainAt", "attempts" -> 0)),
        Document("$unset" -> AskedAgainAt))).toFuture())
      ()
    }

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
      Filters.and(Filters.eq("_id", MongoFacebookSlots.QuotaId), Filters.lte("nextSlot", date(now))),
      Seq(Document("$set" -> Document("nextSlot" -> Document("$add" -> BsonArray(
        Document("$max" -> BsonArray(BsonString("$nextSlot"), BsonDateTime(now.toEpochMilli - slack.toMillis))),
        BsonInt64(spacing.toMillis)))))),
      new UpdateOptions().upsert(true)).toFuture())) match {
      case Success(_)                                                            => true
      case Failure(e: MongoWriteException) if MongoErrors.isDuplicateKey(e)      => false
      case Failure(e)                                                            => throw e
    }

  private val slots = new MongoFacebookSlots(collection)
  def holdSlots(until: Instant): Unit = slots.holdSlots(until)

  def waitingPages(country: String): Long =
    await(collection.countDocuments(Filters.and(Filters.eq("country", country), Filters.in("kind", RescrapeKind.Pages.map(_.name)*))).toFuture())

  private def entryOf(d: Document): RescrapeEntry = {
    def text(field: String) = d.get(field).map(_.asString().getValue).getOrElse("")
    val country = text("country")
    val target  = if (text("kind") == RescrapeKind.Film.name) RescrapeTarget.FilmPages(country, text("filmId"))
                  else RescrapeTarget.Page(text("url"), previewed = text("kind") != RescrapeKind.UnfetchedPage.name)
    RescrapeEntry(country, target, d.get("notBefore").map(v => Instant.ofEpochMilli(v.asDateTime().getValue)).getOrElse(Instant.EPOCH),
      d.get("attempts").map(_.asInt32().getValue).getOrElse(0))
  }
}

object MongoFacebookRescrapeStore {
  private val AskedAgainAt = "askedAgainAt"
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
 * read), then — when the country has a page due and the fleet's next slot has come round — sends
 * ONE page: one Facebook holds a preview of ([[FacebookFetches]]) before any it never fetched. Sent: gone,
 * and when Facebook says the app's hourly allowance is nearly spent the fleet waits
 * ([[FacebookUsageBrake]]) before the next. Rate-limited: the whole fleet's quota waits [[RateLimitHold]] and
 * the page goes back without spending an attempt. Unavailable (unreachable, a 5xx): the same for
 * [[UnavailableHold]]. Refused: that page alone retries, backing off, and is dropped after
 * [[MaxAttempts]].
 */
class FacebookRescrapeDrain(store: FacebookRescrapeStore, graph: FacebookGraph, pages: String => Seq[String],
                            fetches: FacebookFetches, country: String, metrics: ShareCardMetrics, clock: Clock) extends services.Stoppable with Logging {
  import FacebookRescrapeDrain.*

  private val brake = new FacebookUsageBrake(store, clock)

  def tick(): Unit = {
    expandFilms()
    sendPage()
    metrics.rescrapesWaiting(store.waitingPages(country))
  }

  private def expandFilms(): Unit =
    Iterator.continually(store.claim(country, RescrapeKind.Film, clock.instant(), Lease)).take(FilmsPerTick)
      .takeWhile(_.isDefined).flatten.foreach { film =>
        val filmId = film.target match { case RescrapeTarget.FilmPages(_, id) => id; case other => other.key }
        Try(previewed(pages(filmId))) match {
          case Success(urls) =>
            store.add(urls.map { case (url, seen) => RescrapeEntry(country, RescrapeTarget.Page(url, seen), clock.instant()) })
            store.complete(film)
          case Failure(e) if film.attempts >= MaxAttempts =>
            logger.warn(s"share card: re-scrape of $filmId dropped, its pages could not be read ${film.attempts} times: ${e.getMessage}")
            store.complete(film)
          case Failure(e) =>
            logger.info(s"share card: re-scrape of $filmId deferred, its pages could not be read: ${e.getMessage}")
            store.retry(film, clock.instant().plusMillis(backoff(film.attempts).toMillis), countAttempt = true)
        }
      }

  /** Each of `urls` with whether Facebook holds a preview of it — fetched within its preview's lifetime. */
  private def previewed(urls: Seq[String]): Seq[(String, Boolean)] = {
    val seen = fetches.fetchedSince(urls, clock.instant().minusMillis(FacebookFetches.PreviewLifetime.toMillis))
    urls.map(url => url -> seen(url))
  }

  private def urlOf(page: RescrapeEntry): String = page.target match { case RescrapeTarget.Page(u, _) => u; case other => other.key }

  private def sendPage(): Unit = {
    val now = clock.instant()
    // The pages Facebook holds a preview of first; the ones it never fetched with what is left.
    for {
      kind <- RescrapeKind.Pages.find(store.hasDue(country, _, now)) if store.takeSlot(now, Spacing, TickEvery)
      page <- store.claim(country, kind, now, Lease)
    } {
      val url   = urlOf(page)
      val reply = graph.scrape(url)
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
      // After the page's own outcome is stored: a hold the fleet database refuses fails the tick,
      // and must not leave a page Facebook accepted claimed, to be sent again.
      reply.usage.foreach(paceOn)
    }
  }

  /** Facebook's word on how much of the app's hourly allowance is spent, acted on by the fleet's
   *  [[FacebookUsageBrake]] — the same one the web's Facebook sign-ins report to. */
  private def paceOn(usage: AppUsage): Unit = {
    metrics.appUsage(usage.percent)
    brake.observe(usage).foreach(until =>
      logger.info(s"share card: Facebook says ${usage.percent}% of the app's hourly allowance is spent — every country waits until $until"))
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
   *  ([[FacebookUsageBrake.UsageCeiling]]); demand is kept under it by re-scraping only a card whose picture moved
   *  beyond its rating badges ([[ShareCardService.render]]); the pages Facebook holds a preview of go
   *  first, so a burst of new films' pages waits behind them. */
  val Spacing: FiniteDuration = 20.seconds
  /** How often each country looks for due work. Under [[Spacing]], so one country alone keeps up
   *  with the quota. */
  val TickEvery: FiniteDuration = 5.seconds
  /** A claimed entry is held back this long — so an entry whose worker died mid-request is
   *  claimed again, and never while its request (20 s timeout) may still be running. */
  val Lease: FiniteDuration = 5.minutes
  /** How long the whole fleet waits after Facebook names its rate limit: as long as its refusal. */
  val RateLimitHold: FiniteDuration = FacebookUsageBrake.RefusalHold
  /** How long the whole fleet waits after Facebook could not be reached or answered a 5xx. */
  val UnavailableHold: FiniteDuration = 5.minutes
  /** Refusals (or unreadable pages) before an entry is given up. */
  val MaxAttempts: Int = 5
  /** Films expanded per tick, so a backlog of films cannot hold one tick for long. */
  val FilmsPerTick: Int = 20

  /** A refused page's wait before its next attempt: 1, 2, 4, 8 minutes. */
  def backoff(attempts: Int): FiniteDuration = 1.minute * (1L << (attempts - 1).max(0).min(10))
}
