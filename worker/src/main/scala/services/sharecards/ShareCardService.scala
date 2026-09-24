package services.sharecards

import models.{Country, ResolvedMovie}
import play.api.Logging
import services.readmodel.{ReadModelReader, ShareCardLedger}
import services.tasks.{EnqueueResult, TaskQueue, TaskType}
import tools.{Digest, OgCardRenderer}

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*

/**
 * The worker's side of the share cards: what the projection asks ([[ShareCardLedger]]), and the
 * render a `RenderShareCard` task runs.
 *
 * Every render rides the task queue (dedup key = film and inputs, so one render per card however
 * many replicas and triggers ask). A drawn input changing enqueues one; the first-publish gate
 * enqueues one AHEAD of the queue (an earlier `submittedAt` — the queue claims oldest first) plus
 * the task that ends its hold; the backfill feeds the rest in bounded batches.
 *
 * RATINGS-ONLY CHANGES WAIT: at most one such re-render per film per day. Ratings refresh all day
 * (measured ~242 rating-driven changes a day across the fleet, on ~163 films — one PL film moved 11
 * times in a day), and a card a day behind on a rating is still a true card. So when the only thing
 * that moved since the film's current card is the rating badges, the render is queued to run no
 * sooner than a day after that card was written, keyed to that card so every later rating change
 * the same day folds into the one waiting task (its payload merged to the latest ratings). Any other
 * change renders at once, with whatever the ratings are then, and starts a new day.
 *
 * What a document's `shareCard` should be is read from the DIRECTORY, which every replica shares:
 * a card for the film's current inputs drawn from any of its current poster candidates when one
 * exists, else the card it had before (while the new one renders), else none. So replicas agree
 * without telling each other anything.
 */
class ShareCardService(
  country:  Country,
  store:    ShareCardStore,
  posters:  ShareCardPosters,
  reader:   ReadModelReader,
  queue:    TaskQueue,
  metrics:  ShareCardMetrics,
  clock:    Clock
) extends ShareCardLedger with Logging {
  import ShareCardService.*

  def inputs(movie: ResolvedMovie): ShareCardInputs = ShareCardInputs.of(movie, country)

  // The card each film last had, for the window between its inputs changing and the new card
  // landing. Seeded from `web_movies` on first use (a restart must not blank every card that is
  // mid-re-render), then kept by every answer [[current]] gives.
  private lazy val lastKnown: ConcurrentHashMap[String, String] = {
    val map = new ConcurrentHashMap[String, String]()
    val (refs, _) = reader.findAllShareCardRefsChecked()
    refs.foreach(ref => ref.shareCard.foreach(map.put(ref.filmId, _)))
    map
  }
  // What each card last rendered was drawn from, so the next render can name the parts that moved.
  private val fingerprints = new ConcurrentHashMap[String, ShareCardFingerprint]()

  /** The existing card for exactly these inputs, if any. */
  def existing(next: ShareCardInputs): Option[String] = next.candidateNames.find(store.cardExists)

  def current(movie: ResolvedMovie): Option[String] = {
    val picked = existing(inputs(movie)).orElse(Option(lastKnown.get(movie._id)).filter(store.cardExists))
    picked.foreach(lastKnown.put(movie._id, _))
    picked
  }

  def readyToPublish(movie: ResolvedMovie): Boolean = existing(inputs(movie)).isDefined

  def requestFirstCard(movie: ResolvedMovie, until: Instant): Unit = {
    enqueueRender(inputs(movie), Seq(ShareCardReason.NewFilm), first = true)
    // One task per hold, keyed by its end, so a hold renewed after a restart gets its own.
    queue.enqueue(TaskType.ReleaseShareCardHold, s"share-card-hold|${movie._id}|${until.toEpochMilli}",
      Map("filmId" -> movie._id), submittedAt = clock.instant(), notBefore = Some(until))
    ()
  }

  def onProjected(movie: ResolvedMovie, screened: Boolean): Unit =
    if (screened) request(inputs(movie))

  /** Ask for the card of `next` — nothing when it exists, a deferred render when only its ratings
   *  moved, a render now otherwise. What a projection and the backfill both call. */
  def request(next: ShareCardInputs, fallback: String = ShareCardReason.Backfill): Option[EnqueueResult] =
    existing(next) match {
      case Some(name) =>
        fingerprints.put(next.filmId, next.fingerprint)
        // The ratings came back to what this card shows while a deferred render of an in-between
        // value waits: bring its payload back too, so it finds the card there and draws nothing.
        queue.amendWaiting(ratingsKey(next.filmId, name), ratingsPayload(next, name))
        None
      case None => Some(ratingsOnlySince(next).fold(enqueueRender(next, reasonsFor(next, fallback)))(deferRatings(next, _)))
    }

  private def ratingsKey(filmId: String, anchor: String): String = s"share-card-ratings|$filmId|$anchor"
  private def ratingsPayload(next: ShareCardInputs, anchor: String): Map[String, String] =
    next.toPayload ++ Map(ReasonsKey -> ShareCardReason.Ratings, FirstKey -> "false", AnchorKey -> anchor)

  /** The film's current card, when the only drawn part `next` changes from it is the ratings. */
  private def ratingsOnlySince(next: ShareCardInputs): Option[ShareCardFile] =
    Option(lastKnown.get(next.filmId)).filter(store.cardExists).flatMap(ShareCardFile.parse).filter { had =>
      had.layoutHash == next.layoutHash && had.ratingsHash != next.ratingsHash && candidatePosterHashes(next).contains(had.posterHash)
    }

  /** Queue `next` to render no sooner than a day after the card it would replace was written. */
  private def deferRatings(next: ShareCardInputs, had: ShareCardFile): EnqueueResult = {
    val due = store.modified(had.name).map(_.plusMillis(RatingsWindow.toMillis)).filter(_.isAfter(clock.instant()))
    due.fold(enqueueRender(next, Seq(ShareCardReason.Ratings))) { notBefore =>
      val key     = ratingsKey(next.filmId, had.name)
      val payload = ratingsPayload(next, had.name)
      val result  = queue.enqueue(TaskType.RenderShareCard, key, payload, submittedAt = clock.instant(), notBefore = Some(notBefore))
      if (result == EnqueueResult.Duplicate) queue.amendWaiting(key, payload)
      metrics.render(ShareCardMetrics.Outcome.Deferred, Seq(ShareCardReason.Ratings))
      result
    }
  }

  /** A deferred ratings render whose card was replaced meanwhile (another change rendered a newer
   *  card for the film, with the ratings of its day) has nothing left to do. */
  def superseded(payload: Map[String, String], filmId: String): Boolean =
    payload.get(AnchorKey).exists(anchor => store.newerCardOf(ShareCardFile.token(filmId), anchor))

  def recordSuperseded(reasons: Seq[String]): Unit = metrics.render(ShareCardMetrics.Outcome.Superseded, reasons)

  private def candidatePosterHashes(next: ShareCardInputs): Seq[String] =
    if (next.posterUrls.isEmpty) Seq(ShareCardFile.posterHash(None)) else next.posterUrls.map(url => ShareCardFile.posterHash(Some(url)))

  def onPendingCardLanded(filmId: String): Unit = {
    queue.enqueue(TaskType.RescrapeShareCard, s"share-card-rescrape|$filmId", Map("filmId" -> filmId),
      submittedAt = clock.instant(), notBefore = Some(nextRescrapeSlot()))
    ()
  }

  /** Why `next` needs a render: `poster` when the poster its card was drawn from is no longer a
   *  candidate, plus the drawn parts that moved since this process last saw the card (`template`
   *  when the card differs only by the template version, `fallback` when it can't tell); `new_film`
   *  when the film has no card at all. */
  def reasonsFor(next: ShareCardInputs, fallback: String = ShareCardReason.Backfill): Seq[String] =
    Option(lastKnown.get(next.filmId)).flatMap(ShareCardFile.parse) match {
      case None => Seq(ShareCardReason.NewFilm)
      case Some(had) =>
        val poster = Option.when(!candidatePosterHashes(next).contains(had.posterHash))(ShareCardReason.Poster).toSeq
        val drawn = Option(fingerprints.get(next.filmId)) match {
          case Some(previous) => next.fingerprint.changedFrom(previous)
          case None if had.drawnHash == next.copy(template = next.template - 1).drawnHash => Seq(ShareCardReason.Template)
          case None if had.drawnHash == next.drawnHash => Nil
          case None => Seq(fallback)
        }
        Some(poster ++ drawn).filter(_.nonEmpty).getOrElse(Seq(fallback))
    }

  /** Queue a render of `next`. A first card is placed ahead of the queue's backlog. */
  def enqueueRender(next: ShareCardInputs, reasons: Seq[String], first: Boolean = false): EnqueueResult = {
    val now = clock.instant()
    val candidates = Digest.sha256Hex(next.posterUrls.mkString("\n")).take(8)
    queue.enqueue(TaskType.RenderShareCard, s"share-card|${next.filmId}|${next.drawnHash}|$candidates",
      next.toPayload ++ Map(ReasonsKey -> reasons.mkString(","), FirstKey -> first.toString),
      submittedAt = if (first) now.minusSeconds(FirstCardHeadStart.toSeconds) else now)
  }

  /** The render task's work: the card for `next`, written to the store unless one for these
   *  inputs is there already. A film with posters whose every candidate fails gets no card (the
   *  task retries), rather than a text-only card frozen under a name its poster would share. */
  def render(next: ShareCardInputs, reasons: Seq[String]): String = {
    import ShareCardMetrics.Outcome
    val (outcome, card) = existing(next) match {
      case Some(name)                      => (Outcome.Existing, Some(name))
      case None if next.posterUrls.isEmpty => (Outcome.Rendered, Some(write(next, None)))
      case None => posters.load(next.posterUrls).fold((Outcome.Failed, Option.empty[String]))(chosen =>
                     (Outcome.Rendered, Some(write(next, Some(chosen)))))
    }
    card.foreach { name =>
      fingerprints.put(next.filmId, next.fingerprint)
      lastKnown.put(next.filmId, name)
    }
    metrics.render(outcome, reasons)
    outcome
  }

  private def write(next: ShareCardInputs, poster: Option[(String, java.awt.image.BufferedImage)]): String = {
    val name  = next.fileName(poster.map(_._1))
    val bytes = OgCardRenderer.render(next.title, next.subtitle, next.badges, poster.map(_._2), next.host, next.director, next.synopsis)
    store.writeAtomically(store.cardPath(name), bytes)
    name
  }

  // Re-scrape requests are spaced out, not sent in a burst: a backfill of pending films must not
  // look like abuse to the Graph API.
  private var lastRescrapeSlot = Instant.EPOCH
  private def nextRescrapeSlot(): Instant = synchronized {
    val slot = Seq(clock.instant(), lastRescrapeSlot.plusMillis(RescrapeSpacing.toMillis)).max
    lastRescrapeSlot = slot
    slot
  }
}

object ShareCardService {
  val ReasonsKey = "reasons"
  val FirstKey   = "first"
  /** On a deferred ratings render: the card it would replace. */
  val AnchorKey  = "anchor"

  /** At most one ratings-only re-render per film per this window. */
  val RatingsWindow: FiniteDuration = 24.hours

  /** How far ahead of the backlog a first card is placed: the queue claims by `submittedAt`, and a
   *  day covers any real backlog. */
  val FirstCardHeadStart: FiniteDuration = 1.day

  /** At most one Facebook re-scrape every 10 seconds per process. */
  val RescrapeSpacing: FiniteDuration = 10.seconds

  def reasons(payload: Map[String, String]): Seq[String] =
    payload.get(ReasonsKey).toSeq.flatMap(_.split(',')).filter(ShareCardReason.all.contains) match {
      case Seq() => Seq(ShareCardReason.Backfill)
      case some  => some
    }
}
