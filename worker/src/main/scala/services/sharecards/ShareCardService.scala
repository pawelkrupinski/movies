package services.sharecards

import models.{Country, ResolvedMovie}
import play.api.Logging
import services.readmodel.{ReadModelReader, ShareCardLedger}
import services.tasks.{EnqueueResult, TaskQueue, TaskType}
import tools.{Digest, OgCardRenderer}

import java.nio.file.Files
import java.time.{Clock, Instant}
import javax.imageio.ImageIO
import scala.util.Try
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
    store.markPublished(ShareCardFile.token(movie._id))
    enqueueRender(inputs(movie), Seq(ShareCardReason.NewFilm), first = true)
    // One task per hold, keyed by its end, so a hold renewed after a restart gets its own.
    queue.enqueue(TaskType.ReleaseShareCardHold, s"share-card-hold|${movie._id}|${until.toEpochMilli}",
      Map("filmId" -> movie._id), submittedAt = clock.instant(), notBefore = Some(until))
    ()
  }

  def onProjected(movie: ResolvedMovie, screened: Boolean): Unit =
    if (screened) request(inputs(movie))

  /** Ask for the card of `next` — nothing when it exists, a render otherwise. What a projection
   *  and the backfill both call. */
  def request(next: ShareCardInputs, fallback: String = ShareCardReason.Backfill): Option[EnqueueResult] =
    existing(next) match {
      case Some(_) => fingerprints.put(next.filmId, next.fingerprint); None
      case None    => Some(enqueueRender(next, reasonsFor(next, fallback)))
    }

  private def candidatePosterHashes(next: ShareCardInputs): Seq[String] =
    if (next.posterUrls.isEmpty) Seq(ShareCardFile.posterHash(None)) else next.posterUrls.map(url => ShareCardFile.posterHash(Some(url)))

  def onPendingCardLanded(filmId: String): Unit = rescrape(filmId)

  /** Ask Facebook to fetch the film's pages again — no sooner than [[RescrapeDelay]] from now, by
   *  when the card the pages name is on `web_movies`, and spaced [[RescrapeSpacing]] from the last. */
  private def rescrape(filmId: String): Unit = {
    queue.enqueue(TaskType.RescrapeShareCard, s"share-card-rescrape|$filmId", Map("filmId" -> filmId),
      submittedAt = clock.instant(), notBefore = Some(nextRescrapeSlot()))
    ()
  }

  /** True for a film first published by the gate less than [[RecentWindow]] ago — the films people
   *  are sharing, whose previews are worth refreshing when the card changes. */
  private def recent(filmId: String): Boolean =
    store.publishedAt(ShareCardFile.token(filmId)).exists(_.isAfter(clock.instant().minusMillis(RecentWindow.toMillis)))

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
   *  task retries), rather than a text-only card frozen under a name its poster would share.
   *
   *  A card with a poster is drawn on its BASE — everything but the rating badges, cached as a
   *  high-quality JPEG under the store's `.base/`, keyed by every non-rating input and the poster.
   *  When the base is there (a ratings change: the commonest re-render) the card is that base
   *  decoded plus the badges (`base_hit`); otherwise the base is rebuilt from the cached POSTER —
   *  never from an older base or card, so no card is more than one q95 step from its poster — and
   *  kept (`base_rebuild`). A posterless card is cheap to draw whole and keeps no base (`full`). */
  def render(next: ShareCardInputs, reasons: Seq[String]): String = {
    import ShareCardMetrics.Outcome
    val (outcome, card) = existing(next) match {
      case Some(name)                      => (Outcome.Existing, Some(name))
      case None if next.posterUrls.isEmpty => (Outcome.Rendered, Some(drawWhole(next)))
      case None => onBase(next).orElse(rebuildBase(next)).fold((Outcome.Failed, Option.empty[String]))(name => (Outcome.Rendered, Some(name)))
    }
    card.foreach { name =>
      fingerprints.put(next.filmId, next.fingerprint)
      val before = Option(lastKnown.put(next.filmId, name))
      // A recent film's card URL changed: its previews show the old one.
      if (outcome == Outcome.Rendered && before.exists(_ != name) && recent(next.filmId)) rescrape(next.filmId)
    }
    metrics.render(outcome, reasons)
    outcome
  }

  private def slot(next: ShareCardInputs, hasPoster: Boolean) = OgCardRenderer.badgeSlot(next.title, next.subtitle, hasPoster)

  /** The card drawn on an existing base of one of `next`'s posters. */
  private def onBase(next: ShareCardInputs): Option[String] =
    next.posterUrls.iterator.flatMap { url =>
      val path = store.basePath(next.baseKey(Some(url)))
      if (!Files.isRegularFile(path)) None
      else Try(Option(ImageIO.read(path.toFile))).toOption.flatten.map(url -> _)
    }.nextOption().map { case (url, base) =>
      val name = next.fileName(Some(url))
      store.writeAtomically(store.cardPath(name), OgCardRenderer.encodeCard(OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges)))
      metrics.renderPath(ShareCardMetrics.Path.BaseHit)
      name
    }

  /** The base rebuilt from the film's (cached) poster and kept, then the card drawn on it. */
  private def rebuildBase(next: ShareCardInputs): Option[String] =
    posters.load(next.posterUrls).map { case (url, poster) =>
      val base = OgCardRenderer.renderBase(next.title, next.subtitle, Some(poster), next.host, next.director, next.synopsis)
      store.writeAtomically(store.basePath(next.baseKey(Some(url))), OgCardRenderer.encodeBase(base))
      val name = next.fileName(Some(url))
      store.writeAtomically(store.cardPath(name), OgCardRenderer.encodeCard(OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges)))
      metrics.renderPath(ShareCardMetrics.Path.BaseRebuild)
      name
    }

  /** A posterless card, drawn whole. */
  private def drawWhole(next: ShareCardInputs): String = {
    val name  = next.fileName(None)
    val bytes = OgCardRenderer.render(next.title, next.subtitle, next.badges, None, next.host, next.director, next.synopsis)
    store.writeAtomically(store.cardPath(name), bytes)
    metrics.renderPath(ShareCardMetrics.Path.Full)
    name
  }

  // Re-scrape requests are spaced out, not sent in a burst: a backfill of pending films must not
  // look like abuse to the Graph API.
  private var lastRescrapeSlot = Instant.EPOCH
  private def nextRescrapeSlot(): Instant = synchronized {
    val slot = Seq(clock.instant().plusMillis(RescrapeDelay.toMillis), lastRescrapeSlot.plusMillis(RescrapeSpacing.toMillis)).max
    lastRescrapeSlot = slot
    slot
  }
}

object ShareCardService {
  val ReasonsKey = "reasons"
  val FirstKey   = "first"

  /** How far ahead of the backlog a first card is placed: the queue claims by `submittedAt`, and a
   *  day covers any real backlog. */
  val FirstCardHeadStart: FiniteDuration = 1.day

  /** At most one Facebook re-scrape every 10 seconds per process. */
  val RescrapeSpacing: FiniteDuration = 10.seconds

  /** A re-scrape waits this long: the card's `web_movies` document is rewritten when its render task
   *  completes, and Facebook must find the new URL, not the old. */
  val RescrapeDelay: FiniteDuration = 1.minute

  /** A card change re-scrapes a film's pages during its first week after first publication. */
  val RecentWindow: FiniteDuration = 7.days

  def reasons(payload: Map[String, String]): Seq[String] =
    payload.get(ReasonsKey).toSeq.flatMap(_.split(',')).filter(ShareCardReason.all.contains) match {
      case Seq() => Seq(ShareCardReason.Backfill)
      case some  => some
    }
}
