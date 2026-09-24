package services.sharecards

import models.{Country, ResolvedMovie}
import play.api.Logging
import services.readmodel.ShareCardLedger
import services.tasks.{EnqueueResult, TaskQueue, TaskType}
import tools.{Digest, OgCardRenderer}

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap
import javax.imageio.ImageIO
import scala.concurrent.duration.*
import scala.util.Try

/**
 * The worker's side of the share cards: what the projection asks ([[ShareCardLedger]]), and the
 * render a `RenderShareCard` task runs.
 *
 * Every render rides the task queue (dedup key = film and inputs, so one render per card however
 * many replicas and triggers ask). A drawn input changing enqueues one; the first-publish gate
 * enqueues one AHEAD of the queue (an earlier `submittedAt` — the queue claims oldest first) plus
 * the task that ends its hold; the backfill feeds the rest in bounded batches.
 *
 * A film's card is ONE file, overwritten by each render ([[ShareCardStore]]); what a document's
 * `shareCard` should be is read from that file's stamped version — the latest card, whatever inputs
 * it was drawn from, so a film keeps its old card while the new one renders and switches the moment
 * it lands. Replicas agree because they read the same directory.
 */
class ShareCardService(
  country:  Country,
  store:    ShareCardStore,
  posters:  ShareCardPosters,
  queue:    TaskQueue,
  metrics:  ShareCardMetrics,
  clock:    Clock
) extends ShareCardLedger with Logging {
  import ShareCardService.*

  def inputs(movie: ResolvedMovie): ShareCardInputs = ShareCardInputs.of(movie, country)

  // What each card last rendered was drawn from, so the next render can name the parts that moved.
  private val fingerprints = new ConcurrentHashMap[String, ShareCardFingerprint]()

  /** The version of the film's card on disk, whatever it was drawn from. */
  def onDisk(filmId: String): Option[String] = store.version(store.cardPath(filmId))

  /** The card's version when it is current for exactly these inputs. */
  def existing(next: ShareCardInputs): Option[String] = onDisk(next.filmId).filter(next.acceptableVersions.contains)

  /** True when the film's current card was drawn without a poster although the film has some —
   *  every one failed when it was rendered. The backfill retries those ([[retryPoster]]). */
  def lacksPoster(next: ShareCardInputs): Boolean = existing(next).exists(next.isPosterless)

  /** Queue a render that tries `next`'s posters again for a card drawn without one. */
  def retryPoster(next: ShareCardInputs): EnqueueResult =
    queue.enqueue(TaskType.RenderShareCard, s"share-card-poster|${next.filmId}|${next.drawnHash}",
      next.toPayload ++ Map(ReasonsKey -> ShareCardReason.Poster, FirstKey -> "false", RetryPosterKey -> "true"),
      submittedAt = clock.instant())

  def current(movie: ResolvedMovie): Option[String] = onDisk(movie._id).map(ShareCardFile.url(movie._id, _))

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

  /** Ask for the card of `next` — nothing when it is current, a render otherwise. What a projection
   *  and the backfill both call; `askedAt` is when `next` was read (the backfill's sweep, which may
   *  be a day old), so an older read never supersedes a newer one ([[renderIfLatest]]). */
  def request(next: ShareCardInputs, fallback: String = ShareCardReason.Backfill,
              askedAt: Instant = clock.instant()): Option[EnqueueResult] =
    existing(next) match {
      case Some(_) => ask(next, askedAt); fingerprints.put(next.filmId, next.fingerprint); None
      case None    => Some(enqueueRender(next, reasonsFor(next, fallback), askedAt = askedAt))
    }

  // THE LATEST ASK PER FILM. A film's card is one file at one URL naming its version, overwritten by
  // every render, and renders run on several threads: a render of inputs that a newer request has
  // replaced must not land, or — finishing after the newer one — it puts the older picture under
  // the URL `web_movies` names for the newer version, which a preview cache keeps for a year. Kept
  // per process: the worker is one replica, and after a restart nothing is superseded until asked.
  private val latestAsk = new ConcurrentHashMap[String, ShareCardService.Ask]()
  private def ask(next: ShareCardInputs, at: Instant): Unit = {
    latestAsk.merge(next.filmId, ShareCardService.Ask(renderKey(next), at), (had, now) => if (now.at.isBefore(had.at)) had else now)
    ()
  }

  /** True when a newer request for the film asked for other inputs than `next`. */
  def superseded(next: ShareCardInputs): Boolean = Option(latestAsk.get(next.filmId)).exists(_.key != renderKey(next))

  /** What one render draws: the drawn inputs and the poster candidates it may choose from. */
  private def renderKey(next: ShareCardInputs): String =
    s"${next.drawnHash}|${Digest.sha256Hex(next.posterUrls.mkString("\n")).take(8)}"

  // One render of a film at a time, so the supersession check and the write it guards cannot
  // interleave with another render of the film. Striped: a lock per film would never be freed.
  private val renderLocks = Array.fill(64)(new Object)

  /** The render task's entry: [[render]], unless a newer request superseded `next` (`superseded`,
   *  nothing drawn). */
  def renderIfLatest(next: ShareCardInputs, reasons: Seq[String], first: Boolean = false, retryPoster: Boolean = false): String =
    renderLocks(Math.floorMod(next.filmId.hashCode, renderLocks.length)).synchronized {
      if (!superseded(next)) render(next, reasons, first, retryPoster)
      else { metrics.render(ShareCardMetrics.Outcome.Superseded, reasons); ShareCardMetrics.Outcome.Superseded }
    }

  def onPendingCardLanded(filmId: String): Unit = rescrape(filmId)

  /** The film left the read model: its card, base and poster go now (a file written within the
   *  grace period stays — another replica may be publishing the film again; the daily prune gets it). */
  def onRetired(filmId: String): Unit = {
    val gone = store.deleteFilm(filmId, olderThan = clock.instant().minusMillis(ShareCardJanitor.Grace.toMillis))
    gone.foreach(metrics.pruned(_, ShareCardMetrics.PruneReason.Retired))
    fingerprints.remove(filmId)
    latestAsk.remove(filmId)
    ()
  }

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
    store.published(store.cardPath(filmId)).exists(_.isAfter(clock.instant().minusMillis(RecentWindow.toMillis)))

  /** Why `next` needs a render: `poster` when the poster its card was drawn from is no longer a
   *  candidate, plus the drawn parts that moved since this process last saw the card (`template`
   *  when the card differs only by the template version, `fallback` when it can't tell); `new_film`
   *  when the film has no card at all. */
  def reasonsFor(next: ShareCardInputs, fallback: String = ShareCardReason.Backfill): Seq[String] =
    onDisk(next.filmId).flatMap(ShareCardVersion.parse) match {
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

  private def candidatePosterHashes(next: ShareCardInputs): Seq[String] =
    if (next.posterUrls.isEmpty) Seq(ShareCardFile.posterHash(None)) else next.posterUrls.map(url => ShareCardFile.posterHash(Some(url)))

  /** Queue a render of `next`. A first card is placed ahead of the queue's backlog. */
  def enqueueRender(next: ShareCardInputs, reasons: Seq[String], first: Boolean = false,
                    askedAt: Instant = clock.instant()): EnqueueResult = {
    val now = clock.instant()
    ask(next, askedAt)
    queue.enqueue(TaskType.RenderShareCard, s"share-card|${next.filmId}|${renderKey(next)}",
      next.toPayload ++ Map(ReasonsKey -> reasons.mkString(","), FirstKey -> first.toString),
      submittedAt = if (first) now.minusSeconds(FirstCardHeadStart.toSeconds) else now)
  }

  /** The render task's work: the film's card for `next`, written over its one card file unless it
   *  is current already. A film with posters whose every candidate fails gets a card WITHOUT a
   *  poster (`rendered_no_poster`) — a film with no card could never be shared well, and coverage
   *  would stall on it — versioned as posterless, so the card a working poster later draws
   *  replaces it. `retryPoster` re-tries the posters for such a card; failing again, it stays.
   *
   *  A card with a poster is drawn on its BASE — everything but the rating badges, kept as the
   *  film's one high-quality JPEG under `.base/`, stamped with every non-rating input and the
   *  poster. When the base is current (a ratings change: the commonest re-render) the card is that
   *  base decoded plus the badges (`base_hit`); otherwise the base is rebuilt from the film's cached
   *  POSTER — never from an older base or card, so no card is more than one q95 step from its
   *  poster — and kept (`base_rebuild`). A posterless card is cheap to draw whole (`full`).
   *
   *  `first` marks the first-publish gate's render: the card then records the film's first
   *  publication, which every later card of the film carries forward. */
  def render(next: ShareCardInputs, reasons: Seq[String], first: Boolean = false, retryPoster: Boolean = false): String = {
    import ShareCardMetrics.Outcome
    val before = onDisk(next.filmId)
    // Anything thrown while drawing is this card's failure — counted, and the task retried — never
    // an exception out of the task.
    def withPoster(retry: Boolean): Option[String] =
      Try(onBase(next, first).orElse(rebuildBase(next, first, retry))).recover { case e: Exception =>
        logger.warn(s"share card: ${next.filmId} could not be drawn: ${e.getClass.getSimpleName}: ${e.getMessage}"); None
      }.get
    val (outcome, card) = existing(next) match {
      case Some(version) if retryPoster && next.isPosterless(version) =>
        withPoster(retry = true).fold((Outcome.Existing, Some(version)))(drawn => (Outcome.Rendered, Some(drawn)))
      case Some(version)                   => (Outcome.Existing, Some(version))
      case None if next.posterUrls.isEmpty => (Outcome.Rendered, Some(drawWhole(next, first)))
      case None =>
        withPoster(retry = false).map(drawn => (Outcome.Rendered, Some(drawn))).getOrElse(
          Try(drawWhole(next, first)).toOption.fold((Outcome.Failed, Option.empty[String]))(drawn => (Outcome.RenderedNoPoster, Some(drawn))))
    }
    card.foreach { version =>
      fingerprints.put(next.filmId, next.fingerprint)
      // A recent film's card changed: its previews show the old one.
      if (outcome != Outcome.Existing && before.exists(_ != version) && recent(next.filmId)) rescrape(next.filmId)
    }
    metrics.render(outcome, reasons)
    outcome
  }

  private def slot(next: ShareCardInputs, hasPoster: Boolean) = OgCardRenderer.badgeSlot(next.title, next.subtitle, hasPoster)

  /** When the film was first published, as its card should record it. */
  private def publishedAt(next: ShareCardInputs, first: Boolean): Option[Instant] =
    store.published(store.cardPath(next.filmId)).orElse(Option.when(first)(clock.instant()))

  private def writeCard(next: ShareCardInputs, image: java.awt.image.BufferedImage, version: String, first: Boolean): String = {
    store.writeAtomically(store.cardPath(next.filmId), OgCardRenderer.encodeCard(image), version, publishedAt(next, first))
    version
  }

  /** The card drawn on the film's base, when that base is current for one of `next`'s posters. */
  private def onBase(next: ShareCardInputs, first: Boolean): Option[String] = {
    val path = store.basePath(next.filmId)
    store.version(path).flatMap(v => next.posterUrls.find(url => next.baseVersion(Some(url)) == v))
      .flatMap(url => Try(Option(ImageIO.read(path.toFile))).toOption.flatten.map(url -> _))
      .map { case (url, base) =>
        metrics.renderPath(ShareCardMetrics.Path.BaseHit)
        writeCard(next, OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges), next.version(Some(url)), first)
      }
  }

  /** The base rebuilt from the film's (cached) poster and kept, then the card drawn on it. */
  private def rebuildBase(next: ShareCardInputs, first: Boolean, retry: Boolean): Option[String] =
    posters.load(next.filmId, next.posterUrls, retry).map { case (url, poster) =>
      val base = OgCardRenderer.renderBase(next.title, next.subtitle, Some(poster), next.host, next.director, next.synopsis)
      store.writeAtomically(store.basePath(next.filmId), OgCardRenderer.encodeBase(base), next.baseVersion(Some(url)))
      metrics.renderPath(ShareCardMetrics.Path.BaseRebuild)
      writeCard(next, OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges), next.version(Some(url)), first)
    }

  /** A posterless card, drawn whole. */
  private def drawWhole(next: ShareCardInputs, first: Boolean): String = {
    val image = OgCardRenderer.renderImage(next.title, next.subtitle, next.badges, None, next.host, next.director, next.synopsis)
    metrics.renderPath(ShareCardMetrics.Path.Full)
    writeCard(next, image, next.version(None), first)
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
  private final case class Ask(key: String, at: Instant)

  val ReasonsKey = "reasons"
  val FirstKey   = "first"
  /** On a render that re-tries the posters of a card drawn without one. */
  val RetryPosterKey = "retryPoster"

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
