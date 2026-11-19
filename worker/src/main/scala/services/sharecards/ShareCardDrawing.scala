package services.sharecards

import play.api.Logging
import tools.OgCardRenderer

import java.time.{Clock, Instant}
import javax.imageio.ImageIO
import scala.util.Try

/**
 * The drawing of a film's share card for a render: on its current BASE (everything but the rating badges, kept as the
 * film's one high-quality JPEG under `.base/`, stamped with every non-rating input and the poster) plus the badges
 * (`base_hit`); on a base rebuilt from the film's cached POSTER (`base_rebuild`) -- never from an older base or card,
 * so no card is more than one q95 step from its poster; or, for a film without a working poster, whole (`full`).
 * Each card is written over the film's one card file ([[ShareCardStore]]); what to draw is
 * [[ShareCardService.render]]'s.
 */
final class ShareCardDrawing(store: ShareCardStore, posters: ShareCardPosters, metrics: ShareCardMetrics, clock: Clock)
  extends Logging {

  /** The render's outcome for `next` under `mode`, and the card version on disk after it, `existing` the card current
   *  for `next` already. A card on disk asked for AFTER these inputs (another replica's newer render) supersedes
   *  this render: nothing is written. */
  def drawn(next: ShareCardInputs, existing: Option[String], first: Boolean, askedAt: Option[Instant],
            mode: RenderMode): (String, Option[String]) =
    Try(new CardDrawing(next, existing, first, askedAt).drawn(mode)).recover { case e: NewerAskOnDisk =>
      logger.info(s"share card: ${next.filmId} not written — ${e.getMessage}")
      (ShareCardMetrics.Outcome.Superseded, Option.empty[String])
    }.get

  /** One render's drawing of `next`'s card: on its current base, on a rebuilt base, or without a poster.
   *  Anything thrown while drawing is this card's failure — counted, and the task retried — never an exception
   *  out of the task. Except a card on disk asked for AFTER these inputs (another replica's newer render, see
   *  `ShareCardStore`): that is supersession, and ends the render. */
  private final class CardDrawing(next: ShareCardInputs, existing: Option[String], first: Boolean, askedAt: Option[Instant]) {
    import ShareCardMetrics.Outcome

    /** The render's outcome under `mode`, and the card version on disk after it. */
    def drawn(mode: RenderMode): (String, Option[String]) = existing match {
      case Some(version) if mode == RenderMode.PosterRetry && next.isPosterless(version) =>
        withPoster(retry = true).fold((Outcome.Existing, Some(version)))(drawn => (Outcome.Rendered, Some(drawn)))
      case Some(version)                   => (Outcome.Existing, Some(version))
      case None if next.posterUrls.isEmpty =>
        posterless().fold((Outcome.Failed, Option.empty[String]))(drawn => (Outcome.Rendered, Some(drawn)))
      case None =>
        withPoster(retry = false).map(drawn => (Outcome.Rendered, Some(drawn))).getOrElse(
          posterless().fold((Outcome.Failed, Option.empty[String]))(drawn => (Outcome.RenderedNoPoster, Some(drawn))))
    }

    private def drawing(draw: => Option[String]): Option[String] =
      Try(draw).recover {
        case e: Exception if !e.isInstanceOf[NewerAskOnDisk] =>
          logger.warn(s"share card: ${next.filmId} could not be drawn: ${e.getClass.getSimpleName}: ${e.getMessage}"); None
      }.get

    private def withPoster(retry: Boolean): Option[String] =
      drawing(onBase(next, first, askedAt).orElse(rebuildBase(next, first, retry, askedAt)))

    private def posterless(): Option[String] = drawing(Some(drawWhole(next, first, askedAt)))
  }

  private def slot(next: ShareCardInputs, hasPoster: Boolean) = OgCardRenderer.badgeSlot(next.title, next.subtitle, hasPoster)

  /** When the film was first published, as its card should record it. */
  private def publishedAt(next: ShareCardInputs, first: Boolean): Option[Instant] =
    store.published(store.cardPath(next.filmId)).orElse(Option.when(first)(clock.instant()))

  private def writeCard(next: ShareCardInputs, image: java.awt.image.BufferedImage, version: String, first: Boolean,
                        askedAt: Option[Instant]): String = {
    store.writeAtomically(store.cardPath(next.filmId), OgCardRenderer.encodeCard(image), version, publishedAt(next, first), askedAt)
    version
  }

  /** The card drawn on the film's base, when that base is current for one of `next`'s posters. */
  private def onBase(next: ShareCardInputs, first: Boolean, askedAt: Option[Instant]): Option[String] = {
    val path = store.basePath(next.filmId)
    store.version(path).flatMap(v => next.posterUrls.find(url => next.baseVersion(Some(url)) == v))
      .flatMap(url => Try(Option(ImageIO.read(path.toFile))).toOption.flatten.map(url -> _))
      .map { case (url, base) =>
        metrics.renderPath(ShareCardMetrics.Path.BaseHit)
        writeCard(next, OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges), next.version(Some(url)), first, askedAt)
      }
  }

  /** The base rebuilt from the film's (cached) poster and kept, then the card drawn on it. */
  private def rebuildBase(next: ShareCardInputs, first: Boolean, retry: Boolean, askedAt: Option[Instant]): Option[String] =
    posters.load(next.filmId, next.posterUrls, retry).map { case (url, poster) =>
      val base = OgCardRenderer.renderBase(next.title, next.subtitle, Some(poster), next.host, next.director, next.synopsis)
      store.writeAtomically(store.basePath(next.filmId), OgCardRenderer.encodeBase(base), next.baseVersion(Some(url)))
      metrics.renderPath(ShareCardMetrics.Path.BaseRebuild)
      writeCard(next, OgCardRenderer.withBadges(base, slot(next, hasPoster = true), next.badges), next.version(Some(url)), first, askedAt)
    }

  /** A posterless card, drawn whole. */
  private def drawWhole(next: ShareCardInputs, first: Boolean, askedAt: Option[Instant]): String = {
    val image = OgCardRenderer.renderImage(next.title, next.subtitle, next.badges, None, next.host, next.director, next.synopsis)
    metrics.renderPath(ShareCardMetrics.Path.Full)
    writeCard(next, image, next.version(None), first, askedAt)
  }
}
