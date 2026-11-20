package services.scrapes

import scala.util.{Failure, Success, Try}

import models.Cinema
import play.api.Logging

/** A store of per-venue scrape records whose every operation is best effort: what it keeps records something that
 *  already happened (a scrape, the guards' state after it), so a failure is logged — `<storeName>.<op>(<venue>)
 *  failed`, on the store's own logger — and answered `None`, never propagated into the scrape. */
private[scrapes] trait BestEffortVenueStore extends Logging {
  /** The name a failure is logged under. */
  protected def storeName: String

  protected final def bestEffort[A](cinema: Cinema, op: String)(body: => A): Option[A] =
    Try(body) match {
      case Success(value) => Some(value)
      case Failure(e)     =>
        logger.warn(s"$storeName.$op(${cinema.displayName}) failed: ${e.getMessage}")
        None
    }
}
