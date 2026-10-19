package services.readmodel

import play.api.Logging

/** A pass over many rows, where one row's failure must cost that row alone. */
private[readmodel] trait ContinuingSteps { self: Logging =>
  /** Run one per-row step of a pass over many rows; a throw is logged and costs that row alone. */
  protected def continuing[A](failure: => String)(step: => A): Option[A] =
    try Some(step)
    catch { case scala.util.control.NonFatal(exception) =>
      logger.warn(s"$failure, continuing: ${exception.getMessage}")
      None
    }
}
