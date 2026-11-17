package services.sharecards

/** Which kind of render a share card's render task is — one of three, never a combination:
 *
 *  - [[RenderMode.Ordinary]]: the card for the film's current inputs, drawn unless current already.
 *  - [[RenderMode.FirstCard]]: the first-publish gate's render. It is placed ahead of the queue's
 *    backlog, and the card it writes records the film's first publication, which every later card
 *    of the film carries forward.
 *  - [[RenderMode.PosterRetry]]: the backfill re-trying the posters of a card drawn without one;
 *    failing again, the posterless card stays.
 *
 *  It travels in the render task's payload under the keys [[ShareCardService.FirstKey]] and
 *  [[ShareCardService.RetryPosterKey]], so tasks queued by an earlier release read the same. */
enum RenderMode {
  case Ordinary, FirstCard, PosterRetry

  /** The payload entries that carry this mode. */
  def payload: Map[String, String] = this match {
    case Ordinary    => Map(ShareCardService.FirstKey -> "false")
    case FirstCard   => Map(ShareCardService.FirstKey -> "true")
    case PosterRetry => Map(ShareCardService.FirstKey -> "false", ShareCardService.RetryPosterKey -> "true")
  }
}

object RenderMode {
  /** The mode a render task's payload carries; [[RenderMode.Ordinary]] when it names none. */
  def fromPayload(payload: Map[String, String]): RenderMode =
    if (payload.get(ShareCardService.FirstKey).contains("true")) FirstCard
    else if (payload.get(ShareCardService.RetryPosterKey).contains("true")) PosterRetry
    else Ordinary
}
