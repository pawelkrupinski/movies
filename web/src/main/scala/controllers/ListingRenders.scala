package controllers

import services.metrics.web.WebRenderMetrics

/** What a listing render allocates, reported to `record` (`WebRenderMetrics`): the whole page (`listing`) and, within
 *  it, building its schedules (`listing_schedules`), so the histogram says how much of a listing's cost is which. */
final class ListingRenders(record: (String, Long) => Unit) {

  /** `body`, its bytes' allocation reported as the whole listing's. */
  def listing(body: ResponseBody): ResponseBody = ResponseBody.measured(body, record(WebRenderMetrics.Listing, _))

  /** `schedules`, its allocation reported as the listing's schedules'. */
  def schedules[A](build: => A): A = tools.ThreadAllocation.measure(record(WebRenderMetrics.ListingSchedules, _))(build)
}
