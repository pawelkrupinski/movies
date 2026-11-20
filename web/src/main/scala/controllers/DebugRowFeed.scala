package controllers

import models.StoredMovieRecord

import play.api.libs.json.Json

/** One country's `movies` changes as the /debug live view's SSE frames: each upserted row rendered
 *  server-side through the same `_debugRow` partial the page uses, so a live-inserted row is
 *  byte-identical to the initial render, and a delete as the `_id` the page drops. */
final class DebugRowFeed(stack: DebugStack, country: models.Country)(using latestYear: services.evidence.LatestTitleYear) {

  // The stack's own title rules: a row keys as its own corpus keyed it.
  private val normalizer = stack.movieRepository.normalizer

  // The frames render under the country the connection WATCHES — its stack's own title rules
  // and one of its own cities. They used to render under `City.all.head` (Poznań) and Poland's
  // rules in every deployment, so a UK /debug stream keyed its rows with Polish sanitizing.
  private given frameCity: models.City = country.cities.head

  private[controllers] def upsertFrame(row: StoredMovieRecord): String = {
    val html = views.html._debugRow(row, normalizer, latestYear).body
    s"data: ${Json.stringify(Json.obj("type" -> "upsert", "id" -> row.id.value, "html" -> html))}\n\n"
  }

  private[controllers] def deleteFrame(id: String): String =
    s"data: ${Json.stringify(Json.obj("type" -> "delete", "id" -> id))}\n\n"

  /** Every change from now on, as its frame, to `offer`; the watch to close, when the repository can watch at all. */
  def open(offer: String => Unit): Option[AutoCloseable] =
    stack.movieRepository.watchChanges(
      onUpsert = row => offer(upsertFrame(row)),
      onDelete = id  => offer(deleteFrame(id.value))
    )
}
