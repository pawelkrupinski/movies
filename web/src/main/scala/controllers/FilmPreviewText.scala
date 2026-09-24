package controllers

/** The film page's share-preview TEXT — `og:description`. Pulled out of
 *  [[MovieController]] so the controller is left with HTTP concerns and this
 *  stays a pure, independently-testable mapping from a [[FilmSchedule]]. (The
 *  preview IMAGE is the worker's share card; see [[ShareCardUrl]].) */
object FilmPreviewText {

  /** Build the `og:description` / `twitter:description` text for the film
   * page: the synopsis alone, truncated to keep WhatsApp / Messenger /
   * Telegram previews readable. Ratings stay off it — the OG card image
   * already carries them as badges. Empty for films with no synopsis. */
  def previewDescription(film: FilmSchedule): String = {
    // og:description is plain text — drop the markdown emphasis markers.
    val synopsis = tools.SynopsisMarkdown.strip(film.synopsis.getOrElse("")).trim
    // 300 chars is the practical cap most preview UIs render before
    // truncating; we add an ellipsis to make truncation visible.
    if (synopsis.length > 300) synopsis.take(297) + "…" else synopsis
  }
}
