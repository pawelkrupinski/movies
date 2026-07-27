package services.cinemas.common

/**
 * Normalises a cinema's raw age-rating / certificate label into the verbatim
 * rating we store, or `None` when the source carries no real rating yet.
 *
 * Shared by every UK chain whose payload exposes a certificate (Vue, Odeon,
 * Flicks) so the "what counts as a rating" rule lives in one place rather than
 * drifting across three parsers. The label is trimmed and otherwise kept
 * EXACTLY as the source spells it — BBFC "12A" stays "12A", Vue's lower-case
 * "12a" stays "12a"; no upper-casing or other transformation. A blank string
 * or a not-yet-classified placeholder ("TBC"/"Unknown", any case) collapses to
 * `None`, so an unrated row stays unrated rather than showing a placeholder.
 */
object AgeRating {

  private val Placeholders = Set("tbc", "unknown")

  def normalize(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty)
      .filterNot(r => Placeholders.contains(r.toLowerCase))

  def normalize(raw: Option[String]): Option[String] =
    raw.flatMap(normalize)
}
