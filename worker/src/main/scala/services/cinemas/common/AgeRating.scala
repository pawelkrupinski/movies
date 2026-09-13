package services.cinemas.common

/**
 * Normalises a cinema's raw age-rating / certificate label into the verbatim
 * rating we store, or `None` when the source carries no real rating yet.
 *
 * Shared by every source whose payload exposes a certificate (the UK chains,
 * filmstarts FSK, the PL chains) so the "what counts as a rating" rule lives in
 * one place rather than drifting across parsers. The label is trimmed and
 * UPPER-CASED to its canonical form — BBFC's "12a" becomes "12A" (the official
 * spelling); every other short code ("PG", "FSK 6", "15+", "PG-13") is already
 * upper-case, so this only fixes the lower-case-`a` drift. A blank string or a
 * not-yet-classified placeholder ("TBC"/"Unknown", any case) collapses to
 * `None`, so an unrated row stays unrated rather than showing a placeholder.
 */
object AgeRating {

  private val Placeholders = Set("tbc", "unknown")

  // Polish "no age restriction" markers — a marker, not a rating, so a source
  // that spells "no restriction" as a value (rather than leaving the field
  // blank) should drop it via `normalizeDroppingNoRestriction` rather than
  // badge it verbatim. "BO" = "bez ograniczeń"; "b.o." is the abbreviated
  // spelling. Shared by every source whose payload uses this marker (Multikino's
  // `certificate.name`, Cinema1's `ratings[].value`) so the marker set doesn't
  // drift across parsers.
  private val NoRestrictionMarkers = Set("bo", "b.o.")

  def normalize(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty)
      .filterNot(r => Placeholders.contains(r.toLowerCase))
      .map(_.toUpperCase)

  def normalize(raw: Option[String]): Option[String] =
    raw.flatMap(normalize)

  /** As `normalize`, but also drops the Polish "no restriction" marker(s) first. */
  def normalizeDroppingNoRestriction(raw: Option[String]): Option[String] =
    normalize(raw.filterNot(r => NoRestrictionMarkers.contains(r.trim.toLowerCase)))
}
