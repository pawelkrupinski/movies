package services.enrichment

/**
 * Display precision for the rating values we persist. The principle: store only
 * as much as the badge shows. The IMDb and Filmweb badges render a 0–10 vote
 * average to ONE decimal (`f"$r%.1f"` in `_ratingBadges.scala.html`), while both
 * sources report a high-precision average that drifts by ~1e-5 on every fetch as
 * votes trickle in. Persisting that full precision means the 4h refresh sweep
 * rewrites the row (and re-projects the read model) for a delta the user can
 * never see — the `7.47111 → 7.47116` write churn.
 *
 * Rounding to the displayed decimal at the storage boundary makes "did the value
 * change" a plain equality on what's shown, and is the change signal the
 * per-film refresh cadence keys off. (Metascore and Rotten Tomatoes already
 * store the integer they display, so they need no rounding.)
 */
object RatingDisplay {
  /** Round a 0–10 vote average to the single decimal the badge renders. HALF_UP
   *  matches `String.format`'s `%.1f`, so `oneDecimal(r)` and the badge's
   *  `f"$r%.1f"` never disagree on the shown value. */
  def oneDecimal(value: Double): Double =
    BigDecimal(value).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble

  /** The badge TEXT for a 0–10 vote average — the one-decimal label IMDb/Filmweb
   *  render. Used as the "changed to" value recorded in the rating cadence. */
  def label(value: Double): String = f"$value%.1f"
}
