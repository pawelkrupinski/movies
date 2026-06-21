package services.config

/**
 * Human-readable secondary value for a time-unit knob, shown alongside the raw
 * number on the `/admin/config` page. The unit is inferred from the key suffix:
 *
 *   - a millisecond knob (`*_MS` / `*_MILLIS` / `*_MILLISECONDS`) over 1000ms
 *     reads out in seconds — `5000` → `5 s`;
 *   - a second knob (`*_SECONDS` / `*_SECS`) over 60s reads out in minutes —
 *     `90` → `1.5 min`.
 *
 * Returns None for a non-numeric value, a non-time key, or a value below the
 * threshold (where the raw number is already the clearest form).
 */
object TimeKnobFormat {

  def humanize(key: String, value: String): Option[String] = {
    val k = key.toUpperCase
    value.trim.toLongOption.flatMap { n =>
      if (isMillis(k) && n >= 1000) Some(fmt(n / 1000.0, "s"))
      else if (isSeconds(k) && n >= 60) Some(fmt(n / 60.0, "min"))
      else None
    }
  }

  private def isMillis(k: String): Boolean =
    k.endsWith("_MS") || k.endsWith("_MILLIS") || k.endsWith("_MILLISECONDS")

  private def isSeconds(k: String): Boolean =
    k.endsWith("_SECONDS") || k.endsWith("_SECS")

  // Whole numbers print clean (5 s), otherwise one decimal (1.5 min).
  private def fmt(v: Double, unit: String): String = {
    val n = if (v == Math.floor(v)) v.toLong.toString else f"$v%.1f"
    s"$n $unit"
  }
}
