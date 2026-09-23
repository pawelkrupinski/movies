package services.users

/**
 * Where a user-state store reports how each atomic write went — the seam
 * `services.metrics.UserStateWriteMetrics` counts through, so the store knows
 * nothing about Prometheus.
 *
 * Exactly one outcome per write, which is what makes the four add up to the
 * endpoint's traffic: `ok`; `conflict` (it raced another first write for the
 * same user, lost the upsert to the unique index, and succeeded on the retry);
 * `store_failure` (the store threw — the caller answers 503); `unavailable`
 * (there is no store to write to — also a 503). Before the writes were atomic, a
 * conflict was a lost update or, after five lost races, a 503; the `conflict`
 * count is how to see that race is still happening and still harmless.
 */
trait UserStateWriteOutcomes {
  def record(endpoint: String, outcome: String): Unit
}

object UserStateWriteOutcomes {
  val none: UserStateWriteOutcomes = (_: String, _: String) => ()

  /** The `endpoint` label: which write API the request came through. */
  object Endpoint {
    val Hide      = "hide"
    val Unhide    = "unhide"
    val Clear     = "clear"
    val LegacyPut = "legacy_put"
    val all: Seq[String] = Seq(Hide, Unhide, Clear, LegacyPut)

    def of(change: HiddenFilmsChange): String = change match {
      case _: HiddenFilmsChange.Hide   => Hide
      case _: HiddenFilmsChange.Unhide => Unhide
      case HiddenFilmsChange.Clear     => Clear
    }
  }

  object Outcome {
    val Ok           = "ok"
    val Conflict     = "conflict"
    val StoreFailure = "store_failure"
    val Unavailable  = "unavailable"
    val all: Seq[String] = Seq(Ok, Conflict, StoreFailure, Unavailable)
  }
}
