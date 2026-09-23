package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

/**
 * WHEN the legacy `PUT /api/me/state` was last called — the one signal
 * `kinowo_web_http_requests_total{country, method="PUT", route="/api/me/state", status}`
 * (already recorded, generically, by `HttpMetricsFilter` for every route —
 * see [[services.metrics.WebHttpMetrics]]) can't answer on its own. That
 * counter says HOW OFTEN; this gauge says HOW RECENTLY, and "safe to retire"
 * needs both: a route with a healthy-looking call rate over the last hour
 * reads very differently from one whose last call was 90 days ago and the
 * rate panel is just Prometheus counting zero forever. Deliberately NOT a
 * second counter — duplicating `kinowo_web_http_requests_total` for one
 * route would just be the same number under a different name.
 *
 * `PUT /api/me/state` is the endpoint `hideFilm`/`unhideFilm`/
 * `clearHiddenFilms`/`hiddenFilms()` exist to make obsolete for hiddenFilms
 * (disabledCinemas already left the sync path entirely — see
 * `StateSyncService`, mobile, and `shared.js`, web). Once every shipped
 * client is on the new endpoints this gauge should read "no calls in N days"
 * — that reading is what says the legacy SET fields can actually be deleted,
 * not just "it still compiles". A `language`-only PUT is not counted: that is
 * the web client's own, intended use (`language` has no granular successor),
 * so the endpoint itself stays — see `UserStateController.put`.
 */
class LegacyUserStateMetrics(registry: PrometheusRegistry, country: String) {
  private val lastPutCall = new AtomicReference[Instant](null)

  GaugeWithCallback.builder()
    .name("kinowo_web_legacy_userstate_put_last_called_seconds")
    .help("Unix time PUT /api/me/state (the pre-per-country, full-state-replace endpoint) was last " +
      "called, by country. No series until the first call since this process booted — that's " +
      "\"unknown\", not \"never\", since a restart forgets it. Read alongside " +
      "kinowo_web_http_requests_total{route=\"/api/me/state\",method=\"PUT\"} for call volume.")
    .labelNames("country")
    .callback { callback =>
      Option(lastPutCall.get()).foreach(instant => callback.call(instant.getEpochSecond.toDouble, country))
    }
    .register(registry)

  /** Called once per non-language-only `PUT /api/me/state`, from `UserStateController.put()`. */
  def recordPutCall(now: Instant = Instant.now()): Unit = lastPutCall.set(now)
}
