package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, GaugeWithCallback}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.SlugRedirectKind
import play.api.mvc.RequestHeader

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap

/**
 * The retirement signal for the OLD ADDRESSES of cities — which kind of legacy
 * city address is still served, to which kind of client — so the old-owner table
 * ([[models.LegacySlugOwners]]) and the legacy redirects can be dropped once
 * nobody but crawlers uses them.
 *
 * [[WebRouteUsageMetrics]] cannot tell them apart: every city redirect is a hit on
 * the one `/:city/` pattern. This labels each by [[LegacyAddressKind]] instead.
 * Same design and for the same reason: a last-called TIMESTAMP per series, read
 * through `max_over_time(…[30d])`, because a pod restart drops the first
 * increment of a counter born after it — exactly the lone call this exists to
 * see — plus a counter for volume.
 *
 * Cardinality: kind (5) × client (5) per country. Never the slug.
 */
class LegacyAddressMetrics(registry: PrometheusRegistry, country: String, clock: Clock) {

  private val lastCalled = new ConcurrentHashMap[(LegacyAddressKind, String), Instant]()

  GaugeWithCallback.builder()
    .name("kinowo_web_legacy_address_last_called_seconds")
    .help("Unix time a legacy city address of each kind was last served since this process booted, by " +
      "country and client kind (ios_app, android_app, browser, bot, other); read with max_over_time " +
      "across restarts. Temporary — drop when legacy:front_door_old_owner shows no human/app calls " +
      "over 30 days.")
    .labelNames("country", "kind", "client")
    .callback { callback =>
      lastCalled.forEach { case ((kind, client), instant) =>
        callback.call(instant.getEpochSecond.toDouble, country, kind.label, client)
      }
    }
    .register(registry)

  private val hits = Counter.builder()
    .name("kinowo_web_legacy_address_hits_total")
    .help("Legacy city addresses served, by country, kind and client kind. Temporary — drop when " +
      "legacy:front_door_old_owner shows no human/app calls over 30 days.")
    .labelNames("country", "kind", "client")
    .register(registry)

  /** One served legacy hit. Skips what [[WebHttpMetrics.isMeasured]] skips — the
   *  `/metrics` scrape and the synthetic probe. */
  def record(kind: LegacyAddressKind, request: RequestHeader): Unit =
    if (WebHttpMetrics.isMeasured(request)) {
      val client = WebRouteUsageMetrics.clientKind(request.headers.get(play.api.http.HeaderNames.USER_AGENT))
      lastCalled.put((kind, client), clock.instant())
      hits.labelValues(country, kind.label, client).inc()
    }
}

/** The kinds of legacy city address [[LegacyAddressMetrics]] counts. */
enum LegacyAddressKind(val label: String) {
  /** A bare `showtimes.cc/{slug}/` the old-owner table decided. */
  case FrontDoorOldOwner extends LegacyAddressKind("legacy:front_door_old_owner")
  /** A bare `showtimes.cc/{slug}/` the table does not list: the one country that has it, or the first. */
  case FrontDoorNew extends LegacyAddressKind("legacy:front_door_new")
  /** A retired or renamed page's slug (`/uk/kent/`). */
  case RetiredSlug extends LegacyAddressKind("legacy:retired_slug")
  /** One of the twelve US and Spanish slugs a page was qualified to (`/us/birmingham-alabama/`). */
  case QualifiedToBare extends LegacyAddressKind("legacy:qualified_to_bare")
  /** Another name of a place a page covers: a town (`/uk/ilkley/`), a page's qualified slug. */
  case TownAlias extends LegacyAddressKind("legacy:town_alias")
}

object LegacyAddressKind {
  def of(kind: SlugRedirectKind): LegacyAddressKind = kind match {
    case SlugRedirectKind.Retired           => RetiredSlug
    case SlugRedirectKind.FormerlyQualified => QualifiedToBare
    case SlugRedirectKind.Alias             => TownAlias
  }
}
