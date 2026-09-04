package controllers

import java.util.concurrent.ConcurrentHashMap

/** The closed set of host labels the anonymous poster-telemetry beacon
 *  (`POST /uptime/img-event`) is allowed to create rows for.
 *
 *  WHY THIS EXISTS. The beacon is written by every visitor's browser, so its
 *  `host` field is client-controlled, and the host becomes a KEY:
 *  [[services.UptimeMonitor]] stores one bucket map per service name and flushes
 *  each to Mongo. An unclamped key is therefore not a display bug but unbounded
 *  growth in a process that already OOM-killed itself once on the /uptime page's
 *  object count (2026-08-31) — a caller posting a fresh random host each time
 *  would add a row, a bucket map and a Mongo document per request, for as long
 *  as it kept going.
 *
 *  Enumerating the legitimate hosts is not an option: they are every cinema
 *  origin in every country plus whatever CDN TMDB is on that month, and a list
 *  that drifts would silently stop recording the outage it exists to catch.
 *
 *  So the bound is a CAP WITH A FOLD, the same shape
 *  [[services.metrics.WebHttpMetrics]] uses for unmatched routes: the first
 *  [[limit]] well-formed hosts each get their own row, and everything after
 *  them — along with anything that is not shaped like a hostname at all —
 *  collapses into a single [[ImageOriginRoster.Overflow]] bucket. Nothing is
 *  dropped silently, and no caller can make this map grow past `limit + 1`.
 *
 *  One instance per process, held by [[UptimeController]]; `label` is called
 *  once per reported event and is safe from every request thread. */
class ImageOriginRoster(limit: Int = ImageOriginRoster.DefaultLimit) {

  private val known = ConcurrentHashMap.newKeySet[String]()

  /** The row this event should be recorded under: the host itself while there
   *  is room for it, [[ImageOriginRoster.Overflow]] once there is not. */
  def label(rawHost: String): String = {
    val host = ImageOriginRoster.normalise(rawHost)
    if (host.isEmpty) ImageOriginRoster.Overflow
    else if (known.contains(host)) host
    // `size` is read before the add rather than after, so concurrent first
    // sightings can overshoot the cap by at most the number of threads in
    // flight — which is the right trade against locking every event.
    else if (known.size >= limit) ImageOriginRoster.Overflow
    else { known.add(host); host }
  }

  /** How many distinct origins have been seen. Test seam. */
  def size: Int = known.size
}

object ImageOriginRoster {

  /** Where a malformed host, and every host past the cap, is recorded. Reads as
   *  a row on /uptime rather than vanishing, so an origin that got folded is
   *  visible as something to raise the cap for. */
  val Overflow = "other"

  /** Comfortably above the real population — five countries' cinema origins
   *  plus the poster CDNs came to well under 60 when this was written — and far
   *  below anything that would cost memory. */
  val DefaultLimit = 200

  private val MaxHostLength = 100

  /** A hostname, lowercased, or the empty string when the input is not one.
   *  Deliberately stricter than a URL parser: this only ever receives the host
   *  the tracker already extracted, so anything with a slash, a port, a space
   *  or a credential in it is a caller doing something other than reporting a
   *  poster. */
  private val Hostname = """[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+""".r

  def normalise(rawHost: String): String = {
    val host = rawHost.trim.toLowerCase
    if (host.length > MaxHostLength) "" else Hostname.findFirstIn(host).filter(_ == host).getOrElse("")
  }
}
