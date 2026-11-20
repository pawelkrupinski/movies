package tools

import java.net.http.HttpClient
import java.time.Duration

/** A per-host override of the network timeouts. `connectTimeout` bounds the
 *  TCP+TLS handshake (the JVM throws HttpConnectTimeoutException if either runs
 *  over); `requestTimeout` bounds the response read. Each defaults to the
 *  matching [[HostPolicies]] default, so a row overrides only what it names.
 *  `hostSuffixes` matches by exact host or a dotted sub-domain (see
 *  `HostPolicies.hostMatches`), so `www.x` matches `x` but an unrelated `*.y`
 *  does not.
 *
 *  `minRequestInterval` is the OUTBOUND pace [[PacedHttpFetch]] holds the
 *  whole fleet to for this host — the minimum gap between two requests to it,
 *  across every thread. `None` (the default) means unpaced: the host absorbs
 *  our natural concurrency, so only a host we structurally out-run names one.
 *
 *  `paceKnob` names the `/admin/config` knob that overrides `minRequestInterval` at
 *  RUNTIME. A host whose tolerance we don't know (nobody publishes one) can
 *  only be tuned empirically — push the pace until the 429s stop — and doing
 *  that through redeploys costs a worker restart and a cold JVM per attempt.
 *  Naming a knob makes the pace flippable from `/admin/config` mid-flight: the
 *  resolve in `HostPolicies` reads it per request, so the next request uses the
 *  new value. Still DATA, not an if-branch — any future host can name its own key.
 *
 *  `fleetMinInterval` is the pace [[PacedHttpFetch]] holds the WHOLE FLEET to for a host every country's
 *  worker asks from the same egress (a shared origin, where `minRequestInterval` paces one process); `fleetPaceKnob`
 *  retunes it at runtime as `paceKnob` does.
 *
 *  `headers` are sent on EVERY request to this host, GET and POST alike, on top
 *  of the defaults — for a host whose edge admits us only when we identify
 *  ourselves a particular way. An explicit caller header of the same name still
 *  wins (per-call beats per-host). This belongs on the transport, not the client,
 *  because it is a property of the ORIGIN's admission rule rather than of the API
 *  contract — and because applying it at the terminal fetch means none of the
 *  eleven `HttpFetch` decorators in the chain can silently drop it.
 *
 *  `httpVersion` is the protocol [[RealHttpFetch]] offers the host: HTTP/1.1 by default,
 *  HTTP/2 (ALPN, falling back to 1.1 where the server declines) for a host measured to
 *  gain from multiplexing — see [[HostPolicies.DefaultHttpVersion]]. */
final case class HostPolicy(
  hostSuffixes: Set[String],
  connectTimeout: Duration = HostPolicies.DefaultConnectTimeout,
  requestTimeout: Duration = HostPolicies.DefaultRequestTimeout,
  minRequestInterval: Option[Duration] = None,
  paceKnob: Option[tools.PaceKnob] = None,
  headers: Map[String, String] = Map.empty,
  fleetMinInterval: Option[Duration] = None,
  fleetPaceKnob: Option[tools.PaceKnob] = None,
  httpVersion: HttpClient.Version = HostPolicies.DefaultHttpVersion,
)
