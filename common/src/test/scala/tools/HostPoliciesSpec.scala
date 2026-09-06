package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Duration

/**
 * The per-host policy table and its lookups — the only part of the network
 * policy reachable without a live server. The actual slow-handshake / slow-read
 * behaviour (a host whose TLS handshake or response read runs far past our
 * defaults and dies with HttpConnectTimeoutException / HttpTimeoutException) needs
 * a real upstream and can't be reproduced in a unit test, so we assert the closest
 * reachable mechanism: that each policied host resolves to its intended connect /
 * request budget, pace, and headers, while every other host keeps the defaults.
 * Host matching is exact-host-or-dotted-sub-domain, so `www.x` matches `x` but an
 * unrelated `*.y` does not, and a malformed URL falls through to the defaults.
 * (That a real client carries the connect budget is RealHttpFetchSpec's job.)
 */
class HostPoliciesSpec extends AnyFlatSpec with Matchers {

  // ── Suffix matching, for EVERY row ────────────────────────────────────────
  // Rows match by host SUFFIX. That is what lets `www.x` share `x`'s row — and
  // what leaves a sibling market on a different host (flicks.us beside
  // flicks.co.uk, sensacine.com beside filmstarts.de) with NO row unless it
  // earns its own. Both halves are pinned over the whole table, so a new row
  // is covered the day it lands rather than when someone remembers to add a
  // host-specific case.

  private def subdomainOf(suffix: String)   = s"https://www.$suffix/x"
  private def exactHost(suffix: String)     = s"https://$suffix/x"
  /** Ends with the suffix's TEXT but not with `.` + suffix — the host the dotted
   *  rule exists to exclude (`xfilmstarts.de` is not a Filmstarts host). */
  private def lookalikeOf(suffix: String)   = s"https://x$suffix/x"

  "every row" should "give a dotted sub-domain of each of its hosts the row's own values" in {
    for (row <- HostPolicies.all; suffix <- row.hostSuffixes) withClue(s"$suffix: ") {
      val url = subdomainOf(suffix)
      HostPolicies.connectTimeoutFor(url) shouldBe row.connectTimeout
      HostPolicies.requestTimeoutFor(url) shouldBe row.requestTimeout
      HostPolicies.headersFor(url) shouldBe row.headers
      // The pace goes through the live knob, so compare the two lookups that
      // share that path rather than the compiled-in value an override may shift.
      HostPolicies.requestIntervalFor(url) shouldBe HostPolicies.requestIntervalFor(exactHost(suffix))
      HostPolicies.requestIntervalFor(url).isDefined shouldBe row.minRequestInterval.isDefined
    }
  }

  it should "leave a host that merely ENDS with the suffix text on the defaults" in {
    for (row <- HostPolicies.all; suffix <- row.hostSuffixes) withClue(s"$suffix: ") {
      val url = lookalikeOf(suffix)
      HostPolicies.connectTimeoutFor(url) shouldBe HostPolicies.DefaultConnectTimeout
      HostPolicies.requestTimeoutFor(url) shouldBe HostPolicies.DefaultRequestTimeout
      HostPolicies.headersFor(url) shouldBe empty
      HostPolicies.requestIntervalFor(url) shouldBe None
    }
  }

  it should "differ from the defaults in at least one of connect timeout, request timeout, pace, or headers" in {
    // A row that no longer overrides anything is dead data: the host would get
    // exactly what it gets with no row, so the row only misleads the next reader
    // into thinking the host is special-cased.
    for (row <- HostPolicies.all) withClue(s"${row.hostSuffixes}: ") {
      val overridesSomething =
        row.connectTimeout != HostPolicies.DefaultConnectTimeout ||
        row.requestTimeout != HostPolicies.DefaultRequestTimeout ||
        row.minRequestInterval.isDefined ||
        row.headers.nonEmpty
      overridesSomething shouldBe true
    }
  }

  it should "name at least one host, in lowercase, and share none with another row" in {
    // `hostMatches` lowercases the URL's host but compares the suffix verbatim,
    // so a capitalised suffix would match nothing. And first-match-wins means a
    // suffix repeated on a later row is silently shadowed by the earlier one.
    for (row <- HostPolicies.all) withClue(s"${row.hostSuffixes}: ") {
      row.hostSuffixes should not be empty
      row.hostSuffixes.foreach(suffix => suffix shouldBe suffix.toLowerCase)
    }
    val suffixes = HostPolicies.all.toList.flatMap(_.hostSuffixes)
    suffixes.distinct shouldBe suffixes
  }

  // ── Slow-TLS connect budget (Kino Iluzjon) ────────────────────────────────
  // Iluzjon's TLS handshake runs 20-30s server-side; under the 5s default connect
  // budget every fetch died with HttpConnectTimeoutException. Its host policy gives
  // it a long connect budget; the response-read budget stays the default.

  "connectTimeoutFor" should "give Kino Iluzjon's host and its sub-domains the long connect budget" in {
    HostPolicies.connectTimeoutFor("https://www.iluzjon.fn.org.pl/repertuar.html") shouldBe Duration.ofSeconds(40)
    HostPolicies.connectTimeoutFor("https://iluzjon.fn.org.pl/filmy/info/42/x.html") shouldBe Duration.ofSeconds(40)
  }

  it should "keep the tight default for unrelated hosts (incl. a different fn.org.pl sub-domain) and a malformed URL" in {
    HostPolicies.connectTimeoutFor("https://www.multikino.pl/repertuar") shouldBe HostPolicies.DefaultConnectTimeout
    HostPolicies.connectTimeoutFor("https://api.themoviedb.org/3/movie/1") shouldBe HostPolicies.DefaultConnectTimeout
    HostPolicies.connectTimeoutFor("https://other.fn.org.pl/x") shouldBe HostPolicies.DefaultConnectTimeout
    HostPolicies.connectTimeoutFor("not a url") shouldBe HostPolicies.DefaultConnectTimeout
  }

  it should "be longer than the tight default and above the worst handshake measured (~27s)" in {
    val iluzjon = HostPolicies.connectTimeoutFor("https://iluzjon.fn.org.pl/x")
    iluzjon.compareTo(HostPolicies.DefaultConnectTimeout) should be > 0
    iluzjon.compareTo(Duration.ofSeconds(30)) should be > 0
  }

  // ── Fast-fail request budget (stall-prone enrichment host: Helios REST) ────
  // restapi.helios.pl's detail endpoints intermittently hang ~30s for our
  // datacenter egress; under the 30s default each hang pinned a ParallelDetail-
  // Fetch slot, draining the worker's CPU credit into a throttle spiral
  // (2026-06-23). Its host policy gives the tight response-read budget, while
  // every other host keeps the generous default.

  "requestTimeoutFor" should "give Helios's REST host and its sub-domains the tight fast-fail budget" in {
    HostPolicies.requestTimeoutFor("https://restapi.helios.pl/api/cinema/4b/screen/6c") shouldBe Duration.ofSeconds(4)
    HostPolicies.requestTimeoutFor("https://www.restapi.helios.pl/api/movie/1") shouldBe Duration.ofSeconds(4)
  }

  it should "keep the default for Helios's own NUXT site, an unrelated host, and a malformed URL" in {
    HostPolicies.requestTimeoutFor("https://www.helios.pl/poznan/kino-helios/repertuar") shouldBe HostPolicies.DefaultRequestTimeout
    HostPolicies.requestTimeoutFor("https://api.themoviedb.org/3/movie/1") shouldBe HostPolicies.DefaultRequestTimeout
    HostPolicies.requestTimeoutFor("not a url") shouldBe HostPolicies.DefaultRequestTimeout
  }

  it should "make Helios's budget below the ~5-7s degraded tail (so a slow host times out and trips the breaker) but above the ~1-3s healthy call" in {
    val helios = HostPolicies.requestTimeoutFor("https://restapi.helios.pl/api/x")
    helios.compareTo(Duration.ofSeconds(5)) should be < 0   // below the degraded tail → slow host times out
    helios.compareTo(Duration.ofSeconds(3)) should be > 0   // above the healthy ~1-3s call
  }

  // ── Metacritic middle budget (slow Cloudflare origin) ─────────────────────

  it should "give Metacritic and its sub-domains a budget between the fast-fail and the default" in {
    // ~35% of metascore fetches legitimately take >5s, so the 8s fast-fail budget
    // would cut them; 15s caps a real hang while clearing the healthy tail.
    HostPolicies.requestTimeoutFor("https://www.metacritic.com/movie/dune-part-two/") shouldBe Duration.ofSeconds(15)
    HostPolicies.requestTimeoutFor("https://metacritic.com/search/x/?category=2") shouldBe Duration.ofSeconds(15)
    val mc = HostPolicies.requestTimeoutFor("https://www.metacritic.com/x")
    mc.compareTo(HostPolicies.requestTimeoutFor("https://restapi.helios.pl/api/x")) should be > 0
    mc.compareTo(HostPolicies.DefaultRequestTimeout) should be < 0
  }

  // ── Per-host identifying headers (IMDb's GraphQL CDN) ─────────────────────
  // On 2026-08-01 IMDb's edge started 403-ing every POST to
  // caching.graphql.imdb.com that arrives without a client-identifying header —
  // an nginx-shaped "403 Forbidden" page, not a GraphQL error, so the block is at
  // the CDN not the API. Every IMDb rating in all three countries silently became
  // "rating none". The same request WITH `x-imdb-client-name` returns 200 (verified
  // against the live endpoint from both a residential and the Fly egress, so this
  // is a request-shape rule, not an IP ban). The header rides on the host policy
  // table — data, one row, like every other per-host rule.

  "headersFor" should "give IMDb's GraphQL CDN the client-name header its edge now demands" in {
    HostPolicies.headersFor("https://caching.graphql.imdb.com/") shouldBe
      Map("x-imdb-client-name" -> "imdb-web-next")
  }

  it should "leave every other host — including IMDb's suggestion endpoint, which is not blocked — header-free" in {
    HostPolicies.headersFor("https://v3.sg.media-imdb.com/suggestion/i/interstellar.json") shouldBe empty
    HostPolicies.headersFor("https://api.themoviedb.org/3/movie/1") shouldBe empty
    HostPolicies.headersFor("not a url") shouldBe empty
  }
}
