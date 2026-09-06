package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.zip.GZIPInputStream

/** The plain `/{city}/` page is served as a pre-rendered, pre-gzipped blob to
 *  anonymous, gzip-accepting visitors. These assert the controller wiring of
 *  [[PageResponseCache]]: the right responses carry `Content-Encoding: gzip`
 *  and decode to the real page, while a non-gzip client still renders
 *  correctly. */
class PageCacheControllerSpec extends AnyFlatSpec with Matchers {

  private def cacheTestRecord(): MovieRecord = {
    val now = LocalDateTime.now()
    MovieRecord(
      imdbId = Some("tt123"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Cache Test Film"),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        )
      )
    )
  }

  private def buildController(): (MovieController, services.readmodel.WebReadModel) =
    TestMovieController.build(Seq(("Cache Test Film", Some(2024), cacheTestRecord())))

  private def gzipRequest(path: String) =
    FakeRequest("GET", path).withHeaders("Accept-Encoding" -> "gzip, deflate")

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes.toArray))
    new String(in.readAllBytes(), StandardCharsets.UTF_8)
  }

  "the / index page" should "be served gzip-precompressed to a gzip-accepting anonymous visitor" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    gunzip(contentAsBytes(result)) should include ("Cache Test Film")
  }

  it should "serve byte-identical bytes on a repeat request at the same cache version" in {
    val (ctrl, _) = buildController()
    val first  = contentAsBytes(ctrl.index("poznan")(gzipRequest("/poznan/")))
    val second = contentAsBytes(ctrl.index("poznan")(gzipRequest("/poznan/")))
    second shouldBe first
  }

  it should "re-serve a fresh valid page after the cache version advances" in {
    val (ctrl, cache) = buildController()
    ctrl.index("poznan")(gzipRequest("/poznan/"))

    Thread.sleep(1100) // mtime is second-resolution; ensure the rehydrate advances it
    cache.reload()

    val after = ctrl.index("poznan")(gzipRequest("/poznan/"))
    status(after) shouldBe OK
    header("Content-Encoding", after) shouldBe Some("gzip")
    gunzip(contentAsBytes(after)) should include ("Cache Test Film")
  }

  "a client that does not accept gzip" should "get an uncompressed page, not the precompressed blob" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(FakeRequest("GET", "/poznan/"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe None
    contentAsString(result) should include ("Cache Test Film")
  }

  // ── Browser conditional-GET (304 on refresh) ───────────────────────────────

  // `max-age=0` is what keeps this test's name true: the browser stores the page
  // and revalidates before every re-use, exactly as `private, no-cache` made it.
  // What changed is who may ANSWER that revalidation — the `s-maxage` lets
  // Cloudflare hold a copy for a minute and 304 the client itself instead of
  // waking the JVM, which only became safe once the page stopped naming the
  // visitor (see `SharedCacheableListingSpec`).
  "a cacheable page" should "carry Last-Modified + Cache-Control so the browser revalidates" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/"))

    header("Last-Modified", result) shouldBe defined
    // No TTL: the per-city ETag is exact, so the edge revalidates rather than
    // trusting a clock for N seconds.
    header("Cache-Control", result) shouldBe Some("public, max-age=0, must-revalidate, no-transform")
  }

  it should "304 a refresh whose If-Modified-Since is current, with no body" in {
    val (ctrl, _) = buildController()
    val first   = ctrl.index("poznan")(gzipRequest("/poznan/"))
    val lastMod = header("Last-Modified", first).get

    val refresh = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("If-Modified-Since" -> lastMod))
    status(refresh) shouldBe NOT_MODIFIED
    header("Content-Encoding", refresh) shouldBe None
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  it should "200 with a fresh body after the cache version advances despite an old If-Modified-Since" in {
    val (ctrl, cache) = buildController()
    val lastMod = header("Last-Modified", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    Thread.sleep(1100)
    cache.reload()

    val after = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("If-Modified-Since" -> lastMod))
    status(after) shouldBe OK
    header("Content-Encoding", after) shouldBe Some("gzip")
    gunzip(contentAsBytes(after)) should include ("Cache Test Film")
  }

  // ── Filter variants (`?date=`, `?q=`, …) ───────────────────────────────────
  //
  // These stay out of the shared gzip cache and out of the edge — they are
  // combinatorially many and would evict the bare city pages that earn their
  // place. What they DO get is a validator, so that `private, no-cache` means
  // "ask, then usually 304" rather than "ask, then always re-download". A
  // shared `?date=tomorrow` link was re-sending the whole listing on every
  // refresh because a revalidation had nothing to validate against.

  "a filtered page" should "still tell the browser to revalidate before re-use" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))

    status(result) shouldBe OK
    header("Cache-Control", result) shouldBe Some("private, no-cache, no-transform")
  }

  it should "carry validators so that revalidation can come back empty" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))

    header("ETag", result) shouldBe defined
    header("Last-Modified", result) shouldBe defined
  }

  it should "304 a refresh carrying the ETag it was given" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=tomorrow").withHeaders("If-None-Match" -> etag))
    status(refresh) shouldBe NOT_MODIFIED
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  // The page puts its own URL in `og:url`, so two filters really do render
  // different bytes — one must never validate the other.
  it should "not answer one filter with another filter's validator" in {
    val (ctrl, _) = buildController()
    val tomorrow = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get
    val week     = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=week"))).get

    tomorrow should not be week
    val crossed = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=week").withHeaders("If-None-Match" -> tomorrow))
    status(crossed) shouldBe OK
    contentAsString(crossed) should include ("Cache Test Film")
  }

  // The bare page keeps the precompressed blob; a filter variant must not take
  // an entry in that byte-bounded LRU.
  it should "not be served from the shared precompressed blob" in {
    val (ctrl, _) = buildController()
    header("Content-Encoding", ctrl.index("poznan")(gzipRequest("/poznan/"))) shouldBe Some("gzip")
    header("Content-Encoding", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))) shouldBe None
  }

  // One deployment answers on two hosts — a country's own domain and the shared
  // brand apex — and `og:url`, the canonical link and the JSON-LD are all built
  // from the request's own origin. The gzip blob has been keyed on the host
  // since a page was served advertising the other one's canonical URL; the
  // validator has to name the same thing, or the two hosts' pages share one.
  "the validator" should "distinguish the two hosts one deployment answers on" in {
    val (ctrl, _) = buildController()
    val own   = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("Host" -> "kinowo.net"))
    val apex  = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("Host" -> "showtimes.cc"))

    header("ETag", own) shouldBe defined
    header("ETag", own) should not be header("ETag", apex)
  }

  it should "distinguish them on a filtered page too, which keeps no blob at all" in {
    val (ctrl, _) = buildController()
    val own  = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=tomorrow").withHeaders("Host" -> "kinowo.net"))
    val apex = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=tomorrow").withHeaders("Host" -> "showtimes.cc"))

    header("ETag", own) shouldBe defined
    header("ETag", own) should not be header("ETag", apex)
  }

  // ── The ETag has to SURVIVE Cloudflare, not just leave the origin ──────────
  //
  // It did not. Measured 2026-09-06, same URL, same `Accept-Encoding: gzip`:
  //
  //   origin (curl --resolve, straight at the k3s node, Cloudflare bypassed)
  //     etag: "7ea9812c-6a9cfc88"
  //   edge (https://showtimes.cc/uk/manchester/)
  //     <no etag at all>
  //
  // while `/uk/manchester/api/repertoire` — the SAME helper, the same one line
  // that stamps the validator — kept its ETag at both. The bodies were
  // byte-identical (3826089 bytes each way), so nothing had actually been
  // rewritten. Cloudflare drops a STRONG ETag from anything it serves as HTML
  // because its HTML pipeline (Email Obfuscation, Rocket Loader, minification)
  // MAY rewrite the body, and a strong validator would then be a lie. It keeps a
  // WEAK one, which promises only semantic equivalence.
  //
  // And weak is what this validator has always actually been. It is
  // `bodyKey.hashCode` + the read-model stamp — a CONTENT VERSION, not a hash of
  // the bytes — so `W/` states a fact rather than making a concession: two
  // responses one second apart share a validator and can still differ in which
  // showtimes have already started. The same tag is stamped on the gzipped and
  // the identity response too, which a strong validator is not allowed to do.
  "the validator" should "be a WEAK ETag, which is what reaches the client through Cloudflare" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    etag should startWith ("W/\"")
    etag should endWith ("\"")
  }

  it should "be weak on a filtered page too" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get

    etag should startWith ("W/\"")
  }

  it should "be weak on the JSON payloads that share the helper" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))).get

    etag should startWith ("W/\"")
  }

  // A 304 has to say what the 200 would have said about varying, or a cache that
  // keeps the 304's headers can pair this validator with the wrong encoding of the
  // page. It only got away with it because Cloudflare adds the token itself: the
  // live origin answered a conditional with `vary: Origin` while the 200 beside it
  // said `vary: Accept-Encoding,Origin`.
  "a 304" should "carry the same Vary the 200 would have" in {
    val (ctrl, _) = buildController()
    val full = ctrl.index("poznan")(gzipRequest("/poznan/"))
    val etag = header("ETag", full).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag))
    status(refresh) shouldBe NOT_MODIFIED
    header("Vary", refresh) shouldBe header("Vary", full)
    header("Vary", refresh) shouldBe Some("Accept-Encoding")
  }

  it should "carry it on the JSON payloads too" in {
    val (ctrl, _) = buildController()
    val full = ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))
    val etag = header("ETag", full).get

    val refresh = ctrl.apiRepertoire("poznan")(
      gzipRequest("/poznan/api/repertoire").withHeaders("If-None-Match" -> etag))
    status(refresh) shouldBe NOT_MODIFIED
    header("Vary", refresh) shouldBe header("Vary", full)
  }

  // ── ONE encoding on the wire, because the edge caches only one ─────────────
  //
  // Brotli was built here and taken back out: Cloudflare caches ONE variant per
  // URL and asks the origin with its own `Accept-Encoding` even on a bypass, so
  // the br copy reached gzip-only clients DECOMPRESSED -- 1,470,154 B on the
  // Android app's own `/api/repertoire` fetch, seven times its proper size. A real
  // browser header, `br` and all, gets gzip.
  "the origin" should "answer gzip to a browser that would prefer brotli" in {
    val (ctrl, _) = buildController()
    Seq("gzip, deflate, br, zstd", "br, gzip", "*").foreach { accept =>
      val page = ctrl.index("poznan")(
        FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> accept))
      withClue(s"the bare listing, Accept-Encoding: $accept: ")(
        header("Content-Encoding", page) shouldBe Some("gzip"))
      gunzip(contentAsBytes(page)) should include ("Cache Test Film")

      val json = ctrl.apiRepertoire("poznan")(
        FakeRequest("GET", "/poznan/api/repertoire").withHeaders("Accept-Encoding" -> accept))
      withClue(s"the listing JSON, Accept-Encoding: $accept: ")(
        header("Content-Encoding", json) shouldBe Some("gzip"))
    }
  }

  // ── A bypassed page keeps no blob ──────────────────────────────────────────
  //
  // `private, no-cache` pages are answered `BYPASS` by the edge; they are still
  // rendered per request here, and must not take an entry in the byte-bounded LRU.
  it should "still keep no blob" in {
    val cache = new EncodedResponseCache
    val (ctrl, _) = TestMovieController.build(
      Seq(("Cache Test Film", Some(2024), cacheTestRecord())), responseCache = cache)

    ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))
    withClue("a filter variant must not take an entry in the byte-bounded LRU: ")(
      cache.heldEntries shouldBe 0)

    // …while the bare listing still does, so this is measuring the right thing.
    ctrl.index("poznan")(gzipRequest("/poznan/"))
    cache.heldEntries shouldBe 1
  }

  it should "leave the compression to the GzipFilter" in {
    val (ctrl, _) = buildController()
    header("Content-Encoding", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))) shouldBe None
  }

  // A client that refuses gzip still gets a body it can read, uncompressed.
  it should "send an uncompressed body to a client that refuses gzip" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> "gzip;q=0"))

    header("Content-Encoding", result) shouldBe None
    contentAsString(result) should include ("Cache Test Film")
  }

  // ── `no-transform`, without which the ETag above reaches nobody ────────────
  //
  // Cloudflare deletes the ETag from every text/html response these zones serve.
  // Measured 2026-09-06 against an UNCACHED page (`cf-cache-status: BYPASS`, so
  // not a stale stored copy) on BOTH domains: the origin sent
  // `W/"45a95918-6a9d0554"` and the edge sent no ETag at all, while the JSON this
  // same controller method builds came through with its tag intact. Making the tag
  // weak did NOT fix it -- weak was stripped exactly as strong had been.
  //
  // What it is reserving the right to do is legible in the response: the origin
  // sends `content-encoding: gzip`, the edge hands the client `content-encoding:
  // br`. It recompresses the body, so no validator we write describes what it
  // serves, so it drops ours. `no-transform` withdraws that permission.
  "every cacheable response" should "forbid the edge transforming the body, or it drops the ETag" in {
    val (ctrl, _) = buildController()

    header("Cache-Control", ctrl.index("poznan")(gzipRequest("/poznan/"))).get should
      include ("no-transform")
    header("Cache-Control", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get should
      include ("no-transform")
    header("Cache-Control", ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))).get should
      include ("no-transform")
  }

  it should "keep saying no-transform on the 304, which is the response that carries the win" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag))
    status(refresh) shouldBe NOT_MODIFIED
    header("Cache-Control", refresh).get should include ("no-transform")
  }

  // ── Weak comparison, which is the one RFC 9110 mandates for If-None-Match ──
  //
  // Exact string equality was survivable while we only ever emitted one
  // spelling of one tag. With a `W/` marker in play it stops being: an
  // intermediary is allowed to hand the tag back bare, and a browser that has
  // held two variants of a URL sends BOTH, comma-separated. Either one answered
  // 200 with the whole ~750 KB body under a `contains` check.
  "a conditional GET" should "304 when the tag comes back stripped of its weak marker" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag.stripPrefix("W/")))
    status(refresh) shouldBe NOT_MODIFIED
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  it should "304 when its tag is one of several the client offers" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/").withHeaders("If-None-Match" -> s"""W/"stale-one", $etag, W/"stale-two""""))
    status(refresh) shouldBe NOT_MODIFIED
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  it should "still serve the body when none of the offered tags is ours" in {
    val (ctrl, _) = buildController()
    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/").withHeaders("If-None-Match" -> """W/"stale-one", "stale-two""""))
    status(refresh) shouldBe OK
  }

  // ── The day the payload was cut for ────────────────────────────────────────

  "a zoned payload's validator" should "advance to the new day even when the model has not moved" in {
    val zone      = java.time.ZoneId.of("Europe/Warsaw")
    val beforeMid = java.time.ZonedDateTime.of(2026, 9, 5, 23, 40, 0, 0, zone).toInstant
    val afterMid  = java.time.ZonedDateTime.of(2026, 9, 6, 0, 5, 0, 0, zone).toInstant
    val dayStart  = java.time.ZonedDateTime.of(2026, 9, 6, 0, 0, 0, 0, zone).toInstant

    // Same model stamp on both sides of midnight — the day is what moved.
    MovieController.dayFlooredValidator(beforeMid, Some(zone), now = beforeMid) shouldBe beforeMid
    MovieController.dayFlooredValidator(beforeMid, Some(zone), now = afterMid)  shouldBe dayStart
  }

  it should "leave a stamp from later in the same day alone" in {
    val zone  = java.time.ZoneId.of("Europe/Warsaw")
    val noon  = java.time.ZonedDateTime.of(2026, 9, 6, 12, 0, 0, 0, zone).toInstant
    val later = java.time.ZonedDateTime.of(2026, 9, 6, 15, 0, 0, 0, zone).toInstant

    MovieController.dayFlooredValidator(noon, Some(zone), now = later) shouldBe noon
  }

  it should "leave a payload with no day in it on the model stamp alone" in {
    val stamp = java.time.Instant.parse("2020-01-01T00:00:00Z")
    MovieController.dayFlooredValidator(stamp, None) shouldBe stamp
  }
}
