package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * The city listing may be held by a shared cache, and this is why that is safe.
 *
 * The rule `MovieController.SharedMaxAgeSeconds` states is absolute: only a
 * response that is byte-identical for every client may carry `s-maxage`, because
 * a shared cache hands one visitor's copy to the next. The listing used to fail
 * that on three bytes — the avatar `<div>`, the display name inside it, and
 * `IS_LOGGED_IN = true` — so it was pinned to `private, no-cache` and Cloudflare
 * held none of the HTML, only the JSON endpoints.
 *
 * Those three moved to `/api/me`, which `shared.js` fetches per client and which
 * says `private, no-store` itself. What is left is a controller that cannot
 * identify a visitor even if a page asked it to — `MovieController` takes no
 * `UserRepository` any more — so the assertions here are the direct ones: a
 * session cookie must move neither a byte of the body nor the header.
 */
class SharedCacheableListingSpec extends AnyFlatSpec with Matchers {

  private val Now = LocalDateTime.now()

  private def controller() = TestMovieController.build(
    Seq(("Test Film", Some(2024), MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](Helios -> SourceData(
        title = Some("Test Film"), releaseYear = Some(2024),
        showtimes = Seq(models.Showtime(Now.plusHours(2), None, None, Nil)))))))
  )._1

  // A real Play session, signed the way a browser's would be. Nothing on the
  // server side is meant to read it on this path — that is the point of sending
  // it — so the only thing it can prove is that it changed nothing.
  private def signedInRequest(path: String = "/poznan/") =
    FakeRequest("GET", path).withSession("userId" -> "alice@example.com")

  // NO TTL. The listing carries a strong per-city ETag, so the edge revalidates
  // against the validator instead of trusting a clock — `s-maxage` would only
  // add a window in which a changed city page is served stale.
  private val SharedCacheControl = "public, max-age=0, must-revalidate"

  "The listing rendered for a request carrying a session" should "name nobody" in {
    val body = contentAsString(controller().index("poznan")(signedInRequest()))

    body should not include ("alice@example.com")
    // The avatar `<div>`. Matched on the attribute rather than the bare class
    // name, which `_sharedStyles` ships to everybody either way.
    body should not include ("id=\"auth-menu\"")
    // And the flag that was the last user-specific byte once the avatar moved.
    body should not include ("IS_LOGGED_IN")
  }

  // The strongest form of the claim, and the one an edge cache actually relies
  // on: not "no name appears" but "the same bytes came back". Both requests go
  // through the gzip cache, so a difference of even one byte in the rendered
  // document would have minted a second entry instead of returning this one —
  // which makes this a regression test for ANY per-client variation somebody
  // re-introduces later, not just for an avatar.
  it should "be byte-identical to the one rendered without it" in {
    val ctrl = controller()
    val anonymous = contentAsBytes(ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> "gzip")))
    val withSession = contentAsBytes(ctrl.index("poznan")(
      signedInRequest().withHeaders("Accept-Encoding" -> "gzip")))

    withSession shouldBe anonymous
  }

  it should "be offered to the shared cache all the same" in {
    header("Cache-Control", controller().index("poznan")(
      signedInRequest().withHeaders("Accept-Encoding" -> "gzip"))).value shouldBe SharedCacheControl
  }

  "The listing with no session at all" should "get the same shared-cache header" in {
    header("Cache-Control", controller().index("poznan")(
      FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> "gzip"))).value shouldBe
      SharedCacheControl
  }

  // A client that cannot take gzip gets the uncompressed body rather than the
  // pre-compressed blob — but it is the same page, so it gets the same offer,
  // kept apart in the shared cache by `Vary: Accept-Encoding`.
  "A listing served uncompressed" should "still be shared-cacheable" in {
    header("Cache-Control", controller().index("poznan")(signedInRequest())).value shouldBe
      SharedCacheControl
  }

  // A filter query moves the OG meta, and `request.path` — the gzip cache's key —
  // drops the query string. Client-independent still, but one edge entry per
  // filter combination is not worth minting, so these stay browser-only.
  "A filtered listing" should "stay out of the shared cache" in {
    header("Cache-Control", controller().index("poznan")(
      FakeRequest("GET", "/poznan/?cinema=Helios").withHeaders("Accept-Encoding" -> "gzip")))
      .value shouldBe "private, no-cache"
  }

  // The one response that DOES name a visitor is the one the page fetches, and it
  // has to be the opposite of shareable — a stored copy is an avatar rebuilt for
  // whoever asks next, which is the failure the split exists to prevent.
  "The endpoint that names the visitor" should "forbid storing the answer" in {
    PerUserResponse.CacheControl shouldBe "private, no-store"
  }

  // The point of the policy, stated as its own case: an edge copy of a city
  // listing is only ever reused after asking us, so it cannot go stale.
  "The listing offered to the edge" should "carry no TTL for a shared cache to trust" in {
    val cc = header("Cache-Control", controller().index("poznan")(
      FakeRequest("GET", "/poznan/"))).value

    cc should include ("must-revalidate")
    cc should not include ("s-maxage")
    cc should not include ("max-age=6")   // any non-zero freshness lifetime
  }

  // And it still has the validator that makes revalidation cheap — without an
  // ETag a shared cache answers a conditional with the whole body.
  it should "carry the strong ETag that revalidation depends on" in {
    val etag = header("ETag", controller().index("poznan")(FakeRequest("GET", "/poznan/"))).value
    etag should startWith ("\"")   // strong, not W/
  }

  // ── The gzip cache must key on the HOST too ────────────────────────────────
  //
  // `og:url`, `<link rel=canonical>` and the JSON-LD are built from the request
  // host (`PageMeta.origin`), and one deployment can serve two hostnames — a
  // country's own domain and the shared brand apex. Keyed on path alone, the
  // blob rendered for the first host is handed to the second, advertising the
  // wrong canonical URL for the page. A shared cache keys on host itself, so
  // this is the ORIGIN's own cache getting it wrong.
  "The gzipped listing" should "not be reused across hostnames" in {
    val ctrl = controller()
    def render(host: String): String = {
      val result = ctrl.index("poznan")(FakeRequest("GET", "/poznan/")
        .withHeaders("Accept-Encoding" -> "gzip", "X-Forwarded-Host" -> host))
      // ⚠️ GUNZIP FIRST. The body is pre-compressed, so a substring assertion
      // against `contentAsString` matches nothing whatever the page says and
      // the case passes without testing anything.
      val raw = contentAsBytes(result).toArray
      val in  = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(raw))
      try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
    }

    val first  = render("kinowo.net")
    first should include ("<link rel=\"canonical\" href=\"http://kinowo.net/poznan/\">")
    val second = render("showtimes.cc")

    // The canonical and og:url are the host-derived bits. (`og:image` is an
    // absolute asset URL on the brand host by design, so the page naming
    // kinowo.net there is not this bug.)
    second should include ("<link rel=\"canonical\" href=\"http://showtimes.cc/poznan/\">")
    second should not include ("<link rel=\"canonical\" href=\"http://kinowo.net")
    second should not include ("og:url\"         content=\"http://kinowo.net")
  }

  // ── No Set-Cookie on the response the edge is meant to hold ────────────────
  //
  // MEASURED, not guessed: with the Cache Rule in place the listing went from
  // `cf-cache-status: DYNAMIC` (not even eligible) to `BYPASS` — eligible, and
  // then skipped. Cloudflare bypasses a response carrying `Set-Cookie`, so the
  // `city=` cookie was the last thing standing between us and an edge copy.
  //
  // The cookie itself is not lost: it exists so the bare `/` landing can bounce
  // a returning visitor to their city, it is `httpOnly=false` precisely so the
  // client can work with it, and `shared.js` now writes it on load with the
  // same name, path and lifetime.

  "The listing offered to the edge" should "carry no Set-Cookie for Cloudflare to bypass on" in {
    val result = controller().index("poznan")(FakeRequest("GET", "/poznan/"))

    header("Cache-Control", result).value should include ("public")
    // ⚠️ `header("Set-Cookie", …)` is ALWAYS None on a Result — Play keeps
    // cookies aside until the response is serialised, so asserting on the
    // header tests nothing at all. `cookies(…)` is the real question.
    cookies(result).get("city") shouldBe None
  }

  // The filtered variants are `private, no-cache` — no shared cache may hold
  // them, so a cookie there costs nothing and still serves a no-JS visitor.
  "A filtered listing" should "still set the city cookie server-side" in {
    val result = controller().index("poznan")(FakeRequest("GET", "/poznan/?cinema=Helios"))

    cookies(result).get("city").map(_.value) shouldBe Some("poznan")
  }
}
