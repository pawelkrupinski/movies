package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PosterProxySpec extends AnyFlatSpec with Matchers {

  // The whole point of this proxy is to force HTTPS on mixed-content
  // (HTTP-only) origins like kinobulgarska19.pl that browsers were
  // silently blocking on the HTTPS kinowo.fly.dev page.
  "PosterProxy.proxy" should "force the request URL to HTTPS regardless of origin scheme" in {
    val http  = PosterProxy.proxy("http://kinobulgarska19.pl/wp-content/uploads/2026/04/x.jpg")
    val https = PosterProxy.proxy("https://kinoapollo.pl/wp-content/uploads/2026/04/y.png")
    http  should startWith ("https://images.weserv.nl/")
    https should startWith ("https://images.weserv.nl/")
  }

  it should "URL-encode the origin URL so cinema paths with query strings round-trip cleanly" in {
    val proxied = PosterProxy.proxy("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/235/poster.jpg?rev=abc&v=1")
    // The original `?rev=abc&v=1` query string must be encoded as part
    // of the `url=` value, NOT bleed out into the weserv URL's own
    // query (which would make weserv interpret `&v=1` as one of its
    // own parameters and either ignore it or break the cache key).
    proxied should include ("url=image.bilety24.pl%2Fsf_api_thumb_400%2Fdealer-default%2F235%2Fposter.jpg%3Frev%3Dabc%26v%3D1")
  }

  it should "request a 480×720 webp cover-crop with attention-based focal point" in {
    val proxied = PosterProxy.proxy("https://example.com/poster.png")
    proxied should include ("&w=480")
    // `h` + `fit=cover` + `a=attention` together turn landscape banners
    // ("_plakat_cut" crops some cinemas publish) into portrait by
    // cropping around the most salient region of the original instead
    // of letterboxing or letting `object-fit: cover` do a dumb center
    // crop client-side. Already-portrait sources pass through with
    // minimal loss.
    proxied should include ("&h=720")
    proxied should include ("&fit=cover")
    proxied should include ("&a=attention")
    proxied should include ("&output=webp")
  }

  // Defensive corner case: SourceData.posterUrl is Some("") on a row
  // whose cinema scrape recorded the field as present-but-empty.
  // Returning the empty string here means the template still emits an
  // <img src=""> which triggers the `onerror` fallback to .no-poster
  // rather than firing a doomed proxy request.
  it should "return the input untouched when given an empty string" in {
    PosterProxy.proxy("") shouldBe ""
  }

  // www.multikino.pl blocks weserv's outbound IP with 403 (verified by
  // probing 62 unique multikino poster URLs through weserv — every
  // single one returned 404, while the same URLs fetched directly with
  // a browser-shaped Referer return 200). For these the proxy is a
  // regression; pass the URL through unchanged so the browser fetches
  // multikino directly.
  it should "pass multikino.pl URLs through unproxied (their origin blocks weserv)" in {
    val multikino = "https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/x/y.jpg?rev=abc"
    PosterProxy.proxy(multikino) shouldBe multikino
  }

  // Sanity: the carve-out is hostname-specific, not pattern-based —
  // similar paths on other hosts still go through the proxy.
  it should "still proxy non-skipped HTTPS hosts like cinema-city.pl" in {
    val cc = "https://www.cinema-city.pl/xmedia-cw/repo/feats/posters/7807S2R.jpg"
    PosterProxy.proxy(cc) should startWith ("https://images.weserv.nl/")
  }

  // acsta.net is AlloCiné's image CDN — the poster origin behind the DE
  // (Webedia) listings. weserv refuses the whole domain at its own edge:
  //   GET images.weserv.nl/?url=de.web.img3.acsta.net%2F...
  //     -> 400 {"status":"error","code":400,
  //             "message":"Domain or TLD blocked by policy"}
  // while the same asset fetched directly returns 200 image/jpeg. This is
  // the inverse of the multikino case (there the *origin* blocks weserv;
  // here *weserv* blocks the origin) but the remedy is identical: skip the
  // proxy and let the browser fetch the origin, which is already HTTPS.
  //
  // Verified 2026-07-19 against prod /uptime, where `img: images.weserv.nl`
  // sat red for ~2.5h on 26 distinct acsta URLs — i.e. every DE film whose
  // poster came from AlloCiné rendered as a broken image.
  it should "pass acsta.net URLs through unproxied (weserv blocks the domain by policy)" in {
    val allocine = "https://de.web.img3.acsta.net/img/fe/d6/fed6aa938ed8c8469d6ebd7ce46f25b3.jpg"
    PosterProxy.proxy(allocine) shouldBe allocine
  }

  // The block is domain-wide, not per-subdomain: AlloCiné round-robins
  // posters across img1..imgN and per-country prefixes (de./fr./www.), and
  // all of them 400 the same way. Match on the registrable domain so a new
  // shard doesn't silently reintroduce the breakage.
  it should "pass every acsta.net subdomain through unproxied" in {
    Seq(
      "https://de.web.img3.acsta.net/img/a/b/c.jpg",
      "https://fr.web.img4.acsta.net/img/a/b/c.jpg",
      "https://www.acsta.net/img/a/b/c.jpg"
    ).foreach(url => PosterProxy.proxy(url) shouldBe url)
  }

  // m.media-amazon.com is IMDb's poster CDN, and it 403s weserv's outbound IP
  // exactly like multikino does (weserv surfaces that as its own 404):
  //   {"status":"error","code":404,"message":"The requested URL returned error: 403"}
  // Probed every unique Amazon poster referenced by the DE, PL and UK city
  // pages on 2026-07-19: 535/535 failed through weserv, 535/535 returned
  // 200 image/jpeg fetched directly — with or without a browser UA/Referer,
  // so it's weserv's IP being blocked, not a missing header.
  //
  // This one is mostly invisible in /uptime because Amazon is nearly always a
  // *fallback* poster rather than the primary src (182 fallback refs vs 6
  // primary on /berlin/), and the onerror chain silently walks past a failed
  // fallback. The films that DO have it primary just render no poster.
  it should "pass m.media-amazon.com URLs through unproxied (their origin blocks weserv)" in {
    val imdbPoster = "https://m.media-amazon.com/images/M/MV5BYzBjMDg4YjctYzg3ZS00ZDFmLmpwZw@@._V1_.jpg"
    PosterProxy.proxy(imdbPoster) shouldBe imdbPoster
  }

  // Guard the suffix match against the classic "endsWith" hole — a
  // lookalike domain that merely ends in the same letters must NOT be
  // treated as acsta.net and must still be proxied.
  it should "not mistake a lookalike domain for acsta.net" in {
    val lookalike = "https://notacsta.net/img/a/b/c.jpg"
    PosterProxy.proxy(lookalike) should startWith ("https://images.weserv.nl/")
  }
}
