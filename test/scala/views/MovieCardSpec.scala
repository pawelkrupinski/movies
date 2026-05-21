package views

import models.Movie
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.twirl.api.Html

class MovieCardSpec extends AnyFlatSpec with Matchers {

  private val movie = Movie("Karuppu", Some(120))

  "_movieCard" should "render a 'Brak plakatu' placeholder when the poster URL is missing" in {
    val rendered = views.html._movieCard(movie, None)(Html("")).body
    rendered                          should include ("Brak plakatu")
    rendered                          should include ("class=\"no-poster\"")
    rendered                          should not include "<img"
  }

  it should "include an onerror fallback so a broken poster URL also shows 'Brak plakatu'" in {
    // Karuppu (Cinema City 8203s2r) returns a posterLink that 404s — the live page
    // ends up with a broken image. The onerror handler swaps the <img> for the
    // visible "Brak plakatu" placeholder when the browser can't load the asset.
    val rendered = views.html._movieCard(movie, Some("https://example.com/broken.jpg"))(Html("")).body
    // The src URL goes through `tools.PosterProxy` (HTTPS-forcing /
    // resizing image proxy on `images.weserv.nl`), so the original
    // string isn't in the HTML — but the proxied URL embeds it,
    // along with the `w=480&output=webp` size + format hints.
    // Twirl HTML-escapes `&` → `&amp;` when interpolating into attribute
    // values, so the literal HTML carries `&amp;w=480` etc. Browsers
    // unescape on read; weserv sees the original query string. The
    // helper is still tested for its raw output in PosterProxySpec.
    rendered                          should include ("https://images.weserv.nl/?url=example.com%2Fbroken.jpg")
    rendered                          should include ("&amp;w=480")
    rendered                          should include ("&amp;h=720")
    rendered                          should include ("&amp;fit=cover")
    rendered                          should include ("&amp;a=attention")
    rendered                          should include ("&amp;output=webp")
    rendered                          should include ("onerror")
    rendered                          should include ("Brak plakatu")
    rendered                          should include ("display:none")
  }

  // The whole-movie favourites star (drives /ulubione + the visual yellow
  // state). Lives next to the existing hide button on the poster.
  it should "render a poster-overlay favourite button paired with the hide button" in {
    val rendered = views.html._movieCard(movie, None)(Html("")).body
    rendered should include ("""class="fav-poster-btn"""")
    rendered should include ("""class="hide-btn"""")
    // No per-button onclick — a single delegated click handler in
    // `_sharedJs` routes `.fav-poster-btn` / `.hide-btn` taps to the
    // respective JS functions. Saves ~13 KB raw across ~190 cards.
    rendered should not include "onclick=\"toggleFavMovie"
    rendered should not include "onclick=\"hideFilm"
  }
}
