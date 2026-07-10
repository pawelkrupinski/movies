package views

import testsupport.TestMessages.given

import models.{CinemaCityKinepolis, Imdb, Movie, MovieRecord, Multikino, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.twirl.api.Html
import services.readmodel.TestReadModel

class MovieCardSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan
  private val movie = Movie("Karuppu", Some(120))
  private val emptyResolved = TestReadModel.resolved("Karuppu", None, MovieRecord())

  "_movieCard" should "render a 'Brak plakatu' placeholder when the poster URL is missing" in {
    val rendered = views.html._movieCard(movie, None, emptyResolved)(Html("")).body
    rendered                          should include ("Brak plakatu")
    rendered                          should include ("class=\"no-poster\"")
    rendered                          should not include "<img"
  }

  it should "include an onerror fallback so a broken poster URL also shows 'Brak plakatu'" in {
    // Karuppu (Cinema City 8203s2r) returns a posterLink that 404s — the live page
    // ends up with a broken image. The onerror handler swaps the <img> for the
    // visible "Brak plakatu" placeholder when the browser can't load the asset.
    val rendered = views.html._movieCard(movie, Some("https://example.com/broken.jpg"), emptyResolved)(Html("")).body
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

  it should "ship every non-primary poster URL as a pipe-separated data-fallbacks chain" in {
    // Werdykt-style row: Multikino is primary (currently 403s in the
    // wild), Cinema City has a working poster, TMDB and IMDb have
    // their own. The card should expose them all as a chain so
    // `onerror` can walk through cinema posters before TMDB/IMDb.
    val multikino  = "https://www.multikino.pl/.../werdykt.jpg"
    val cinemaCity = "https://www.cinema-city.pl/.../8194S2R.jpg"
    val tmdb       = "https://image.tmdb.org/t/p/w500/x.jpg"
    val imdb       = "https://m.media-amazon.com/.../poster.jpg"
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino           -> SourceData(posterUrl = Some(multikino)),
        CinemaCityKinepolis -> SourceData(posterUrl = Some(cinemaCity)),
        Tmdb                -> SourceData(posterUrl = Some(tmdb)),
        Imdb                -> SourceData(posterUrl = Some(imdb))
      )
    )
    val rendered = views.html._movieCard(movie, Some(multikino), TestReadModel.resolved("Karuppu", None, record))(Html("")).body
    rendered should include ("data-fallbacks=\"")
    // Three fallbacks, joined by a literal pipe. Each individually goes
    // through PosterProxy → weserv (Cinema City is HTTPS but still gets
    // resize + webp; the SkipHosts in PosterProxy doesn't include
    // weserv-friendly cinema hosts).
    rendered should include ("cinema-city.pl")
    rendered should include ("image.tmdb.org")
    rendered should include ("media-amazon.com")
    // Pipe separator between the proxied URLs.
    rendered should include ("output=webp|https://images.weserv.nl")
    // The walking-onerror handler reads `data-fallbacks`, splits on `|`,
    // and falls through to the `.no-poster` block when empty.
    rendered should include ("dataset.fallbacks")
    rendered should include ("split('|')")
  }

  it should "omit data-fallbacks when no alternative poster exists" in {
    val onlyMultikino = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(posterUrl = Some("https://www.multikino.pl/x.jpg"))
      )
    )
    val rendered = views.html._movieCard(
      movie, Some("https://www.multikino.pl/x.jpg"), TestReadModel.resolved("Karuppu", None, onlyMultikino)
    )(Html("")).body
    rendered should not include "data-fallbacks="
    // The walking-onerror handler is still wired up — it just no-ops
    // (empty chain) and falls straight through to `.no-poster`.
    rendered should include ("dataset.fallbacks")
  }

  it should "render a poster-overlay hide button with delegated click handler" in {
    val rendered = views.html._movieCard(movie, None, emptyResolved)(Html("")).body
    rendered should include ("""class="hide-btn"""")
    rendered should not include "onclick=\"hideFilm"
  }
}
