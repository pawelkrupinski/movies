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
    rendered                          should include ("https://example.com/broken.jpg")
    rendered                          should include ("onerror")
    rendered                          should include ("Brak plakatu")
    rendered                          should include ("display:none")
  }
}
