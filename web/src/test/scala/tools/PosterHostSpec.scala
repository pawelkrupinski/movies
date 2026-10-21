package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The low-cardinality name a poster's origin is counted under on /metrics (`poster_host`): the
 *  registrable domain, so a CDN's many shard hosts (`de.web.img2.acsta.net`, `…img3…`) are one
 *  series and a chain's per-venue subdomains (`arthousecrouchend.savoysystems.co.uk`) are its
 *  chain's. */
class PosterHostSpec extends AnyFlatSpec with Matchers {

  "family" should "name a poster's registrable domain" in {
    PosterHost.family("https://static.kinoheld.de/images/film/x.jpg") shouldBe Some("kinoheld.de")
    PosterHost.family("https://image.tmdb.org/t/p/w500/a.jpg") shouldBe Some("tmdb.org")
    PosterHost.family("https://de.web.img2.acsta.net/img/a.jpg") shouldBe Some("acsta.net")
    PosterHost.family("http://kinobulgarska19.pl/x.jpg") shouldBe Some("kinobulgarska19.pl")
  }

  it should "keep a second-level country suffix with the name it belongs to" in {
    PosterHost.family("https://arthousecrouchend.savoysystems.co.uk/a.png") shouldBe Some("savoysystems.co.uk")
    PosterHost.family("https://cms.cinemas.bfi.org.uk/a.jpg") shouldBe Some("bfi.org.uk")
    PosterHost.family("https://www.kino.com.pl/a.jpg") shouldBe Some("kino.com.pl")
  }

  it should "read the origin behind a weserv URL, whether its target carries a scheme or not" in {
    PosterHost.family("https://images.weserv.nl/?url=https%3A%2F%2Fstatic.kinoheld.de%2Fa.jpg&w=480") shouldBe Some("kinoheld.de")
    PosterHost.family("https://images.weserv.nl/?url=kinobulgarska19.pl%2Fa.jpg&w=480") shouldBe Some("kinobulgarska19.pl")
  }

  it should "lower-case the host and refuse what is not an absolute http(s) URL" in {
    PosterHost.family("https://Static.KINOHELD.de/a.jpg") shouldBe Some("kinoheld.de")
    PosterHost.family("/assets/poster.jpg") shouldBe None
    PosterHost.family("") shouldBe None
    PosterHost.family("data:image/png;base64,AAAA") shouldBe None
  }
}
