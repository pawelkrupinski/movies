package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

/** Cinemeta's title→imdbId corroboration: an exact title binds; a fuzzy hit binds
 *  only with a corroborating year; a year-contradicting hit is refused. */
class CinemetaClientSpec extends AnyFlatSpec with Matchers {

  private def client(body: String) = new CinemetaClient(new GetOnlyHttpFetch {
    def get(url: String): String = if (url.contains("search=")) body else throw new RuntimeException(s"unstubbed: $url")
  })

  "CinemetaClient" should "bind the imdbId on an exact title match" in {
    client("""{"metas":[{"id":"tt31000001","name":"Cactus Pears","releaseInfo":"2026"}]}""")
      .findImdbId(Seq("Cactus Pears"), Some(2026)) shouldBe Some("tt31000001")
  }

  it should "bind a contains-match only when the year corroborates" in {
    val body = """{"metas":[{"id":"tt42","name":"Varavu: The Beginning","releaseInfo":"2026"}]}"""
    client(body).findImdbId(Seq("Varavu"), Some(2026)) shouldBe Some("tt42")
  }

  it should "refuse a year-contradicting match (avoid binding a same-name wrong film)" in {
    // Same title, but Cinemeta's entry is 10 years off → refused.
    client("""{"metas":[{"id":"tt999","name":"Alpha","releaseInfo":"2015"}]}""")
      .findImdbId(Seq("Alpha"), Some(2026)) shouldBe None
  }

  it should "refuse a bare fuzzy hit with no corroborating year" in {
    client("""{"metas":[{"id":"tt777","name":"Alpha Something Else","releaseInfo":"2026"}]}""")
      .findImdbId(Seq("Alpha"), None) shouldBe None
  }

  it should "return None on an empty catalogue" in {
    client("""{"metas":[]}""").findImdbId(Seq("Nothing Here"), Some(2026)) shouldBe None
  }
}
