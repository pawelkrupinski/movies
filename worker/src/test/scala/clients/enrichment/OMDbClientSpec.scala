package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.OMDbClient
import tools.GetOnlyHttpFetch

class OMDbClientSpec extends AnyFlatSpec with Matchers {

  /** Stub whose `get` is a url→body function; records every requested url. */
  private class FnFetch(f: String => String) extends GetOnlyHttpFetch {
    val urls = scala.collection.mutable.ListBuffer.empty[String]
    def get(url: String): String = { urls += url; f(url) }
  }
  private def client(f: String => String, key: Option[String] = Some("k")) =
    new OMDbClient(new FnFetch(f), apiKey = key)

  private def tParam(url: String): String =
    java.net.URLDecoder.decode(url.split("[?&]").find(_.startsWith("t=")).map(_.drop(2)).getOrElse(""), "UTF-8")

  // ── findImdbId: exact-title acceptance ───────────────────────────────────────

  "findImdbId" should "accept OMDb's best movie match on an exact (diacritic-folded) title" in {
    val omdb = client(_ => """{"Title":"Sirat","Year":"2025","Director":"Oliver Laxe","imdbID":"tt32298285","Response":"True"}""")
    omdb.findImdbId(Seq("Sirât"), Some(2025), Set.empty) shouldBe Some("tt32298285")
  }

  it should "REJECT a same-title-different-film match with no corroboration" in {
    // "Faworyta" (The Favourite) must NOT bind OMDb's "Carska faworyta" (1918):
    // not an exact title, no director, no year to corroborate.
    val omdb = client(_ => """{"Title":"Carska faworyta","Year":"1918","Director":"N/A","imdbID":"tt0000001","Response":"True"}""")
    omdb.findImdbId(Seq("Faworyta"), None, Set.empty) shouldBe None
  }

  it should "restrict the search to type=movie (never a series)" in {
    val fetch = new FnFetch(_ => """{"Response":"False"}""")
    new OMDbClient(fetch, apiKey = Some("k")).findImdbId(Seq("Bodyguard"), None, Set.empty)
    fetch.urls.head should include ("type=movie")
  }

  // ── corroboration by director / year ─────────────────────────────────────────

  it should "accept a non-exact title when the director overlaps and the year agrees" in {
    // Polish "Mawka" → OMDb "Mavka", same director + year → corroborated.
    val omdb = client(_ => """{"Title":"Mavka","Year":"2026","Director":"Katya Tsarik","imdbID":"tt11808706","Response":"True"}""")
    omdb.findImdbId(Seq("Mawka"), Some(2026), Set("Katya Tsarik")) shouldBe Some("tt11808706")
  }

  it should "REJECT a candidate whose year contradicts ours when the title isn't exact" in {
    val omdb = client(_ => """{"Title":"Mavka","Year":"2018","Director":"Katya Tsarik","imdbID":"tt11808706","Response":"True"}""")
    omdb.findImdbId(Seq("Mawka"), Some(2026), Set("Katya Tsarik")) shouldBe None
  }

  it should "REJECT a candidate whose director contradicts ours" in {
    val omdb = client(_ => """{"Title":"Aftersun","Year":"2022","Director":"Someone Else","imdbID":"tt19770238","Response":"True"}""")
    omdb.findImdbId(Seq("Aftersun"), Some(2022), Set("Charlotte Wells")) shouldBe None
  }

  // ── director-walk backstop ───────────────────────────────────────────────────

  it should "fall back to a director walk and accept the LONE director match" in {
    val omdb = client { url =>
      if (url.contains("?t=")) """{"Title":"Unrelated","Year":"1990","Director":"Nobody","imdbID":"tt0000009","Response":"True"}"""
      else if (url.contains("?s=")) """{"Search":[{"imdbID":"ttAAA"},{"imdbID":"ttBBB"}],"Response":"True"}"""
      else if (url.contains("i=ttAAA")) """{"Title":"Other","Year":"2000","Director":"Other Person","imdbID":"ttAAA","Response":"True"}"""
      else """{"Title":"Right Film","Year":"2024","Director":"Jane Director","imdbID":"ttBBB","Response":"True"}"""
    }
    omdb.findImdbId(Seq("Ambiguous"), None, Set("Jane Director")) shouldBe Some("ttBBB")
  }

  it should "REFUSE the director walk when more than one candidate's director matches" in {
    val omdb = client { url =>
      if (url.contains("?t=")) """{"Response":"False"}"""
      else if (url.contains("?s=")) """{"Search":[{"imdbID":"ttAAA"},{"imdbID":"ttBBB"}],"Response":"True"}"""
      else if (url.contains("i=ttAAA")) """{"Title":"Dup A","Year":"2024","Director":"Jane Director","imdbID":"ttAAA","Response":"True"}"""
      else """{"Title":"Dup B","Year":"2024","Director":"Jane Director","imdbID":"ttBBB","Response":"True"}"""
    }
    omdb.findImdbId(Seq("Ambiguous"), None, Set("Jane Director")) shouldBe None
  }

  it should "NOT walk (no ?s= call) when we have no director to corroborate with" in {
    val fetch = new FnFetch(url => if (url.contains("?s=")) fail("must not search without a director") else """{"Response":"False"}""")
    new OMDbClient(fetch, apiKey = Some("k")).findImdbId(Seq("Whatever"), None, Set.empty) shouldBe None
  }

  // ── feature gate ─────────────────────────────────────────────────────────────

  it should "return None and make NO HTTP call when the key is unset" in {
    val omdb = client(_ => throw new RuntimeException("no HTTP when key unset"), key = None)
    omdb.findImdbId(Seq("Sirat"), Some(2025), Set("X")) shouldBe None
  }

  it should "try the next title spelling when the first abstains" in {
    val omdb = client { url =>
      if (tParam(url).startsWith("Mawka")) """{"Response":"False"}"""
      else """{"Title":"Mavka","Year":"2026","Director":"N/A","imdbID":"tt11808706","Response":"True"}"""
    }
    omdb.findImdbId(Seq("Mawka", "Mavka"), Some(2026), Set.empty) shouldBe Some("tt11808706")
  }

  // ── rottenTomatoesUrl (by imdb id) ───────────────────────────────────────────

  "rottenTomatoesUrl" should "extract OMDb's tomatoURL" in {
    val omdb = client(_ => """{"tomatoURL":"https://www.rottentomatoes.com/m/freak_show","Response":"True"}""")
    omdb.rottenTomatoesUrl("tt5089534") shouldBe Some("https://www.rottentomatoes.com/m/freak_show")
  }

  it should "return None when tomatoURL is N/A" in {
    client(_ => """{"tomatoURL":"N/A","Response":"True"}""").rottenTomatoesUrl("tt0000001") shouldBe None
  }

  it should "hit the documented endpoint with the id, tomatoes flag and key" in {
    val fetch = new FnFetch(_ => """{"tomatoURL":"https://www.rottentomatoes.com/m/x","Response":"True"}""")
    new OMDbClient(fetch, apiKey = Some("abc123")).rottenTomatoesUrl("tt5089534")
    fetch.urls.head shouldBe "https://www.omdbapi.com/?i=tt5089534&tomatoes=true&apikey=abc123"
  }

  it should "return None and make NO HTTP call when the key is unset" in {
    client(_ => throw new RuntimeException("no HTTP when key unset"), key = None).rottenTomatoesUrl("tt5089534") shouldBe None
  }

  it should "swallow a network/HTTP failure and return None" in {
    client(_ => throw new RuntimeException("HTTP 503")).rottenTomatoesUrl("tt5089534") shouldBe None
  }
}
