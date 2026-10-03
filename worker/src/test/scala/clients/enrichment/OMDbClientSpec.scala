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
  private def client(f: String => String, key: Option[settings.OmdbApiKey] = Some(settings.OmdbApiKey("k"))) =
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
    val fetch = new FnFetch(_ => """{"Response":"False","Error":"Movie not found!"}""")
    new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k"))).findImdbId(Seq("Bodyguard"), None, Set.empty)
    fetch.urls.head should include ("type=movie")
  }

  it should "throw, not answer None, when OMDb cannot be read — a spent quota is no verdict on the film" in {
    // OMDb answers an exhausted free-key quota with HTTP 401; a None here backed every film
    // still waiting behind it off for days as if OMDb had no such film.
    val quotaSpent = client(url => throw new tools.HttpStatusException(401, "GET", url, None))
    a [tools.HttpStatusException] should be thrownBy quotaSpent.findImdbId(Seq("Sirât"), Some(2025), Set.empty)
    val notJson = client(_ => "<html>Service Unavailable</html>")
    an [Exception] should be thrownBy notJson.findImdbId(Seq("Sirât"), Some(2025), Set.empty)
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

  it should "NOT read an initial or a surname-first credit as a contradicting director" in {
    // OMDb credits "Alejandro G. Iñárritu" where TMDB writes the name in full; a
    // substring test called that a different director and refused an exact title.
    val omdb = client(_ => """{"Title":"Birdman","Year":"2014","Director":"Alejandro G. Iñárritu","imdbID":"tt2562232","Response":"True"}""")
    omdb.findImdbId(Seq("Birdman"), Some(2014), Set("Alejandro González Iñárritu")) shouldBe Some("tt2562232")
    val enyedi = client(_ => """{"Title":"On Body and Soul","Year":"2017","Director":"Ildikó Enyedi","imdbID":"tt5607714","Response":"True"}""")
    enyedi.findImdbId(Seq("On Body and Soul"), Some(2017), Set("Enyedi Ildikó")) shouldBe Some("tt5607714")
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
      if (url.contains("?t=")) """{"Response":"False","Error":"Movie not found!"}"""
      else if (url.contains("?s=")) """{"Search":[{"imdbID":"ttAAA"},{"imdbID":"ttBBB"}],"Response":"True"}"""
      else if (url.contains("i=ttAAA")) """{"Title":"Dup A","Year":"2024","Director":"Jane Director","imdbID":"ttAAA","Response":"True"}"""
      else """{"Title":"Dup B","Year":"2024","Director":"Jane Director","imdbID":"ttBBB","Response":"True"}"""
    }
    omdb.findImdbId(Seq("Ambiguous"), None, Set("Jane Director")) shouldBe None
  }

  it should "NOT walk (no ?s= call) when we have no director to corroborate with" in {
    val fetch = new FnFetch(url => if (url.contains("?s=")) fail("must not search without a director") else """{"Response":"False","Error":"Movie not found!"}""")
    new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k"))).findImdbId(Seq("Whatever"), None, Set.empty) shouldBe None
  }

  // ── feature gate ─────────────────────────────────────────────────────────────

  it should "return None and make NO HTTP call when the key is unset" in {
    val omdb = client(_ => throw new RuntimeException("no HTTP when key unset"), key = None)
    omdb.findImdbId(Seq("Sirat"), Some(2025), Set("X")) shouldBe None
  }

  it should "try the next title spelling when the first abstains" in {
    val omdb = client { url =>
      if (tParam(url).startsWith("Mawka")) """{"Response":"False","Error":"Movie not found!"}"""
      else """{"Title":"Mavka","Year":"2026","Director":"N/A","imdbID":"tt11808706","Response":"True"}"""
    }
    omdb.findImdbId(Seq("Mawka", "Mavka"), Some(2026), Set.empty) shouldBe Some("tt11808706")
  }

  // ── a failed read is not a miss ──────────────────────────────────────────────
  // Recorded from omdbapi.com (test/resources/fixtures/omdb/): OMDb answers HTTP 200 and
  // says "none" inside the JSON, so the client must tell "nothing matches" from an error.

  private def recorded(name: String): String = clients.tools.FixtureFile.read(s"test/resources/fixtures/omdb/$name")

  "an OMDb answer" should "read its recorded 'Movie not found!', 'Too many results.' and 'Incorrect IMDb ID.' as none" in {
    client(url => if (url.contains("?t=")) recorded("title_not_found.json") else recorded("search_too_many_results.json"))
      .findImdbId(Seq("zzqqxxnotafilm"), None, Set("Somebody")) shouldBe None
    client(url => if (url.contains("?s=")) """{"Search":[{"imdbID":"tt0000000"}],"Response":"True"}""" else
      if (url.contains("?i=")) recorded("id_incorrect.json") else recorded("title_not_found.json"))
      .findImdbId(Seq("zzqqxxnotafilm"), None, Set("Somebody")) shouldBe None
  }

  it should "accept the recorded Aftersun record" in {
    client(_ => recorded("title_aftersun_2022.json")).findImdbId(Seq("Aftersun"), Some(2022), Set("Charlotte Wells")) shouldBe Some("tt19770238")
  }

  it should "THROW on an OMDb error document rather than answer none" in {
    // Was Try(Json.parse(...)).getOrElse(JsNull): a spent key read as "no film", backed off for days.
    val failure = the[tools.UnexpectedBodyException] thrownBy client(_ => recorded("invalid_api_key.json"))
      .findImdbId(Seq("Aftersun"), Some(2022), Set.empty)
    failure.getMessage should (include("OMDb error Invalid API key!") and include("apikey=***"))
  }

  it should "THROW when the read itself failed" in {
    a[tools.HttpStatusException] should be thrownBy
      client(url => throw new tools.HttpStatusException(503, "GET", url, None)).findImdbId(Seq("Aftersun"), Some(2022), Set.empty)
    a[tools.UnexpectedBodyException] should be thrownBy
      client(_ => "<html><body>502 Bad Gateway</body></html>").findImdbId(Seq("Aftersun"), Some(2022), Set.empty)
  }
}
