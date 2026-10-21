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
    new OMDbClient(new FnFetch(f), apiKey = key, clock = tools.SpecClock.Pinned)

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
    new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k")), clock = tools.SpecClock.Pinned).findImdbId(Seq("Bodyguard"), None, Set.empty)
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
    new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k")), clock = tools.SpecClock.Pinned).findImdbId(Seq("Whatever"), None, Set.empty) shouldBe None
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

  // ── a dead key: stop asking ──────────────────────────────────────────────────

  private val t0 = java.time.Instant.parse("2026-10-20T08:00:00Z")
  private def gated(f: String => String) = {
    val clock = new tools.MutableClock(t0)
    val fetch = new FnFetch(f)
    (new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k")), clock = clock), fetch, clock)
  }
  private def ask(omdb: OMDbClient): Option[Throwable] =
    scala.util.Try(omdb.findImdbId(Seq("Aftersun"), Some(2022), Set.empty)).failed.toOption

  "a key OMDb keeps refusing" should "stop being asked after a few refusals in a row, failing fast instead" in {
    // Record run 469: the free key's daily quota spent, OMDb answered 401 to every one of hundreds of asks,
    // each a call the quota would count again once it reset.
    val (omdb, fetch, _) = gated(url => throw new tools.HttpStatusException(401, "GET", url, None))
    val failures = (1 to 20).map(_ => ask(omdb))
    fetch.urls.size shouldBe OMDbClient.RefusalsBeforePause
    every(failures) shouldBe defined
    failures.last.get.getMessage should include("OMDb refused")
  }

  it should "count OMDb's own 'Request limit reached!' / 'Invalid API key!' documents, and a fallback chain's 401" in {
    val (byDocument, docFetch, _) = gated(_ => recorded("invalid_api_key.json"))
    (1 to 20).foreach(_ => ask(byDocument))
    docFetch.urls.size shouldBe OMDbClient.RefusalsBeforePause
    // FallbackHttpFetch's composite when every leg was refused: a plain RuntimeException naming each leg's 401.
    val (byChain, chainFetch, _) = gated(url => throw new RuntimeException(
      s"All 2 backends failed for get $url:\n  direct: HttpStatusException: HTTP 401 for GET $url\n  proxy: IOException: WWW-Authenticate header missing for response code 401"))
    (1 to 20).foreach(_ => ask(byChain))
    chainFetch.urls.size shouldBe OMDbClient.RefusalsBeforePause
  }

  it should "ask once more after the pause, and keep asking once OMDb answers again" in {
    var refusing = true
    val (omdb, fetch, clock) = gated { url =>
      if (refusing) throw new tools.HttpStatusException(401, "GET", url, None)
      """{"Title":"Aftersun","Year":"2022","Director":"Charlotte Wells","imdbID":"tt19770238","Response":"True"}"""
    }
    (1 to 10).foreach(_ => ask(omdb))
    val paused = fetch.urls.size
    clock.advanceMillis(OMDbClient.RefusalPause.toMillis)
    ask(omdb) shouldBe defined // the one probe after the pause, still refused: paused again
    fetch.urls.size shouldBe paused + 1
    ask(omdb)
    fetch.urls.size shouldBe paused + 1
    refusing = false
    clock.advanceMillis(OMDbClient.RefusalPause.toMillis)
    (1 to 5).foreach(_ => ask(omdb) shouldBe None)
    fetch.urls.size shouldBe paused + 1 + 5
  }

  /** A fetch whose reads on the thread named `slow` wait at a gate; every other read answers `f` at once. */
  private class GatedFetch(f: String => String) extends GetOnlyHttpFetch {
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val urls    = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    def get(url: String): String = {
      urls.add(url)
      if (Thread.currentThread.getName == "slow") { entered.countDown(); release.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) }
      f(url)
    }
  }
  private def slowAsk(omdb: OMDbClient): (Thread, java.util.concurrent.atomic.AtomicReference[Option[Throwable]]) = {
    val result = new java.util.concurrent.atomic.AtomicReference[Option[Throwable]](None)
    val thread = new Thread(() => result.set(ask(omdb)), "slow")
    thread.start()
    (thread, result)
  }
  private val aftersun =
    """{"Title":"Aftersun","Year":"2022","Director":"Charlotte Wells","imdbID":"tt19770238","Response":"True"}"""

  it should "let only ONE read probe OMDb when the pause ends, the others failing fast meanwhile" in {
    @volatile var refusing = true
    val fetch = new GatedFetch(url => if (refusing) throw new tools.HttpStatusException(401, "GET", url, None) else aftersun)
    val clock = new tools.MutableClock(t0)
    val omdb  = new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k")), clock = clock)
    (1 to 10).foreach(_ => ask(omdb))
    val paused = fetch.urls.size
    clock.advanceMillis(OMDbClient.RefusalPause.toMillis)
    refusing = false
    val (probe, probed) = slowAsk(omdb)
    fetch.entered.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
    (1 to 5).foreach(_ => ask(omdb) shouldBe defined) // the probe is in flight: not asked again
    fetch.urls.size shouldBe paused + 1
    fetch.release.countDown()
    probe.join(tools.SpecTimeouts.Io.toMillis)
    probed.get shouldBe None
    ask(omdb) shouldBe None // the probe answered: asked again
    fetch.urls.size shouldBe paused + 2
  }

  it should "lift the pause when a read already in flight is answered" in {
    @volatile var refusing = true
    val fetch = new GatedFetch(url =>
      if (refusing && Thread.currentThread.getName != "slow") throw new tools.HttpStatusException(401, "GET", url, None)
      else aftersun)
    val omdb = new OMDbClient(fetch, apiKey = Some(settings.OmdbApiKey("k")), clock = new tools.MutableClock(t0))
    val (slow, slowResult) = slowAsk(omdb)
    fetch.entered.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
    (1 to 10).foreach(_ => ask(omdb)) // refused meanwhile: paused
    ask(omdb).map(_.getMessage).getOrElse("") should include("OMDb refused")
    refusing = false
    fetch.release.countDown()
    slow.join(tools.SpecTimeouts.Io.toMillis)
    slowResult.get shouldBe None
    val before = fetch.urls.size
    ask(omdb) shouldBe None // the key answered, so the pause is over without waiting it out
    fetch.urls.size shouldBe before + 1
  }

  it should "never pause on failures that say nothing about the key" in {
    val (omdb, fetch, _) = gated(url => throw new tools.HttpStatusException(503, "GET", url, None))
    (1 to 20).foreach(_ => ask(omdb))
    fetch.urls.size shouldBe 20
  }
}
