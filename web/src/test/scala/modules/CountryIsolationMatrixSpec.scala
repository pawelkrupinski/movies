package modules

import models.{Cinema, City, Country, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import tools.ObjectGraph

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.io.Source as IoSource

/**
 * One web deployment, one country — rendered for EVERY country from one fixture corpus.
 *
 * The fixture is a single film showing at a cinema in each of the five countries at once,
 * so every page and every API answer has another country's data within reach and has to
 * leave it out. For each country the spec boots the real `Wiring` as that country's
 * deployment — through its `country` member alone, with `KINOWO_COUNTRY` naming a DIFFERENT
 * country, so anything still reading the environment shows up as a leak — and checks:
 *
 *   - the wiring holds no other `Country` (a component built with a defaulted country);
 *   - no page carries a literal that exists only in another language's message bundle, and
 *     no page outside Poland carries a Polish letter — the shape of "Reżyseria" printed on
 *     every country's share card (d1362d654);
 *   - no page or header names another country's host — the shape of the share links that
 *     pointed every country at kinowo.net;
 *   - `/{city}/api/…` names only that country's cities and cinemas.
 *
 * Pages: landing, city listing, film page (its Open Graph block included), browse, support,
 * robots.txt, sitemap.xml and the city sitemap. (There is no separate plan page any more;
 * the listing is the plan.) Deliberately left out: the brand-apex sitemap index and the
 * landing's country picker, whose job IS to link every country (the picker's anchors are
 * stripped before the host check), and `/api/catalog`, which describes every country to the
 * apps by design.
 */
class CountryIsolationMatrixSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-06-10T10:00:00Z")
  private val Title = "Isolation Probe"

  /** The city each country's pages are rendered for: its first with a cinema. */
  private def cityOf(country: Country): City = country.allSorted.find(_.cinemas.nonEmpty).get
  private def cinemaOf(country: Country): Cinema = cityOf(country).cinemas.head

  /** One film, showing tomorrow evening in every country at once. */
  private val corpus: Seq[(String, Option[Int], MovieRecord)] = {
    val evening = LocalDateTime.of(2026, 6, 11, 19, 30)
    val slots: Map[Source, SourceData] = Country.all.map { c =>
      (cinemaOf(c): Source) -> SourceData(title = Some(Title), showtimes = Seq(Showtime(evening, None, None, List("2D"))))
    }.toMap
    Seq((Title, Some(2025), MovieRecord(
      tmdbId = Some(1000001), imdbId = Some("tt1000001"),
      data = slots + (Tmdb -> SourceData(title = Some(Title), synopsis = Some("A film that exists to be filtered."),
        director = Seq("Jane Doe"), cast = Seq("John Roe"), runtimeMinutes = Some(101), releaseYear = Some(2025))))))
  }

  /** The environment a deployment is booted under: deliberately ANOTHER country's. The
   *  wiring's `country` member is the one answer to "which country is this?"; a view or
   *  controller that still asked the environment would render the decoy's brand, host or
   *  language, which every check below then catches. */
  private def decoy(country: Country): Country =
    if (country == Country.Poland) Country.UnitedKingdom else Country.Poland

  /** Run `body` as `country`'s deployment, in a process whose `KINOWO_COUNTRY` names
   *  [[decoy]]. Web unit suites run one at a time in their forked JVM. */
  private def asDeployment[T](country: Country)(body: DeploymentWiring => T): T = {
    val previous = Option(System.getProperty("KINOWO_COUNTRY"))
    System.setProperty("KINOWO_COUNTRY", decoy(country).code)
    try {
      withClue("KINOWO_COUNTRY is set in this shell and overrides the spec's pick: ")(Country.fromEnv shouldBe decoy(country))
      val wiring = new DeploymentWiring(country, corpus, Now)
      wiring.boot()
      body(wiring)
    } finally previous.fold(System.clearProperty("KINOWO_COUNTRY"))(System.setProperty("KINOWO_COUNTRY", _))
  }

  private def request(country: Country, path: String) = {
    val origin = java.net.URI.create(country.webOrigin.get)
    FakeRequest(GET, country.pathPrefix + path)
      .withHeaders(HOST -> origin.getHost, "X-Forwarded-Proto" -> origin.getScheme, "X-Forwarded-Host" -> origin.getHost)
  }

  private def body(country: Country, path: String, action: Action[AnyContent]): String = {
    val result = action(request(country, path))
    withClue(s"${country.code} $path: ")(status(result) shouldBe OK)
    contentAsString(result)
  }

  // ── What another country looks like ────────────────────────────────────────

  /** Each UI language's message values, keyed by language code. */
  private val bundles: Map[String, Seq[String]] =
    Map("pl" -> "messages", "en" -> "messages.en", "de" -> "messages.de", "es" -> "messages.es").map { case (lang, file) =>
      val lines = IoSource.fromResource(file)(using scala.io.Codec.UTF8).getLines().toList
      lang -> lines.map(_.trim).filterNot(l => l.isEmpty || l.startsWith("#")).flatMap { l =>
        l.split("=", 2) match { case Array(_, v) => Some(v.trim.replace("''", "'")); case _ => None }
      }
    }

  /** The literal pieces of `lang`'s bundle that no other bundle contains — text that can only
   *  have come from that language. Placeholders split a value; markup is dropped; short pieces
   *  ("Kino", "IMAX") are shared by nature and left out. */
  private def uniqueLiterals(lang: String): Set[String] = {
    val others = bundles.collect { case (l, vs) if l != lang => vs }.flatten.toSeq
    bundles(lang).flatMap(_.replaceAll("<[^>]*>", " ").split("""\{\d+\}""").map(_.trim))
      .filter(f => f.length >= 8 && f.exists(_.isLetter))
      .filterNot(f => others.exists(_.contains(f)))
      .toSet
  }

  /** `literal` standing as words of its own — "Language" in a sentence, not in `inLanguage`. */
  private def asWord(literal: String) = s"""(?<![\\p{L}\\w])\\Q$literal\\E(?![\\p{L}\\w])""".r

  private val PolishLetters = "[ąćęłńśźżĄĆĘŁŃŚŹŻ]".r

  /** The page minus what legitimately names every country or language: the inline scripts
   *  (the language packs and per-language date words the client-side switch reads, the
   *  country catalog its country switcher reads), the stylesheets (whose comments name UI
   *  copy, and which production minifies away) and the landing's country picker. The
   *  JSON-LD blocks stay: they are what a crawler attributes the page to. */
  private def ownText(html: String): String =
    html.replaceAll("""(?s)<script(?![^>]*application/ld\+json)[^>]*>.*?</script>""", "")
      .replaceAll("""(?s)<style[^>]*>.*?</style>""", "")
      .replaceAll("""(?s)<a [^>]*data-country="[^"]*"[^>]*>.*?</a>""", "")

  private def unescape(html: String): String =
    html.replace("&#x27;", "'").replace("&quot;", "\"").replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">")

  /** Everything on `page` that belongs to a country other than `country`. */
  private def leaks(country: Country, page: String): Seq[String] = {
    val text  = unescape(ownText(page))
    val lang  = country.language.getLanguage
    val words = bundles.keys.filterNot(_ == lang).toSeq.flatMap(l => uniqueLiterals(l).filter(w => asWord(w).findFirstIn(text).isDefined).map(w => s"$l literal: $w"))
    val polish =
      if (lang == "pl") Nil
      else PolishLetters.findAllMatchIn(text).map(m => s"Polish letter: …${text.substring(math.max(0, m.start - 40), math.min(text.length, m.end + 20))}…").take(3).toSeq
    val hosts = Country.all.filterNot(_ == country).flatMap(_.webUrl).map(Country.withoutScheme)
      .filter(host => s"""\\Q$host\\E(?![\\w-])""".r.findFirstIn(text).isDefined).map(h => s"host: $h")
    val venues = Country.all.filterNot(_ == country).map(cinemaOf).map(_.displayName).filter(text.contains).map(v => s"cinema: $v")
    words ++ polish ++ hosts ++ venues
  }

  "the bundle literal sets" should "be non-trivial for every language (positive control)" in {
    Seq("pl", "en", "de", "es").foreach(l => withClue(s"$l: ")(uniqueLiterals(l).size should be > 20))
    uniqueLiterals("pl") should contain ("Reżyseria")
  }

  Country.all.foreach { country =>
    val code = country.code
    val city = cityOf(country)

    s"The $code web wiring" should "hold no other country anywhere in its graph" in asDeployment(country) { wiring =>
      // Every member the wiring declares, not a hand-kept list of controllers: a component
      // added tomorrow is walked without anyone naming it here.
      val unbuilt = ObjectGraph.forceLazyMembers(wiring)
      withClue(s"members the walk cannot see because they failed to build: ${unbuilt.map { case (n, e) => s"$n: $e" }.mkString("; ")}\n") {
        unbuilt shouldBe empty
      }
      val strays = ObjectGraph.collect(wiring) { case c: Country => c }.collect {
        case (path, c) if c != country => s"$path -> ${c.code}"
      }
      withClue(s"$code wiring reaches another country:\n${strays.mkString("\n")}\n")(strays shouldBe empty)
    }

    it should "render no other country's words, letters, hosts or cinemas on any page" in asDeployment(country) { wiring =>
      val mc      = wiring.movieController
      val listing = body(country, s"/${city.slug}/", mc.index(city.slug))
      val slug    = s"""/${city.slug}/movie/([a-z0-9-]+)""".r.findFirstMatchIn(listing).map(_.group(1))
        .getOrElse(fail(s"$code listing links no film page"))
      val pages = Seq(
        "landing"      -> body(country, "/", wiring.landingController.index()),
        "listing"      -> listing,
        "film"         -> body(country, s"/${city.slug}/movie/$slug", mc.filmBySlug(city.slug, slug)),
        "browse"       -> body(country, s"/${city.slug}/movies", mc.browse(city.slug, None, None, None, None, None, None, None, None)),
        "support"      -> body(country, "/support", wiring.supportController.support(None)),
        "robots.txt"   -> body(country, "/robots.txt", mc.robotsTxt),
        "sitemap.xml"  -> body(country, "/sitemap.xml", mc.sitemap),
        "city sitemap" -> body(country, s"/${city.slug}/sitemap.xml", mc.citySitemap(city.slug)),
      )
      // Positive control: the fixture film really is on this country's pages.
      pages.toMap.apply("film") should include (Title)
      val found = pages.flatMap { case (name, page) => leaks(country, page).map(l => s"$name: $l") }
      withClue(s"$code pages carry another country's content:\n${found.mkString("\n")}\n")(found shouldBe empty)
    }

    it should "point its Open Graph and share metadata at its own host" in asDeployment(country) { wiring =>
      val mc      = wiring.movieController
      val listing = body(country, s"/${city.slug}/", mc.index(city.slug))
      val slug    = s"""/${city.slug}/movie/([a-z0-9-]+)""".r.findFirstMatchIn(listing).get.group(1)
      val film    = body(country, s"/${city.slug}/movie/$slug", mc.filmBySlug(city.slug, slug))
      val og      = """<meta property="og:(url|image|locale|site_name)"\s+content="([^"]*)"""".r
        .findAllMatchIn(film).map(m => m.group(1) -> m.group(2)).toMap
      og("url") should startWith (country.webUrl.get + "/")
      og("image") should startWith ("https://")
      og("site_name") should include (city.labels.nominative)
      og("locale") shouldBe country.language.toLanguageTag.replace("-", "_")
      """<link rel="canonical" href="([^"]*)"""".r.findFirstMatchIn(film).map(_.group(1)).get should startWith (country.webUrl.get + "/")
    }

    it should "identify itself on the landing by its own brand, host and language" in asDeployment(country) { wiring =>
      val landing = body(country, "/", wiring.landingController.index())
      val og      = """<meta property="og:(site_name|locale|image)"\s+content="([^"]*)"""".r
        .findAllMatchIn(landing).map(m => m.group(1) -> m.group(2)).toMap
      og("site_name") shouldBe country.brandName
      og("locale") shouldBe country.language.toLanguageTag.replace("-", "_")
      og("image") shouldBe s"${country.ogOrigin}/assets/img/${country.homeOgImage}"
      val jsonLd = """(?s)<script type="application/ld\+json">(.*?)</script>""".r.findFirstMatchIn(landing)
        .map(m => Json.parse(m.group(1))).getOrElse(fail(s"$code landing carries no JSON-LD"))
      val website = jsonLd.as[Seq[JsValue]].find(j => (j \ "@type").asOpt[String].contains("WebSite")).get
      (website \ "name").as[String] shouldBe country.brandName
      (website \ "url").as[String] shouldBe s"${country.ogOrigin}/"
      (website \ "inLanguage").as[String] shouldBe country.language.getLanguage
    }

    it should "answer /{city}/api/* with only its own cities and cinemas" in asDeployment(country) { wiring =>
      val mc = wiring.movieController
      val own = country.cities.flatMap(_.cinemas.map(_.displayName)).toSet
      val foreignCinemas = Country.all.filterNot(_ == country).flatMap(_.cities.flatMap(_.cinemas.map(_.displayName))).toSet -- own
      val foreignCities  = Country.all.filterNot(_ == country).flatMap(_.cities.map(_.slug)).toSet -- country.cities.map(_.slug)
      val answers = Seq(
        "repertoire" -> body(country, s"/${city.slug}/api/repertoire", mc.apiRepertoire(city.slug, None)),
        "details"    -> body(country, s"/${city.slug}/api/details", mc.apiDetails(city.slug)),
        "cinemas"    -> body(country, s"/${city.slug}/api/cinemas", mc.apiCinemas(city.slug)),
      )
      answers.toMap.apply("repertoire") should include (Title) // positive control
      answers.foreach { case (name, json) =>
        val strings = allStrings(Json.parse(json))
        withClue(s"$code api/$name names another country's cinema: ")(strings.filter(foreignCinemas).toSeq.sorted shouldBe empty)
        withClue(s"$code api/$name names another country's city: ")(strings.filter(foreignCities).toSeq.sorted shouldBe empty)
        withClue(s"$code api/$name: ")(leaks(country, json).filterNot(_.startsWith("cinema:")) shouldBe empty)
      }
      // A sibling country's city is not this deployment's to answer for.
      val elsewhere = cityOf(Country.all.find(_ != country).get)
      status(mc.apiRepertoire(elsewhere.slug, None)(request(country, s"/${elsewhere.slug}/api/repertoire"))) shouldBe NOT_FOUND
    }
  }

  private def allStrings(json: JsValue): Set[String] = json match {
    case play.api.libs.json.JsString(s) => Set(s)
    case play.api.libs.json.JsArray(xs)  => xs.flatMap(allStrings).toSet
    case play.api.libs.json.JsObject(fs) => fs.values.flatMap(allStrings).toSet ++ fs.keys
    case _                               => Set.empty
  }
}

/** The web wiring booted as `serving`'s deployment, on a fixed clock. Top-level so the
 *  object-graph walk starts at the wiring, not at the spec that built it. */
private class DeploymentWiring(serving: Country, corpus: Seq[(String, Option[Int], MovieRecord)], now: Instant)
    extends TestWebWiring(corpus) {
  override lazy val country: Country       = serving
  override lazy val clock: java.time.Clock = java.time.Clock.fixed(now, ZoneOffset.UTC)
}
