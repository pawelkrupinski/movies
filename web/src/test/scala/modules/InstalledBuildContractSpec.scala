package modules

import controllers.WellKnownController
import models.{Catalog, City, Country, InstalledBuildSlugs}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import tools.SpecTimeouts

import java.net.URI
import scala.concurrent.Await
import scala.io.Source

/**
 * The catalog and the server, as the INSTALLED app builds from before slugs were per
 * country see them: `mobile-ios-2.0.11` and `mobile-android-2.0.11` (both acbbd2a1a,
 * 2026-09-27). Their rules are re-implemented here from those tags' code — read
 * with `git show mobile-ios-2.0.11:ios/Kinowo/Models/City.swift` and the like — and
 * run over the catalog the server emits NOW:
 *
 *  - they decode per city only `slug, name, lat, lon, country, region, subregion,
 *    timezone` (iOS `Models/City.swift`, Android `model/City.kt`); never `pageSlug`,
 *    `formerSlugs`, `otherSlugs` or `bareSlugOwners`;
 *  - every lookup is the FIRST city with a slug, across all countries: its country
 *    (iOS `country(ofSlug:)`, Android `countryOf`), its zone, else the fallback's
 *    (iOS `zone(ofSlug:fallback:)`, Android `zoneFor`);
 *  - a saved city is the bare slug, its country stored beside it, and an API URL is
 *    that country's `baseUrl` + `/{slug}/api/{repertoire|details|cinemas}` — both
 *    HTTP stacks follow the redirect;
 *  - a deep link drops a leading country segment (unless it is itself a known slug),
 *    takes the next segment as the city, rejects an unknown one, and switches to the
 *    country the first matching city names (iOS `DeepLink.parse` +
 *    `KinowoApp.handleDeepLink`, Android `DeepLink.parse` + `KinowoViewModel.handleDeepLink`).
 *
 * "Before" is the catalog those builds shipped against: origin/main 7fdc618ca's
 * `ios/Kinowo/catalog-seed.json`, each city's country and slug copied once into
 * `installed-build/cities-7fdc618ca.tsv` — the slugs a user of those builds can
 * have saved.
 *
 * TEMPORARY, with `models.LegacySlugOwners` and [[InstalledBuildSlugs]]: drop it
 * once those builds are gone.
 */
class InstalledBuildContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private given system: ActorSystem = ActorSystem("installed-build-contract-spec")
  private given mat: Materializer   = Materializer(system)

  override def afterAll(): Unit = Await.result(system.terminate(), SpecTimeouts.Io): Unit

  // ── The catalog, decoded the way 2.0.11 decodes it ─────────────────────────────

  private final case class OldCountry(code: String, baseUrl: String, timezone: String)
  private final case class OldCity(slug: String, country: String, timezone: Option[String])

  private val body = Json.parse(Catalog.json)
  private val oldCountries: Seq[OldCountry] = (body \ "countries").as[Seq[JsObject]].map { c =>
    OldCountry((c \ "code").as[String], (c \ "baseUrl").as[String], (c \ "timezone").as[String])
  }
  private val oldCities: Seq[OldCity] = (body \ "cities").as[Seq[JsObject]].map { c =>
    OldCity((c \ "slug").as[String], (c \ "country").as[String], (c \ "timezone").asOpt[String])
  }

  private object Old {
    def country(code: String): OldCountry = oldCountries.find(_.code == code).getOrElse(fail(s"no country $code"))
    def countryOf(slug: String): Option[String] = oldCities.find(_.slug == slug).map(_.country)
    def zoneOf(slug: String, fallback: String): String = oldCities.find(_.slug == slug).flatMap(_.timezone).getOrElse(fallback)
    def apiUrl(code: String, slug: String, endpoint: String): String = s"${country(code).baseUrl}/$slug/api/$endpoint"

    private val webHosts = Set("kinowo.net", "www.kinowo.net", "showtimes.cc", "www.showtimes.cc")
    private val countrySegments = Set("uk", "de", "us", "es")
    private val known = oldCities.map(_.slug).toSet

    /** The city slug a link opens, and the country the app switches to for it. */
    def open(url: String): Option[(String, String)] = {
      val uri = URI.create(url)
      val city = uri.getScheme match {
        case "https" | "http" if webHosts(uri.getHost) =>
          val raw      = uri.getPath.split("/").filter(_.nonEmpty).toSeq
          val segments = if (raw.headOption.exists(s => countrySegments(s) && !known(s))) raw.drop(1) else raw
          segments.headOption
        case "kinowo" => Option(uri.getHost)
        case _        => None
      }
      city.filter(known).flatMap(slug => countryOf(slug).map(_ -> slug))
    }
  }

  // ── What the server means by each slug ─────────────────────────────────────────

  /** Catalog `slug` → the page it names on the server, with its country. */
  private val serverCity: Map[String, (Country, City)] =
    Country.switchable.flatMap(c => c.cities.map(city => InstalledBuildSlugs.of(c, city) -> (c, city))).toMap

  private val deployments: Map[Country, CityRoutesDeployment] =
    Country.switchable.map(c => c -> new CityRoutesDeployment(c)).toMap

  /** A URL an old build builds, through the deployment its `baseUrl` names: where it lands. */
  private def fetch(url: String): Either[String, String] = {
    val country = Country.switchable.filter(c => url.startsWith(c.webUrl.get + "/")).maxBy(_.webUrl.get.length)
    deployments(country).landsOnOk(url.stripPrefix(country.webOrigin.get))
  }

  private val before: Seq[(String, String)] =
    Source.fromResource("installed-build/cities-7fdc618ca.tsv").getLines().filter(_.nonEmpty)
      .map(_.split("\t") match { case Array(code, slug) => code -> slug; case other => fail(other.mkString("\t")) }).toSeq

  private val endpoints = Seq("repertoire", "details", "cinemas")

  // ── The contract ───────────────────────────────────────────────────────────────

  "The catalog an installed 2.0.11 build decodes" should "name every city by a slug no other city has" in {
    oldCities.map(_.slug).distinct.size shouldBe oldCities.size
    oldCities.size shouldBe Country.switchable.map(_.cities.size).sum
  }

  it should "put every city in its own country, on its own zone" in {
    val failures = for {
      (slug, (country, city)) <- serverCity.toSeq.sortBy(_._1)
      found = Old.countryOf(slug) -> Old.zoneOf(slug, Old.country(country.code).timezone)
      if found != (Some(country.code) -> city.zoneId.getId)
    } yield s"$slug: $found, not ${country.code} on ${city.zoneId}"
    withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
  }

  it should "land every API URL it builds on its city's page, on its country" in {
    val failures = for {
      old      <- oldCities
      (country, city) = serverCity(old.slug)
      endpoint <- endpoints
      url       = Old.apiUrl(old.country, old.slug, endpoint)
      problem  <- (fetch(url) match {
                    case Right(landed) => Option.when(landed != s"${country.pathPrefix}/${city.slug}/api/$endpoint")(s"$url → $landed")
                    case Left(problem) => Some(problem)
                  }).toSeq
    } yield problem
    withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
  }

  "A city an installed build saved before slugs were per country" should "still mean the city it meant" in {
    before.size shouldBe 2939
    val failures = for {
      (code, slug) <- before
      country       = Country.byCode(code).getOrElse(fail(code))
      problem      <- (country.resolve(slug) match {
        case None => Some(s"$code/$slug no longer resolves on its country")
        case Some(meant) =>
          val url = Old.apiUrl(code, slug, "repertoire")
          val landed = fetch(url)
          val row = oldCities.find(_.slug == slug)
          if (landed != Right(s"${country.pathPrefix}/${meant.slug}/api/repertoire")) Some(s"$url → $landed, not ${meant.slug}")
          // The catalog still names it: the same page, so the zone and a link to it agree.
          else if (row.exists(r => serverCity(r.slug) != (country -> meant))) Some(s"$code/$slug names ${row.map(r => serverCity(r.slug))} now, not ${meant.slug}")
          else if (row.exists(_ => Old.zoneOf(slug, Old.country(code).timezone) != meant.zoneId.getId)) Some(s"$code/$slug on the wrong zone")
          // It does not: a page retired since, which the build reads on its country's
          // zone and the server 301s onto its successor (checked above).
          else Option.when(row.isEmpty && !country.renamedSlugs.contains(slug))(s"$code/$slug left the catalog without being renamed")
      }).toSeq
    } yield problem
    withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
  }

  /** Every city segment a country's links carry — a page's slug, a renamed one, an
   *  alias (the last two are the catalog's `otherSlugs`, `formerSlugs` among them) —
   *  with the page the web sends it to. */
  private val everySegment: Seq[(Country, String, City)] = for {
    country <- Country.switchable
    segment <- (country.cities.map(_.slug) ++ country.renamedSlugs.keys ++ country.slugAliases.keys).distinct.sorted
    page    <- country.resolve(segment).toSeq
  } yield (country, segment, page)

  /** The served association file's excluded paths. */
  private lazy val aasaExcludes: Set[String] = {
    val controller = new WellKnownController(Helpers.stubControllerComponents())
    val served = Json.parse(contentAsString(controller.appleAppSiteAssociation(FakeRequest())))
    (served \ "applinks" \ "details" \ 0 \ "components").as[Seq[JsObject]]
      .filter(c => (c \ "exclude").asOpt[Boolean].contains(true)).map(c => (c \ "/").as[String]).toSet
  }

  "A deep link to any page or other name of one" should "open its city, or be left to Safari by the AASA" in {
    // What a 2.0.11 build opens for each: a city other than the web's page is wrong,
    // and so is nothing at all for a link that names a page.
    val wrong = for {
      (country, segment, page) <- everySegment
      opened = Old.open(s"${country.webUrl.get}/$segment/movie/dune").map { case (_, slug) => serverCity(slug) }
      if opened.fold(country.bySlug.contains(segment))(_ != (country -> page))
    } yield (country, segment)
    // iOS: every one excluded, listing and everything under it, so it opens in Safari.
    val notExcluded = wrong.filterNot { case (c, segment) =>
      aasaExcludes(s"${c.pathPrefix}/$segment/") && aasaExcludes(s"${c.pathPrefix}/$segment/*")
    }
    withClue(notExcluded.map { case (c, s) => s"${c.code}/$s" }.take(40).mkString("\n")) { notExcluded shouldBe empty }
    // Android App Links are baked into the APK and cannot be carved out: these are
    // its known residual. A displaced page opens the slug's old owner's city
    // (`/uk/york/` → York, Pennsylvania) or, where that owner has no page any more
    // (`/us/kent/`, the UK's retired county), nothing; an alias that is another
    // country's catalog slug opens that city (`/uk/rochester/` → Rochester, New York).
    wrong.map { case (c, s) => c.code -> s } shouldBe InstalledBuildSlugs.misopened.map { case (c, s) => c.code -> s }
    wrong.size shouldBe 81
    // …and nothing else is excluded: every other city link still opens the app.
    val fileExcludes = Json.parse(Source.fromResource("wellknown/apple-app-site-association").mkString)
      .\("applinks").\("details").\(0).\("components").as[Seq[JsObject]].map(c => (c \ "/").as[String]).toSet
    (aasaExcludes -- fileExcludes).size shouldBe 2 * wrong.size
  }

  it should "open every displaced page under the slug the catalog names it by" in {
    InstalledBuildSlugs.displaced.foreach { case (country, city) =>
      val slug = InstalledBuildSlugs.of(country, city)
      Old.open(s"${country.webUrl.get}/$slug/movie/dune") shouldBe Some(country.code -> slug)
      // …which the web 301s to the page, so a link shared from such a build works anywhere.
      fetch(s"${country.webUrl.get}/$slug/") shouldBe Right(s"${country.pathPrefix}/${city.slug}/")
    }
  }

  "A film link an installed build shares from a displaced page" should "land on that film on the page, with a 200" in {
    // Such a build mints its share links under the catalog `slug` it knows the city by.
    val york  = Country.UnitedKingdom.bySlug("york")
    val title = "Yorkshire Feature"
    val film = models.MovieRecord(imdbId = Some("tt1010101"), data = Map[models.Source, models.SourceData](
      york.cinemas.head -> models.SourceData(title = Some(title), releaseYear = Some(2024),
                                             showtimes = Seq(models.Showtime(controllers.TestCityControllers.now.plusDays(2), None, None, Nil)))))
    val uk = new CityRoutesDeployment(Country.UnitedKingdom, films = Seq((title, Some(2024), film)))
    val qualified = InstalledBuildSlugs.of(Country.UnitedKingdom, york)
    qualified shouldBe "york-north-yorkshire"
    // The film's canonical address, as the page names it.
    val canonical = uk.get(s"/uk/york/movie?title=${java.net.URLEncoder.encode(title, "UTF-8")}").header.headers.getOrElse(LOCATION, fail("no canonical film address"))
    val filmSlug  = canonical.split("/movie/").lastOption.getOrElse(fail(canonical))
    val onPage    = s"/uk/york/movie/$filmSlug"
    // The canonical form, its pre-rename `/film/` spelling, and the legacy `?title=` form.
    uk.landsOnOk(s"/uk/$qualified/movie/$filmSlug", hops = 3) shouldBe Right(onPage)
    uk.landsOnOk(s"/uk/$qualified/film/$filmSlug", hops = 3) shouldBe Right(onPage)
    uk.landsOnOk(s"/uk/$qualified/movie?title=${java.net.URLEncoder.encode(title, "UTF-8")}", hops = 3) shouldBe Right(onPage)
    // …and the build that shared it opens it on the same city.
    Old.open(s"https://showtimes.cc/uk/$qualified/movie/$filmSlug") shouldBe Some("uk" -> qualified)
  }

  "A link naming no country" should "open the city of the slug's old owner" in {
    val failures = for {
      old    <- oldCities
      target <- Country.bareSlugTarget(old.slug).toSeq
      if Old.open(s"kinowo://${old.slug}") != Some(target.country.code -> InstalledBuildSlugs.of(target.country, target.city))
    } yield s"kinowo://${old.slug} → ${Old.open(s"kinowo://${old.slug}")}, not ${target.country.code}/${target.city.slug}"
    withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
  }
}
