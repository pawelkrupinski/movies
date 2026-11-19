package services.enrichment

import tools.SpecClock.given

import clients.HttpTmdbClient
import clients.tools.{FailingHttpFetch, FixtureFile}
import models.{Filmweb, Imdb, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import tools.{HttpFetch, RoutingHttpFetch}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Every rating site's whole enrichment flow -- the per-row refresh and the full-corpus walk -- replayed over the
 * recorded pages (`test/resources/fixtures/{metacritic,rottentomatoes,filmweb}`) for a fixed set of rows, and what it
 * did written out: each per-row refresh's answer, the row it left, its repository writes; each walk's summary, rows
 * and cadence reports. The text is checked in (`rating-sites-replay.txt`) from the code before the rating-site port
 * redesign, so the redesign is held to doing exactly what the classes it replaced did: same values, same links, same
 * cadence reports, same failures.
 *
 * On a mismatch the actual text is written to `target/rating-sites-replay.actual.txt` for a diff.
 */
class RatingSitesReplaySpec extends AnyFlatSpec with Matchers {

  private val Golden = "test/resources/fixtures/rating-sites-replay.txt"

  private def fixture(path: String): String = FixtureFile.read(s"test/resources/fixtures/$path")

  /** A site's recorded pages by URL fragment; a URL containing `broken` fails as a 503 would, anything else unrouted
   *  is the 404 of a page the site does not have. */
  private def site(routes: Seq[(String, String)]): HttpFetch = new HttpFetch {
    private val pages = new RoutingHttpFetch(routes, unroutedIsNotFound = true)
    def get(url: String): String =
      if (url.contains("broken")) throw new RuntimeException(s"HTTP 503 $url") else pages.get(url)
    override def post(url: String, body: String, contentType: String): String =
      if (body.contains("broken")) throw new RuntimeException(s"HTTP 503 $url") else pages.post(url, body, contentType)
  }

  private val disabledTmdb = new HttpTmdbClient(FailingHttpFetch.Unexpected, apiKey = None)

  /** A row as the scenario names it: its title, year and record. */
  private final case class Row(title: String, year: Option[Int], record: MovieRecord)

  private val nolan  = Map[Source, SourceData](Tmdb -> SourceData(director = Seq("Christopher Nolan")))
  private val holland = Map[Source, SourceData](Tmdb -> SourceData(director = Seq("Agnieszka Holland")))
  private val cinemaNolan = Map[Source, SourceData](Multikino -> SourceData(director = Seq("Christopher Nolan"), releaseYear = Some(2008)))

  // ── Metacritic ─────────────────────────────────────────────────────────────

  private val mcKnight = "https://www.metacritic.com/movie/the-dark-knight"
  private val mcClimber = "https://www.metacritic.com/movie/girl-climber"
  private val mcRoutes = Seq(
    "/movie/the-dark-knight" -> fixture("metacritic/movie_the_dark_knight.html"),
    "/movie/girl-climber"    -> fixture("metacritic/movie_girl_climber.html"),
    "/search/"               -> fixture("metacritic/search_top_gun.html"))
  private val mcRows = Seq(
    Row("The Dark Knight", Some(2008), MovieRecord(tmdbId = Some(1), metacriticUrl = Some(mcKnight), metascore = Some(70))),
    Row("Knight Same", Some(2008), MovieRecord(tmdbId = None, metacriticUrl = Some(mcKnight), metascore = Some(85))),
    Row("Girl Climber", Some(2024), MovieRecord(tmdbId = None, metacriticUrl = Some(mcClimber), metascore = Some(60))),
    Row("Knight Denied", Some(2008), MovieRecord(tmdbId = None, metacriticUrl = Some(mcKnight), metascore = Some(70), data = holland)),
    Row("Knight Agreed", Some(2008), MovieRecord(tmdbId = None, metacriticUrl = Some(mcKnight), metascore = Some(70), data = nolan)),
    Row("The Dark Knight", None, MovieRecord(tmdbId = Some(2))),
    Row("Unknown Film", Some(2020), MovieRecord(tmdbId = Some(3), metascore = Some(50))),
    Row("The Dark Knight", Some(2009), MovieRecord(data = cinemaNolan)),
    Row("Broken Page", Some(2020), MovieRecord(tmdbId = None, metacriticUrl = Some("https://www.metacritic.com/movie/broken"), metascore = Some(40))),
    Row("Top Gun", Some(1986), MovieRecord(tmdbId = Some(4))))

  // ── Rotten Tomatoes ────────────────────────────────────────────────────────

  private val rtKnight = "https://www.rottentomatoes.com/m/the_dark_knight"
  private val rtScorecard = "https://www.rottentomatoes.com/m/scorecard_film"
  private val rtRoutes = Seq(
    "/m/the_dark_knight" -> fixture("rottentomatoes/movie_the_dark_knight.html"),
    "/m/scorecard_film"  -> fixture("rottentomatoes/movie_scorecard_criticsscore.html"),
    "/search"            -> fixture("rottentomatoes/search_top_gun.html"))
  private val rtRows = Seq(
    Row("The Dark Knight", Some(2008), MovieRecord(tmdbId = Some(1), rottenTomatoesUrl = Some(rtKnight), rottenTomatoes = Some(70))),
    Row("Knight Same", Some(2008), MovieRecord(tmdbId = None, rottenTomatoesUrl = Some(rtKnight), rottenTomatoes = Some(94))),
    Row("Scorecard", None, MovieRecord(tmdbId = None, rottenTomatoesUrl = Some(rtScorecard), rottenTomatoes = Some(10))),
    Row("Knight Year", Some(1975), MovieRecord(tmdbId = None, rottenTomatoesUrl = Some(rtKnight), rottenTomatoes = Some(70))),
    Row("Knight Year Agreed", Some(1975), MovieRecord(tmdbId = None, rottenTomatoesUrl = Some(rtKnight), rottenTomatoes = Some(70), data = nolan)),
    Row("Knight Denied", None, MovieRecord(tmdbId = None, rottenTomatoesUrl = Some(rtKnight), rottenTomatoes = Some(70), data = holland)),
    Row("The Dark Knight", None, MovieRecord(tmdbId = Some(2))),
    Row("Unknown Film", Some(2020), MovieRecord(tmdbId = Some(3), rottenTomatoes = Some(50))),
    Row("The Dark Knight", Some(2009), MovieRecord(data = cinemaNolan)),
    Row("Broken Page", Some(2020), MovieRecord(tmdbId = None, rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/broken"), rottenTomatoes = Some(40))),
    Row("Top Gun", Some(1986), MovieRecord(tmdbId = Some(4))))

  // ── Filmweb ────────────────────────────────────────────────────────────────

  private val fwSkarpetki = "https://www.filmweb.pl/serial/Niesamowite+przygody+skarpetek+4%3A+Do+roboty%21-2026-10118632"
  private val fwRoutes = Seq(
    "query=niesamowite+przygody+skarpetek+4.+do+roboty%21"  -> fixture("filmweb/search_skarpetki_4_period_users_only.json"),
    "query=niesamowite+przygody+skarpetek+4%3A+do+roboty%21" -> fixture("filmweb/search_skarpetki_4_colon.json"),
    "/film/10118632/info"    -> fixture("filmweb/info_skarpetki_4.json"),
    "/film/10118632/preview" -> fixture("filmweb/preview_skarpetki_4.json"),
    "/film/10118632/rating"  -> fixture("filmweb/rating_skarpetki_4.json"))
  private val skarpetki = "Niesamowite przygody skarpetek 4. Do roboty!"
  private val fwSlot = Map[Source, SourceData](Filmweb -> SourceData(originalTitle = Some("Skarpetki"), releaseYear = Some(2026)))
  private val fwRows = Seq(
    Row(skarpetki, Some(2026), MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), filmwebUrl = Some(fwSkarpetki), filmwebRating = Some(1.0), data = fwSlot)),
    Row("Skarpetki Whole", Some(2026), MovieRecord(tmdbId = Some(2), filmwebUrl = Some(fwSkarpetki), filmwebRating = Some(1.0))),
    Row("Skarpetki Wrong Year", Some(1990), MovieRecord(tmdbId = Some(3), imdbId = Some("tt3"), filmwebUrl = Some(fwSkarpetki), filmwebRating = Some(1.0), data = fwSlot)),
    Row(skarpetki, None, MovieRecord(tmdbId = Some(4))),
    Row(skarpetki, Some(2025), MovieRecord(tmdbId = Some(5), imdbId = Some("tt5"))),
    Row("Unknown Film", Some(2020), MovieRecord(tmdbId = Some(6), filmwebRating = Some(5.0))),
    Row("Broken Page", Some(2020), MovieRecord(tmdbId = None, filmwebUrl = Some("https://www.filmweb.pl/film/Broken-2020-999"), filmwebRating = Some(4.0), data = fwSlot)))

  // ── IMDb ───────────────────────────────────────────────────────────────────

  /** IMDb's GraphQL by the id in the request body: a rating for tt1/tt2, none for tt3, a failure for `broken`. */
  private val imdbHttp: HttpFetch = new HttpFetch {
    def get(url: String): String = throw new RuntimeException("ImdbClient uses POST")
    override def post(url: String, body: String, contentType: String): String =
      if (body.contains("broken")) throw new RuntimeException("HTTP 503")
      else if (body.contains("tt1")) """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.43,"voteCount":1234}}}}"""
      else if (body.contains("tt2")) """{"data":{"title":{"ratingsSummary":{"aggregateRating":6.0,"voteCount":12}}}}"""
      else """{"data":{"title":{"ratingsSummary":{"aggregateRating":null,"voteCount":0}}}}"""
  }
  private val imdbRows = Seq(
    Row("Moved", Some(2024), MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(5.0))),
    Row("Same", Some(2024), MovieRecord(tmdbId = Some(2), imdbId = Some("tt2"), imdbRating = Some(6.0))),
    Row("Unrated", Some(2024), MovieRecord(tmdbId = Some(3), imdbId = Some("tt3"), imdbRating = Some(3.0))),
    Row("No Id", Some(2024), MovieRecord(tmdbId = Some(4))),
    Row("Broken", Some(2024), MovieRecord(tmdbId = Some(5), imdbId = Some("broken"), imdbRating = Some(2.0))))

  // ── The replay ─────────────────────────────────────────────────────────────

  /** One site: how to build its refresher over a cache and a cadence sink, its rows, and what of a row it owns. */
  private final case class Site(
    name: String,
    rows: Seq[Row],
    build: (CaffeineMovieCache, (CacheKey, Option[Int], Option[String]) => Unit) => CacheRefresher,
    owned: MovieRecord => String)

  private val sites = Seq(
    Site("Metacritic", mcRows,
      (cache, cadence) => SiteRatingsOf.metacritic(cache, disabledTmdb, new MetacriticClient(site(mcRoutes)), cadenceRecorder = cadence),
      r => s"url=${r.metacriticUrl.getOrElse("-")} score=${r.metascore.getOrElse("-")}"),
    Site("RT", rtRows,
      (cache, cadence) => SiteRatingsOf.rottenTomatoes(cache, disabledTmdb, new RottenTomatoesClient(site(rtRoutes)), cadenceRecorder = cadence),
      r => s"url=${r.rottenTomatoesUrl.getOrElse("-")} score=${r.rottenTomatoes.getOrElse("-")}"),
    Site("Filmweb", fwRows,
      (cache, cadence) => SiteRatingsOf.filmweb(cache, disabledTmdb, new FilmwebClient(site(fwRoutes)),
        onImdbIdMissing = (title, year, search) => imdbMissing.add(s"$title|${year.getOrElse("-")}|$search"), cadenceRecorder = cadence),
      r => s"url=${r.filmwebUrl.getOrElse("-")} rating=${r.filmwebRating.getOrElse("-")} slot=${r.data.get(Filmweb).getOrElse("-")}"),
    Site("IMDb", imdbRows,
      (cache, cadence) => new ImdbRatings(cache, new ImdbClient(http = imdbHttp), cadence),
      r => s"id=${r.imdbId.getOrElse("-")} rating=${r.imdbRating.getOrElse("-")} slot=${r.data.get(Imdb).getOrElse("-")}"))

  private val imdbMissing = new ConcurrentLinkedQueue[String]

  private def cacheOf(rows: Seq[Row]): (CaffeineMovieCache, InMemoryMovieRepository) = {
    val repository = new InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record)), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    repository.upserts.clear()
    (cache, repository)
  }

  private def drainMissing(): Seq[String] = {
    val all = imdbMissing.asScala.toSeq.sorted
    imdbMissing.clear()
    all
  }

  private def replay(site: Site): Seq[String] = {
    val perRow = site.rows.flatMap { row =>
      val (cache, repository) = cacheOf(Seq(row))
      val cadence = new ConcurrentLinkedQueue[String]
      val refresher = site.build(cache, (k, id, v) => cadence.add(s"${k.cleanTitle}|${id.getOrElse("-")}|${v.getOrElse("-")}"))
      val key = cache.keyOf(row.title, row.year)
      val answer = Try(refresher.refreshOneSync(key)).fold(e => s"threw ${e.getClass.getSimpleName}", v => s"answered ${v.getOrElse("-")}")
      Seq(s"${site.name} row '${row.title}' (${row.year.getOrElse("-")}): $answer; " +
        s"${cache.get(key).fold("gone")(site.owned)}; writes=${repository.upserts.size}" +
        (if (cadence.isEmpty) "" else s"; cadence=${cadence.asScala.toSeq.sorted.mkString(",")}")) ++
        drainMissing().map(m => s"  imdb-missing $m")
    }
    val (cache, _) = cacheOf(site.rows)
    val cadence = new ConcurrentLinkedQueue[String]
    val refresher = site.build(cache, (k, id, v) => cadence.add(s"${k.cleanTitle}|${id.getOrElse("-")}|${v.getOrElse("-")}"))
    val summary = refresher.refreshAllNow()
    val message = summary.message.replaceAll("""\d+ms""", "Nms")
    val walk = Seq(s"${site.name} walk: walked=${summary.walked} changed=${summary.changed} discovered=${summary.discovered} " +
      s"failed=${summary.failed} message=$message") ++
      site.rows.map(row => s"  ${row.title} (${row.year.getOrElse("-")}): " +
        cache.get(cache.keyOf(row.title, row.year)).fold("gone")(site.owned)) ++
      cadence.asScala.toSeq.sorted.map(c => s"  cadence $c") ++
      drainMissing().map(m => s"  imdb-missing $m")
    perRow ++ walk ++ recheck(site)
  }

  /** The operator's re-check of given rows (`recheck`), keyed as the per-row refresh is -- the walk's own keys are the
   *  repository's, which no site's slug probe finds. */
  private def recheck(site: Site): Seq[String] = {
    val (cache, _) = cacheOf(site.rows)
    val cadence = new ConcurrentLinkedQueue[String]
    val refresher = site.build(cache, (k, id, v) => cadence.add(s"${k.cleanTitle}|${id.getOrElse("-")}|${v.getOrElse("-")}"))
    val rows = site.rows.flatMap(row => { val key = cache.keyOf(row.title, row.year); cache.get(key).map(key -> _) })
    val summary = refresher.recheck(rows)
    val message = summary.message.replaceAll("""\d+ms""", "Nms")
    Seq(s"${site.name} recheck: walked=${summary.walked} changed=${summary.changed} discovered=${summary.discovered} " +
      s"failed=${summary.failed} message=$message") ++
      site.rows.map(row => s"  ${row.title} (${row.year.getOrElse("-")}): " +
        cache.get(cache.keyOf(row.title, row.year)).fold("gone")(site.owned)) ++
      cadence.asScala.toSeq.sorted.map(c => s"  cadence $c") ++
      drainMissing().map(m => s"  imdb-missing $m")
  }

  "every rating site's refresh and walk, replayed over the recorded pages" should "do what they did before the port redesign" in {
    val actual = sites.flatMap(replay).mkString("", "\n", "\n")
    val golden = Try(new String(Files.readAllBytes(Paths.get(Golden)), StandardCharsets.UTF_8)).getOrElse("")
    if (actual != golden) {
      val out = Paths.get("target/rating-sites-replay.actual.txt")
      Files.createDirectories(out.getParent)
      Files.write(out, actual.getBytes(StandardCharsets.UTF_8))
    }
    actual shouldBe golden
  }
}
