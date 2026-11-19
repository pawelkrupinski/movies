package services.enrichment

import models.CacheKey

import clients.tools.FixtureFile
import models.{Imdb, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import tools.{GetOnlyHttpFetch, HttpFetch, HttpStatusException, RoutingHttpFetch}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The IMDb sources' whole flows -- `ImdbRatings` (the rating and the details slot off a row's IMDb id) and
 * `OmdbBackfill` (a missing IMDb id recovered from OMDb) -- replayed for a fixed set of rows, and what each did written
 * out: every per-row refresh's answer, the row it left and its repository writes (and a second refresh's, which an
 * unchanged read must not write); every walk's summary, rows and cadence reports; IMDb's operator re-check. The text
 * is checked in (`imdb-sources-replay.txt`) from the classes as they were before the coupling split of the IMDb
 * sources, so the split is held to doing exactly what they did.
 *
 * IMDb's details are its recorded title records (`test/resources/fixtures/imdb/identity_record_*.json`; the CDN
 * refuses this machine a fresh TitleDetails capture), its refusal the 403 its CDN answers a blocked client; its rating
 * answers are the minimal ratingsSummary shape [[RatingSitesReplaySpec]] replays. OMDb's are its recorded answers
 * (`test/resources/fixtures/omdb`).
 *
 * On a mismatch the actual text is written to `target/imdb-sources-replay.actual.txt` for a diff.
 */
class ImdbSourcesReplaySpec extends AnyFlatSpec with Matchers {

  private val Golden = "test/resources/fixtures/imdb-sources-replay.txt"

  private def fixture(path: String): String = FixtureFile.read(s"test/resources/fixtures/$path")

  private final case class Row(title: String, year: Option[Int], record: MovieRecord)

  private def cacheOf(rows: Seq[Row]): (CaffeineMovieCache, InMemoryMovieRepository) = {
    val repository = new InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record)), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    repository.upserts.clear()
    (cache, repository)
  }

  private type Cadence = (CacheKey, Option[Int], Option[String]) => Unit

  private def cadenceInto(sink: ConcurrentLinkedQueue[String]): Cadence =
    (k, id, v) => { sink.add(s"${k.cleanTitle}|${id.getOrElse("-")}|${v.getOrElse("-")}"); () }

  private def answer(run: => Option[String]): String =
    Try(run).fold(e => s"threw ${e.getClass.getSimpleName}", v => s"answered ${v.getOrElse("-")}")

  // ── IMDb ───────────────────────────────────────────────────────────────────

  /** IMDb's GraphQL by query and id: the rating query answers a rating by id, the details query a recorded record;
   *  a blocked id is refused with the 403 IMDb's CDN answers a blocked client. */
  private val imdbHttp: HttpFetch = new HttpFetch {
    def get(url: String): String = throw new RuntimeException("ImdbClient uses POST")
    override def post(url: String, body: String, contentType: String): String = {
      val id      = """"id":"([^"]+)"""".r.findFirstMatchIn(body).map(_.group(1)).getOrElse("")
      val details = body.contains("TitleDetails")
      def refused = throw new HttpStatusException(403, "POST", url, None)
      if (details) id match {
        case "tt1" | "tt4" | "tt5" => fixture("imdb/identity_record_kuzma.json")
        case "tt2" | "tt7"         => fixture("imdb/identity_record_snow_leopard.json")
        case "tt3" | "tt6"         => refused
        case _                     => """{"data":{"title":null}}"""
      } else id match {
        case "tt1" => rating(7.43, 1234)
        case "tt2" => rating(6.0, 12)
        case "tt3" => rating(6.0, 12)
        case "tt4" => """{"data":{"title":{"ratingsSummary":{"aggregateRating":null,"voteCount":0}}}}"""
        case "tt5" => refused
        case "tt7" => rating(8.04, 50)
        case _     => """{"data":{"title":null}}"""
      }
    }
    private def rating(value: Double, votes: Int) =
      s"""{"data":{"title":{"ratingsSummary":{"aggregateRating":$value,"voteCount":$votes}}}}"""
  }

  private val imdbRows = Seq(
    Row("Kuzma", Some(2026), MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(5.0))),
    Row("Snow Leopard", Some(2023), MovieRecord(tmdbId = Some(2), imdbId = Some("tt2"), imdbRating = Some(6.0))),
    Row("Details Blocked", Some(2024), MovieRecord(tmdbId = Some(3), imdbId = Some("tt3"), imdbRating = Some(5.0))),
    Row("Unrated", Some(2026), MovieRecord(tmdbId = Some(4), imdbId = Some("tt4"), imdbRating = Some(3.0))),
    Row("Rating Blocked", Some(2026), MovieRecord(tmdbId = Some(5), imdbId = Some("tt5"), imdbRating = Some(2.0))),
    Row("Both Blocked", Some(2024), MovieRecord(tmdbId = Some(6), imdbId = Some("tt6"), imdbRating = Some(2.0))),
    Row("No Such Title", Some(2024), MovieRecord(tmdbId = Some(8), imdbId = Some("tt8"), imdbRating = Some(4.0))),
    Row("No Id", Some(2024), MovieRecord(tmdbId = Some(9))),
    Row("Snow Held", Some(2023), MovieRecord(imdbId = Some("tt7"), imdbRating = Some(8.0),
      data = Map[Source, SourceData](Tmdb -> SourceData(director = Seq("Pema Tseden"))))))

  private def imdbOwned(r: MovieRecord): String =
    s"id=${r.imdbId.getOrElse("-")} rating=${r.imdbRating.getOrElse("-")} slot=${r.data.get(Imdb).getOrElse("-")}"

  private def imdbRatings(cache: CaffeineMovieCache, cadence: Cadence) =
    ImdbSourcesOf.ratings(cache, new ImdbClient(http = imdbHttp), cadence)

  private def imdbReplay: Seq[String] = {
    val perRow = imdbRows.map { row =>
      val (cache, repository) = cacheOf(Seq(row))
      val cadence = new ConcurrentLinkedQueue[String]
      val ratings = imdbRatings(cache, cadenceInto(cadence))
      val key = cache.keyOf(row.title, row.year)
      val first = answer(ratings.refreshOneSync(key))
      val writes = repository.upserts.size
      val second = answer(ratings.refreshOneSync(key))
      s"IMDb row '${row.title}' (${row.year.getOrElse("-")}): $first; ${cache.get(key).fold("gone")(imdbOwned)}; " +
        s"writes=$writes; again $second, writes=${repository.upserts.size - writes}" +
        (if (cadence.isEmpty) "" else s"; cadence=${cadence.asScala.toSeq.sorted.mkString(",")}")
    }
    perRow ++ imdbWalk("walk", (ratings, _) => ratings.refreshAllNow()) ++
      imdbWalk("recheck", (ratings, cache) => ratings.recheck(imdbRows.flatMap(row => {
        val key = cache.keyOf(row.title, row.year); cache.get(key).map(key -> _) })))
  }

  private def imdbWalk(name: String, run: (ImdbRatings, CaffeineMovieCache) => services.tasks.BulkRefreshResult): Seq[String] = {
    val (cache, repository) = cacheOf(imdbRows)
    val cadence = new ConcurrentLinkedQueue[String]
    val summary = run(imdbRatings(cache, cadenceInto(cadence)), cache)
    Seq(s"IMDb $name: ${summaryOf(summary)} writes=${repository.upserts.size}") ++
      imdbRows.map(row => s"  ${row.title} (${row.year.getOrElse("-")}): " +
        cache.get(cache.keyOf(row.title, row.year)).fold("gone")(imdbOwned)) ++
      cadence.asScala.toSeq.sorted.map(c => s"  cadence $c")
  }

  private def summaryOf(summary: services.tasks.BulkRefreshResult): String =
    s"walked=${summary.walked} changed=${summary.changed} discovered=${summary.discovered} failed=${summary.failed} " +
      s"message=${summary.message.replaceAll("""\d+ms""", "Nms")}"

  // ── OMDb ───────────────────────────────────────────────────────────────────

  /** OMDb's recorded answers by the title asked: Aftersun's record, "not found" for anything else; `Broken` fails as
   *  a 503 would. */
  private val omdbHttp: HttpFetch = new GetOnlyHttpFetch {
    private val answers = new RoutingHttpFetch(Seq(
      "t=Aftersun" -> fixture("omdb/title_aftersun_2022.json"),
      "?t="        -> fixture("omdb/title_not_found.json"),
      "?s="        -> fixture("omdb/search_too_many_results.json"),
      "?i="        -> fixture("omdb/id_incorrect.json")), getOnly = true)
    def get(url: String): String =
      if (url.contains("Broken")) throw new HttpStatusException(503, "GET", url, None) else answers.get(url)
  }

  private def omdbClient(budget: OmdbBudget = OmdbBudget.Unmetered) =
    new OMDbClient(omdbHttp, apiKey = Some(settings.OmdbApiKey("test-key")), clock = _root_.tools.SpecClock.Pinned, budget = budget)

  private val tmdbOriginal = (title: String) => Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some(title)))

  private val omdbRows = Seq(
    Row("Aftersun", Some(2022), MovieRecord(tmdbId = Some(1))),
    Row("Po słońcu", Some(2022), MovieRecord(tmdbId = Some(2), data = tmdbOriginal("Aftersun"))),
    Row("Unknown Film", Some(2020), MovieRecord(tmdbId = Some(3))),
    Row("Aftersun", Some(2021), MovieRecord(tmdbId = Some(4))),
    Row("Identified", Some(2022), MovieRecord(tmdbId = Some(5), imdbId = Some("tt0000005"))),
    Row("No Tmdb", Some(2022), MovieRecord()),
    Row("Broken", Some(2022), MovieRecord(tmdbId = Some(7))))

  private def omdbOwned(r: MovieRecord): String = s"id=${r.imdbId.getOrElse("-")}"

  private def attemptsOf(store: InMemoryOmdbAttemptStore): String =
    store.all().toSeq.sortBy(_._1).map { case (film, a) => s"$film@${a.level}" }.mkString("[", ",", "]")

  private def omdbReplay: Seq[String] = {
    val perRow = omdbRows.map { row =>
      val (cache, repository) = cacheOf(Seq(row))
      val attempts = new InMemoryOmdbAttemptStore
      val backfill = ImdbSourcesOf.backfill(cache, omdbClient(), attempts, clock = _root_.tools.SpecClock.Pinned)
      val key = cache.keyOf(row.title, row.year)
      val first = answer(backfill.refreshOneSync(key))
      val writes = repository.upserts.size
      val second = answer(backfill.refreshOneSync(key))
      s"OMDb row '${row.title}' (${row.year.getOrElse("-")}): $first; ${cache.get(key).fold("gone")(omdbOwned)}; " +
        s"writes=$writes; attempts=${attemptsOf(attempts)}; again $second, writes=${repository.upserts.size - writes}"
    }
    perRow ++ omdbWalk("walk", omdbRows, OmdbBudget.Unmetered) ++
      // A spent budget: every row that would ask OMDb waits for a later sweep, untouched.
      omdbWalk("walk, budget spent", omdbRows.filter(r => r.record.tmdbId.isDefined && r.record.imdbId.isEmpty),
        new InMemoryOmdbBudget(limit = 0))
  }

  private def omdbWalk(name: String, rows: Seq[Row], budget: OmdbBudget): Seq[String] = {
    val (cache, repository) = cacheOf(rows)
    val attempts = new InMemoryOmdbAttemptStore
    val summary = ImdbSourcesOf.backfill(cache, omdbClient(budget), attempts, clock = _root_.tools.SpecClock.Pinned).refreshAllNow()
    Seq(s"OMDb $name: ${summaryOf(summary)} writes=${repository.upserts.size} attempts=${attemptsOf(attempts)}") ++
      rows.map(row => s"  ${row.title} (${row.year.getOrElse("-")}): " +
        cache.get(cache.keyOf(row.title, row.year)).fold("gone")(omdbOwned))
  }

  "the IMDb sources' refresh and walk, replayed" should "do what they did before the coupling split" in {
    val actual = (imdbReplay ++ omdbReplay).mkString("", "\n", "\n")
    val golden = Try(new String(Files.readAllBytes(Paths.get(Golden)), StandardCharsets.UTF_8)).getOrElse("")
    if (actual != golden) {
      val out = Paths.get("target/imdb-sources-replay.actual.txt")
      Files.createDirectories(out.getParent)
      Files.write(out, actual.getBytes(StandardCharsets.UTF_8))
    }
    actual shouldBe golden
  }
}
