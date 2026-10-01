package services.tasks

import models.{Cinema, CinemaMovie, Movie, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaMovieJson, PagedChunkScraper}

import java.time.Instant
import scala.collection.mutable
import scala.concurrent.duration.*

/** In-memory twin of [[MongoChunkPageMemo]]. */
final class InMemoryChunkPageMemo extends ChunkPageMemo {
  private val entries = mutable.Map.empty[(String, String), ChunkPageMemo.Entry]
  def recall(cinema: String, key: String): Option[ChunkPageMemo.Entry] = synchronized(entries.get(cinema -> key))
  def remember(cinema: String, key: String, entry: ChunkPageMemo.Entry): Unit = synchronized(entries.update(cinema -> key, entry))
}

/** A day page identical to the one a chunk last parsed is not parsed again: its parse is reused. */
class ChunkPageMemoSpec extends AnyFlatSpec with Matchers {

  private final class PagedScraper(val pages: mutable.Map[String, String], var version: Int = 1) extends PagedChunkScraper {
    val cinema: Cinema = Multikino
    var parses = 0
    def scrapeHosts: Set[String] = Set("pages.example")
    def planChunks(): Seq[String] = pages.keys.toSeq.sorted
    def fetchChunkPage(key: String): String = pages(key)
    def parseChunkPage(key: String, page: String): Seq[CinemaMovie] = {
      parses += 1
      Seq(CinemaMovie(Movie(title = s"$key:$page"), cinema, posterUrl = None, filmUrl = None, synopsis = None,
        cast = Nil, director = Nil, showtimes = Nil))
    }
    def pageParserVersion: Int = version
  }

  private final class World {
    val scraper = new PagedScraper(mutable.Map("2026-10-02" -> "<p>one</p>"))
    val store   = new InMemoryChunkScrapeStore()
    val memo    = new InMemoryChunkPageMemo
    val seen    = mutable.Buffer.empty[String]
    val clock   = java.time.Clock.fixed(Instant.parse("2026-10-01T10:00:00Z"), java.time.ZoneOffset.UTC)
    val handler = new ScrapeChunkHandler(Map(Multikino.displayName -> scraper), store, clock = clock, pageMemo = memo,
      memoMetrics = (outcome: String) => { seen += outcome; () })
    /** One run over the chunk: its stored slice. */
    def scrape(): String = {
      val runId = store.startRun(Multikino.displayName, Seq("2026-10-02"), clock.instant(), 1.hour).get
      handler.handle(services.tasks.Task("t", TaskType.ScrapeChunk, "d", Map(ChunkScrapeKeys.CinemaKey -> Multikino.displayName,
        ChunkScrapeKeys.RunIdKey -> runId, ChunkScrapeKeys.ChunkKey -> "2026-10-02"), attempts = 1))
      val stored = store.loadChunks(Multikino.displayName, runId)("2026-10-02")
      store.completeRun(Multikino.displayName, runId)
      stored
    }
  }

  "A page-at-a-time chunk" should "reuse its last parse when the page is unchanged" in {
    val w = new World; import w.*
    val first = scrape()
    scraper.parses shouldBe 1
    scrape() shouldBe first
    scraper.parses shouldBe 1
    seen.toSeq shouldBe Seq(ChunkPageMemoMetrics.New, ChunkPageMemoMetrics.Hit)
  }

  it should "parse the page again when it changed" in {
    val w = new World; import w.*
    scrape()
    scraper.pages("2026-10-02") = "<p>two</p>"
    CinemaMovieJson.decode(scrape(), Multikino).map(_.movie.title) shouldBe Seq("2026-10-02:<p>two</p>")
    scraper.parses shouldBe 2
    seen.last shouldBe ChunkPageMemoMetrics.Changed
  }

  it should "parse an unchanged page again once the parser has changed" in {
    val w = new World; import w.*
    scrape()
    scraper.version = 2
    scrape()
    scraper.parses shouldBe 2
    seen.last shouldBe ChunkPageMemoMetrics.Parser
  }
}
