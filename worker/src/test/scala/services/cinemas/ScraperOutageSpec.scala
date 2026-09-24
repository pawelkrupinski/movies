package services.cinemas

import clients.tools.FailingHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.CachingDetailFetch

import java.time.LocalDate
import java.util.concurrent.{Executors, TimeUnit, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * A failure is never data: EVERY scraper in the catalog, with its whole upstream down,
 * must fail its scrape — never come back as an empty successful one.
 *
 * An empty listing is the one answer an outage must not give. It reads as a white
 * "0 showtimes" scrape on /uptime (indistinguishable from a dormant venue), and the
 * cache treats it as the venue's whole programme. That shape was fixed client by client
 * (ecc6d7f63: Kino Diana, Kino Tatry, Filmweb, bilety24 subdomains, Kino Kijów, MCSW
 * Elektrownia) and pinned only for the clients the fix touched ([[ListingOutageSpec]]).
 * This walks the whole catalog instead, so a client added tomorrow is held to it
 * without anyone remembering to list it.
 *
 * Every seam the catalog takes (the shared fetch, Multikino's, biletyna's, Zyte's,
 * flicks', Vue's, Odeon's) answers with the same fault, and each of the three shapes a
 * dead upstream takes on the wire — HTTP 500, a request timeout, a refused connection —
 * gets its own pass, one test per client class naming every venue of it that swallowed.
 */
class ScraperOutageSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 9, 24)

  /** Longer than any client's own bounded wait on an already-failed future, short
   *  enough that a scraper which HANGS on a dead upstream is reported, not waited out. */
  private val perScrape = 30.seconds

  private def catalog(down: tools.HttpFetch): CinemaScraperCatalog =
    new CinemaScraperCatalog(
      down, mkFetch = down, bnFetch = down, today = today,
      chainDetailCache = (_, h, ttl) => new CachingDetailFetch(h, ttl),
      zyteFetch = down, flicksFetch = down, vueFetch = down, odeonFetch = down,
      // A token, so Odeon reaches its (dead) upstream rather than throwing on the
      // missing token before any fetch — that throw would pass for the wrong reason.
      odeonAuthToken = () => Some("token"), titles = titleNormalizer)

  /** Clients this spec found swallowing a total outage on its first run, fixed in the
   *  next commit. `pendingUntilFixed` fails the moment one of them passes. */
  private val KnownSwallowing = Set("KinoGramClient", "KinoKreskaClient", "KinoMikroClient",
    "KinoSwiatowidElblagClient", "NoweHoryzontyClient", "SdkClient", "VisualTicketClient")

  private sealed trait Verdict
  private case object Failed extends Verdict
  private final case class Answered(films: Int) extends Verdict
  private case object Hung extends Verdict

  private def scrapeAll(scrapers: Seq[CinemaScraper]): Seq[(CinemaScraper, Verdict)] = {
    val pool = Executors.newFixedThreadPool(16)
    given ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try {
      val running = scrapers.map(s => s -> Future(Try(s.fetch())))
      running.map { case (s, f) =>
        s -> (Try(Await.result(f, perScrape)) match {
          case Success(Success(movies)) => Answered(movies.size)
          case Success(Failure(_))      => Failed
          case Failure(_: TimeoutException) => Hung
          case Failure(e)               => throw e
        })
      }
    } finally { pool.shutdownNow(); pool.awaitTermination(5, TimeUnit.SECONDS) }
  }

  FailingHttpFetch.Faults.foreach { fault =>
    // `byCity`, not `all`: disabled cities' scrapers are still modelled and wired, and
    // come back the day their city does.
    val scrapers = catalog(fault.fetch).byCity.values.flatten.toSeq
    lazy val verdicts = scrapeAll(scrapers)

    scrapers.groupBy(_.getClass.getSimpleName).toSeq.sortBy(_._1).foreach { case (clientName, ofClient) =>
      clientName should s"fail its scrape, not return an empty one, when every upstream answers ${fault.name}" in {
       def check(): Unit = {
        val mine = verdicts.filter { case (s, _) => ofClient.contains(s) }
        val swallowed = mine.collect { case (s, Answered(n)) => s"${s.cinema.displayName} (returned $n films)" }
        val hung      = mine.collect { case (s, Hung) => s.cinema.displayName }
        withClue(s"${swallowed.size} of ${ofClient.size} venues read ${fault.name} as a successful scrape: ${swallowed.take(10).mkString(", ")} — ") {
          swallowed shouldBe empty
        }
        withClue(s"${hung.size} venues were still scraping a dead upstream after $perScrape: ${hung.take(10).mkString(", ")} — ") {
          hung shouldBe empty
        }
       }
       if (KnownSwallowing(clientName)) pendingUntilFixed(check()) else check()
      }
    }
  }
}
