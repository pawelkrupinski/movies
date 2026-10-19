package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import services.cinemas.common.CinemaScraper

import java.util.concurrent.atomic.AtomicInteger

/**
 * Test double: a `CinemaScraper` whose `fetch()` evaluates `listing` afresh on every
 * call — so a `throw` there is a failing scrape — and counts the calls, so a spec can
 * assert whether the scraper was (not) probed. Everything else is a constructor
 * argument: which venue it claims, the hosts it declares, whether its listing is
 * whole, its public page. A replayed sequence of outcomes is
 * [[ScriptedCinemaScraper]].
 */
class StubCinemaScraper(
  val cinema:                     Cinema = Multikino,
  listing:                        => Seq[CinemaMovie] = Seq.empty,
  val scrapeHosts:                Set[String] = Set.empty,
  override val listingIsComplete: Boolean = true,
  override val sourceUrl:         Option[String] = None
) extends CinemaScraper {
  private val fetched = new AtomicInteger(0)

  /** How many times `fetch()` has been called, including the ones that threw. */
  def calls: Int = fetched.get()

  def fetch(): Seq[CinemaMovie] = { fetched.incrementAndGet(); listing }
}
