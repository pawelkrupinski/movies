package clients.tools

import org.scalatest.Assertions.fail
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

/** A scraper's reading of one of its venue's film pages, for a spec that asserts the facts the page states. */
object FilmPages {

  /** The page `ref` as `scraper` reads it — a failed test when the scraper reads no film page at all, or could not
   *  read this one. */
  def detailOf(scraper: CinemaScraper, ref: String): FilmDetail = scraper match {
    case enricher: DetailEnricher => enricher.fetchFilmDetail(ref).getOrElse(fail(s"${scraper.cinema.displayName} could not read $ref"))
    case _                        => fail(s"${scraper.cinema.displayName} reads no film page, so states none of $ref's facts")
  }
}
