package services.cinemas.common

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.{HttpFetch, HttpRead}

import java.time.LocalDate

/**
 * A programme served a page per day, whose own page for today links the other days in a picker.
 * Today's page must answer — it is the one that names the days, so its failure fails the scrape
 * (red, never a white "0 films"). Every later day it links is read by [[ListingPages.readMore]]:
 * side by side, one that fails dropping only that day and making the listing incomplete.
 */
object DayPickerProgramme {

  /** `(day, page)` for today and each later day `pickedDays` reads off today's page, today first. */
  def read(label: String, http: HttpFetch, baseUri: String, today: LocalDate, dayUrl: LocalDate => String)(
    pickedDays: Document => Seq[LocalDate]
  ): Seq[(LocalDate, Document)] = {
    def page(url: String): Document = Jsoup.parse(HttpRead.page(http, url), baseUri)
    val first    = page(dayUrl(today))
    val later = pickedDays(first).filter(_.isAfter(today))
    (today -> first) +: ListingPages.readMore(label, later, dayUrl)(page)
  }

}
