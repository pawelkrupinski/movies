package services.cinemas.common

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.LocalDate

/**
 * A programme served a page per day, whose own page for today links the other days in a picker.
 * Today's page must answer — it is the one that names the days, so its failure fails the scrape
 * (red, never a white "0 films"). Every later day it links is read by [[ListingPages.readEach]]:
 * side by side, one that fails dropping only that day, unless every one did.
 */
object DayPickerProgramme {

  /** `(day, page)` for today and each later day `pickedDays` reads off today's page, today first. */
  def read(label: String, http: HttpFetch, baseUri: String, today: LocalDate, dayUrl: LocalDate => String)(
    pickedDays: Document => Seq[LocalDate]
  ): Seq[(LocalDate, Document)] = {
    def page(url: String): Document = Jsoup.parse(http.get(url), baseUri)
    val first    = page(dayUrl(today))
    val later = pickedDays(first).filter(_.isAfter(today))
    (today -> first) +: ListingPages.readEach(label, later, dayUrl)(page)
  }

}
