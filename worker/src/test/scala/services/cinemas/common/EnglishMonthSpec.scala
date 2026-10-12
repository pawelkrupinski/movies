package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** UK pages spell September three ways ("Sep", "Sept", "September"); the lookup goes by
 *  the first three letters rather than the JDK's locale data, whose en-GB short form is
 *  "Sept" in one release and "Sep" in another. */
class EnglishMonthSpec extends AnyFlatSpec with Matchers {

  "ScraperParse.englishMonth" should "read a month by its first three letters, in any case" in {
    Seq("Sep", "Sept", "September", "SEPT").map(ScraperParse.englishMonth) shouldBe Seq.fill(4)(Some(9))
    ScraperParse.englishMonth("oct") shouldBe Some(10)
    ScraperParse.englishMonth("May") shouldBe Some(5)
  }

  it should "refuse anything that is no month" in {
    Seq("Mo", "Monday", "", "Sponsor").map(ScraperParse.englishMonth) shouldBe Seq.fill(4)(None)
  }
}
