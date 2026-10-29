package tools

import models.{Country, RegalStowmarket}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Listing, ScreeningDays}
import services.movies.ListingKey

import java.time.LocalDate

/** A lean read's season-only listing keeps, through a capture's file, the year of its first day as well as its season —
 *  the year a rule dating a film against its screenings reads (`ScreeningDays.firstYear`). Read back as the season's
 *  opening year, UK Regal Stowmarket's "Song Sung Blue- The Music of Neil Diamond", first screening in 2027, replayed as
 *  a 2026 listing, current for the 2025 film the live capture held it was not current for: the capture replayed to other
 *  takes than it captured (Identity capture #2084, #2104). */
class UnmatchedClustersSeasonSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private val uk = Country.all.find(_.code == "uk").get

  private def listing(title: String, screenings: ScreeningDays): Listing = {
    val page = s"https://regalstowmarket.co.uk/RegalStowmarket.dll/WhatsOn?f=${title.hashCode.abs}"
    Listing(RegalStowmarket, ListingKey.Native("Regal Stowmarket", page, title), title, title, title, None, Nil, Some(150), Some(page), None,
      screenings = screenings)
  }

  private def throughItsFile(listings: Seq[Listing]): Seq[Listing] = {
    val file = tempDirectory("capture-season").resolve("uk.json.gz")
    UnmatchedClusters.write(file, UnmatchedClusters.Capture(uk, listings, Nil, Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, Map.empty))
    UnmatchedClusters.read(file).listings.sortBy(_.rawTitle)
  }

  "A season-only listing" should "keep its season and its first day's year through a capture's file" in {
    val listings = Seq(
      listing("Song Sung Blue- The Music of Neil Diamond", ScreeningDays.ofSeason(LocalDate.of(2027, 1, 15))),
      listing("The Wizard of Oz", ScreeningDays.ofSeason(LocalDate.of(2026, 9, 3))))
    throughItsFile(listings).map(l => (l.rawTitle, l.screenings.season, l.screenings.firstYear)) shouldBe Seq(
      ("Song Sung Blue- The Music of Neil Diamond", Some(2026), Some(2027)),
      ("The Wizard of Oz", Some(2026), Some(2026)))
  }

  it should "read a file written before it kept the year as of its season's opening year" in {
    services.identity.ListingBson.seasonDay(2026, None) shouldBe LocalDate.of(2026, 7, 1)
    services.identity.ListingBson.seasonDay(2026, Some(2026)) shouldBe LocalDate.of(2026, 7, 1)
    services.identity.ListingBson.seasonDay(2026, Some(2027)) shouldBe LocalDate.of(2027, 1, 1)
  }

  "A listing's screening days" should "come back as they were" in {
    val days = ScreeningDays.of(Seq(LocalDate.of(2026, 10, 30), LocalDate.of(2026, 11, 2)))
    throughItsFile(Seq(listing("Digger", days))).map(_.screenings) shouldBe Seq(days)
  }
}
