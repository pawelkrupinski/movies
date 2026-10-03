package views

import testsupport.TestMessages.given

import models.{City, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.LocalDateTime

/**
 * `MemoisingMinifier` keeps every distinct inline block it has minified, so a block whose
 * text changes on every render is a new entry on every render — forever. The listing's
 * main script carried its render instant (`RENDERED_AT`) inside its minified block, and
 * so grew the memo by ~25 KB of source and its minified form per listing rendered. The
 * instant now rides on `#view-root`, outside it.
 */
class RepertoireMinifierMemoSpec extends AnyFlatSpec with Matchers {

  private implicit val city: City = City.bySlug("poznan").getOrElse(fail("no city 'poznan'"))
  private val films = new controllers.MovieControllerService(
    TestReadModel.fromRecords(Seq(("Film", Some(2026), MovieRecord()))), clock = controllers.TestMovieController.clock
  ).toSchedules(city)

  "the listing's inline blocks" should "minify once however many instants they are rendered at" in {
    val minifier = new tools.MemoisingMinifier
    val first = views.html._repertoireView(films, minifier, LocalDateTime.of(2026, 6, 10, 9, 0)).body
    val held  = minifier.cachedBlocks
    views.html._repertoireView(films, minifier, LocalDateTime.of(2026, 6, 10, 9, 1)).body
    views.html._repertoireView(films, minifier, LocalDateTime.of(2026, 6, 10, 18, 30)).body
    minifier.cachedBlocks shouldBe held
    first should include ("data-rendered-at=")
  }

  // What production watches (`kinowo_web_cache_*{cache="minifier"}`): with the blocks
  // constant, every render after the first is a hit, and the entry count stays put.
  it should "report its blocks and a hit ratio that climbs with every render" in {
    val minifier = new tools.MemoisingMinifier
    for (minute <- 0 until 10) views.html._repertoireView(films, minifier, LocalDateTime.of(2026, 6, 10, 9, minute)).body
    val occupancy = minifier.occupancy
    occupancy.entries shouldBe minifier.cachedBlocks.toLong
    occupancy.hitRatio.get should be >= 0.9
  }
}
