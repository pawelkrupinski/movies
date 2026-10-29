package services.movies

import tools.ScratchDirectories
import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class ListingCorporaSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private def corpusIn(cache: Path, run: String, code: String): Path = {
    val dir = cache.resolve(s"recordings/$run/pair/test/resources/fixtures/corpus")
    Files.createDirectories(dir)
    Files.write(dir.resolve(s"cinema-scrapes-$code.json.gz"), Array.emptyByteArray)
  }

  "a country's recorded corpus" should "come from the newest recording in the identity cache when none is checked in or named" in {
    val cache = tempDirectory("identity-cache")
    corpusIn(cache, "38498856157", "us")
    val newest = corpusIn(cache, "38713274617", "us")
    corpusIn(cache, "38713274617arsenal", "us")  // a hand-named copy, no run of its own
    ListingCorpora.recordedPath(Country.UnitedStates, None, Some(cache)) shouldBe Some(newest)
    // a directory named for the run wins over the cache
    val named = tempDirectory("corpora")
    Files.write(named.resolve("cinema-scrapes-us.json.gz"), Array.emptyByteArray)
    ListingCorpora.recordedPath(Country.UnitedStates, Some(named), Some(cache)) shouldBe Some(named.resolve("cinema-scrapes-us.json.gz"))
    ListingCorpora.recordedPath(Country.UnitedStates, None, Some(tempDirectory("empty"))) shouldBe None
  }
}
