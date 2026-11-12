package services.cinemas.uk

import models.UkRoster
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

/** Every UK venue's sources, read from `generated/uk-sources/sources.tsv` (`data/uk/scripts/generate_roster.py`)
 *  through [[UkFeed.parse]] into [[UkSourcesData]]. */
class UkSourcesDataSpec extends AnyFlatSpec with Matchers {

  "UkSourcesData" should "read exactly as the generated Scala did: every venue's primary and fallback, in scrape order" in {
    // UkSourcesData.scala became a table under generated/ (2026-11-12). This digest is of the dump
    // below as the generated Scala gave it; it moves only when roster.json does, so regenerate it
    // then (the failure prints the new dump's digest).
    val dump = UkSourcesData.venues.map { v =>
      val c = v.cinema
      s"SRC $c|${c.displayName}|${c.pillName}|${c.opening}|${c.hashCode}|${c.slug}|${v.primary}|${v.flicksFallback}"
    }.mkString("\n")
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(dump.getBytes(StandardCharsets.UTF_8))
    digest.map("%02x".format(_)).mkString shouldBe "46da1b91f895611f0b03bc5645dd0c615631b94ad5aa73b98ec648459cf67125"
  }

  it should "hold the roster's own venue instances, each once" in {
    UkSourcesData.venues.map(_.cinema) should contain theSameElementsInOrderAs UkRoster.all
    UkSourcesData.venues.find(_.cinema eq UkRoster("JunctionGoole")).map(_.primary) shouldBe
      Some(UkFeed.Spektrix("junctiongoole", None))
  }

  "UkFeed.parse" should "build each kind from its arguments, a Spektrix building optional" in {
    UkFeed.parse("flicks", Seq("ilkley-cinema")) shouldBe UkFeed.Flicks("ilkley-cinema")
    UkFeed.parse("savoyBoxOffice", Seq("https://x/X.dll/")) shouldBe UkFeed.SavoyBoxOffice("https://x/X.dll/")
    UkFeed.parse("spektrix", Seq("leeds", "Hyde Park Picture House")) shouldBe
      UkFeed.Spektrix("leeds", Some("Hyde Park Picture House"))
    UkFeed.parse("quadDerby", Seq.empty) shouldBe UkFeed.QuadDerby
  }

  it should "refuse an unknown kind, or arguments its kind does not take" in {
    the[IllegalArgumentException] thrownBy UkFeed.parse("regal", Seq("1")) should have message "no UK feed regal(1)"
    an[IllegalArgumentException] should be thrownBy UkFeed.parse("odeon", Seq.empty)
    an[IllegalArgumentException] should be thrownBy UkFeed.parse("bfiSouthbank", Seq("extra"))
  }
}
