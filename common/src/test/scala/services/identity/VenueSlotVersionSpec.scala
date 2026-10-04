package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.VersionedSources.{resource, unlexable}


/** The venue slot code version (`VenueSlotMemo.codeVersion`) digests what BUILDS a venue slot (`IdentityRulesSources`,
 *  `build.sbt`): a restarted worker reuses its stored slots only under the version they were recorded under, so a
 *  deploy that moves it rebuilds every slot of the country. It digested everything `IdentityProjectionPlan.scala`
 *  reaches — the resolver, its calibration and learned decorations, the movie repository, the roster — and every
 *  deploy from 2026-10-04 02:27 on moved it: no restart after one reused a slot (a US first tick 62–69 s, not ~14). */
class VenueSlotVersionSpec extends AnyFlatSpec with Matchers {

  private lazy val digested: Seq[String] = resource("/venue-slot-sources.txt").linesIterator.toSeq

  "the venue slot version" should "digest the code that builds a slot" in {
    digested should contain allOf ("scala/services/identity/IdentityProjectionPlan.scala", "scala/services/movies/CinemaSlotBuilder.scala",
      "scala/services/movies/ScrapeListing.scala", "scala/services/movies/MovieRecordMerge.scala",
      "scala/services/movies/ShowtimesDigest.scala", "scala/services/movies/TitleNormalizer.scala", "scala/models/SourceData.scala")
  }

  it should "leave out the resolver, its learned data and the stores, which build no slot" in {
    digested should contain noneOf ("scala/services/identity/IncrementalResolver.scala", "scala/services/identity/IdentityCalibration.scala",
      "scala/services/identity/IdentityMeasures.scala", "scala/services/identity/TitleDecorations.scala",
      "scala/services/movies/MovieRepository.scala", "resources/identity-decorations.json", "resources/identity-weights.json")
  }

  it should "lex every Scala source it digests, so a comment edit to none of them moves it" in {
    unlexable(digested) shouldBe empty
  }

  it should "be what the memo's environment is made under" in {
    VenueSlotMemo.codeVersion should not be "unknown"
    VenueSlotMemo.codeVersion shouldBe resource("/venue-slot-version.txt")
  }
}
