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

  private lazy val digested: Seq[String] = resource("/services/identity/venue-slot-sources.txt").linesIterator.toSeq

  "the venue slot version" should "digest the code that builds a slot" in {
    digested should contain allOf ("identity/src/main/scala/services/identity/VenueSlots.scala", "common/src/main/scala/services/movies/CinemaSlotBuilder.scala",
      "common/src/main/scala/services/movies/ScrapeListing.scala", "common/src/main/scala/services/movies/MovieRecordMerge.scala",
      "common/src/main/scala/services/movies/ShowtimesDigest.scala", "common/src/main/scala/services/movies/TitleNormalizer.scala", "common/src/main/scala/models/SourceData.scala")
  }

  it should "leave out the resolver, its learned data and the stores, which build no slot" in {
    digested should contain noneOf ("identity/src/main/scala/services/identity/IncrementalResolver.scala", "identity/src/main/scala/services/identity/IdentityCalibration.scala",
      "identity/src/main/scala/services/identity/IdentityMeasures.scala", "identity/src/main/scala/services/identity/TitleDecorations.scala",
      "common/src/main/scala/services/movies/MovieRepository.scala", "identity/src/main/resources/identity-decorations.json", "identity/src/main/resources/identity-weights.json")
  }

  it should "lex every Scala source it digests, so a comment edit to none of them moves it" in {
    unlexable(digested) shouldBe empty
  }

  it should "leave out the rest of the projection, which builds no slot" in {
    // The projection plan held the slot memo and the venue build until 2026-10-04: every change to the rest of it moved
    // the version, and the next boot rebuilt every slot (a US first tick 65 s and 3.1 GB, not ~25 s).
    digested should contain noneOf ("identity/src/main/scala/services/identity/IdentityProjectionPlan.scala", "identity/src/main/scala/services/identity/LiveProjectionIndex.scala",
      "identity/src/main/scala/services/identity/ProjectionScope.scala")
  }

  it should "be what the memo's environment is made under" in {
    VenueSlotMemo.codeVersion should not be "unknown"
    VenueSlotMemo.codeVersion shouldBe resource("/services/identity/venue-slot-version.txt")
  }
}
