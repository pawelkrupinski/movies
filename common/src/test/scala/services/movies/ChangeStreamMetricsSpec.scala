package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ChangeStreamMetrics.Kind

class ChangeStreamMetricsSpec extends AnyFlatSpec with Matchers {

  "updateKinds" should "classify a cinema slot write as source_data" in {
    ChangeStreamMetrics.updateKinds(Set("sourceData.Multikino Stary Browar", "updatedAt")) shouldBe Set(Kind.SourceData)
  }

  it should "classify a rating value or url as rating" in {
    ChangeStreamMetrics.updateKinds(Set("imdbRating", "updatedAt"))       shouldBe Set(Kind.Rating)
    ChangeStreamMetrics.updateKinds(Set("rottenTomatoesUrl", "updatedAt")) shouldBe Set(Kind.Rating)
  }

  it should "classify resolution-lifecycle fields as identity" in {
    ChangeStreamMetrics.updateKinds(Set("tmdbId", "updatedAt"))        shouldBe Set(Kind.Identity)
    ChangeStreamMetrics.updateKinds(Set("detailPending", "updatedAt")) shouldBe Set(Kind.Identity)
    // `wikidataId` is an external identifier like the two above, and it reaches the
    // change stream the same way now that `MovieRecordPatch` carries it — before that
    // it could only ride a whole-document replace. An unclassified field falls to the
    // `other` catch-all, which is where a real identity change would have gone.
    ChangeStreamMetrics.updateKinds(Set("wikidataId", "updatedAt"))     shouldBe Set(Kind.Identity)
  }

  it should "flag an updatedAt-only write as the no-op canary" in {
    ChangeStreamMetrics.updateKinds(Set("updatedAt")) shouldBe Set(Kind.UpdatedAtOnly)
    ChangeStreamMetrics.updateKinds(Set.empty)        shouldBe Set(Kind.UpdatedAtOnly)
  }

  it should "report every kind a multi-field update touched" in {
    ChangeStreamMetrics.updateKinds(Set("sourceData.Helios", "detailPending", "updatedAt")) shouldBe
      Set(Kind.SourceData, Kind.Identity)
  }

  it should "fall back to other for an unrecognised field" in {
    ChangeStreamMetrics.updateKinds(Set("someNewField", "updatedAt")) shouldBe Set(Kind.Other)
  }

  it should "treat a REMOVED cinema slot as source_data, not a no-op" in {
    // A prune ($unset sourceData.X) leaves only updatedAt in updatedFields and the
    // slot in removedFields — a real change, so both must be considered.
    ChangeStreamMetrics.updateKinds(Set("updatedAt"), Set("sourceData.Multikino")) shouldBe Set(Kind.SourceData)
  }

  it should "flag a genuine no-op (nothing set OR removed) as updated_at_only" in {
    ChangeStreamMetrics.updateKinds(Set("updatedAt"), Set.empty) shouldBe Set(Kind.UpdatedAtOnly)
  }

  "normalizeOp" should "keep known ops and collapse the rest to other" in {
    ChangeStreamMetrics.normalizeOp("update")     shouldBe ChangeStreamMetrics.Op.Update
    ChangeStreamMetrics.normalizeOp("delete")     shouldBe ChangeStreamMetrics.Op.Delete
    ChangeStreamMetrics.normalizeOp("invalidate") shouldBe ChangeStreamMetrics.Op.Other
  }

  // The slot split moved cinema slots out of `movies`, so a slot change reaches the
  // stream as a bare `updatedAt` bump plus a `slotsUpdatedAt` marker. Without counting
  // that marker, every scrape would classify as `updated_at_only` — burying the
  // redundant-write canary in legitimate traffic and zeroing `source_data`.
  it should "count a slotsUpdatedAt write as source_data, not as the no-op canary" in {
    ChangeStreamMetrics.updateKinds(Set("updatedAt", "slotsUpdatedAt")) shouldBe
      Set(ChangeStreamMetrics.Kind.SourceData)
  }

  it should "still report a bare updatedAt write as the no-op canary" in {
    ChangeStreamMetrics.updateKinds(Set("updatedAt")) shouldBe
      Set(ChangeStreamMetrics.Kind.UpdatedAtOnly)
  }
}
