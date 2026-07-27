package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The reaper deletes on the strength of one set difference, so that difference is what
 * gets pinned. Everything else in the script is guardrails around it: a corpus scan that
 * has to report complete, and a cap on how much one run can remove.
 */
class ReapOrphanedFilmRowsSpec extends AnyFlatSpec with Matchers {

  "orphanFilmIds" should "name only side rows whose film is gone from the corpus" in {
    ReapOrphanedFilmRows.orphanFilmIds(
      liveFilmIds = Set("alive|2026", "also-alive|"),
      sideFilmIds = Seq("alive|2026", "vanished|2025", "also-alive|", "merged-away|2024")
    ) shouldBe Seq("merged-away|2024", "vanished|2025")
  }

  it should "call nothing an orphan when every side film is still live" in {
    ReapOrphanedFilmRows.orphanFilmIds(Set("a|2026", "b|2026"), Seq("a|2026", "b|2026")) shouldBe empty
  }

  // The failure mode the script's completeness guard exists for: an EMPTY live set makes
  // every side row look orphaned. The pure function faithfully says so — which is why the
  // caller must never reach it on a partial scan.
  it should "call every side film an orphan against an empty live set" in {
    ReapOrphanedFilmRows.orphanFilmIds(Set.empty, Seq("a|2026", "b|2026")) shouldBe Seq("a|2026", "b|2026")
  }
}
