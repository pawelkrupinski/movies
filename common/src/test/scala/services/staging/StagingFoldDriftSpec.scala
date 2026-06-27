package services.staging

import models.{MikroBronowice, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/**
 * Regression for the staging "fold" loop: a concluded newcomer whose re-derived
 * display title sanitizes AWAY from the sanitized middle baked into its `_id` at
 * creation must still be selected (and so folded + deleted), not stranded.
 *
 * The live "Toy Story 5- dubbing" drift is now closed at the source — `sanitize`
 * keys the numeral in Arabic AFTER the decoration strip, so the scrape title and
 * its display form both key on `toystory5` (see SanitizeNumeralKeySpec). But a row
 * keyed during the brief romanize-era still carries the stale `_id` middle
 * `toystoryv` until it is re-scraped, and its re-derived title now sanitizes to
 * `toystory5` — a transitional drift the fold must tolerate. The old `MongoStagingFolder` picked
 * the fold group by a `_id`-middle regex, so the re-sanitized title matched
 * nothing — the fold no-op'd "done" without deleting the row, and the reaper
 * re-enqueued it every minute forever. `StagingFold.selectStagingGroup` keys on
 * `sanitize(r.title)` instead (the SAME key the reaper groups + the gauge counts
 * on), so the fold consumes exactly the rows the reaper marked ready.
 */
class StagingFoldDriftSpec extends AnyFlatSpec with Matchers {

  // A row persisted under the romanize-era key (`toystoryv`), read back after the
  // deromanize fix shifted the title's sanitize to `toystory5`.
  private val staleId = "Mikro Bronowice|toystoryv|"
  private val record  = MovieRecord(
    tmdbNoMatch = true,
    searchTitle = Some("Toy Story 5- dubbing"),
    data = Map[Source, SourceData](MikroBronowice -> SourceData(title = Some("Toy Story 5- dubbing"))))
  private val row     = StagingRecord.fromStorage(staleId, record).getOrElse(fail(s"fromStorage($staleId) returned None"))
  private val idMiddle = staleId.substring(staleId.indexOf('|') + 1, staleId.lastIndexOf('|'))

  "a stale-keyed staging row" should "actually drift (premise check)" in {
    TitleNormalizer.sanitize(row.title) should not be idMiddle
    info(s"_id middle=$idMiddle  title='${row.title}'  sanitize(title)=${TitleNormalizer.sanitize(row.title)}")
  }

  "selectStagingGroup" should "select a row whose title sanitizes away from its _id middle" in {
    StagingFold.selectStagingGroup(Seq(row), row.title) shouldBe Seq(row)
  }

  it should "NOT be found by the legacy _id-middle match (documents the bug)" in {
    // What MongoStagingFolder used to do: regex the `_id` middle against the
    // re-sanitized title. The drift makes it select nothing → the fold no-op.
    val legacyKey = TitleNormalizer.sanitize(row.title)
    Seq(row).filter(_.id.matches(s"^[^|]+\\|${java.util.regex.Pattern.quote(legacyKey)}\\|.*")) shouldBe empty
  }

  it should "still group an undecorated, non-drifting row by sanitized title" in {
    val plain   = StagingRecord(MikroBronowice, "Kumotry", Some(2026),
      MovieRecord(tmdbNoMatch = true, data = Map[Source, SourceData](MikroBronowice -> SourceData(title = Some("Kumotry")))))
    StagingFold.selectStagingGroup(Seq(plain), "Kumotry") shouldBe Seq(plain)
    StagingFold.selectStagingGroup(Seq(plain), "Something Else") shouldBe empty
  }
}
