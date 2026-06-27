package services.staging

import models.{MikroBronowice, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/**
 * Regression for the 30-minute staging "fold" rectangle: a concluded newcomer
 * whose re-derived display title sanitizes AWAY from the sanitized middle baked
 * into its `_id` at creation must still be selected (and so folded + deleted),
 * not stranded.
 *
 * The prod culprit was "Toy Story 5- dubbing" @ Kino Mikro: `chooseDisplay`
 * strips the "- dubbing" decoration to "Toy Story 5", whose `sanitize` romanizes
 * the now-trailing "5" → `toystoryv`, drifting from the `_id` middle `toystory5`
 * (sanitize of the un-stripped scrape title). The old `MongoStagingFolder` picked
 * the fold group by a `_id`-middle regex, so the re-sanitized title matched
 * nothing — the fold no-op'd "done" without deleting the row, and the reaper
 * re-enqueued it every minute forever. `StagingFold.selectStagingGroup` keys on
 * `sanitize(r.title)` instead (the SAME key the reaper groups + the gauge counts
 * on), so the fold consumes exactly the rows the reaper marked ready.
 */
class StagingFoldDriftSpec extends AnyFlatSpec with Matchers {

  private val scraped = "Toy Story 5- dubbing"
  private val id      = StagingRecord.idFor(MikroBronowice, scraped, None)
  private val record  = MovieRecord(
    tmdbNoMatch = true,
    searchTitle = Some(scraped),
    data = Map[Source, SourceData](MikroBronowice -> SourceData(title = Some(scraped))))
  private val row     = StagingRecord.fromStorage(id, record).getOrElse(fail(s"fromStorage($id) returned None"))

  // Guard the premise: this row genuinely drifts (its display title sanitizes
  // away from its `_id` middle). If TitleNormalizer ever stops drifting here, the
  // test below would pass trivially and prove nothing — so assert the drift.
  private val idMiddle = id.substring(id.indexOf('|') + 1, id.lastIndexOf('|'))

  "the Toy Story 5 staging row" should "actually drift (premise check)" in {
    TitleNormalizer.sanitize(row.title) should not be idMiddle
    info(s"_id middle=$idMiddle  title='${row.title}'  sanitize(title)=${TitleNormalizer.sanitize(row.title)}")
  }

  "selectStagingGroup" should "select a row whose display title sanitizes away from its _id middle" in {
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
