package scripts

import models.{CinemaShowing, KinoMuza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

/**
 * The guard that decides which rows this one-off actually touches.
 *
 * Pinned here rather than exercised through Mongo because it is the only part of the
 * script that can do harm: a forced re-resolve strips a row back to its scraped data
 * and drops its ratings until they are re-fetched, so running it on a row that has
 * already been corrected is a real regression, not a no-op. Shapes taken from prod
 * PL, 2026-08-29.
 */
class ReresolveSelfLockedRowsSpec extends AnyFlatSpec with Matchers {

  private val StandUp = "grzegorzdolniakmoglobycgorzejstandup|2009"
  private val Up      = 14160
  private val expected = Map(StandUp -> Up)

  private val slot: Map[Source, SourceData] =
    Map(CinemaShowing(KinoMuza, "grzegorzdolniakmoglobycgorzejstandup") ->
        SourceData(title = Some("Grzegorz Dolniak \"Mogło być gorzej\" Stand up")))

  private def row(tmdbId: Option[Int], slots: Map[Source, SourceData] = slot) =
    Some(StoredMovieRecord("Grzegorz Dolniak \"Mogło być gorzej\" Stand up", Some(2009),
                           MovieRecord(tmdbId = tmdbId, data = slots)))

  "a row still locked to the wrong film" should "be re-resolved" in {
    val (locked, skipped) =
      ReresolveSelfLockedRows.stillLocked(Seq(StandUp -> row(Some(Up))), expected)

    locked.map(_.title) shouldBe Seq("Grzegorz Dolniak \"Mogło być gorzej\" Stand up")
    skipped shouldBe empty
  }

  "a row that has since been corrected" should "be left alone" in {
    // Someone re-enriched it, or the worker re-keyed it. Its ratings are its own now.
    val (locked, skipped) =
      ReresolveSelfLockedRows.stillLocked(Seq(StandUp -> row(Some(999999))), expected)

    locked shouldBe empty
    skipped.head should include("now tmdbId=999999")
  }

  "a row that has since been unresolved" should "be left alone" in {
    // Already the outcome this script is trying to reach — forcing again would only
    // strip and re-search for nothing.
    val (locked, skipped) =
      ReresolveSelfLockedRows.stillLocked(Seq(StandUp -> row(None)), expected)

    locked shouldBe empty
    skipped.head should include("now tmdbId=—")
  }

  "a stranded row no cinema names" should "be left alone" in {
    // Prod really holds one of these (`klapskinokobietjakzyczebyniezwariowac|1976`): it
    // HAS a cinema slot, but the slot carries no title, so the row's display title falls
    // back to its `_id`. `resetToScrapedData` would find nothing to reset it to and the
    // payload would carry that id-derived string into a TMDB search. Having a slot is
    // therefore not the test — having a slot a cinema put a TITLE on is.
    val titleless = Map[Source, SourceData](CinemaShowing(KinoMuza, "grzegorzdolniakmoglobycgorzejstandup") -> SourceData())
    val (locked, skipped) =
      ReresolveSelfLockedRows.stillLocked(Seq(StandUp -> row(Some(Up), titleless)), expected)

    locked shouldBe empty
    skipped.head should include("no cinema publishes a title")
  }

  "a row that no longer exists" should "be reported, not crash" in {
    val (locked, skipped) = ReresolveSelfLockedRows.stillLocked(Seq(StandUp -> None), expected)

    locked shouldBe empty
    skipped.head should include("row is gone")
  }

  "every id this script carries" should "name the wrong film it is locked to" in {
    // The id is the whole safety mechanism — an entry without one would be forced
    // unconditionally. Also guards against a copy-paste that drops the mapping.
    ReresolveSelfLockedRows.lockedTo should have size 12  // 11 actionable + the stranded KLAPS row the slot guard drops
    ReresolveSelfLockedRows.lockedTo.keys.foreach(_ should include("|"))
    ReresolveSelfLockedRows.lockedTo.values.foreach(_ should be > 0)
  }
}
