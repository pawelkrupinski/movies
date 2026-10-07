package services.movies

import models.{CinemaShowing, CineworldFeltham, Country, KinoEtiuda, KinoMuza, Multikino, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `upsertSlots` is `upsertSlot` of each of a film's slots, in one read and one write rather than one of each per slot:
 *  a venue page lands on every slot of the row that names it, and a widely shown film's per-slot round trips held the
 *  detail drain (JFR, 2026-10-07). Batched must not mean different, so the same writes go through both, each on a
 *  database of its own, and the rows, what the roster refuses and which rows were rewritten come out alike. */
class SlotsBatchWriteIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val film    = "odyseja|2026"
  private val etiuda  = CinemaShowing(KinoEtiuda, "odyseja").displayName
  private val muza    = CinemaShowing(KinoMuza, "odyseja").displayName
  private val kino    = CinemaShowing(Multikino, "odyseja").displayName
  private val foreign = CinemaShowing(CineworldFeltham, "odyseja").displayName

  "upsertSlots" should "leave a film's slots exactly where upsertSlot of each does, rewriting only the rows that moved" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "slots-batch") { batchDb =>
      tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "slots-single") { singleDb =>
        val roster = VenueRoster.of(Country.Poland)
        val batch  = new MongoSlotsRepository(Some(batchDb), roster = roster)
        val single = new MongoSlotsRepository(Some(singleDb), roster = roster)
        try {
          val start = Map(etiuda -> SourceData(title = Some("Odyseja")), muza -> SourceData(title = Some("Odyseja")))
          Seq(batch, single).foreach(_.replaceFilm(film, start))
          val stampsBefore = batch.rowWrittenAtChecked().required

          val writes = Map(
            etiuda  -> SourceData(title = Some("Odyseja")),                              // unchanged: not rewritten
            muza    -> SourceData(title = Some("Odyseja"), synopsis = Some("from the page")),
            kino    -> SourceData(title = Some("Odyseja"), runtimeMinutes = Some(180)), // a new row
            foreign -> SourceData(title = Some("The Odyssey")))                          // outside the roster
          batch.upsertSlots(film, writes) shouldBe WriteOutcome.Written
          writes.foreach { case (key, slot) => single.upsertSlot(film, key, slot) }

          batch.findForFilmChecked(film) shouldBe single.findForFilmChecked(film)
          batch.findForFilmChecked(film).required.keySet shouldBe Set(etiuda, muza, kino)
          val stampsAfter = batch.rowWrittenAtChecked().required
          val etiudaRow   = SlotKeyed.idOf(film, etiuda)
          stampsAfter(etiudaRow) shouldBe stampsBefore(etiudaRow)
          batch.upsertSlots(film, Map.empty) shouldBe WriteOutcome.Written
        } finally { batch.close(); single.close() }
      }
    }
}
