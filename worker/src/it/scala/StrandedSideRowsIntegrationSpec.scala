package services.movies

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.Env

import java.time.LocalDateTime

/** The Mongo half of the stranded-row sweep: the `$group` id read, the `_id`-projected
 *  keyset scan (forced onto several pages) and the `$in` delete, against the real
 *  collections. The rule itself is `StrandedSideRowsSpec`'s; this proves the store
 *  answers it — exactly the stranded rows go, a live film keeps every row. */
class StrandedSideRowsIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri = Env.get("MONGODB_URI").get

  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1), bookingUrl = None))
  private def slotKey(title: String) = s"multikino␟$title"
  private def film(title: String): MovieRecord = MovieRecord(data = Map[Source, SourceData](
    Multikino -> SourceData(title = Some(title), releaseYear = Some(2026), showtimes = tomorrow)))

  "deleteStrandedSideRows" should "remove exactly the rows whose film has no movies document" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "stranded-side-rows") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      // One id per page, so the live-id scan has to page (two films ⇒ three fetches).
      val repository = new MongoMovieRepository(Some(db), normalizer = titleNormalizer,
        screenings = Some(screenings), slots = Some(slots), findAllBatchSize = 1)
      try {
        repository.enabled shouldBe true
        repository.upsert("__stranded-live-a__", Some(2026), film("__stranded-live-a__"))
        repository.upsert("__stranded-live-b__", Some(2026), film("__stranded-live-b__"))
        val liveIds = repository.findAll().map(_.id.value).toSet
        liveIds should have size 2

        // What a delete or merge from before the cascade left behind: rows under ids no
        // document holds — one film in both side collections, one in screenings only.
        screenings.upsertSlot("deadboth|2020", slotKey("deadboth"), tomorrow)
        slots.upsertSlot("deadboth|2020", slotKey("deadboth"), SourceData(title = Some("deadboth")))
        screenings.upsertSlot("deadscreeningsonly|1986", slotKey("deadscreeningsonly"), tomorrow)
        // A live film's screenings row whose movie_slots twin is gone.
        screenings.upsertSlot(liveIds.head, "kino-x␟twinless", tomorrow)

        screenings.filmIdsChecked() shouldBe ((liveIds + "deadboth|2020" + "deadscreeningsonly|1986", true))
        slots.filmIdsChecked()      shouldBe ((liveIds + "deadboth|2020", true))

        repository.deleteStrandedSideRows() shouldBe StrandedSideRows(screenings = 2, slots = 1, twinless = 1,
          filmIds = Set("deadboth|2020", "deadscreeningsonly|1986"))

        screenings.filmIdsChecked() shouldBe ((liveIds, true))
        slots.filmIdsChecked()      shouldBe ((liveIds, true))
        liveIds.foreach { id =>
          withClue(s"live film $id\n") {
            screenings.findForFilmChecked(id)._1 should not be empty
            slots.findForFilmChecked(id)._1      should not be empty
          }
        }
        screenings.findForFilmChecked("deadboth|2020")           shouldBe ((Map.empty, true))
        slots.findForFilmChecked("deadboth|2020")                shouldBe ((Map.empty, true))
        screenings.findForFilmChecked("deadscreeningsonly|1986") shouldBe ((Map.empty, true))

        repository.deleteStrandedSideRows() shouldBe StrandedSideRows.none
      } finally repository.close()
    }
}
