package services.movies

import ch.qos.logback.classic.Level
import models.{Helios, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.LogCapture

import java.time.LocalDateTime

/**
 * `MovieRepository.deleteStrandedSideRows` removes the `screenings` / `movie_slots` rows
 * whose film has no `movies` document — and ONLY those. The rule is
 * [[StrandedSideRows.sweep]]'s, shared with `MongoMovieRepository`, so what this pins
 * against the in-memory store is what production follows; the Mongo half (the `$group`
 * read and the `$in` delete) is `StrandedSideRowsIntegrationSpec`'s.
 *
 * The refusals matter as much as the removals: a sweep that convicts on an unreadable side
 * store, an incomplete corpus scan, or an empty corpus is the 129-film-outage shape again.
 */
class StrandedSideRowsSpec extends AnyFlatSpec with Matchers {

  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1), bookingUrl = None))
  private def slotKey(title: String) = s"helios␟$title"

  /** Production's storage shape — showtimes in `screenings`, cinema slots in `movie_slots`. */
  private def split(screenings: ScreeningsRepository = new InMemoryScreeningsRepository,
                    slots: SlotsRepository = new InMemorySlotsRepository) = {
    val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
    (repository, screenings, slots)
  }

  /** A live film written through the repository, so its rows land in BOTH side stores
   *  the way every scrape's do. Returns its stored id. */
  private def liveFilm(repository: InMemoryMovieRepository, title: String): String = {
    repository.upsert(title, Some(2026), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(title), releaseYear = Some(2026), showtimes = tomorrow))))
    repository.findAll().find(_.title == title).get.id.value
  }

  /** Rows filed under an id no document holds — what a pre-cascade delete or merge left. */
  private def strand(screenings: ScreeningsRepository, slots: SlotsRepository, filmId: String,
                     inScreenings: Boolean = true, inSlots: Boolean = true): Unit = {
    if (inScreenings) screenings.upsertSlot(filmId, slotKey(filmId), tomorrow)
    if (inSlots)      slots.upsertSlot(filmId, slotKey(filmId), SourceData(title = Some(filmId)))
  }

  "deleteStrandedSideRows" should "remove the rows of films with no document and leave a live film's rows alone" in {
    val (repository, screenings, slots) = split()
    val liveId = liveFilm(repository, "Live")
    strand(screenings, slots, "deadboth|2020")
    strand(screenings, slots, "deadscreeningsonly|1986", inSlots = false)
    strand(screenings, slots, "deadslotsonly|1999", inScreenings = false)

    repository.deleteStrandedSideRows() shouldBe StrandedSideRows(screenings = 2, slots = 2,
      filmIds = Set("deadboth|2020", "deadscreeningsonly|1986", "deadslotsonly|1999"))

    screenings.filmIdsChecked() shouldBe ((Set(liveId), true))
    slots.filmIdsChecked()      shouldBe ((Set(liveId), true))
    screenings.findForFilm(liveId) should not be empty
    slots.findForFilm(liveId)      should not be empty
    repository.findAll().map(_.id.value) shouldBe Seq(liveId)
  }

  it should "remove a live film's screenings row whose movie_slots twin is gone, and keep its twinned rows" in {
    val (repository, screenings, slots) = split()
    val liveId = liveFilm(repository, "Live")
    // A second venue's showtimes whose slot was dropped: projects nothing, inflates the census.
    screenings.upsertSlot(liveId, "kino-x␟live", tomorrow)
    screenings.rowIdsChecked()._1 should have size 2
    slots.rowIdsChecked()._1      should have size 1

    repository.deleteStrandedSideRows() shouldBe StrandedSideRows(screenings = 0, slots = 0, filmIds = Set.empty, twinless = 1)

    screenings.rowIdsChecked() shouldBe slots.rowIdsChecked()
    screenings.findForFilm(liveId).keySet shouldBe slots.findForFilm(liveId).keySet
    repository.deleteStrandedSideRows() shouldBe StrandedSideRows.none
  }

  it should "remove no twinless row while the slot store's ids cannot be read" in {
    val unreadableSlots = new InMemorySlotsRepository {
      override def rowIdsChecked(): (Set[String], Boolean) = (Set.empty, false)
    }
    val (repository, screenings, _) = split(slots = unreadableSlots)
    val liveId = liveFilm(repository, "Live")
    screenings.upsertSlot(liveId, "kino-x␟live", tomorrow)

    repository.deleteStrandedSideRows().twinless shouldBe 0
    screenings.rowIdsChecked()._1 should have size 2
  }

  it should "be a no-op once nothing is stranded, and against a store with no side collections" in {
    val (repository, screenings, slots) = split()
    liveFilm(repository, "Live")
    strand(screenings, slots, "dead|2020")
    repository.deleteStrandedSideRows().filmIds shouldBe Set("dead|2020")
    repository.deleteStrandedSideRows()         shouldBe StrandedSideRows.none

    new InMemoryMovieRepository(normalizer = titleNormalizer).deleteStrandedSideRows() shouldBe StrandedSideRows.none
  }

  it should "skip a side store whose id read failed, while still sweeping the other" in {
    val screeningsStore = new InMemoryScreeningsRepository
    val (repository, screenings, slots) = split(screenings = new UnreadableScreeningsRepository(screeningsStore))
    liveFilm(repository, "Live")
    strand(screenings, slots, "dead|2020")

    repository.deleteStrandedSideRows() shouldBe StrandedSideRows(screenings = 0, slots = 1, filmIds = Set("dead|2020"))

    // The unreadable store's rows survive: "could not read the ids" is not "no ids".
    screeningsStore.findForFilm("dead|2020") should not be empty
    slots.findForFilm("dead|2020")           shouldBe empty
  }

  it should "refuse to sweep when the corpus reads as EMPTY" in {
    val (repository, screenings, slots) = split()
    strand(screenings, slots, "dead|2020")

    repository.deleteStrandedSideRows() shouldBe StrandedSideRows.none
    screenings.findForFilm("dead|2020") should not be empty
    slots.findForFilm("dead|2020")      should not be empty
  }

  it should "refuse to sweep when the corpus scan was INCOMPLETE" in {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    strand(screenings, slots, "dead|2020")

    StrandedSideRows.sweep(Some(screenings), Some(slots), liveIds = () => None) shouldBe StrandedSideRows.none
    screenings.findForFilm("dead|2020") should not be empty
    slots.findForFilm("dead|2020")      should not be empty
  }

  it should "put the removal on the removal-audit log with the film ids" in {
    val (repository, screenings, slots) = split()
    liveFilm(repository, "Live")
    strand(screenings, slots, "dead|2020")

    val events = LogCapture.thisThread(RemovalAudit.LoggerName, Some(Level.INFO))(repository.deleteStrandedSideRows())

    val lines = events.map(_.getFormattedMessage).filter(_.contains("movies.deleteStrandedSideRows"))
    withClue(s"audit lines: $lines\n") {
      lines should have size 1
      lines.head should include ("dead|2020")
      lines.head should include ("1 screenings row(s) + 1 slot row(s)")
    }
  }
}
