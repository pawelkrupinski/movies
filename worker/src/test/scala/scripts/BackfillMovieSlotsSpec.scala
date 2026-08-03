package scripts

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Cinema, CinemaShowing, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, InMemorySlotsRepository, MovieRepository, StoredMovieRecord}

/**
 * The backfill closes the tail the lazy split leaves behind: films nobody re-scrapes
 * would otherwise keep reading from the embedded map forever, and the embedded copy
 * could never be retired.
 */
class BackfillMovieSlotsSpec extends AnyFlatSpec with Matchers {

  private val cinema: Cinema = Cinema.all.head
  private val slotA: Source  = cinema
  private val slotB: Source  = CinemaShowing(cinema, "dubbing")

  private def sd(title: String) = SourceData(title = Some(title))

  private def repoOf(rows: (String, Option[Int], MovieRecord)*): MovieRepository =
    new InMemoryMovieRepository(rows)

  "the backfill" should "copy every film's embedded slots into movie_slots" in {
    val repo  = repoOf(
      ("Film A", Some(2026), MovieRecord(data = Map(slotA -> sd("A"), slotB -> sd("A-dub")))),
      ("Film B", Some(2026), MovieRecord(data = Map(slotA -> sd("B")))))
    val slots = new InMemorySlotsRepository

    val (scanned, written, slotRows, complete) = BackfillMovieSlots.run(repo, slots, dryRun = false)
    scanned shouldBe 2
    written shouldBe 2
    slotRows shouldBe 3
    complete shouldBe true

    val idA = StoredMovieRecord.idFor("Film A", Some(2026), titleNormalizer)
    slots.findForFilm(idA).keySet shouldBe Set(slotA.displayName, slotB.displayName)
  }

  it should "write nothing under --dry-run" in {
    val repo  = repoOf(("Film A", Some(2026), MovieRecord(data = Map(slotA -> sd("A")))))
    val slots = new InMemorySlotsRepository
    val (_, written, _, _) = BackfillMovieSlots.run(repo, slots, dryRun = true)
    written shouldBe 1              // still REPORTS what it would do
    slots.findAll() shouldBe empty  // …but touched nothing
  }

  it should "converge on a re-run rather than duplicating rows" in {
    val repo  = repoOf(("Film A", Some(2026), MovieRecord(data = Map(slotA -> sd("A")))))
    val slots = new InMemorySlotsRepository
    BackfillMovieSlots.run(repo, slots, dryRun = false)
    val first = slots.findAll()
    BackfillMovieSlots.run(repo, slots, dryRun = false)
    slots.findAll() shouldBe first
  }

  it should "skip a film with no slots rather than writing an empty row set" in {
    // An empty write would `replaceFilm(id, Map.empty)`, whose $nin:[] clears the film —
    // so a slot-less row must be skipped, not "migrated" to nothing.
    val repo  = repoOf(("Slotless", Some(2026), MovieRecord(data = Map.empty[Source, SourceData])))
    val slots = new InMemorySlotsRepository
    slots.replaceFilm(StoredMovieRecord.idFor("Slotless", Some(2026), titleNormalizer), Map("pre-existing" -> sd("keep")))
    val (scanned, written, _, _) = BackfillMovieSlots.run(repo, slots, dryRun = false)
    scanned shouldBe 1
    written shouldBe 0
    slots.findForFilm(StoredMovieRecord.idFor("Slotless", Some(2026), titleNormalizer)) should not be empty
  }
}
