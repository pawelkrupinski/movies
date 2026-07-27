package scripts

import models.{Cinema, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, InMemorySlotsRepository, MovieRepository, StoredMovieRecord}

/**
 * The verifier is the safety net for the states the unit tests could not see, so its own
 * verdicts are pinned here — a check that cannot fail is not a check.
 */
class VerifyMovieSlotsSpec extends AnyFlatSpec with Matchers {

  private val slotA: Source = Cinema.all.head
  private def sd(t: String) = SourceData(title = Some(t))

  private def repoOf(rows: (String, Option[Int], MovieRecord)*): MovieRepository =
    new InMemoryMovieRepository(rows)

  private def idOf(t: String) = StoredMovieRecord.idFor(t, Some(2026))

  "the verifier" should "pass a film whose slots live in movie_slots" in {
    val repo  = repoOf(("Migrated", Some(2026), MovieRecord(data = Map.empty[Source, SourceData])))
    val slots = new InMemorySlotsRepository
    slots.replaceFilm(idOf("Migrated"), Map(slotA.displayName -> sd("A")))
    val r = VerifyMovieSlots.run(repo, slots)
    r.healthy      shouldBe true
    r.withSlotRows shouldBe 1
    r.withEmbedded shouldBe 0
    r.stripped     shouldBe 1
  }

  it should "pass a film not yet migrated, which still has its embedded copy" in {
    val repo  = repoOf(("Legacy", Some(2026), MovieRecord(data = Map(slotA -> sd("A")))))
    val r = VerifyMovieSlots.run(repo, new InMemorySlotsRepository)
    r.healthy      shouldBe true
    r.withSlotRows shouldBe 0
    r.withEmbedded shouldBe 1
  }

  // The unrecoverable state: the embedded copy was dropped but the slot write never
  // landed, so the film has no cinemas anywhere.
  it should "FLAG a film with slots in neither place" in {
    val repo = repoOf(("Lost", Some(2026), MovieRecord(data = Map.empty[Source, SourceData])))
    val r = VerifyMovieSlots.run(repo, new InMemorySlotsRepository)
    r.healthy     shouldBe false
    r.withNeither shouldBe Seq(idOf("Lost"))
  }

  // Exactly the shape prod was in after the phase-6 deploy: movie_slots filling, not one
  // document stripped, because a later patch kept writing the embedded map back.
  it should "WARN when the corpus has slot rows but nothing has been stripped" in {
    val repo  = repoOf(("Both", Some(2026), MovieRecord(data = Map(slotA -> sd("A")))))
    val slots = new InMemorySlotsRepository
    slots.replaceFilm(idOf("Both"), Map(slotA.displayName -> sd("A")))
    val r = VerifyMovieSlots.run(repo, slots)
    r.healthy shouldBe true      // not broken — every film still reads
    r.stalled shouldBe true      // …but the split is achieving nothing
  }

  it should "not call a corpus stalled once documents start shrinking" in {
    val slots = new InMemorySlotsRepository
    slots.replaceFilm(idOf("Done"), Map(slotA.displayName -> sd("A")))
    slots.replaceFilm(idOf("Pending"), Map(slotA.displayName -> sd("B")))
    val repo = repoOf(
      ("Done",    Some(2026), MovieRecord(data = Map.empty[Source, SourceData])),
      ("Pending", Some(2026), MovieRecord(data = Map(slotA -> sd("B")))))
    val r = VerifyMovieSlots.run(repo, slots)
    r.stalled  shouldBe false
    r.stripped shouldBe 1
  }
}
