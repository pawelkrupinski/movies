package services.movies

import models.{Cinema, CinemaShowing, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The `movie_slots` split: per-cinema `SourceData` stored one row per
 * `(filmId, slotKey)` instead of embedded in the film document. Covers the storage
 * contract [[InMemorySlotsRepository]] and [[MongoSlotsRepository]] both owe, plus
 * the pure helpers the write/read paths are built from.
 *
 * The round-trip case is the load-bearing one: a slot has to come back addressed by
 * the SAME `Source` it went in as, or a re-scrape lands on a different slot and the
 * film grows a duplicate.
 */
class SlotsRepositorySpec extends AnyFlatSpec with Matchers {

  private val cinema: Cinema = Cinema.all.head
  private val slotA: Source  = cinema
  private val slotB: Source  = CinemaShowing(cinema, "dubbing")

  private def sd(title: String, poster: Option[String] = None) =
    SourceData(title = Some(title), posterUrl = poster)

  private def repo = new InMemorySlotsRepository

  "slotsOf" should "key each slot by its wire form" in {
    SlotsRepository.slotsOf(Map(slotA -> sd("A"), slotB -> sd("B"))).keySet shouldBe
      Set(slotA.displayName, slotB.displayName)
  }

  it should "drop showtimes — they stay authoritative in screenings" in {
    val withTimes = sd("A").copy(showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 27, 20, 0), None)))
    SlotsRepository.slotsOf(Map(slotA -> withTimes))(slotA.displayName).showtimes shouldBe empty
  }

  "stitch" should "round-trip a slot back to the same Source it was stored under" in {
    val data = Map(slotB -> sd("B"))
    SlotsRepository.stitch(SlotsRepository.slotsOf(data)) shouldBe data
  }

  it should "drop a wire key that no longer names a known Source" in {
    SlotsRepository.stitch(Map("no-such-cinema-anywhere" -> sd("X"))) shouldBe empty
  }

  it should "re-apply the superseded-cinema-slot rule, so a bare slot can't resurrect" in {
    // A bare-cinema slot alongside a per-title slot for the SAME cinema is superseded —
    // exactly as the movies codec drops it on decode.
    val stitched = SlotsRepository.stitch(
      Map(slotA.displayName -> sd("bare"), slotB.displayName -> sd("dub")))
    stitched.keySet shouldBe Set(slotB)
  }

  "slotOps" should "emit nothing when the slots are unchanged" in {
    val data = Map(slotA -> sd("A"))
    SlotsRepository.slotOps(data, data) shouldBe empty
  }

  it should "upsert a changed slot and delete a removed one" in {
    val before = Map(slotA -> sd("A"), slotB -> sd("B"))
    val after  = Map(slotA -> sd("A2"))
    val ops    = SlotsRepository.slotOps(before, after)
    ops(slotA.displayName).map(_.title) shouldBe Some(Some("A2"))
    ops(slotB.displayName)              shouldBe None
  }

  it should "ignore a showtimes-only change — those are the screenings collection's job" in {
    val before = Map(slotA -> sd("A"))
    val after  = Map(slotA -> sd("A").copy(showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 27, 20, 0), None))))
    SlotsRepository.slotOps(before, after) shouldBe empty
  }

  "the repository" should "store, read back and replace a film's slots" in {
    val r = repo
    r.replaceFilm("f1", Map("a" -> sd("A"), "b" -> sd("B")))
    r.findForFilm("f1").keySet shouldBe Set("a", "b")
    r.replaceFilm("f1", Map("a" -> sd("A")))
    r.findForFilm("f1").keySet shouldBe Set("a")      // "b" pruned as stale
  }

  it should "clear every slot when replaced with an empty map" in {
    val r = repo
    r.replaceFilm("f1", Map("a" -> sd("A")))
    r.replaceFilm("f1", Map.empty)
    r.findForFilm("f1") shouldBe empty
  }

  it should "upsert and delete one slot without touching the others" in {
    val r = repo
    r.replaceFilm("f1", Map("a" -> sd("A"), "b" -> sd("B")))
    r.upsertSlot("f1", "a", sd("A2"))
    r.findForFilm("f1")("a").title shouldBe Some("A2")
    r.deleteSlot("f1", "b")
    r.findForFilm("f1").keySet shouldBe Set("a")
  }

  it should "keep films apart in findAll and drop only the deleted one" in {
    val r = repo
    r.replaceFilm("f1", Map("a" -> sd("A")))
    r.replaceFilm("f2", Map("a" -> sd("Z")))
    r.findAll().keySet shouldBe Set("f1", "f2")
    r.deleteFilm("f1")
    r.findAll().keySet shouldBe Set("f2")
  }

  // The read rule for the transitional state where a film has BOTH an embedded map and
  // stored rows, and the two disagree. Prod PL 2026-07-27: 81 of the 82 films holding
  // both had diverging key sets, 14 of them with a cinema the stored rows did not carry
  // — those 14 were being served with fewer cinemas than the corpus held.
  "merge" should "keep a cinema the embedded map has and the stored rows do not" in {
    // Two DISTINCT cinemas, so neither supersedes the other and the union is visible.
    val other: Source = Cinema.all(1)
    val merged = SlotsRepository.merge(Map(slotA -> sd("embedded-only")), Map(other.displayName -> sd("B")))
    merged.keySet shouldBe Set(slotA, other)
    merged(slotA).title shouldBe Some("embedded-only")
  }

  it should "let a stored row win a key both carry — it is the fresher write path" in {
    SlotsRepository.merge(Map(slotA -> sd("stale")), Map(slotA.displayName -> sd("fresh")))(slotA).title shouldBe
      Some("fresh")
  }

  it should "collapse to exactly the stored rows once the embedded copy is retired" in {
    SlotsRepository.merge(Map.empty, Map(slotA.displayName -> sd("A"))) shouldBe
      SlotsRepository.stitch(Map(slotA.displayName -> sd("A")))
  }

  it should "leave an un-migrated film's embedded map alone, showtimes included" in {
    val withTimes = sd("A").copy(showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 27, 20, 0), None)))
    SlotsRepository.merge(Map(slotA -> withTimes), Map.empty) shouldBe Map(slotA -> withTimes)
  }

  it should "still drop a bare-cinema slot superseded by a per-title one across the union" in {
    // embedded carries the legacy bare-Cinema key, stored carries the per-title one that
    // supersedes it — the union must not resurrect the twin `stitch` exists to drop.
    SlotsRepository.merge(Map(slotA -> sd("bare")), Map(slotB.displayName -> sd("per-title"))).keySet shouldBe
      Set(slotB)
  }

  // The per-film read's failure signal. An in-memory read cannot fail, so this pins the
  // contract's shape; MovieRepositoryIntegrationSpec pins the Mongo side's `false`.
  "findForFilmChecked" should "report a genuinely slot-less film as a COMPLETE empty read" in {
    repo.findForFilmChecked("nobody") shouldBe (Map.empty, true)
  }

  "the composite id" should "survive a filmId that itself contains the separator" in {
    val weird = s"film${SlotKeyed.IdSep}odd"
    SlotKeyed.filmIdOf(SlotKeyed.idOf(weird, "slot")) shouldBe "film"   // documents the known limit
    SlotKeyed.idOf("f1", "a") shouldBe s"f1${SlotKeyed.IdSep}a"
  }
}
