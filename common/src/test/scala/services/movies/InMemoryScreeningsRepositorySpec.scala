package services.movies

import models.{KinoMuranow, Kinoteka, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger

class InMemoryScreeningsRepositorySpec extends AnyFlatSpec with Matchers {

  private def st(h: Int) = Showtime(LocalDateTime.of(2026, 6, 8, h, 0), Some(s"https://book/$h"))

  "upsertSlot" should "add a slot's showtimes, grouped per film and slot key" in {
    val repo = new InMemoryScreeningsRepository
    repo.upsertSlot("wonka|2026", "Kino Muranów␟wonka", Seq(st(18), st(20)))
    repo.upsertSlot("wonka|2026", "Kinoteka␟wonka",     Seq(st(19)))
    repo.findForFilm("wonka|2026") shouldBe Map(
      "Kino Muranów␟wonka" -> Seq(st(18), st(20)),
      "Kinoteka␟wonka"     -> Seq(st(19)))
    repo.findForFilm("absent|2026") shouldBe empty
    repo.findAll().keySet shouldBe Set("wonka|2026")
  }

  "replaceFilm" should "set a film's screenings to exactly the given slots (deleting the rest)" in {
    val repo = new InMemoryScreeningsRepository
    repo.upsertSlot("f|2026", "A␟f", Seq(st(10)))
    repo.upsertSlot("f|2026", "B␟f", Seq(st(11)))
    repo.replaceFilm("f|2026", Map("B␟f" -> Seq(st(12)))) // A dropped, B updated
    repo.findForFilm("f|2026") shouldBe Map("B␟f" -> Seq(st(12)))
    repo.replaceFilm("f|2026", Map.empty)                // empties → film gone
    repo.findForFilm("f|2026") shouldBe empty
    repo.findAll() shouldBe empty
  }

  "deleteSlot / deleteFilm" should "remove one slot or the whole film" in {
    val repo = new InMemoryScreeningsRepository
    repo.upsertSlot("f|2026", "A␟f", Seq(st(10)))
    repo.upsertSlot("f|2026", "B␟f", Seq(st(11)))
    repo.deleteSlot("f|2026", "A␟f")
    repo.findForFilm("f|2026") shouldBe Map("B␟f" -> Seq(st(11)))
    repo.deleteFilm("f|2026")
    repo.findForFilm("f|2026") shouldBe empty
  }

  "watch" should "ring on a genuine change, NOT on a no-op write, and stop after close" in {
    val repo  = new InMemoryScreeningsRepository
    val rings = new AtomicInteger(0)
    val lastFilm = new java.util.concurrent.atomic.AtomicReference[String]("")
    val handle = repo.watch(filmId => { rings.incrementAndGet(); lastFilm.set(filmId) }).get

    repo.upsertSlot("f|2026", "A␟f", Seq(st(10)))
    rings.get()    shouldBe 1
    lastFilm.get() shouldBe "f|2026"

    repo.upsertSlot("f|2026", "A␟f", Seq(st(10))) // identical → no-op, must not ring
    rings.get() shouldBe 1

    repo.upsertSlot("f|2026", "A␟f", Seq(st(10), st(12))) // changed → rings
    rings.get() shouldBe 2

    repo.deleteSlot("f|2026", "absent") // nothing to delete → no ring
    rings.get() shouldBe 2

    handle.close()
    repo.upsertSlot("f|2026", "B␟f", Seq(st(13))) // unsubscribed → no more rings
    rings.get() shouldBe 2
  }

  // ── pure split helpers (the write-routing logic) ────────────────────────────
  "showtimesOf" should "keep only cinema slots that carry showtimes, keyed by wire-key" in {
    val data = Map[models.Source, SourceData](
      KinoMuranow -> SourceData(title = Some("Wonka"), showtimes = Seq(st(18), st(20))),
      Kinoteka    -> SourceData(title = Some("Wonka")),                              // no showtimes → excluded
      Tmdb        -> SourceData(title = Some("Wonka"), showtimes = Seq.empty))       // Tmdb never has showtimes
    ScreeningsRepository.showtimesOf(data) shouldBe Map("Kino Muranów" -> Seq(st(18), st(20)))
  }

  "slotOps" should "emit an upsert only for a slot whose showtimes changed, a delete when they empty, and nothing for a metadata-only change" in {
    val before = Map[models.Source, SourceData](
      KinoMuranow -> SourceData(director = Seq("X"), showtimes = Seq(st(18))),
      Kinoteka    -> SourceData(showtimes = Seq(st(19))))
    val after = Map[models.Source, SourceData](
      KinoMuranow -> SourceData(director = Seq("Y"), showtimes = Seq(st(18))),       // metadata-only → no op
      Kinoteka    -> SourceData(showtimes = Seq.empty))                              // emptied → delete
    ScreeningsRepository.slotOps(before, after) shouldBe Map("Kinoteka" -> None)

    // a genuine showtime change → upsert
    val after2 = before + (KinoMuranow -> SourceData(showtimes = Seq(st(18), st(21))))
    ScreeningsRepository.slotOps(before, after2) shouldBe Map("Kino Muranów" -> Some(Seq(st(18), st(21))))

    // a slot removed entirely → delete
    ScreeningsRepository.slotOps(before, Map(KinoMuranow -> before(KinoMuranow))) shouldBe Map("Kinoteka" -> None)
  }

  "stripShowtimes" should "empty every slot's showtimes, leaving the rest intact" in {
    val data = Map[models.Source, SourceData](
      KinoMuranow -> SourceData(title = Some("W"), director = Seq("D"), showtimes = Seq(st(18))),
      Tmdb        -> SourceData(title = Some("W")))
    val stripped = ScreeningsRepository.stripShowtimes(data)
    stripped(KinoMuranow).showtimes shouldBe empty
    stripped(KinoMuranow).director  shouldBe Seq("D") // metadata preserved
    stripped(Tmdb)                  shouldBe data(Tmdb) // untouched (no showtimes)
  }

  "stitch" should "re-inject showtimes from screenings, falling back to the embedded copy per slot" in {
    val data = Map[models.Source, SourceData](
      KinoMuranow -> SourceData(director = Seq("D"), showtimes = Seq.empty),          // stripped on disk
      Kinoteka    -> SourceData(showtimes = Seq(st(9))))                              // not-yet-migrated (embedded)
    val screenings = Map("Kino Muranów" -> Seq(st(18), st(20)))                       // only Muranów in screenings
    val stitched = ScreeningsRepository.stitch(data, screenings)
    stitched(KinoMuranow).showtimes shouldBe Seq(st(18), st(20)) // from screenings
    stitched(KinoMuranow).director  shouldBe Seq("D")            // metadata kept
    stitched(Kinoteka).showtimes    shouldBe Seq(st(9))          // embedded fallback (no screenings doc)

    ScreeningsRepository.stitch(data, Map.empty) shouldBe data   // no screenings → unchanged
  }
}
