package services.movies

import models.Showtime
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
}
