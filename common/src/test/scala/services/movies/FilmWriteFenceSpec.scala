package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, TimeUnit}

/** The fence a change-stream read must pass before the cache stores it: refused when a
 *  local write of the film was in flight at the read, or began after it. */
class FilmWriteFenceSpec extends AnyFlatSpec with Matchers {

  private val film = FilmId("film|2026")

  private def applies(fence: FilmWriteFence, mark: Long): Boolean = fence.ifUndisturbed(film.value, mark)(())

  "FilmWriteFence" should "apply a read no write disturbed" in {
    val fence = new FilmWriteFence()
    applies(fence, fence.mark(film.value)) shouldBe true
  }

  it should "apply a read taken after a write finished" in {
    val fence = new FilmWriteFence()
    fence.writing(film)(())
    applies(fence, fence.mark(film.value)) shouldBe true
  }

  it should "refuse a read taken before a write that began since" in {
    val fence = new FilmWriteFence()
    val mark  = fence.mark(film.value)
    fence.writing(film)(())
    applies(fence, mark) shouldBe false
  }

  it should "refuse a read taken while a write was in flight, even once the write has finished" in {
    val fence = new FilmWriteFence()
    val mark  = fence.writing(film)(fence.mark(film.value))
    mark shouldBe FilmWriteFence.InFlight
    applies(fence, mark) shouldBe false
  }

  it should "mark every film at once, refusing only the ones written since" in {
    val fence = new FilmWriteFence()
    val other = "other|2026"
    val marks = fence.markAll()
    fence.writing(film)(())
    fence.ifUndisturbed(film.value, marks.of(film.value))(()) shouldBe false
    fence.ifUndisturbed(other, marks.of(other))(()) shouldBe true
    fence.writing(film)(fence.markAll().of(film.value)) shouldBe FilmWriteFence.InFlight
  }

  it should "not run the apply it refuses" in {
    val fence = new FilmWriteFence()
    val mark  = fence.mark(film.value)
    fence.writing(film)(())
    var ran = false
    fence.ifUndisturbed(film.value, mark) { ran = true }
    ran shouldBe false
  }

  it should "always apply an unfenced delivery — one made synchronously with the write itself" in {
    val fence = new FilmWriteFence()
    fence.writing(film)(applies(fence, FilmWriteFence.Unfenced)) shouldBe true
  }

  it should "leave another film's reads alone" in {
    val fence = new FilmWriteFence()
    val other = "other|2026"
    val mark  = fence.mark(other)
    fence.writing(film)(())
    // Different stripes unless the two ids collide — pick a stripe count where they don't.
    Math.floorMod(film.value.hashCode, FilmWriteFence.DefaultStripes) should not be
      Math.floorMod(other.hashCode, FilmWriteFence.DefaultStripes)
    fence.ifUndisturbed(other, mark)(()) shouldBe true
  }

  // The check and the apply are one step with respect to a write's START: a write that
  // begins while a read is being applied waits for that apply, then overwrites it.
  it should "hold a write's start until an apply already under way has finished" in {
    val fence    = new FilmWriteFence()
    val applying = new CountDownLatch(1)
    val release  = new CountDownLatch(1)
    val order    = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val mark     = fence.mark(film.value)
    val apply = new Thread(() => { fence.ifUndisturbed(film.value, mark) {
      applying.countDown(); release.await(5, TimeUnit.SECONDS); order.add("applied")
    }; () })
    apply.start()
    applying.await(5, TimeUnit.SECONDS) shouldBe true
    val write = new Thread(() => fence.writing(film) { order.add("wrote"); () })
    write.start()
    // The write is parked on the fence's monitor, not running.
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while (write.getState != Thread.State.BLOCKED && System.nanoTime() < deadline) Thread.onSpinWait()
    write.getState shouldBe Thread.State.BLOCKED
    order.isEmpty shouldBe true
    release.countDown()
    apply.join(5000); write.join(5000)
    order.toArray.toSeq shouldBe Seq("applied", "wrote")
  }
}
