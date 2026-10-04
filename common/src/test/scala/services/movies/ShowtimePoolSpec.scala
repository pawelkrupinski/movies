package services.movies

import models.Showtime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class ShowtimePoolSpec extends AnyFlatSpec with Matchers {

  private def at = LocalDateTime.of(2026, 10, 5, 19, 0)

  // A scan holding the corpus decoded every showtime's instant, room and format afresh: 1.2M showtimes on a
  // worker-us boot, each with its own LocalDateTime/LocalDate/LocalTime and format list (~100 MB of the boot peak).
  "a pool's showtimes" should "share their instants, rooms and formats, and keep everything else as read" in {
    val pool = new ShowtimePool
    def read(url: String) = Showtime(at, Some(url), Some(new String("Sala 1")), List(new String("2D")))
    val Seq(a) = pool.showtimes(Seq(read("https://book/a")))
    val Seq(b) = pool.showtimes(Seq(read("https://book/b")))
    assert(a.dateTime eq b.dateTime)
    assert(a.room eq b.room)
    assert(a.format eq b.format)
    a shouldBe read("https://book/a")
    b.bookingUrl shouldBe Some("https://book/b")
  }

  it should "hand back the showtimes it already shares as they are" in {
    val pool  = new ShowtimePool
    val first = pool.showtimes(Seq(Showtime(at, None)))
    val again = pool.showtimes(first)
    assert(again.head eq first.head)
  }

  // Each `screenings` row stores its URLs' shared prefix once and every showtime the rest; read back, each row's prefix
  // was a String of its own — ~100k on a US scan for a few thousand distinct venue booking pages.
  it should "share the URL prefix equal rows were stored with, keeping each showtime's whole URL" in {
    val pool = new ShowtimePool
    def row(session: String) = Showtime(at, Some(s"https://web.picturehouses.com/order/showtimes/$session"))
      .withUrlPrefix(new String("https://web.picturehouses.com/order/showtimes/"))
    val Seq(a) = pool.showtimes(Seq(row("021-98528/seats")))
    val Seq(b) = pool.showtimes(Seq(row("021-98558/seats")))
    assert(a.urlSplitPrefix.get eq b.urlSplitPrefix.get)
    a.bookingUrl shouldBe Some("https://web.picturehouses.com/order/showtimes/021-98528/seats")
    b.bookingUrl shouldBe Some("https://web.picturehouses.com/order/showtimes/021-98558/seats")
  }
}
