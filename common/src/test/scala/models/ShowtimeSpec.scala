package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * Locks the grace-window rule both the web's `toSchedules` list filter and the
 * worker's `kinowo_worker_movies_served` gauge count by, so the two can't drift on
 * the "just-started screening still counts" edge — and that a showtime behaves as the
 * same value whether its booking URL is held whole, as built, or split at a prefix its
 * row shares (`withUrlPrefix`).
 */
class ShowtimeSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 6, 8, 18, 0)
  private def at(t: LocalDateTime) = Showtime(t, bookingUrl = None)

  "isUpcoming" should "include a showtime in the future" in {
    at(now.plusHours(1)).isUpcoming(now) shouldBe true
  }

  it should "include a showtime that started within the 30-minute grace" in {
    at(now.minusMinutes(29)).isUpcoming(now) shouldBe true
  }

  it should "exclude a showtime past the grace window" in {
    at(now.minusMinutes(31)).isUpcoming(now) shouldBe false
  }

  it should "exclude a showtime exactly at the grace boundary (strictly after)" in {
    // dateTime.isAfter(now - 30min): the boundary itself is not after, so it drops.
    at(now.minus(Showtime.Grace)).isUpcoming(now) shouldBe false
  }

  private val start = LocalDateTime.of(2026, 6, 10, 18, 30)
  private val url   = "https://kino.example/buy?show=101"
  private val whole = Showtime(start, Some(url), Some("Sala 1"), List("2D"))
  private val split = whole.withUrlPrefix(new String("https://kino.example/"))

  "a showtime split at a prefix" should "spell the same URL, and equal and hash as the whole one" in {
    split.urlSplitPrefix shouldBe Some("https://kino.example/")
    split.bookingUrl shouldBe Some(url)
    split shouldBe whole
    whole shouldBe split
    split.hashCode shouldBe whole.hashCode
    split.toString shouldBe whole.toString
    split.toString shouldBe s"Showtime($start,Some($url),Some(Sala 1),List(2D))"
  }

  it should "differ from a showtime with another URL however each is held" in {
    split should not be Showtime(start, Some(url + "1"), Some("Sala 1"), List("2D"))
    split should not be whole.copy(bookingUrl = None)
    split should not be Showtime(start, Some("https://kino.example/buy?show=102"), Some("Sala 1"), List("2D")).withUrlPrefix("https://kino.example/")
  }

  it should "keep its split through a copy of another field, and drop it for a new URL" in {
    split.copy(room = None).urlSplitPrefix shouldBe Some("https://kino.example/")
    split.copy(room = None).bookingUrl shouldBe Some(url)
    split.copy(bookingUrl = Some("https://other.example/")).urlSplitPrefix shouldBe None
  }

  it should "round-trip and hash a URL outside ASCII" in {
    val accented = Showtime(start, Some("https://kino.example/bilety/łódź?s=1"))
    val held     = accented.withUrlPrefix("https://kino.example/")
    held.bookingUrl shouldBe accented.bookingUrl
    held.hashCode shouldBe accented.hashCode
  }

  "a URL that does not start with the prefix" should "stay whole" in {
    whole.withUrlPrefix("https://elsewhere.example/").urlSplitPrefix shouldBe None
    Showtime(start, None).withUrlPrefix("https://kino.example/").bookingUrl shouldBe None
  }

  "the prefix a group's URLs share" should "be empty for fewer than two URLs" in {
    Showtime.commonUrlPrefix(Seq(whole, Showtime(start, None))) shouldBe ""
    Showtime.commonUrlPrefix(Seq(whole, split, Showtime(start, Some("https://kino.example/buy?show=2")))) shouldBe "https://kino.example/buy?show="
  }

  it should "never end between the two halves of a surrogate pair" in {
    val urls = Seq("https://kino.example/\uD83D\uDE00a", "https://kino.example/\uD83D\uDE01b").map(u => Showtime(start, Some(u)))
    Showtime.commonUrlPrefix(urls) shouldBe "https://kino.example/"
    urls.map(_.withUrlPrefix(Showtime.commonUrlPrefix(urls))).flatMap(_.bookingUrl) shouldBe urls.flatMap(_.bookingUrl)
  }
}
