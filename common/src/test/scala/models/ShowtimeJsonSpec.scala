package models

import java.time.LocalDateTime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

/** The JSON `Json.format[Showtime]` wrote and read while `Showtime` was a case class — captured
 *  from that macro on 2026-10-03 — which the chunk store, the corpus fixture and the read-model
 *  snapshot all hold. */
class ShowtimeJsonSpec extends AnyFlatSpec with Matchers {

  private implicit val dateTimes: Format[LocalDateTime] =
    Format(Reads.DefaultLocalDateTimeReads, Writes.DefaultLocalDateTimeWrites)
  private val format = ShowtimeJson.format
  private val at     = LocalDateTime.of(2026, 12, 17, 13, 0)

  "a showtime" should "write the case-class macro's JSON" in {
    Json.stringify(format.writes(Showtime(at, None))) shouldBe """{"dateTime":"2026-12-17T13:00:00","format":[]}"""
    Json.stringify(format.writes(Showtime(at, Some("u"), Some("r"), List("2D", "NAP")))) shouldBe
      """{"dateTime":"2026-12-17T13:00:00","bookingUrl":"u","room":"r","format":["2D","NAP"]}"""
    Json.stringify(format.writes(Showtime(at, Some("https://k.example/1")).withUrlPrefix("https://k.example/"))) shouldBe
      """{"dateTime":"2026-12-17T13:00:00","bookingUrl":"https://k.example/1","format":[]}"""
  }

  it should "read as the macro read" in {
    format.reads(Json.parse("""{"dateTime":"2026-12-17T13:00:00"}""")) shouldBe JsSuccess(Showtime(at, None))
    format.reads(Json.parse("""{"dateTime":"2026-12-17T13:00:00","format":["A"],"bookingUrl":null}""")) shouldBe
      JsSuccess(Showtime(at, None, None, List("A")))
    format.reads(Json.parse("""{"bookingUrl":"u"}""")).isError shouldBe true
  }
}
