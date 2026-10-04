package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClockStyleSpec extends AnyFlatSpec with Matchers {

  private def spelled(style: ClockStyle, hour: Int, minute: Int) = {
    val out = new java.lang.StringBuilder; style.appendTime(out, hour, minute); out.toString
  }

  "ClockStyle.TwelveHour" should "print midnight and noon as 12, with AM before noon and PM from it" in {
    spelled(ClockStyle.TwelveHour, 0, 5) shouldBe "12:05 AM"
    spelled(ClockStyle.TwelveHour, 9, 0) shouldBe "9:00 AM"
    spelled(ClockStyle.TwelveHour, 12, 30) shouldBe "12:30 PM"
    spelled(ClockStyle.TwelveHour, 23, 59) shouldBe "11:59 PM"
    ClockStyle.TwelveHour.hourLabel(0) shouldBe "12 AM"
    ClockStyle.TwelveHour.hourLabel(18) shouldBe "6 PM"
  }

  "ClockStyle.TwentyFourHour" should "print zero-padded hours and minutes" in {
    spelled(ClockStyle.TwentyFourHour, 0, 5) shouldBe "00:05"
    spelled(ClockStyle.TwentyFourHour, 19, 30) shouldBe "19:30"
    ClockStyle.TwentyFourHour.hourLabel(7) shouldBe "07"
  }

  "Country.clockStyle" should "be 12-hour in the US only" in {
    Country.all.filter(_.clockStyle == ClockStyle.TwelveHour) shouldBe Seq(Country.UnitedStates)
  }
}
