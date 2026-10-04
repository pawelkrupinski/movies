package models

/**
 * How a country's visitors read a clock time: "19:30" in Poland, the UK, Germany and Spain,
 * "7:30 PM" in the US. The pills on the web print it, and the listing's script reads the
 * pill's own text back as its time — `slotTime` in `_repertoireView` accepts both spellings.
 * Stored and wire times stay 24-hour `LocalDateTime`s; this is display only.
 */
sealed abstract class ClockStyle {
  /** `hour:minute` as this style spells it, into `out` without allocating a string. */
  def appendTime(out: java.lang.StringBuilder, hour: Int, minute: Int): Unit

  /** A whole hour as this style spells it ("19" / "7 PM") — the "from hour" picker's labels. */
  def hourLabel(hour: Int): String

  protected final def appendTwoDigits(out: java.lang.StringBuilder, n: Int): Unit =
    out.append((n / 10 + '0').toChar).append((n % 10 + '0').toChar)
}

object ClockStyle {
  case object TwentyFourHour extends ClockStyle {
    def appendTime(out: java.lang.StringBuilder, hour: Int, minute: Int): Unit = {
      appendTwoDigits(out, hour); out.append(':'); appendTwoDigits(out, minute)
    }
    def hourLabel(hour: Int): String = f"$hour%02d"
  }

  /** "12:05 AM" just after midnight, "12:30 PM" just after noon, as US listings print them. */
  case object TwelveHour extends ClockStyle {
    def appendTime(out: java.lang.StringBuilder, hour: Int, minute: Int): Unit = {
      out.append(clockHour(hour)).append(':'); appendTwoDigits(out, minute); out.append(meridiem(hour))
    }
    def hourLabel(hour: Int): String = s"${clockHour(hour)}${meridiem(hour)}"
    private def clockHour(hour: Int): Int = if (hour % 12 == 0) 12 else hour % 12
    private def meridiem(hour: Int): String = if (hour < 12) " AM" else " PM"
  }
}
