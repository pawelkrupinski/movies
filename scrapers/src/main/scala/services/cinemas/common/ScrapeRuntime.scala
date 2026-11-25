package services.cinemas.common


/** Runtimes as the cinema pages spell them, in minutes. */
private[cinemas] object ScrapeRuntime {

  private val RuntimeHours   = """(?i)(\d+)\s*(?:godz|h\b)""".r
  private val RuntimeMinutes = """(?i)(\d+)\s*m(?:in|\b)""".r

  /** A runtime's minutes from the hour and minute counts a pattern captured, either group possibly unmatched (`null`):
   *  "PT1H30M"'s `1` and `30` → 90, "95min"'s `null` and `95` → 95. */
  def minutesOf(hours: String, minutes: String): Int =
    Option(hours).map(_.toInt * 60).getOrElse(0) + Option(minutes).map(_.toInt).getOrElse(0)

  /** A runtime spelled in hours and minutes — "1 godz. 53 min", "3 godz 03 min",
   *  "95 min", "ok. 85 MIN", "1h 50m" — in minutes; `None` when neither part is present. */
  def hoursMinutesRuntime(s: String): Option[Int] = {
    val hours   = RuntimeHours.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    val minutes = RuntimeMinutes.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    Some(hours * 60 + minutes).filter(_ > 0)
  }

  private val MinutesCount = """(\d+)\s*min""".r

  /** The first "<n> min" in `s` — "119 min.", "45min", "Czas: 90 minut" — as its minutes; `None` when there is none.
   *  Case-sensitive, as every client reading it was: "MIN" is no match. */
  def runtimeMinutesIn(s: String): Option[Int] = MinutesCount.findFirstMatchIn(s).flatMap(_.group(1).toIntOption)

  private val DigitRun = """(\d+)""".r

  /** The first run of digits in `s` as a number: a runtime field's minutes ("120 min"). */
  def firstNumberIn(s: String): Option[Int] = DigitRun.findFirstMatchIn(s).map(_.group(1).toInt)
}
