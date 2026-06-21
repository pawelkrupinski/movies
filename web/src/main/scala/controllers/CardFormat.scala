package controllers

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

/** Stateless display formatters shared by the serving-side Scala controllers
 *  and the Twirl templates (a plain Scala object is callable from both). These
 *  were duplicated across [[MovieController]], [[PlanController]], and several
 *  `_*.scala.html` partials — centralised here so the rendered strings stay in
 *  lock-step (and snapshot-stable) wherever a time / date / runtime is shown.
 */
object CardFormat {

  private val TimeFmt = DateTimeFormatter.ofPattern("HH:mm")

  /** A showtime clock as `HH:mm` (e.g. "17:30"). Used by the JSON API payloads
   *  and the OG-card showtime chips. */
  def time(dateTime: LocalDateTime): String = dateTime.format(TimeFmt)

  /** A long Polish date label ("Czwartek 4 czerwca", year appended only when it
   *  isn't the current year). Shown on the listing date headers, the JSON API's
   *  day labels, and the city OG card. */
  def date(d: LocalDate): String = DateFormatter.format(d)

  /** The compact runtime pill text the card / detail templates render between
   *  `<span class="pill runtime">…</span>` — "2h" when the minutes part is zero,
   *  otherwise "1h 50min" (matching the inline Twirl conditional this replaced,
   *  whose `@if` block carries the leading space). Callers guard on `> 0`. */
  def runtimePill(mins: Int): String =
    s"${mins / 60}h" + (if (mins % 60 != 0) s" ${mins % 60}min" else "")

  /** The spaced runtime text ("0h 55min" / "1h 42min") the OG city card draws —
   *  always shows the minutes part, distinct from [[runtimePill]] only at a whole
   *  hour, where this keeps the "2h 0min" form. */
  def runtimeText(mins: Int): String = s"${mins / 60}h ${mins % 60}min"
}
