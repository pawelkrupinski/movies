package controllers

import java.time.LocalDate

object DateFormatter {

  private val polishDays = Vector(
    "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"
  )

  private val polishMonths = Vector(
    "", "stycznia", "lutego", "marca", "kwietnia", "maja", "czerwca",
    "lipca", "sierpnia", "września", "października", "listopada", "grudnia"
  )

  def format(date: LocalDate): String = {
    val dayName     = polishDays(date.getDayOfWeek.getValue - 1).capitalize
    val monthName   = polishMonths(date.getMonthValue)
    val currentYear = LocalDate.now().getYear
    val yearSuffix  = if (date.getYear == currentYear) "" else s" ${date.getYear}"
    s"$dayName ${date.getDayOfMonth} $monthName$yearSuffix"
  }
}
