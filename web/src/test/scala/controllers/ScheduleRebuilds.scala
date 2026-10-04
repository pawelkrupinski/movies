package controllers

/** How many of a repeated render's schedules the schedule cache built again rather than reused: the
 *  cache hands back the SAME object for a film nothing moved in, so a schedule not among the first
 *  render's objects is a rebuild. What `PerformanceBudgets.ScheduleRebuildsOnRepeatRender` counts. */
object ScheduleRebuilds {
  def between(first: Seq[FilmSchedule], again: Seq[FilmSchedule]): Long = {
    val held = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[FilmSchedule, java.lang.Boolean])
    first.foreach(held.add)
    again.count(s => !held.contains(s)).toLong
  }
}
