package modules.timing

/** The time a composition root builds on, declared once for the web tier's and the worker's wirings: everything that
 *  asks "what time is it", "how long did that take" or waits takes one of these by constructor, and a test wiring
 *  overrides [[clock]] to pin the time. */
trait TimeSources {
  /** The one wall clock the wiring reads. */
  lazy val clock: java.time.Clock = java.time.Clock.systemUTC()
  /** The one monotonic source elapsed time is measured on — the clock's twin for durations, never a date. */
  lazy val stopwatch: tools.Stopwatch = tools.Stopwatch.System
  /** The one blocking sleep this wiring's waits and back-offs are built on — the stopwatch's twin for waiting. */
  lazy val sleep: scala.concurrent.duration.FiniteDuration => Unit = tools.Sleep.Blocking

  /** The latest year a title may name, read off [[clock]] at each ask: what every title reader the wiring builds (and
   *  the record accessors they reach, the debug pages' display titles among them) caps a year by. */
  given latestTitleYear: services.evidence.LatestTitleYear = services.evidence.LatestTitleYear(clock)
}
