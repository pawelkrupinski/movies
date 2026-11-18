package tools

/** What a failure says, as text that is never null: its message, or -- an exception's message is often null (a bare
 *  `IOException`, an NPE, some driver errors) or empty -- its class name. Whatever a failure is recorded as or read
 *  for (the scrape archive's barren row, a task's last error, whether the page answered gone) starts from this, so no
 *  reader meets a null: the chunked planner ran its "page gone" regex over a null message and threw out of its own
 *  failure path (worker-uk 2026-11-12..17, Sentry KINOWO-34). */
object FailureText {
  def of(failure: Throwable): String = Option(failure.getMessage).filter(_.nonEmpty).getOrElse(failure.getClass.getName)
}
