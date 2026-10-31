package services.identity
package adapters

/** What set each identity projection off, as its `trigger` names it in its log lines and take changes
 *  ([[IdentityProjection]]): [[asked]] notes what asks for the next, [[next]] names the one starting. */
private[identity] final class ProjectionTriggers {

  /** What asked for a projection since the last one began ([[asked]]): drained by the next. */
  private val askedFor = new java.util.concurrent.atomic.AtomicReference(Set.empty[IdentityProjection.Trigger])
  /** Whether this process has projected yet: its first projection is a deploy's (or a pod restart's). */
  private var projected = false

  /** Note what asks for the projection the caller is about to request. */
  def asked(trigger: IdentityProjection.Trigger): Unit = { askedFor.updateAndGet(_ + trigger); () }

  /** What set this projection off: `deploy` for the process's first, `periodic` for the hourly reconcile or another tick of
   *  the period, else what [[asked]] for it since the last (`scrape`, `answer`, `film-change`, several joined by `+`), or
   *  `retry` for an unsettled projection run again with nothing new asked. */
  def next(mode: IdentityProjection.Mode): String = {
    val first = !projected
    projected = true
    val asked = askedFor.getAndSet(Set.empty)
    if (first) "deploy"
    else if (mode != IdentityProjection.Mode.Changed) "periodic"
    else if (asked.isEmpty) "retry"
    else asked.toSeq.map(_.label).sorted.mkString("+")
  }
}
