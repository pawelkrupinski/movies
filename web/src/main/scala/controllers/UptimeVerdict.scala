package controllers

/** One /uptime service's health verdict from its recent bar series ([[UptimeTriage]] sorts the page's sections by it):
 *  failing, gone upstream, no screenings, thin or healthy. */
private[controllers] object UptimeVerdict {
  sealed trait Health
  case object Failing extends Health
  // Red, but for a reason no one can act on: the venue's page is a 404. Split out
  // of Failing so the section that means "something broke today" stays readable.
  // Named `Missing`, not `Gone`, for the same reason `Healthy` is not `Ok`: Play's
  // `Results` already has both `Gone` and `NotFound` and a controller inherits
  // them. The section it feeds is still called "gone upstream".
  case object Missing extends Health
  case object Zero    extends Health
  // Green, but every recent scrape held no screening in the next 72h — a feed that
  // stopped carrying the programme and kept a stray far-off slot (Kino Polonez,
  // 2026-09-26). Least severe of the flagged verdicts.
  case object Thin    extends Health
  case object Healthy extends Health

  /** The verdict of a series' `statuses`: a failing one whose `errors` say the page is gone is [[Missing]]. Errors
   *  are read ONLY for a row that already classified as Failing — the question "is this a 404" is meaningless
   *  otherwise, and asking it for all 5,031 US services would undo the point of classifying over names alone. */
  def of(statuses: Seq[String], errors: => Seq[String]): Health = classify(statuses) match {
    case Failing if goneUpstream(errors) => Missing
    case other                           => other
  }

  /** Classify a bar series by its last `RecentScrapes` buckets that recorded any
   *  activity (ignoring untouched "empty" slots): all red ⇒ Failing, all white
   *  ⇒ Zero, all green-but-thin ⇒ Thin, anything else (any success, or no activity at all) ⇒ Healthy. A lone
   *  red blip among greens stays Healthy; a brand-new service that has only ever
   *  failed is Failing from its first bucket. Yellow (partial failure) is NOT
   *  failing. */
  private def classify(statuses: Seq[String]): Health = {
    val recent = statuses.iterator.filter(_ != "empty").toSeq.takeRight(UptimeTriage.RecentScrapes)
    if (recent.isEmpty) Healthy
    else if (recent.forall(_ == "red")) Failing
    else if (recent.forall(_ == "zero")) Zero
    else if (recent.forall(_ == "thin")) Thin
    else Healthy
  }

  /** Durable evidence in the window clearly dominates — nothing else broke, the
   *  venue simply isn't there. A LONE non-404 (a 500, a timeout, a WAF hiccup) is
   *  tolerated when durable errors outnumber it more than 2-to-1; anything short
   *  of that makes it an ordinary failure again, a venue whose page EXISTS and is
   *  misbehaving, which is worth acting on.
   *
   *  The tolerance exists because "every recorded error" used to mean literally
   *  every one: Kino Center's filmstarts.de page 404s on 8 of its last 9 recorded
   *  attempts, with one stray HTTP 503 (confirmed still 404 live) — and because
   *  the window is a sliding 3-bucket recency check, not a durable "since" marker,
   *  a single future 503 could always land inside it before the last one aged
   *  out, keeping a genuinely dead page in Failing forever. Two or more non-404s
   *  still describe a venue flickering between gone and merely broken, so those
   *  stay failing.
   *
   *  Recency, not duration, is the test here — the buckets only reach back a day.
   *  The worker asks the longer question against the scrape archive
   *  ([[services.scrapes.GoneUpstream]], "404ing for over a day") before it backs
   *  a venue's scrape cadence off; both sides share the one predicate that
   *  matters, `isNotFound`. */
  private def goneUpstream(errors: Seq[String]): Boolean = {
    val (durable, other) = errors.partition(services.scrapes.GoneUpstream.saysPageIsGone)
    other.sizeIs <= 1 && durable.size > other.size * 2
  }

  /** Worst-first, and `Missing` ranks BELOW Failing: a cinema whose page is gone
   *  but whose enrichment is also failing has something live to fix, so it belongs
   *  in Failing. */
  def worse(a: Health, b: Health): Health =
    if (a == Failing || b == Failing) Failing
    else if (a == Missing || b == Missing) Missing
    else if (a == Zero || b == Zero) Zero
    else if (a == Thin || b == Thin) Thin
    else Healthy
}
