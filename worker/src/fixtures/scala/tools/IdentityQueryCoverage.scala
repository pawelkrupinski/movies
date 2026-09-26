package tools

import models.Country
import services.observations.LookupQuery
import services.scrapes.ArchivedScrape

import scala.collection.mutable

/**
 * The identity program's PHASE-1 GATE (docs/design/identity-resolver.md, "Phase 1"): what
 * fraction of the resolver's query set the recorded answers can serve, per corpus.
 *
 * The query set is [[IdentityLookupSweep]]'s — every listing's own detail page and the TMDB
 * resolve of every distinct evidence, a function of the listing set alone — and it is the same
 * set a recording leg run with `KINOWO_IDENTITY_LOOKUPS=true` records, so a gap reported here
 * is exactly what that recording fills.
 *
 * A logical lookup is ANSWERABLE when every HTTP request it made was served by the recording (a
 * remembered 404 counts: it is the service's answer). One unserved request makes the whole
 * lookup unanswerable, because the replay then answers it with something the service never
 * said. The gate is met at 100% on every corpus.
 */
object IdentityQueryCoverage {

  /** One HTTP request, under both names the recordings file it by, and whether what came back
   *  was a failed READ (a timeout, a 5xx, a 403, a 429 — remembered or not) rather than an answer
   *  about the request. */
  final case class Request(query: String, fixtureKey: String, host: String, failedRead: Boolean = false)

  final case class Coverage(label: String, lookups: Int, answerableLookups: Int, requests: Int, answeredRequests: Int,
                            gaps: Seq[Request], served: Seq[Request]) {
    /** Served, but only by a remembered failed read: replayable, and no evidence. */
    def failedReads: Seq[Request] = served.filter(_.failedRead)

    def lookupShare: Double  = if (lookups == 0) 1.0 else answerableLookups.toDouble / lookups
    def requestShare: Double = if (requests == 0) 1.0 else answeredRequests.toDouble / requests
    def met: Boolean         = gaps.isEmpty

    def gapsByHost: Seq[(String, Int)] = gaps.groupBy(_.host).view.mapValues(_.size).toSeq.sortBy { case (h, n) => (-n, h) }

    def line: String =
      f"$label%-8s lookups $answerableLookups%,d/$lookups%,d answerable (${lookupShare * 100}%.2f%%), " +
        f"requests $answeredRequests%,d/$requests%,d served (${requestShare * 100}%.2f%%" +
        (if (failedReads.isEmpty) ")" else s", ${failedReads.size} of them by a remembered failed read: " +
          failedReads.groupBy(_.host).view.mapValues(_.size).toSeq.sortBy(-_._2).map { case (h, n) => s"$h $n" }.mkString(", ") + ")") +
        (if (met) " — GATE MET" else s" — ${gaps.size} gap(s): " +
          gapsByHost.map { case (h, n) => s"$h $n" }.mkString(", ") + "; e.g. " + gaps.take(3).map(_.query).mkString(" | "))
  }

  /** Coverage from the requests each lookup made (distinct requests, the order the sweep issued
   *  them) and which requests the recording could not serve. */
  def of(label: String, byLookup: Seq[(String, Seq[Request])], missed: Request => Boolean): Coverage = {
    val requests = byLookup.flatMap(_._2).distinctBy(_.query)
    val gaps     = requests.filter(missed)
    val gapKeys  = gaps.map(_.query).toSet
    Coverage(label, byLookup.size, byLookup.count(_._2.forall(r => !gapKeys(r.query))),
      requests.size, requests.size - gaps.size, gaps, requests.filterNot(missed))
  }

  /** Every request through `inner`, filed under the lookup running when it was made. */
  final class RequestLog(inner: HttpFetch) extends HttpFetch {
    private val pending  = mutable.ArrayBuffer.empty[Request]
    private val finished = mutable.ArrayBuffer.empty[(String, Seq[Request])]

    private def note[A](method: String, url: String, body: Option[String])(call: => A): A = {
      val query   = LookupQuery.of(method, url, body)
      val outcome = scala.util.Try(call)
      val failed  = outcome.failed.toOption.exists(e => !services.observations.LookupAnswer.failureOf(e, method).definitive)
      synchronized {
        pending += Request(query.key, clients.tools.RecordingHttpFetch.fixtureKey(url, body, foldYear = false), query.host, failed)
      }
      outcome.get
    }

    /** Close the running lookup: every request since the last cut is its. */
    def cut(lookup: String): Unit = synchronized { finished += lookup -> pending.toSeq; pending.clear() }
    def byLookup: Seq[(String, Seq[Request])] = synchronized(finished.toSeq)

    override def get(url: String): String = note("GET", url, None)(inner.get(url))
    override def get(url: String, headers: Map[String, String]): String = note("GET", url, None)(inner.get(url, headers))
    override def getBytes(url: String): Array[Byte] = note("BYTES", url, None)(inner.getBytes(url))
    override def post(url: String, body: String, contentType: String): String =
      note("POST", url, Some(body))(inner.post(url, body, contentType))
  }

  /** Run the resolver's query set over `rows` against `fetch` — the recording under test — and
   *  measure it. `missed` is asked AFTER the sweep, so a recording that learns its gaps as it
   *  goes reports all of them. */
  def measure(label: String, country: Country, storage: ConvergenceStorage, rows: Seq[ArchivedScrape],
              fetch: HttpFetch, missed: () => Request => Boolean): (Coverage, IdentityLookupSweep.Summary) = {
    val log     = new RequestLog(fetch)
    val wiring  = FetchReplayWiring(country, storage, rows, log)
    val summary = IdentityLookupSweep.over(wiring, country, log.cut)
    (of(label, log.byLookup, missed()), summary)
  }
}
