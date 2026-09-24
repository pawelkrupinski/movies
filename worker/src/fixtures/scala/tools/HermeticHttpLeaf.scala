package tools

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * Every request a HERMETIC convergence run wanted and the recorded tree could not answer.
 *
 * One per run, shared by every wiring in it (the boot and each order-independence pass),
 * so the suite can refuse the run once, naming every gap, rather than once per pass.
 * Keyed by the fixture file the recorder would have written, which is the name somebody
 * fixing the gap needs: it is what `RecordingHttpFetch` writes and `FakeHttpFetch` reads.
 */
final class MissingFixtures {
  private val missing = new ConcurrentHashMap[String, String]()

  /** Remember a gap: the fixture key, and the (credential-masked) request that wanted it. */
  def record(fixtureKey: String, request: String): Unit = { missing.putIfAbsent(fixtureKey, request); () }

  def isEmpty: Boolean = missing.isEmpty
  def size: Int        = missing.size

  /** The gaps, sorted so two runs over the same tree report them in the same order. */
  def keys: Seq[(String, String)] = missing.asScala.toSeq.sortBy(_._1)

  /** What the suite fails with: how many, the first `limit` of them by fixture key, and
   *  how to close the gap. Recording is the NIGHTLY RECORDER's job, never a hand edit —
   *  a hand-written fixture is exactly the drift the tree exists to prevent. */
  def report(tree: String, limit: Int = 25): String = {
    val shown = keys.take(limit).map { case (key, request) => s"  $tree/$key   <- $request" }
    val more  = if (size > limit) Seq(s"  … and ${size - limit} more") else Nil
    (Seq(s"HERMETIC run needed $size request(s) the recorded fixture tree does not hold — no live fill " +
         "was attempted:") ++ shown ++ more ++ Seq(
      "The tree and the corpus are a PAIR recorded together by `Record scrape fixtures` (its `enrichment` " +
      "jobs). Re-run that workflow to re-record them; never hand-write a fixture.")).mkString("\n")
  }
}

/**
 * Thrown instead of reaching the network in a hermetic run. Deliberately NOT an
 * `HttpStatusException`: no status came back, so no caller may read it as a verdict about
 * the URL, and `EnrichmentCache` treats it as transient — it is never persisted.
 */
final class MissingFixtureException(val fixtureKey: String, request: String)
  extends java.io.IOException(s"HERMETIC: no recorded fixture $fixtureKey for $request — live fill refused")

/**
 * The WIRE, replaced. What sits at the very bottom of both phase chains
 * (`HttpWiring.realHttpLeaf`) in a hermetic convergence run.
 *
 * Replacing the leaf rather than any layer above it is the point: every client, every
 * throttle, breaker, cache and fixture layer stays exactly as production and the recording
 * run build them, and the ONLY thing that changes is that a request which falls through
 * all of them — the one that would have gone to TMDB, IMDb, Cinemeta, Metacritic or a
 * cinema's detail page — is refused and named instead of fetched. Anything the recorded
 * tree or the remembered verdicts answer never gets here, so a complete recording makes
 * no call to this at all.
 *
 * Why the convergence legs needed it: they used to fill whatever the tree lacked live, so
 * a leg's verdict depended on Cinemeta answering 504, Metacritic timing out, or a UK boot
 * making 345 live fills of which 63 failed — flakes in a suite whose one claim is that the
 * same inputs produce the same outputs.
 */
final class HermeticHttpLeaf(missing: MissingFixtures) extends HttpFetch {

  override def get(url: String): String                      = refuse(url, body = None)
  override def getBytes(url: String): Array[Byte]            = refuse(url, body = None)
  override def get(url: String, headers: Map[String, String]): String = refuse(url, body = None)
  override def post(url: String, body: String, contentType: String): String = refuse(url, Some(body))

  /** `foldYear = false`: the convergence trees are recorded that way on both chains (see
   *  `ArchiveReplayWiring`), so this is the file the replay looked for and did not find. */
  private def refuse(url: String, body: Option[String]): Nothing = {
    val key     = clients.tools.RecordingHttpFetch.fixtureKey(url, body, foldYear = false)
    val request = s"${if (body.isDefined) "POST" else "GET"} ${RedactedUrl(url)}"
    missing.record(key, request)
    throw new MissingFixtureException(key, request)
  }
}
