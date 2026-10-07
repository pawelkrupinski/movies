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
  private val missing   = new ConcurrentHashMap[String, String]()
  private val refetches = new ConcurrentHashMap[String, MissingFixtures.Refetch]()

  /** Remember a gap: the fixture key, and the (credential-masked) request that wanted it. */
  def record(fixtureKey: String, request: String): Unit = { missing.putIfAbsent(fixtureKey, request); () }

  /** Remember a gap a later run can fetch ([[MissingFixtures.Refetch]]): one whose whole request is its URL,
   *  any credential in it masked. */
  def record(fixtureKey: String, request: String, refetch: MissingFixtures.Refetch): Unit = {
    record(fixtureKey, request)
    refetches.putIfAbsent(fixtureKey, refetch); ()
  }

  def isEmpty: Boolean = missing.isEmpty
  def size: Int        = missing.size

  /** The gaps, sorted so two runs over the same tree report them in the same order. */
  def keys: Seq[(String, String)] = missing.asScala.toSeq.sortBy(_._1)

  /** What the suite fails with: how many, by host, the first `limit` of them by fixture key, and
   *  how to close the gap. Recording is the NIGHTLY RECORDER's job, never a hand edit —
   *  a hand-written fixture is exactly the drift the tree exists to prevent. */
  def report(tree: String, limit: Int = 25): String = {
    val shown  = keys.take(limit).map { case (key, request) => s"  $tree/$key   <- $request" }
    val more   = if (size > limit) Seq(s"  … and ${size - limit} more") else Nil
    val byHost = keys.groupMapReduce { case (key, _) => key.takeWhile(_ != '/') }(_ => 1)(_ + _)
      .toSeq.sortBy { case (host, count) => (-count, host) }.map { case (host, count) => s"$host $count" }
    (Seq(s"HERMETIC run needed $size request(s) the recorded fixture tree does not hold — no live fill " +
         "was attempted:", s"  by host: ${byHost.mkString(", ")}") ++ shown ++ more ++ Seq(
      "The tree and the corpus are a PAIR recorded together by `Record scrape fixtures` (its `enrichment` " +
      "jobs). Re-run that workflow to re-record them; never hand-write a fixture. Each hermetic leg on main also " +
      "fetches the GETs itself (`FillMissingFixtures`, TMDB's signed with the lane's key) for the legs after it.")).mkString("\n")
  }

  /** Every gap a later run can fetch on its own, one `<fixture key>\t<verb>\t<url>` line each, sorted —
   *  what the next leg's rows fetch before their suites (`FillMissingFixtures`). Written even when empty: an empty
   *  list says this leg missed nothing fillable, which an absent one cannot. */
  def writeRefetches(file: java.nio.file.Path): Int = {
    val lines = refetches.asScala.toSeq.sortBy(_._1).map { case (key, r) => MissingFixtures.Refetch.line(key, r) }
    // A header, so even an empty list is a non-empty file: a release refuses a zero-byte asset (HTTP 400,
    // run 37608911385). `Refetch.parse` reads past it.
    AtomicFiles.writeString(file, (s"# ${lines.size} fetchable gap(s)" +: lines).map(_ + "\n").mkString)
    lines.size
  }
}

object MissingFixtures {

  /** A gap's request, whole: the verb the pipeline asked it with (`GET`, or `BYTES` for a raw-bytes
   *  read, which the remembered verdicts key apart) and its URL with every credential masked
   *  ([[RedactedUrl]]) — so it can be named in a public release asset, and fetched by a process that signs
   *  it again with a key of its own ([[FillCredentials]]), or as it is when it never held one. */
  final case class Refetch(verb: String, url: String, body: Option[Refetch.Body] = None)

  object Refetch {
    val Verbs: Set[String] = Set("GET", "BYTES", "POST")

    /** A POST's body and its content type — what the fill sends again. */
    final case class Body(contentType: String, text: String)

    /** One line of the list: `<fixture key>\t<verb>\t<url>`, and for a POST `\t<content type>\t<body, base64>` — a body
     *  can hold tabs and newlines. */
    def line(key: String, r: Refetch): String =
      (Seq(key, r.verb, r.url) ++ r.body.toSeq.flatMap(b =>
        Seq(b.contentType, java.util.Base64.getEncoder.encodeToString(b.text.getBytes(java.nio.charset.StandardCharsets.UTF_8))))).mkString("\t")

    /** One line of [[MissingFixtures.writeRefetches]], or None for a line that is not one. */
    def parse(line: String): Option[(String, Refetch)] = line.split('\t') match {
      case Array(key, verb, url) if Verbs(verb) && verb != "POST" && url.startsWith("http") => Some(key -> Refetch(verb, url))
      case Array(key, "POST", url, contentType, body) if url.startsWith("http") =>
        scala.util.Try(new String(java.util.Base64.getDecoder.decode(body), java.nio.charset.StandardCharsets.UTF_8)).toOption
          .map(text => key -> Refetch("POST", url, Some(Body(contentType, text))))
      case _ => None
    }
  }

  /** What answering a remembered failure says about the tree: a gap, when the recording remembered only a failure — a
   *  circuit it opened on itself, a 503, a timeout, or a 403 refusing the recording's address — for no fill could close
   *  it otherwise: the replay answers it before the leaf, so the leaf never names it (Poland's Wikidata lookups, "circuit
   *  open" since the recording that hit its own fleet pace, run 37661888315; Cineworld's 403s, which the fill asks again
   *  through the residential proxy, `MissingFixtureFill.route`). Not a definite answer (a 404), and not a gap this run
   *  already named at the leaf. Listed as the leaf lists: credentials masked, a POST whole only when its body names none. */
  def listingPassingFailures(missing: MissingFixtures): CachingEnrichmentFetch.Replayed => Unit = replayed =>
    if (!replayed.failed.definitive && !replayed.failed.message.startsWith(classOf[MissingFixtureException].getName)) {
      val redacted = RedactedUrl(replayed.url)
      if (replayed.body.forall { case (text, _) => redacted == replayed.url && !RedactedUrl.carriesCredential(text) }) {
        val key = clients.tools.RecordingHttpFetch.fixtureKey(replayed.url, replayed.body.map(_._1), foldYear = false)
        missing.record(key, s"${if (replayed.body.isDefined) "POST" else "GET"} $redacted (remembered: ${replayed.failed.message.take(120)})",
          Refetch(replayed.verb, redacted, replayed.body.map { case (text, contentType) => Refetch.Body(contentType, text) }))
      }
    }

  /** Where a hermetic leg leaves its [[writeRefetches]] list: beside the tree it replayed, never in it
   *  (`enrichment-us` → `enrichment-us.refetch.tsv`), so no pack of the tree can carry it. */
  def refetchListBeside(tree: java.nio.file.Path): java.nio.file.Path =
    tree.resolveSibling(s"${tree.getFileName}.refetch.tsv")
}

/**
 * Thrown instead of reaching the network in a hermetic run. Deliberately NOT an
 * `HttpStatusException`: no status came back, so no caller may read it as a verdict about
 * the URL, and `EnrichmentCache` treats it as transient — it is never persisted. [[NeverSent]],
 * so a host's breaker never opens on refusals: open, it answered every later request to that
 * host itself, and this leaf — the one place a gap is named — never saw them.
 */
final class MissingFixtureException(val fixtureKey: String, request: String)
  extends java.io.IOException(s"HERMETIC: no recorded fixture $fixtureKey for $request — live fill refused") with NeverSent

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

  override def get(url: String): String                      = refuse(url, body = None, refetchAs = Some("GET"))
  override def getBytes(url: String): Array[Byte]            = refuse(url, body = None, refetchAs = Some("BYTES"))
  // A header request is listed only when its URL carries a credential (TMDB's: `api_key` beside the bearer header
  // holding the same key), which the fill signs again — headers are never listed, and one whose credential is only in
  // a header could not be asked by the fill at all. A body is never listed.
  override def get(url: String, headers: Map[String, String]): String =
    refuse(url, body = None, refetchAs = Option.when(RedactedUrl(url) != url)("GET"))
  // A POST is listed whole — body and content type — when neither its URL nor its body carries a credential: IMDb's
  // GraphQL, a query naming a title, and most of a US or UK leg's gaps (run 37656742608).
  override def post(url: String, body: String, contentType: String): String =
    refuse(url, Some(body), refetchAs = Option.when(RedactedUrl(url) == url && !RedactedUrl.carriesCredential(body))("POST"),
      contentType = contentType)

  /** `foldYear = false`: the convergence trees are recorded that way on both chains (see
   *  `ArchiveReplayWiring`), so this is the file the replay looked for and did not find. */
  private def refuse(url: String, body: Option[String], refetchAs: Option[String], contentType: String = ""): Nothing = {
    val key      = clients.tools.RecordingHttpFetch.fixtureKey(url, body, foldYear = false)
    val redacted = RedactedUrl(url)
    val request  = s"${if (body.isDefined) "POST" else "GET"} $redacted"
    // Listed with every credential masked: the list is a public release asset, and the fill signs it again.
    refetchAs match {
      case Some(verb) => missing.record(key, request, MissingFixtures.Refetch(verb, redacted, body.map(MissingFixtures.Refetch.Body(contentType, _))))
      case None       => missing.record(key, request)
    }
    throw new MissingFixtureException(key, request)
  }
}
