package tools

import play.api.libs.json._

import java.io.FileOutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Every HTTP answer a small replay needs, in ONE checked-in file — for the hard-cluster
 * convergence spec, whose clusters need a few hundred TMDB / IMDb / detail-page answers
 * out of trees that are 0.3-2.4 GB per country and live in a release asset.
 *
 * Keyed on the request exactly as [[CachingEnrichmentFetch]] keys it (method, the
 * credential-redacted URL, the POST body's hash), and holding FAILURES as well as
 * bodies: a remembered 404 is as much a part of what a pass sees as a 200 is, and a
 * replay that turned it into something else would resolve differently from the run it
 * was recorded from.
 *
 * Two modes, one class:
 *
 *  - REPLAY (`source = None`): an answer is served from the file; a request it does not
 *    hold is a 404, counted in [[misses]] so a stale file is visible without failing a
 *    spec whose change merely asked one new URL.
 *  - RECORD (`source = Some(tree)`): a request the file does not hold is asked of the
 *    source, and whatever it answered — body or failure — is kept for [[write]].
 *
 * Hosts in `refused` are answered 404 in both modes and never recorded. Those are the
 * rating sites (Metacritic, Rotten Tomatoes, Letterboxd, Wikidata) whose pages are
 * hundreds of kilobytes each and which decide no film's IDENTITY — keeping them out is
 * what keeps the file small.
 */
final class RecordedResponses private (
  entries: ConcurrentHashMap[String, RecordedResponses.Entry],
  source:  Option[HttpFetch],
  refused: Set[String]
) extends HttpFetch {
  import RecordedResponses._

  private val missCount = new AtomicInteger(0)
  private val missed    = ConcurrentHashMap.newKeySet[String]()

  /** Requests the file did not hold, answered 404. Non-zero means the fixture is stale
   *  against the code — re-record it (see `scripts/hard-clusters.sh`). */
  def misses: Int = missCount.get
  def missedKeys: Seq[String] = missed.asScala.toSeq.sorted

  def size: Int = entries.size

  override def get(url: String): String = new String(answer("GET", url, None, _.get(url)), StandardCharsets.UTF_8)
  override def get(url: String, headers: Map[String, String]): String =
    new String(answer("GET", url, None, _.get(url, headers)), StandardCharsets.UTF_8)
  override def getBytes(url: String): Array[Byte] = answer("BYTES", url, None, _.getBytes(url), binary = true)
  override def post(url: String, body: String, contentType: String): String =
    new String(answer("POST", url, Some(body), _.post(url, body, contentType)), StandardCharsets.UTF_8)

  private def answer(method: String, url: String, body: Option[String],
                     ask: HttpFetch => Any, binary: Boolean = false): Array[Byte] = {
    val host = Option(new URI(url).getHost).getOrElse("")
    if (refused.exists(r => host == r || host.endsWith("." + r)))
      throw new HttpStatusException(404, method, url, None)
    val key = CachingEnrichmentFetch.keyOf(method, url, body)
    val entry = Option(entries.get(key)).getOrElse {
      source match {
        case None =>
          missCount.incrementAndGet(); missed.add(key)
          throw new HttpStatusException(404, method, url, None)
        case Some(s) =>
          val recorded =
            try ask(s) match {
              case bytes: Array[Byte] => Entry(bytes = Some(bytes), binary = true)
              case text: String       => Entry(bytes = Some(text.getBytes(StandardCharsets.UTF_8)), binary = binary)
              case other              => sys.error(s"unexpected answer $other")
            } catch {
              case status: HttpStatusException => Entry(status = Some(status.code))
              case NonFatal(other)             => Entry(failure = Some(other.getClass.getSimpleName))
            }
          entries.putIfAbsent(key, recorded)
          entries.get(key)
      }
    }
    entry match {
      case Entry(Some(bytes), _, _, _)   => bytes
      case Entry(_, Some(code), _, _)    => throw new RememberedHttpStatusException(code, method, url)
      case Entry(_, _, Some(failure), _) => throw new CachedEnrichmentFailure(s"$failure (recorded) for $url")
      case _                             => throw new HttpStatusException(404, method, url, None)
    }
  }

  /** Write every answer held, sorted by key so the file is a pure function of them. */
  def write(path: Path): Path = {
    Files.createDirectories(path.getParent)
    val json = JsObject(entries.asScala.toSeq.sortBy(_._1).map { case (k, e) => k -> e.toJson })
    val out  = new GZIPOutputStream(new FileOutputStream(path.toFile))
    try out.write(Json.stringify(json).getBytes(StandardCharsets.UTF_8)) finally out.close()
    path
  }
}

object RecordedResponses {

  /** Rating pages: large, and irrelevant to which film a listing is. */
  val RatingHosts: Set[String] = Set("metacritic.com", "rottentomatoes.com", "letterboxd.com", "wikidata.org")

  final case class Entry(bytes: Option[Array[Byte]] = None, status: Option[Int] = None,
                         failure: Option[String] = None, binary: Boolean = false) {
    def toJson: JsObject = (bytes, status, failure) match {
      case (Some(b), _, _) if binary => Json.obj("b64" -> Base64.getEncoder.encodeToString(b))
      case (Some(b), _, _)           => Json.obj("text" -> new String(b, StandardCharsets.UTF_8))
      case (_, Some(code), _)        => Json.obj("status" -> code)
      case (_, _, Some(f))           => Json.obj("failure" -> f)
      case _                         => Json.obj("status" -> 404)
    }
  }

  private def entryOf(js: JsValue): Entry =
    (js \ "text").asOpt[String].map(t => Entry(bytes = Some(t.getBytes(StandardCharsets.UTF_8))))
      .orElse((js \ "b64").asOpt[String].map(b => Entry(bytes = Some(Base64.getDecoder.decode(b)), binary = true)))
      .orElse((js \ "status").asOpt[Int].map(c => Entry(status = Some(c))))
      .getOrElse(Entry(failure = (js \ "failure").asOpt[String]))

  def pathFor(countryCode: String): Path =
    Paths.get("test", "resources", "fixtures", "corpus", s"hard-clusters-responses-${countryCode.toLowerCase}.json.gz")

  private def load(path: Path): ConcurrentHashMap[String, Entry] = {
    val map = new ConcurrentHashMap[String, Entry]()
    if (Files.exists(path)) {
      val in = new GZIPInputStream(Files.newInputStream(path))
      val text = try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
      Json.parse(text).as[JsObject].fields.foreach { case (k, v) => map.put(k, entryOf(v)) }
    }
    map
  }

  /** Serve the checked-in file, nothing else. */
  def replaying(path: Path, refused: Set[String] = RatingHosts): RecordedResponses = {
    // A replay of a file that is not there answers every request 404 — every film unresolved,
    // every pass agreeing with every other, a green spec that replayed nothing. Say so instead.
    require(Files.exists(path), s"no recorded responses at ${path.toAbsolutePath} — record them first")
    new RecordedResponses(load(path), None, refused)
  }

  /** Ask `source` for what `prior` (the file as checked in) does not already hold, and
   *  keep BOTH: a re-record only ever adds answers. Delete the file to start over.
   *
   *  Kept, not re-asked, because the source moves on — the trees are re-recorded from
   *  the live sites, and a cluster added from a newer recorder run would otherwise
   *  re-record every older cluster against pages that have since changed or vanished,
   *  replaying a world none of their proofs ran in. And kept even when no request of
   *  the CURRENT code asks for them, because a regression is exactly a change in what
   *  gets asked: recorded after the fix, the "Avengers: Koniec gry (re-release)" cluster
   *  (PL, 2026-09-24) lost the "Avengers: Doomsday" answers the bug needed, and the
   *  reverted fix then passed on 28 unrecorded 404s. Record once with the bug, once with
   *  the fix, and the file holds both worlds. */
  def recording(source: HttpFetch, refused: Set[String] = RatingHosts, prior: Option[Path] = None): RecordedResponses =
    new RecordedResponses(prior.fold(new ConcurrentHashMap[String, Entry]())(load), Some(source), refused)
}
