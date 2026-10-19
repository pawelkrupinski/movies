package tools

import play.api.libs.json.{JsObject, JsString, JsNumber, Json}

import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

/**
 * An identity slice's answers at the HTTP seam: every request a slice's replay made of its recording, with the
 * recording's own outcome — a body, bytes, a failure the recording remembered, or a GAP (a request the recording
 * itself never answered). Recorded at the seam rather than per lookup so the slice holds every answer every stage
 * reads — TMDB, IMDb, the families, posters, catalogue links, venue pages — whatever the code at a ref asks of them,
 * and no answer a re-record would not have: nothing here is ever answered but by the recording.
 *
 * [[Noting]] wraps a recording's fetch and notes each outcome; [[Replaying]] answers from the notes alone — a gap as the
 * recording's gap (counted, so a lookup reads it `Unknown`), and a request it does not hold as a LOUD miss, named in
 * [[Replaying.lacking]], or, when `widen` holds the full recording, from it, named in [[Replaying.widened]].
 */
object SliceHttp {

  enum Outcome {
    case Body(text: String)
    case Bytes(bytes: Array[Byte])
    case Failed(className: String, message: String, status: Option[Int])
    case Gap
  }

  /** One request: its method, URL and (a POST's) body. */
  final case class Request(method: String, url: String, body: Option[String]) {
    def render: String = s"$method $url${body.fold("")(b => s" ${b.take(160)}")}"
  }

  /** `inner` (a [[RecordedCorpus.Opened]]'s fetch), every request's outcome noted; `metOnThread` tells a gap. */
  final class Noting(inner: HttpFetch, metOnThread: () => Long) extends HttpFetch {
    val noted = new ConcurrentHashMap[Request, Outcome]()
    private def note[A](request: Request)(read: => A)(answered: A => Outcome): A = {
      val before = metOnThread()
      try {
        val answer = read
        noted.put(request, if (metOnThread() != before) Outcome.Gap else answered(answer))
        answer
      } catch {
        case e: Exception =>
          noted.put(request, Outcome.Failed(e.getClass.getName, Option(e.getMessage).getOrElse(""),
            Some(e).collect { case h: HttpStatusException => h.code }))
          throw e
      }
    }
    override def get(url: String): String = note(Request("GET", url, None))(inner.get(url))(Outcome.Body(_))
    override def get(url: String, headers: Map[String, String]): String = note(Request("GET", url, None))(inner.get(url, headers))(Outcome.Body(_))
    override def getBytes(url: String): Array[Byte] = note(Request("BYTES", url, None))(inner.getBytes(url))(Outcome.Bytes(_))
    override def post(url: String, body: String, contentType: String): String =
      note(Request("POST", url, Some(body)))(inner.post(url, body, contentType))(Outcome.Body(_))
  }

  /** The request a slice does not hold — thrown, so the lookup reading it reads `Unknown`, and named in `lacking`. */
  final class SliceLacks(request: Request) extends RuntimeException(s"the slice holds no answer to ${request.render}")

  /** A slice's notes as the fetch: the recording's outcomes replayed, a gap counted on the thread as the recording's
   *  gap leaf counts it ([[gaps]]), anything else LACKING — or, with `widen`, read from the full recording and noted. */
  @nowarn("msg=unused import")
  final class Replaying(notes: Map[Request, Outcome], widen: Option[Noting] = None) extends HttpFetch {
    // The identity adapters, in either package a ref keeps them (OlderRefsPackage).
    import services.identity.*, services.identity.adapters.*

    val lacking = ConcurrentHashMap.newKeySet[Request]()
    val gapsMet = ConcurrentHashMap.newKeySet[Request]()
    private val onThread = ThreadLocal.withInitial[java.lang.Long](() => 0L)
    def metOnThread: Long = onThread.get()
    /** The requests read from the full recording because the slice lacked them. */
    def widened: Map[Request, Outcome] = widen.fold(Map.empty[Request, Outcome])(_.noted.asScala.toMap)
    def gaps: TmdbIdentityLookups.Gaps = new TmdbIdentityLookups.Gaps {
      def answered[A](read: => A): Answer[A] = {
        val before = metOnThread
        scala.util.Try(read).toOption.filter(_ => metOnThread == before)
          .fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
      }
    }
    private def replay(request: Request, empty: => Any)(fromRecording: HttpFetch => Any): Any =
      notes.get(request).orElse(widen.flatMap(w => Option(w.noted.get(request)))) match {
        case Some(Outcome.Body(text))   => text
        case Some(Outcome.Bytes(bytes)) => bytes
        case Some(Outcome.Gap)          => gapsMet.add(request); onThread.set(onThread.get() + 1); empty
        case Some(f: Outcome.Failed)    => throw revive(f, request)
        case None => widen match {
          // the recording's outcome, noted by `recording` whatever it was (a failure too), then replayed as a note
          case Some(recording) =>
            scala.util.Try(fromRecording(recording))
            if (recording.noted.containsKey(request)) replay(request, empty)(_ => throw new SliceLacks(request))
            else { lacking.add(request); throw new SliceLacks(request) }
          case None => lacking.add(request); throw new SliceLacks(request)
        }
      }
    override def get(url: String): String = replay(Request("GET", url, None), "{}")(_.get(url)).asInstanceOf[String]
    override def get(url: String, headers: Map[String, String]): String = replay(Request("GET", url, None), "{}")(_.get(url, headers)).asInstanceOf[String]
    override def getBytes(url: String): Array[Byte] = replay(Request("BYTES", url, None), Array.emptyByteArray)(_.getBytes(url)).asInstanceOf[Array[Byte]]
    override def post(url: String, body: String, contentType: String): String =
      replay(Request("POST", url, Some(body)), "{}")(_.post(url, body, contentType)).asInstanceOf[String]
  }

  /** The recorded failure, thrown again as the class it was where that class takes a message, else an IO failure. */
  def revive(failed: Outcome.Failed, request: Request): Exception = failed match {
    case Outcome.Failed(_, _, Some(code)) => new HttpStatusException(code, request.method, request.url, None)
    case Outcome.Failed(name, message, _) =>
      scala.util.Try(Class.forName(name).getConstructor(classOf[String]).newInstance(message).asInstanceOf[Exception])
        .getOrElse(new java.io.IOException(s"$name: $message"))
  }

  // ── the notes as JSON lines ──────────────────────────────────────────────────────────────

  def toJson(request: Request, outcome: Outcome): String = {
    val head = Seq("m" -> JsString(request.method), "u" -> JsString(request.url)) ++ request.body.map(b => "b" -> JsString(b))
    val tail = outcome match {
      case Outcome.Body(text)   => Seq("k" -> JsString("body"), "v" -> JsString(text))
      case Outcome.Bytes(bytes) => Seq("k" -> JsString("bytes"), "v" -> JsString(java.util.Base64.getEncoder.encodeToString(bytes)))
      case Outcome.Failed(name, message, status) =>
        Seq("k" -> JsString("failed"), "e" -> JsString(name), "msg" -> JsString(message)) ++ status.map(s => "s" -> JsNumber(s))
      case Outcome.Gap          => Seq("k" -> JsString("gap"))
    }
    Json.stringify(JsObject(head ++ tail))
  }

  def fromJson(line: String): (Request, Outcome) = {
    val j = Json.parse(line)
    val request = Request((j \ "m").as[String], (j \ "u").as[String], (j \ "b").asOpt[String])
    request -> ((j \ "k").as[String] match {
      case "body"   => Outcome.Body((j \ "v").as[String])
      case "bytes"  => Outcome.Bytes(java.util.Base64.getDecoder.decode((j \ "v").as[String]))
      case "failed" => Outcome.Failed((j \ "e").as[String], (j \ "msg").as[String], (j \ "s").asOpt[Int])
      case "gap"    => Outcome.Gap
      case other    => sys.error(s"unknown outcome $other in a slice's notes")
    })
  }

  def write(path: java.nio.file.Path, notes: Map[Request, Outcome]): Unit = {
    java.nio.file.Files.createDirectories(path.getParent)
    val out = new java.util.zip.GZIPOutputStream(java.nio.file.Files.newOutputStream(path))
    try notes.toSeq.sortBy(_._1.render).foreach { case (r, o) => out.write((toJson(r, o) + "\n").getBytes(StandardCharsets.UTF_8)) }
    finally out.close()
  }

  def read(path: java.nio.file.Path): Map[Request, Outcome] = {
    val in = new java.util.zip.GZIPInputStream(java.nio.file.Files.newInputStream(path))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8).linesIterator.filter(_.nonEmpty).map(fromJson).toMap finally in.close()
  }
}
