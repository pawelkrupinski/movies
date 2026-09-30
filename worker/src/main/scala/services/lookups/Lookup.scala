package services.lookups

import tools.{HttpStatusException, RedactedUrl}

import java.net.URI
import java.util.Base64
import scala.util.Try

/** One external request, canonically: the method, the credential-masked URL and, where the body
 *  is what distinguishes two calls (IMDb's GraphQL POST), a fingerprint of it. The SAME key the
 *  recorded enrichment trees and the remembered-verdict cache use, so a fixture and a cached verdict
 *  of one request are one key — and the key the identity model's reads file a question under. */
final case class LookupQuery(key: String) {
  /** The service asked, for reporting — derived from the key, never a per-source table. */
  def host: String = key.split(' ') match {
    case Array(_, url, _*) => Try(Option(new URI(url).getHost)).toOption.flatten.getOrElse("")
    case _                 => ""
  }
}

object LookupQuery {
  def of(method: String, url: String, body: Option[String] = None): LookupQuery = {
    val masked = RedactedUrl(url)
    LookupQuery(body.fold(s"$method $masked")(text => s"$method $masked ${Integer.toHexString(text.hashCode)}"))
  }
  /** A venue's per-film detail page, which reaches the resolver PARSED (`DetailEnricher`) rather
   *  than as one HTTP body: `group` is the enricher's detail group (one page serves a whole chain). */
  def venueDetail(group: String, page: String): LookupQuery = LookupQuery(s"$DetailMethod $page $group")
  val DetailMethod = "DETAIL"
}

/** What a request came back with: a body, raw bytes, or a failure (with its HTTP status when it
 *  had one). */
sealed trait LookupAnswer {
  /** Whether this answer says something about the REQUEST rather than about the moment: a body,
   *  or a failure whose status describes the URL (404, 410). A timeout, a 5xx or a 429 is a
   *  failed read, and a failed read is not data — it never replaces a definitive answer. */
  def definitive: Boolean
}
object LookupAnswer {
  final case class Body(text: String) extends LookupAnswer { def definitive = true }

  final case class Bytes(base64: String) extends LookupAnswer {
    def definitive = true
    def bytes: Array[Byte] = Base64.getDecoder.decode(base64)
  }

  final case class Failed(status: Option[Int], method: String, message: String) extends LookupAnswer {
    def definitive: Boolean = status.exists(HttpStatusException.isDurable)
  }

  def ofBytes(bytes: Array[Byte]): Bytes = Bytes(Base64.getEncoder.encodeToString(bytes))

  /** What to keep of a failure. Status-bearing failures keep their code; anything else (a
   *  timeout, a reset socket) keeps its class name so a puzzling miss can be diagnosed later. */
  def failureOf(failure: Throwable, method: String): Failed = failure match {
    case status: HttpStatusException => Failed(Some(status.code), status.method, status.getMessage)
    case other                       => Failed(None, method, s"${other.getClass.getName}: ${other.getMessage}")
  }
}
