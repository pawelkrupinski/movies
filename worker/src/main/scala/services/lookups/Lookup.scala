package services.lookups

import tools.RedactedUrl

import java.net.URI
import scala.util.Try

/** One external request, canonically: the method, the credential-masked URL and, where the body
 *  is what distinguishes two calls (IMDb's GraphQL POST), a fingerprint of it. The SAME key the
 *  recorded enrichment trees and the remembered-verdict cache use, so a fixture and a cached verdict
 *  of one request are one key — and the key the identity model's gaps name a question by. */
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
