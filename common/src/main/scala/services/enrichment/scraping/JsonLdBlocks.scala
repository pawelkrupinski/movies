package services.enrichment.scraping

import org.jsoup.nodes.Element
import play.api.libs.json.{JsValue, Json}

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * A page's `<script type="application/ld+json">` blocks — the schema.org data a cinema page publishes its
 * screenings or its film under (Charlie, Charlie Monroe, biletyna, Flicks), and a rating site its score
 * (Metacritic, Rotten Tomatoes). One reader for every one of them, from either side of the parse:
 *
 *   - a page already parsed into a DOM (a cinema client reads the rest of it with Jsoup anyway):
 *     the blocks Jsoup's `script[type=application/ld+json]` selects, their `data()`;
 *   - a page as raw HTML (a rating page, read for its blocks alone): the blocks found by [[HtmlScripts]],
 *     without building a DOM. `JsonLdScanSpec` holds the two to the same answer on every recorded page.
 *
 * [[scripts]] gives each block's raw text, in page order (a client that pairs a block with the HTML around it
 * needs the text); [[parsed]] those of them that are JSON, a block that is not left out.
 */
object JsonLdBlocks {

  private val Selector = "script[type=application/ld+json]"

  // The `type` attribute however it is quoted — what Jsoup's selector matches, on a script's attribute text.
  private val LdJsonType = """(?i)\btype\s*=\s*(["']?)application/ld\+json\1(?=[\s/>]|$)""".r

  /** The raw text of every JSON-LD block of a parsed page, in page order. */
  def scripts(page: Element): Seq[String] = page.select(Selector).asScala.toSeq.map(_.data())

  /** The raw text of every JSON-LD block of a raw HTML page, in page order, found without a DOM. */
  def scripts(html: String): Seq[String] =
    HtmlScripts.all(html).collect { case s if LdJsonType.findFirstIn(s.attributes).isDefined => s.body }.toSeq

  /** Every JSON-LD block of a parsed page that is JSON, in page order. */
  def parsed(page: Element): Seq[JsValue] = scripts(page).flatMap(asJson)

  /** Every JSON-LD block of a raw HTML page that is JSON, in page order. */
  def parsed(html: String): Seq[JsValue] = scripts(html).flatMap(asJson)

  private def asJson(raw: String): Option[JsValue] = Try(Json.parse(raw)).toOption
}
