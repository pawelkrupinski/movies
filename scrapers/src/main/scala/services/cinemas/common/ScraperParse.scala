package services.cinemas.common

import java.util.Locale

import scala.jdk.CollectionConverters.*

import org.jsoup.nodes.{Document, Element, TextNode}
import play.api.libs.json.{JsArray, JsLookupResult, JsValue}
import tools.JsonScalars
import tools.Whitespace.collapsedSpaces

/** Parsing snippets shared by the hand-rolled cinema scrapers: lists, JSON feed fields, definition lists and production lines. Each of these shapes was copy-pasted across several `*Client`s before, so a fix lands in every scraper at once. */
private[cinemas] object ScraperParse {

  private val SubtitleWord = """(?i)\bsubtitle""".r

  /** Whether a performance note or category flags the screening subtitled: "(Descriptive
    * subtitles)", "Access - Cinema Subtitled". */
  def mentionsSubtitles(text: String): Boolean = SubtitleWord.findFirstIn(text).isDefined

  /** The `<br>`-separated lines of `el`'s text, trimmed and with blanks
    * dropped — a small venue often packs a multi-line "Label: value" block
    * or a run-per-line schedule into ONE `<br>`-joined paragraph, and jsoup's
    * `.text()` alone fuses them with no separator. Inserts a sentinel text
    * node after each `<br>` (on a clone, so the live DOM is untouched), then
    * splits on it. */
  def linesOf(el: Element): Seq[String] = {
    val clone = el.clone()
    clone.select("br").asScala.foreach(_.after("\u0001"))
    clone.text().split('\u0001').iterator.map(_.trim).filter(_.nonEmpty).toSeq
  }

  /** [[linesOf]], also breaking at the end of every paragraph, list item and
    * division — a venue's free-text box mixes `<br>`-joined runs with `<p>`s. */
  def blockLinesOf(el: Element): Seq[String] = {
    val clone = el.clone()
    clone.select("p, li, div").asScala.foreach(_.appendElement("br"))
    linesOf(clone)
  }

  /** Text of the `<dd>` immediately after the `<dt>` whose text contains
    * `label` (case-insensitive), searched within `dtSelector`. Trimmed; empty
    * → `None`. The Drupal-style cinema sites render film metadata as such
    * definition lists. An empty `<dt>` between the label and its value is
    * passed over: NoveKino never closes its label (`<dt>Reżyseria<dt><dd>…`),
    * which parses as a second, empty `<dt>`. */
  def ddField(document: Document, label: String, dtSelector: String = "dt"): Option[String] =
    document.select(dtSelector).asScala
      .find(_.text.toLowerCase(Locale.ROOT).contains(label))
      .flatMap(dt => Iterator.iterate(dt.nextElementSibling)(_.nextElementSibling).takeWhile(_ != null)
        .find(sibling => !(sibling.tagName == "dt" && sibling.text.trim.isEmpty)))
      .filter(_.tagName == "dd")
      .map(_.text.trim)
      .filter(_.nonEmpty)

  /** [[ddField]]'s comma-separated items ([[commaItems]]): a definition list's countries, directors or cast. */
  def ddItems(document: Document, label: String, dtSelector: String = "dt"): Seq[String] =
    ddField(document, label, dtSelector).toSeq.flatMap(commaItems)

  /** `href` as an absolute URL on `site` (a scheme and host, no trailing slash): an absolute one as it is, a path
   *  (with or without its leading `/`) under `site`. */
  def onSite(site: String)(href: String): String = if (href.startsWith("http")) href else s"$site/${href.stripPrefix("/")}"

  /**
   * Convert an ALL-CAPS title to sentence case for the MSI scrapers (Cinema1,
   * Kino Zamek, Kino Kijów, …) whose portals serve ALL-CAPS titles. The casing
   * itself is the shared `tools.TextNormalization.sentenceCase` (also used by
   * the central scrape-time `TitleNormalizer.recase`); this wrapper just runs
   * the MSI-specific de-glue first.
   *
   * A sequel number glued to the next word by a missing space ("3.ALE KOSMOS"
   * on RCK Kołobrzeg) is a source typo — restore the space so the word
   * capitalises and the cleaned title converges with the cinemas that spell it
   * "3. Ale kosmos". A digit-dot-digit decimal ("2.0") is untouched.
   */
  def sentenceCase(title: String): String =
    tools.TextNormalization.sentenceCase(title.replaceAll("""(\d)\.(\p{L})""", "$1. $2"))

  private val FourDigitYear = """(?:19|20)\d{2}""".r

  /** A cinema "production" line — "USA 2026", "Polska, Kanada, Hiszpania, 2026" —
   *  split into (production countries, release year). The year is the 4-digit
   *  part; the remaining comma/slash-separated parts are the countries (verbatim,
   *  canonicalised later in `recordCinemaScrape`). */
  def productionMeta(s: String): (List[String], Option[Int]) = {
    val year = FourDigitYear.findFirstIn(s).map(_.toInt)
    val countries = year.foldLeft(s)((acc, y) => acc.replace(y.toString, ""))
      .split("[,/]").iterator.map(_.trim).filter(_.nonEmpty).toList
    (countries, year)
  }

  private val ListSeparator = """\s*[,/;&]\s*""".r

  /** The items of a venue's one-field list — "USA/UK", "Poland; Germany; Italy", "UK & Namibia", "Joel Coen, Ethan
   *  Coen" — each trimmed, in order. */
  def listItems(s: String): Seq[String] = ListSeparator.split(s).iterator.map(_.trim).filter(_.nonEmpty).toSeq

  /** The items of a comma-separated field — "Joel Coen, Ethan Coen", "Frankreich, Mauretanien" — each trimmed, in
   *  order. Commas only, for a field whose names may carry the "&" or "/" [[listItems]] splits on ("Jean-Pierre &
   *  Luc Dardenne"). */
  def commaItems(s: String): Seq[String] = s.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq

  // A JSON feed's string field, trimmed; a missing, non-string or blank one as none (JsonScalars.text).
  export JsonScalars.text as jsonText

  /** A feed's list of named things — `[{"name": "Dramat"}, …]` or bare strings — as its non-empty names, in order;
   *  none when the list is missing. */
  def jsonNames(list: JsLookupResult): Seq[String] =
    jsonItems(list).flatMap(item => (item \ "name").asOpt[String].orElse(item.asOpt[String])).filter(_.nonEmpty)

  /** [[jsonNames]] for a feed whose list holds only named objects: a bare string in it is no name. */
  def jsonObjectNames(list: JsLookupResult): Seq[String] =
    jsonItems(list).flatMap(item => (item \ "name").asOpt[String]).filter(_.nonEmpty)

  private def jsonItems(list: JsLookupResult): Seq[JsValue] = list.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

  /** The first four digits in a row in `s`: a production-year or premiere-date field's year, unchecked for range
   *  ([[yearIn]] admits 19xx/20xx only). */
  def firstFourDigitsIn(s: String): Option[Int] = FourDigits.findFirstMatchIn(s).map(_.group(1).toInt)

  /** The first four-digit year in `s` — a premiere date's ("24 marca 1980"), a credits line's ("…, USA, 2018"). */
  def yearIn(s: String): Option[Int] = FourDigitYear.findFirstIn(s).map(_.toInt)

  /** A venue's `<strong>label:</strong> value<br>` film facts, as `label -> value`: the label lower-cased,
   *  whitespace-collapsed, its colon stripped; the value the text node right after the `<strong>`. The first
   *  value wins per label; a label with no text after it is left out. */
  def strongLabeledFields(container: Element): Map[String, String] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, String]
    container.select("strong").asScala.foreach { strong =>
      val label = strong.text.trim.toLowerCase(Locale.ROOT).replaceAll("\\s+", " ").stripSuffix(":").trim
      val value = strong.nextSibling() match {
        case text: TextNode => text.text.collapsedSpaces
        case _              => ""
      }
      if (label.nonEmpty && value.nonEmpty && !out.contains(label)) out(label) = value
    }
    out.toMap
  }

  /** The facts a page published from the Kino za Rogiem network's shared film catalogue (kinozarogiem.pl —
   *  GOK Siedlec's and Chorzów's Kino Grajfka's both are) links as that catalogue's WordPress terms, within
   *  `root`: `re_yseria` (and its later twin `re_zyseria`, which some directors are filed under) → the
   *  directors, `produkcja` → the production countries (less the catalogue's "Koprodukcja", which names
   *  none), `rok_produkcji_filmu` → the production year. */
  def kinoZaRogiemTerms(root: Element): FilmDetail = {
    def terms(taxonomies: String*) =
      root.select(taxonomies.map(taxonomy => s"a[href*=/$taxonomy/]").mkString(", ")).asScala.toSeq
        .map(_.text.trim).filter(_.nonEmpty).distinct
    FilmDetail(
      director    = terms("re_yseria", "re_zyseria"),
      countries   = terms("produkcja").filterNot(_.equalsIgnoreCase("Koprodukcja")),
      releaseYear = terms("rok_produkcji_filmu").flatMap(yearIn).headOption)
  }

  private val FilmwebSlugYear ="""filmweb\.pl/film/.+-((?:19|20)\d{2})-\d+/?$""".r

  /** The production year a Filmweb film link's slug carries —
   *  `filmweb.pl/film/Mistyczka-2026-10125135` → 2026. Venues that link a title
   *  to its Filmweb page publish no year of their own, so the slug is their
   *  only year signal; `None` for any other link. */
  def filmwebSlugYear(url: String): Option[Int] =
    FilmwebSlugYear.findFirstMatchIn(url).map(_.group(1).toInt)

  /** Canonical `https://www.youtube.com/watch?v=<id>` form for a YouTube
    * embed / watch / `youtu.be` URL; Vimeo URLs pass through unchanged for the
    * view layer's `TrailerEmbed` to reshape, and anything else is dropped. Each
    * scraper grabs a trailer URL off its own page shape, then funnels it
    * through this single canonicaliser. */
  def canonicalTrailer(url: String): Option[String] =
    models.TrailerEmbed.youTubeId(url).map(id => s"https://www.youtube.com/watch?v=$id")
      .orElse(models.TrailerEmbed.vimeoId(url).map(_ => url))

  private val FourDigits = """(\d{4})""".r
}
