package services.enrichment

import java.util.Locale

import play.api.libs.json._
import services.resolution.YearWindow
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * Harvests film-database cross-reference ids from a Filmweb entity id via
 * Wikidata (P5032 → the item's external-id claims). Last-resort fallback in
 * [[ImdbIdResolver]] when both IMDb's own suggestion endpoint and the
 * director-based disambiguation come up empty — typically classic/repertoire
 * films whose titles differ between the cinema listing, IMDb, and TMDB.
 *
 * A single `wbgetentities` claims call carries EVERY id the item records, so
 * one round-trip backfills IMDb (P345), TMDB (P4947), Rotten Tomatoes (P1258),
 * Metacritic (P1712) and Letterboxd (P6127) at once. The RT/Metacritic slugs
 * matter because those two rating clients otherwise DISCOVER their page by
 * slug-probing/scraping — a Wikidata hit gives the slug deterministically.
 *
 * Two-step Action API flow (avoids the SPARQL endpoint, which applies aggressive
 * rate limits under load):
 *   1. `action=query&list=search&srsearch=haswbstatement:P5032=<filmwebId>`
 *      → up to 3 Wikidata Q-IDs whose P5032 matches the filmweb entity id.
 *   2. `action=wbgetentities&ids=Q…&props=claims`
 *      → fetches the external-id claims from those Q-IDs.
 *
 * Wikimedia's User-Agent policy requires a meaningful UA; `HttpFetch.get(url,
 * headers)` passes it through — test fakes safely ignore the extra headers.
 *
 * "No item" is an answer (`None`); a read that failed — a throttle, a 5xx, Wikimedia's
 * 200 `{"error":…}` document (maxlag), a body that is not the API's JSON — throws, so the
 * resolver ladders never read an outage as "Wikidata has no such film".
 */
class WikidataClient(http: HttpFetch) {
  import WikidataClient._

  /** IMDb tt-id for the given Filmweb entity id, or None when Wikidata has no
   *  cross-reference; a failed read throws. Thin accessor over
   *  [[findIdsByFilmwebId]] for callers that only want the IMDb id. */
  def findImdbIdByFilmwebId(filmwebId: String): Option[String] =
    findIdsByFilmwebId(filmwebId).flatMap(_.imdbId)

  /** Every film-database id Wikidata records for the given Filmweb entity id, or
   *  None when no item matches; a failed read throws. */
  def findIdsByFilmwebId(filmwebId: String): Option[WikidataIds] = {
    val qids = searchByFilmwebId(filmwebId)
    if (qids.isEmpty) None else harvest(qids)
  }

  /** IMDb id for a film found by TITLE (not a Filmweb id) — the direct-title rung
   *  in [[ImdbIdResolver]] for a TMDB-less film with no Filmweb entity page. Two
   *  steps: search Wikidata items that are instance-of film (P31=Q11424) AND match
   *  the title text, then bind the first whose English label corroborates (exact,
   *  or contains + a P577 publication year within one of the queried year). A year
   *  gap of >1 vetoes even an exact label — two films share a title often enough
   *  that a title-only lookup must not cross a year gap. A failed read throws. */
  def findImdbIdByTitle(title: String, year: Option[Int]): Option[String] = {
    val qids = searchFilmsByTitle(title)
    if (qids.isEmpty) None
    else {
      val entities = entitiesOf(entitiesUrl(qids, Seq("claims", "labels"), languages = Seq("en")))
      qids.iterator.flatMap { qid =>
        entities.get(qid).flatMap { e =>
          val label   = (e \ "labels" \ "en" \ "value").asOpt[String]
          val imdbId  = firstClaim(e, PImdb).filter(_.startsWith("tt"))
          val pubYear = firstPublicationYear(e)
          imdbId.filter(_ => titleCorroborates(title, label, year, pubYear))
        }
      }.nextOption()
    }
  }

  private def searchFilmsByTitle(title: String): Seq[String] = {
    val query = URLEncoder.encode(s"$title haswbstatement:P31=$QFilm", StandardCharsets.UTF_8)
    searchHits(s"$ActionBase?action=query&list=search&srsearch=$query&srnamespace=0&srlimit=5&format=json")
  }

  /** The item ids a `list=search` query returned — empty when nothing matched. */
  private def searchHits(url: String): Seq[String] =
    HttpRead.jsonObject(http, url, UserAgentHeader) { js =>
      (js \ "query" \ "search").asOpt[JsArray] match {
        case Some(hits) =>
          ReadOutcome.Answered(hits.value.toSeq.flatMap(entry => (entry \ "title").asOpt[String]).filter(_.startsWith("Q")))
        case None => ReadOutcome.unexpectedBody(url, "no query.search", js.toString)
      }
    }.required

  /** The `entities` a `wbgetentities` call returned, by item id. */
  private def entitiesOf(url: String): collection.Map[String, JsValue] =
    HttpRead.jsonObject(http, url, UserAgentHeader) { js =>
      (js \ "entities").asOpt[JsObject] match {
        case Some(entities) => ReadOutcome.Answered(entities.value)
        case None           => ReadOutcome.unexpectedBody(url, "no entities", js.toString)
      }
    }.required

  /** Year from the first P577 (publication date) claim, whose value is a time
   *  object (`{"time":"+2026-01-01T00:00:00Z",…}`), not a plain string. */
  private def firstPublicationYear(entity: JsValue): Option[Int] =
    (entity \ "claims" \ PPublicationDate).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value" \ "time").asOpt[String])
      .flatMap(t => raw"(\d{4})".r.findFirstIn(t)).map(_.toInt).headOption

  /** Bind a title-search hit only when its label corroborates: an exact
   *  (deburred) label, or a contains-match with a P577 year within one of the
   *  queried year. A year gap of >1 vetoes even an exact label. */
  private def titleCorroborates(queryTitle: String, label: Option[String], queryYear: Option[Int], pubYear: Option[Int]): Boolean =
    label.exists { l =>
      val q = norm(queryTitle); val n = norm(l)
      val exact           = q.nonEmpty && q == n
      val titleContains   = q.nonEmpty && n.nonEmpty && (q.startsWith(n) || n.startsWith(q))
      val yearMatch       = YearWindow.agrees(queryYear, pubYear, YearTolerance).contains(true)
      (exact || (titleContains && yearMatch)) && !YearWindow.contradicts(queryYear, pubYear, YearTolerance)
    }

  // ── As an identity family: search, a director's films, an item's record ─────────────────────────────────────

  /** The film items Wikidata's entity search finds for `text` in `language` and in English, at most `limit`, with the
   *  label each search gave — a novel or a series of the title never passes ([[isFilm]]). */
  def identitySearch(text: String, language: String, limit: Int): Seq[(String, String)] = {
    val found = Seq(language, "en").distinct.flatMap { lang =>
      entitySearch(s"$ActionBase?action=wbsearchentities&search=${quote(text)}&language=$lang&uselang=$lang&type=item&limit=15&format=json")
    }.distinctBy(_._1)
    val items = if (found.isEmpty) Map.empty else entitiesOf(entitiesUrl(found.map(_._1), Seq("claims")))
    found.filter { case (id, _) => items.get(id).exists(isFilm) }.take(limit)
  }

  /** The items crediting as director (P57) the first two people an entity search in `language` finds for `name`. */
  def identityDirectedBy(name: String, language: String): Seq[String] = {
    val people = entitySearch(s"$ActionBase?action=wbsearchentities&search=${quote(name)}&language=$language&type=item&limit=5&format=json").map(_._1)
    val items  = if (people.isEmpty) Map.empty else entitiesOf(entitiesUrl(people, Seq("claims")))
    people.filter(person => items.get(person).exists(item => claimIds(item, "P31").contains(QHuman))).take(2).flatMap { person =>
      searchHits(s"$ActionBase?action=query&list=search&srsearch=haswbstatement:P57=$person&srnamespace=0&srlimit=60&format=json")
    }.distinct.take(60)
  }

  /** The film item's record — titles first in `language`, original title (P1476), earliest publication year (P577),
   *  duration (P2047), directors (P57) by label, countries of origin (P495) by ISO code — and the ids other databases
   *  know it by, each as that database's family spells its own; `None` for no film item. */
  def identityRecord(id: String, language: String): Option[(services.identity.IdentityMeasures.Film, Map[String, String])] =
    entitiesOf(entitiesUrl(Seq(id), Seq("claims", "labels", "aliases", "sitelinks"))).get(id).filter(isFilm).flatMap { item =>
      val order = (Seq(language, "en", "mul") ++ PreferredLanguages).distinct
      def rank(lang: String) = { val at = order.indexOf(lang); if (at < 0) order.size else at }
      val labels   = (item \ "labels").asOpt[Map[String, JsObject]].getOrElse(Map.empty).toSeq.sortBy(l => rank(l._1)).flatMap(l => (l._2 \ "value").asOpt[String])
      val aliases  = (item \ "aliases").asOpt[Map[String, Seq[JsObject]]].getOrElse(Map.empty).toSeq.sortBy(a => rank(a._1))
        .flatMap(_._2.flatMap(a => (a \ "value").asOpt[String]))
      val original = claimValues(item, "P1476").flatMap(v => (v \ "text").asOpt[String]).headOption
      val year     = claimValues(item, PPublicationDate).flatMap(v => (v \ "time").asOpt[String]).flatMap(t => raw"(18|19|20)\d\d".r.findFirstIn(t)).map(_.toInt).minOption
      val runtime  = claimValues(item, "P2047").flatMap(v => (v \ "amount").asOpt[String]).flatMap(_.toDoubleOption).headOption.map(_.toInt)
      val directors = { val ids = claimIds(item, "P57"); val named = if (ids.isEmpty) Map.empty else entitiesOf(entitiesUrl(ids, Seq("labels")))
        ids.flatMap(named.get).flatMap(labelOf(_, "en")) }
      val countries = claimIds(item, "P495").flatMap(isoOf)
      val crossIds = Map("wikidata" -> id) ++ CrossIds.flatMap { case (property, database, own) => firstClaim(item, property).map(value => database -> own(value)) }
      labelOf(item, language).map { title =>
        (services.identity.IdentityMeasures.Film(title, original.filterNot(_ == title),
          (labels ++ aliases).filterNot(t => t == title || original.contains(t)).distinct.take(40), year, runtime,
          Option(directors).filter(_.nonEmpty), Option(countries).filter(_.nonEmpty)), crossIds)
      }
    }

  // ── As a catalogue mapping: the items stating another database's ids ───────────────────────────────────────

  /** The items stating each of `values` under one of `properties` (a catalogue's own id: AlloCiné's P1265, Letterboxd's
   *  P6127 …), with the TMDB movie id (P4947) and IMDb id (P345) each states — ONE query for the whole batch, against the
   *  SPARQL endpoint: the Action API's statement search is one request an id. A value no item states is absent; an
   *  item stating two TMDB or IMDb ids states neither. A failed read throws. */
  def itemsStating(properties: Seq[String], values: Seq[String]): Map[String, Seq[WikidataClient.StatingItem]] =
    if (values.isEmpty || properties.isEmpty) Map.empty
    else {
      val url = s"$SparqlBase?format=json&query=${quote(catalogueQuery(properties, values))}"
      HttpRead.jsonObject(http, url, UserAgentHeader) { js =>
        (js \ "results" \ "bindings").asOpt[Seq[JsObject]] match {
          case Some(rows) => ReadOutcome.Answered(statingItems(rows))
          case None       => ReadOutcome.unexpectedBody(url, "no results.bindings", js.toString.take(300))
        }
      }.required
    }

  /** A country item's ISO code (P297), asked once per client: a country's claims are megabytes, and few countries recur. */
  private val isoCodes = new java.util.concurrent.ConcurrentHashMap[String, Option[String]]()
  private def isoOf(country: String): Option[String] =
    Option(isoCodes.get(country)).getOrElse {
      val code = entitiesOf(entitiesUrl(Seq(country), Seq("claims"))).get(country).flatMap(firstClaim(_, "P297"))
      isoCodes.put(country, code); code
    }

  /** The items a `wbsearchentities` search returned, with their labels. */
  private def entitySearch(url: String): Seq[(String, String)] =
    HttpRead.jsonObject(http, url, UserAgentHeader) { js =>
      (js \ "search").asOpt[Seq[JsObject]] match {
        case Some(hits) => ReadOutcome.Answered(hits.flatMap(hit => (hit \ "id").asOpt[String].map(_ -> (hit \ "label").asOpt[String].getOrElse(""))))
        case None       => ReadOutcome.unexpectedBody(url, "no search", js.toString)
      }
    }.required

  private def searchByFilmwebId(filmwebId: String): Seq[String] = {
    val encoded = URLEncoder.encode(s"haswbstatement:P5032=$filmwebId", StandardCharsets.UTF_8)
    searchHits(s"$ActionBase?action=query&list=search&srsearch=$encoded&srnamespace=0&srlimit=3&format=json")
  }

  private def harvest(qids: Seq[String]): Option[WikidataIds] = {
    val entities = entitiesOf(entitiesUrl(qids, Seq("claims")))
    // Extract each id independently, each from the first Q-ID (by search rank)
    // that carries that property. Keeping the id-types independent preserves the
    // original imdbId behaviour ("first Q-ID with a P345 claim") while letting a
    // sibling Q-ID that happens to hold, say, the RT slug still contribute it.
    def claim(property: String): Option[String] =
      qids.iterator.flatMap { qid =>
        entities.get(qid).iterator.flatMap { entity =>
          (entity \ "claims" \ property).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
            .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value").asOpt[String])
        }
      }.nextOption()

    val ids = WikidataIds(
      imdbId           = claim(PImdb).filter(_.startsWith("tt")),
      tmdbId           = claim(PTmdb).filter(_.forall(_.isDigit)).map(_.toInt),
      rottenTomatoesId = claim(PRottenTomatoes),
      metacriticId     = claim(PMetacritic),
      letterboxdId     = claim(PLetterboxd)
    )
    Some(ids).filter(_.nonEmpty)
  }
}

object WikidataClient {
  private val ActionBase = "https://www.wikidata.org/w/api.php"
  private val SparqlBase = "https://query.wikidata.org/sparql"

  /** An item stating a catalogue id, the property it states it under, and the TMDB and IMDb ids it states. */
  final case class StatingItem(item: String, property: String, tmdbId: Option[Int], imdbId: Option[String])

  /** The SPARQL query [[WikidataClient.itemsStating]] asks: each value, the items stating it under any of the properties
   *  (and which), and their TMDB and IMDb ids — in a stable order, so the same batch is the same request. */
  def catalogueQuery(properties: Seq[String], values: Seq[String]): String = {
    val quoted = values.distinct.sorted.map(v => "\"" + v.replace("\\", "\\\\").replace("\"", "\\\"") + "\"").mkString(" ")
    s"SELECT ?value ?property ?item ?tmdb ?imdb WHERE { VALUES ?value { $quoted } VALUES ?property { ${properties.sorted.map("wdt:" + _).mkString(" ")} } " +
      s"?item ?property ?value . OPTIONAL { ?item wdt:$PTmdb ?tmdb } OPTIONAL { ?item wdt:$PImdb ?imdb } }"
  }

  /** The SPARQL result's rows as each value's items, one per item: the TMDB and IMDb ids an item states once each. */
  private def statingItems(rows: Seq[JsObject]): Map[String, Seq[StatingItem]] = {
    def field(row: JsObject, name: String) = (row \ name \ "value").asOpt[String]
    def lastSegment(uri: String) = uri.substring(uri.lastIndexOf('/') + 1)
    rows.flatMap(row => for { value <- field(row, "value"); item <- field(row, "item") } yield
      (value, lastSegment(item), field(row, "property").fold("")(lastSegment), field(row, "tmdb"), field(row, "imdb")))
      .groupBy(_._1).view.mapValues(_.groupBy(_._2).toSeq.sortBy(_._1).map { case (item, stated) =>
        def sole(values: Seq[String]) = Option(values.distinct).filter(_.sizeIs == 1).flatMap(_.headOption)
        StatingItem(item, stated.map(_._3).distinct.sorted.mkString("/"), sole(stated.flatMap(_._4)).flatMap(_.toIntOption),
          sole(stated.flatMap(_._5)).filter(_.startsWith("tt")))
      }).toMap
  }
  private val YearTolerance = 1

  /**
   * Wikidata's list separator, PERCENT-ENCODED.
   *
   * The API documents it as a literal `|` and accepts either form, but a literal one
   * never reaches it: `java.net.URI` rejects `|` in a query outright, and
   * `RealHttpFetch` builds every request through `URI.create`. So each of these calls
   * threw `IllegalArgumentException: Illegal character in query` before a byte left the
   * process, and this whole rung of the imdbId ladder — Wikidata's P345/P4947/P1258/
   * P1712 harvest — was dead in production while its specs stayed green, because they
   * stubbed by URL substring and never parsed what they were handed.
   *
   * The single-item calls were fine, which is what made it survive: `mkString` emits no
   * separator for one Q-ID. `props=claims|labels` has two by construction, so the
   * title-search path never worked at all.
   */
  private val ListSeparator = "%7C"

  /** One `wbgetentities` call for a set of Q-IDs. Every list this URL carries goes
   *  through [[ListSeparator]] — the reason it exists in one place. */
  private def entitiesUrl(qids: Seq[String], props: Seq[String], languages: Seq[String] = Seq.empty): String = {
    val languageParameter =
      if (languages.isEmpty) "" else s"&languages=${languages.mkString(ListSeparator)}"
    s"$ActionBase?action=wbgetentities&ids=${qids.mkString(ListSeparator)}" +
      s"&props=${props.mkString(ListSeparator)}$languageParameter&format=json"
  }

  // Wikidata property ids for the film-database cross-references we harvest.
  private val PImdb            = "P345"   // IMDb id            → "tt0052080"
  private val PTmdb            = "P4947"  // TMDB movie id      → "603"
  private val PRottenTomatoes  = "P1258"  // RT id (with path)  → "m/the_matrix"
  private val PMetacritic      = "P1712"  // Metacritic id      → "movie/the-matrix"
  private val PLetterboxd      = "P6127"  // Letterboxd id      → "the-matrix"
  private val PPublicationDate = "P577"   // publication date   → {"time":"+2026-…"}
  private val QFilm            = "Q11424" // instance-of value: film (title-search filter)

  private val QHuman = "Q5"
  private val PreferredLanguages = Seq("pl", "de", "es", "fr", "it", "pt", "nl", "sv", "cs")
  /** Film classes an item's P31 may name, and classes that make it no film unless it names one of those too. */
  private val FilmClasses = Set("Q11424", "Q24862", "Q506240", "Q202866", "Q226730", "Q93204", "Q17517379", "Q24869", "Q20650540", "Q1366112",
    "Q29168811", "Q130232", "Q645928", "Q4220917", "Q2484376", "Q336144", "Q622548", "Q18011172", "Q157443", "Q20667187", "Q1257444",
    "Q1054574", "Q2321734", "Q52207399", "Q790192", "Q959790", "Q917641", "Q319221")
  private val NotFilm = Set("Q5398426", "Q5", "Q7725634", "Q482994", "Q1259759", "Q21191270", "Q7889", "Q3464665")
  /** The other databases' ids an item states, under the identity families' names, as each family spells its own. */
  private val CrossIds: Seq[(String, String, String => String)] = Seq(
    (PImdb, "imdb", identity), (PTmdb, "tmdb", identity), ("P5032", "filmweb", identity),
    (PRottenTomatoes, "rt", _.stripPrefix("m/")), (PMetacritic, "metacritic", _.stripPrefix("movie/")))

  /** A film item: one naming a film class, or stating an IMDb title or TMDB movie id — never one only a non-film class
   *  (a novel, a person, a series) names. */
  def isFilm(item: JsValue): Boolean = {
    val classes = claimIds(item, "P31").toSet
    if ((classes intersect NotFilm).nonEmpty && (classes intersect FilmClasses).isEmpty) false
    else (classes intersect FilmClasses).nonEmpty || claimValues(item, PImdb).flatMap(_.asOpt[String]).exists(_.startsWith("tt")) ||
      claimValues(item, PTmdb).nonEmpty
  }
  private def claimValues(item: JsValue, property: String): Seq[JsValue] =
    (item \ "claims" \ property).asOpt[JsArray].fold(Seq.empty[JsValue])(_.value.toSeq.flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value").toOption))
  private def claimIds(item: JsValue, property: String): Seq[String] = claimValues(item, property).flatMap(v => (v \ "id").asOpt[String])
  private def firstClaim(item: JsValue, property: String): Option[String] = claimValues(item, property).flatMap(_.asOpt[String]).headOption
  private def labelOf(item: JsValue, language: String): Option[String] = {
    val labels = (item \ "labels").asOpt[Map[String, JsObject]].getOrElse(Map.empty)
    Seq(language, "en", "mul").flatMap(labels.get).headOption.orElse(labels.values.headOption).flatMap(l => (l \ "value").asOpt[String])
  }

  /** `text` percent-encoded byte by byte, but A–Z a–z 0–9 `_.-~` — a space as `%20`, as Wikimedia's own clients write it. */
  def quote(text: String): String = text.getBytes(StandardCharsets.UTF_8).map { byte =>
    val c = (byte & 0xff).toChar
    if ((c.isLetterOrDigit && c < 128) || "_.-~".contains(c)) c.toString else f"%%${byte & 0xff}%02X"
  }.mkString

  /** Deburred, case-folded, alnum-only — matches the shape the other resolvers'
   *  corroboration uses so label/title comparison is diacritic/case-insensitive. */
  private def norm(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}+", "").toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "")

  val UserAgentHeader: Map[String, String] =
    Map("User-Agent" -> "kinowo/1.0 (pawel.krupinski@gmail.com)")

  /** Extract the numeric Filmweb entity id from a canonical Filmweb film/serial
   *  URL (`…/film/Title-Year-<id>` or `…/serial/…`). Returns None for
   *  search-redirect URLs (`filmweb.pl/search?query=…`), which have no entity
   *  id and aren't actionable. */
  def filmwebEntityId(url: String): Option[String] =
    raw"-(\d+)/?$$".r.findFirstMatchIn(url).map(_.group(1))
}

/** The film-database cross-reference ids Wikidata records for one film. Each is
 *  optional — an item rarely carries all five. `letterboxdId` has no row field
 *  yet; it's harvested here so a Letterboxd resolver can consume it later. */
case class WikidataIds(
  imdbId:           Option[String],   // P345  — "tt0052080"
  tmdbId:           Option[Int],      // P4947 — 603
  rottenTomatoesId: Option[String],   // P1258 — "m/the_matrix"
  metacriticId:     Option[String],   // P1712 — "movie/the-matrix"
  letterboxdId:     Option[String]    // P6127 — "the-matrix"
) {
  def nonEmpty: Boolean =
    imdbId.isDefined || tmdbId.isDefined || rottenTomatoesId.isDefined ||
      metacriticId.isDefined || letterboxdId.isDefined
}
