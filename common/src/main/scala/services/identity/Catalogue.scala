package services.identity

/**
 * A listing's EXACT naming of its film in another film database: the catalogue ids its venue's client captured
 * ([[CatalogueId]], `CinemaMovie.externalIds`) or its venue's film page links (Flicks' Letterboxd and Rotten Tomatoes
 * links, Kinoteka's IMDb link). Unlike a search they name one film, so the agreement stage takes the film one maps to
 * for a cluster nothing else took (`agreement.Catalogue`), never as a guess: the mapping is Wikidata's own statement of
 * the id, else the source's own page.
 */
object CatalogueSources {

  /** A catalogue whose ids Wikidata states under `properties`, each spelled there by `spelled` ("m/" + an RT slug). */
  final case class Mapped(source: String, properties: Seq[String], spelled: String => String)

  /** Webedia's film id (Filmstarts in Germany, SensaCine in Spain): one id across the AlloCiné family, stated as
   *  AlloCiné's (P1265) or Filmstarts' (P8531). Measured 2026-10-05 on 400 recorded DE ids: 295 mapped, 293 to TMDB. */
  val Webedia: Mapped = Mapped("webedia", Seq("P1265", "P8531"), identity)
  /** A Letterboxd film slug (P6127), which a Flicks film page links. */
  val Letterboxd: Mapped = Mapped("letterboxd", Seq("P6127"), identity)
  /** A Rotten Tomatoes film slug (P1258, "m/<slug>"), which a Flicks film page links. */
  val RottenTomatoes: Mapped = Mapped("rt", Seq("P1258"), "m/" + _)

  /** The catalogues an aggregator's showtimes FEED links a venue's screening to: Webedia's (Filmstarts in Germany,
   *  SensaCine in Spain), by the id the feed attaches; kinoprogramm.com's, by the catalogue page it serves as the
   *  listing's own (`/kinofilm/<slug>-<id>`). The feed copies its entry's title, year, directors, running time and
   *  poster onto the listing, so they are that CATALOGUE's claim, not the venue's: the feed can link the wrong entry —
   *  DE Roxy Kitzingen's "To The Bone", Noxon's 2017 feature on the venue's own page, linked to Filmstarts' 227420, Erin
   *  Li's 2014 short, with the short's year, director and 8 minutes — and its facts then only repeat the wrong entry. */
  val FeedIds: Set[String]       = Set(Webedia.source)
  val FeedPages: Seq[String]     = Seq("https://www.kinoprogramm.com/", "https://kinoprogramm.com/")

  /** Are `listing`'s facts a feed catalogue's claim rather than its venue's ([[FeedIds]], [[FeedPages]])? */
  def feedStated(listing: Listing): Boolean = feedStated(listing.catalogueIds, listing.page)

  /** [[feedStated]] for a listing held as its catalogue ids and page alone (the review pages' stored listing). */
  def feedStated(catalogueIds: Seq[CatalogueId], page: Option[String]): Boolean =
    catalogueIds.exists(id => FeedIds(id.source)) || page.exists(page => FeedPages.exists(page.startsWith))

  /** Every catalogue Wikidata maps, by source. */
  val ByWikidata: Map[String, Mapped] = Seq(Webedia, Letterboxd, RottenTomatoes).map(m => m.source -> m).toMap

  /** An IMDb title id names its film itself: TMDB's find takes it to TMDB's record, IMDb's own record to its facts. */
  val Imdb = "imdb"

  /** Does `id` name a film another database's mapping can take: one Wikidata maps, or an IMDb title? */
  def mappable(id: CatalogueId): Boolean = ByWikidata.contains(id.source) || (id.source == Imdb && id.id.startsWith("tt"))
}

/** What one catalogue id maps to: the Wikidata `item` stating it (none when the source's own page mapped it), the TMDB and
 *  IMDb ids that item or page states, and `via`, the mapping's name for an explanation ("Wikidata P8531", "Letterboxd"). */
final case class CatalogueHit(item: Option[String], tmdb: Option[Int], imdb: Option[String], via: String)

/** The catalogue mappings as the agreement stage reads them: `Unknown` while not asked yet — the stage hands the
 *  question to the queue ([[CatalogueQuestion]]) and holds the cluster as the model left it. */
trait CatalogueAnswers {
  /** The catalogue ids a venue's film `page` links: none for a page no client reads its links of. */
  def linked(page: String): Answer[Seq[CatalogueId]]
  /** What `id` maps to: none when no database states it, two or more when Wikidata files it on several items. */
  def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]]
}

object CatalogueAnswers {
  /** No catalogue anywhere: no evidence, and no gap. */
  val Silent: CatalogueAnswers = new CatalogueAnswers {
    def linked(page: String): Answer[Seq[CatalogueId]]    = Answer.Known(Nil)
    def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] = Answer.Known(Nil)
  }
}

/** A catalogue question to ask on the queue: the ids a venue's film page links, or what one id maps to. */
enum CatalogueQuestion {
  case Page(url: String)
  case Id(id: CatalogueId)
}
