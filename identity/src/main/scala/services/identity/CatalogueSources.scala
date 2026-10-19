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

  /** Cineamo's film record id ("567m"), which its showings feed attaches to every German showing it lists — how far each
   *  feed's ids and facts are trusted is [[SourceReliability]]'s. */
  val Cineamo: String = "cineamo"

  /** CinemaxX's own Vista film code ("HO00001648"), the fourth piece of every booking URL its venues publish
   *  (`cinemaxx.de/buchtickets/zusammenfassung/<venue>/<code>/<session>`): one code per film across the chain. Read only
   *  at CinemaxX's own venues — partners booking through cinemaxx.de (Kino am Raschplatz, Holi) file other films under
   *  its codes — and never off a multi-film bill ([[MultiFilmBill]]). Even at its own venues a code is now and then
   *  reused for another billing (DE "Edmund und Luzie" / "Pettersson und Findus", recording 38713274617), so it joins
   *  only listings whose titles already agree ([[TitleGated]]). Experiment `experiment/cinemaxx-code`. */
  object CinemaxX {
    val source = "cinemaxx"
    private val Code = """^https?://(?:www\.)?cinemaxx\.de/[^?#]*/(HO\d{8})(?:[/?#].*)?$""".r
    def ownVenue(cinema: models.Cinema): Boolean = cinema.displayName.toLowerCase(java.util.Locale.ROOT).startsWith("cinemaxx ")
    /** The one code `cm`'s booking URLs carry at a CinemaxX venue, when they carry exactly one. */
    def code(cinema: models.Cinema, cm: models.CinemaMovie): Option[String] =
      if (!ownVenue(cinema) || MultiFilmBill.marker(Seq(cm.movie.title)).isDefined || MultiFilmBill.joinsWorks(cm.movie.title)) None
      else cm.showtimes.flatMap(_.bookingUrl).collect { case Code(code) => code }.distinct match {
        case Seq(one) => Some(one)
        case _        => None
      }
  }

  /** Sources whose ids join listings only beside an agreeing title, as a feed's do ([[ConstraintEdges.catalogueKey]]),
   *  though the venue's own: a chain code reused now and then for another billing. Not a block key on its own
   *  ([[TitleLinks]]): beside an agreeing title the title's own key already blocks them together. */
  val TitleGated: Set[String] = Set(CinemaxX.source)

  /** Box offices whose ids number ONE installation's events, not a chain's films: every Spektrix client and every Savoy
   *  system keeps its own sequence, so two venues' ids meet by coincidence. Measured on UK recording 38326582997: 52 of
   *  the 56 Spektrix ids two venues shared named two films (BEAM Hertford's "Mildred Pierce" was Picture House Uckfield's
   *  "Heart of the Beast"); no Savoy id was shared at all, so scoping it loses no join. [[CatalogueId.of]] files such an
   *  id under its venue. */
  val BoxOfficeOwn: Set[String] = Set("spektrix", "savoy")

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
