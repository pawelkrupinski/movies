package models

/** A venue. Closed by its package-private constructor rather than `sealed`: nothing matches over the venues, so
 *  `sealed` only pinned their declarations into this file, and only `models` may declare one -- the Polish
 *  `case object`s in the `*Cinemas.scala` files beside it, the networks in [[CinemaNetworks]], the data-driven
 *  rosters' classes below. */
abstract class Cinema private[models] (val displayName: String, val pillName: String) extends Source {
  /** Whether the venue screens all year or only in a season ([[Opening]]): generated from
   *  its name for the data-driven rosters (`data/scripts/seasonal_venues.py`), overridden
   *  by hand for a seasonal Polish venue (in its `*Cinemas.scala` file) or a UK one (below).
   *
   *  A member, NOT a defaulted constructor parameter: a default lives on the `Cinema`
   *  companion, so every case object's initialiser would start `Cinema`'s — the roster
   *  object `GermanRoster` reads while it builds — and two threads entering from either
   *  end deadlock on class initialisation (seen in a parallel test run). */
  def opening: Opening = Opening.YearRound
  /** Stable kebab-case id for per-cinema title rules (`TitleNormalizer.cinemaClean`).
   *  Derived from the case-object name so a shared portal client (Bilety24,
   *  SystemBiletowy, Ekobilet, …) can clean each venue it serves without a
   *  hand-maintained map: `KinoOskard` → `"kino-oskard"`, `KinoNaStarowce` →
   *  `"kino-na-starowce"`. */
  lazy val slug: String =
    toString.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(java.util.Locale.ROOT)
}

// ── United Kingdom (Flicks and the venues' own sites) — data-driven ─────────
// `UkRoster` builds every UK venue once, from the table data/uk/scripts/generate_roster.py writes
// out of data/uk/roster.json, keyed on the name of the `case object` it used to be: code and tests
// address a venue as `UkRoster("ActOneActon")`. Which page lists it is `UkPages`, and what scrapes
// it `services.cinemas.uk.UkSourcesData`, both read from tables generated alongside.
// `name` is that Scala name, and stands in for the case object's `toString` and `hashCode`:
// `slug` (title rules, metric labels) is derived from it, and every hash-keyed collection of
// cinemas iterates as it did.
final class UkCinema(venueDisplayName: String, venuePillName: String, name: String, override val opening: Opening)
  extends Cinema(venueDisplayName, venuePillName) {
  def this(venueDisplayName: String, venuePillName: String, name: String) = this(venueDisplayName, venuePillName, name, Opening.YearRound)
  override def toString: String = name
  override def hashCode: Int = name.hashCode
}

// ── The data-driven rosters' venues (Germany, the United States, Spain) ──────────
// One instance per venue and identity equality, like a `case object` — but a HASH OF ITS CONTENT, as a
// `case object`'s (its name's) and a `UkCinema`'s are. Object's own `hashCode` is the JVM's identity hash,
// which differs between JVMs (and between two runs of one): every hash-keyed collection of these venues --
// a film's `data: Map[Source, SourceData]` first -- iterated in a different order in each, and whatever read
// that order (the first of two tied listings, a first-wins merge) decided differently on identical inputs.
// Germany's order passes, once in separate JVMs, disagreed on 6 films' TMDB title searches exactly so
// (`MovieService.measureTitleSearches` measuring "Der Fremde" under "The Stranger" in one JVM and
// "L'Etranger" in the other). The display name is unique across every roster (`Cinema.displayNamesClaimedBy`).
sealed abstract class RosterCinema(venueDisplayName: String, venuePillName: String)
  extends Cinema(venueDisplayName, venuePillName) {
  final override def hashCode: Int = displayName.hashCode
}

// ── Germany (Filmstarts) — data-driven from the full roster ──────────────────
// `GermanRosterData` (generated from data/germany/pages.json + venues.json) carries
// 436 town and cluster pages / 1,515 cinemas. Each venue is a `GermanCinema` instance
// built ONCE in `GermanRoster` and reused everywhere (the page's cinemas,
// `Cinema.byCity`, and the scrape catalog) — so the same `Source` instance is used
// throughout and identity equality holds like the hand-authored `case object` cinemas did.
final class GermanCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends RosterCinema(venueDisplayName, venuePillName)

// ── United States (Flicks) — data-driven from the full roster ────────────────
// `UsRosterData` (generated from data/us/pages.json) carries 1,995 town and
// cluster pages / 4,999 cinemas. Same shape as `GermanCinema`: each venue is
// built ONCE in `UsRoster` and reused everywhere, so one `Source` instance
// serves the page, `Cinema.byCity` and the scrape catalog, and identity
// equality holds like the hand-authored `case object` cinemas.
final class UsCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends RosterCinema(venueDisplayName, venuePillName)

// ── Spain (SensaCine) — data-driven from the full roster ─────────────────────
// `SpanishRosterData` (read from tables generated from data/spain/pages.json + provinces.json + ocine.json)
// carries 240 town and cluster pages / 597 cinemas. Same shape as `GermanCinema` and
// `UsCinema`: each venue is built ONCE in `SpanishRoster` and reused everywhere, so one
// `Source` instance serves the page, `Cinema.byCity` and the scrape catalog, and
// identity equality holds like the hand-authored `case object` cinemas.
final class SpanishCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends RosterCinema(venueDisplayName, venuePillName)

object Cinema {
  /** The display names a generated roster may NOT reuse: every Polish and UK venue's
   *  ([[polishAndUk]]), every network pseudo-cinema's, and every venue of the rosters
   *  built before it (`earlier`) — see `GermanRoster.claimedElsewhere`. */
  private[models] def displayNamesClaimedBy(earlier: Seq[(String, Seq[Cinema])]*): Set[String] =
    ((polishAndUk +: earlier).flatMap(_.flatMap(_._2)) ++ CinemaNetworks.all).map(_.displayName).toSet

  /** The chains' network pseudo-cinemas — [[CinemaNetworks.all]], under the name `services.identity` reads. */
  val Networks: Seq[Cinema] = CinemaNetworks.all

  /** The 41 major Polish cities, by label, each the slug its venues are filed under in
   *  [[PolishPages.majorCinemas]] — in page order, which doubles as the per-source merge priority
   *  (`Source.all`): Poznań's venues, the original ten, keep their resolved precedence. A label here
   *  is the city's `City` object's nominative. */
  private val majorPolishCities: Seq[(String, String)] = Seq(
    "Poznań"              -> "poznan",
    "Wrocław"             -> "wroclaw",
    "Warszawa"            -> "warszawa",
    "Kraków"              -> "krakow",
    "Łódź"                -> "lodz",
    "Katowice"            -> "katowice",
    "Szczecin"            -> "szczecin",
    "Białystok"           -> "bialystok",
    "Trójmiasto"          -> "trojmiasto",
    "Bydgoszcz"           -> "bydgoszcz",
    "Lublin"              -> "lublin",
    "Częstochowa"         -> "czestochowa",
    "Radom"               -> "radom",
    "Sosnowiec"           -> "sosnowiec",
    "Toruń"               -> "torun",
    "Kielce"              -> "kielce",
    "Rzeszów"             -> "rzeszow",
    "Gliwice"             -> "gliwice",
    "Zabrze"              -> "zabrze",
    "Olsztyn"             -> "olsztyn",
    "Bielsko-Biała"       -> "bielsko-biala",
    "Opole"               -> "opole",
    "Rybnik"              -> "rybnik",
    "Gorzów Wielkopolski" -> "gorzow-wielkopolski",
    "Elbląg"              -> "elblag",
    "Koszalin"            -> "koszalin",
    "Kalisz"              -> "kalisz",
    "Zielona Góra"        -> "zielona-gora",
    "Tychy"               -> "tychy",
    "Wałbrzych"           -> "walbrzych",
    "Tarnów"              -> "tarnow",
    "Włocławek"           -> "wloclawek",
    "Legnica"             -> "legnica",
    "Płock"               -> "plock",
    "Bytom"               -> "bytom",
    "Dąbrowa Górnicza"    -> "dabrowa-gornicza",
    "Nowy Sącz"           -> "nowy-sacz",
    "Słupsk"              -> "slupsk",
    "Jelenia Góra"        -> "jelenia-gora",
    "Przemyśl"            -> "przemysl",
    "Konin"               -> "konin",
  )

  /** Poland's and the UK's venues — named before any generated roster's. Split out from [[byCity]] so
   *  `GermanRoster` can see which display names are already claimed before it materialises
   *  the generated roster; see `GermanRoster.claimedElsewhere`. Declared BEFORE `byCity`
   *  so it is assigned by the time that field forces the roster. */
  val polishAndUk: Seq[(String, Seq[Cinema])] =
    majorPolishCities.map { case (label, slug) => label -> PolishPages.majorCinemas(slug) } ++
    PolishPages.rows.map(r => r.nominative -> r.cinemas) ++
    (("London" -> UkPages.london.cinemas) +: UkPages.rows.map(r => ukLabel(r) -> r.cinemas))

  /** A UK page's label here, unique as [[byCity]]'s must be: the page's name,
   *  with its county where two pages share one — the two Bangors are both an area. */
  private def ukLabel(r: UkPages.Row): String =
    if (UkPages.rows.count(_.nominative == r.nominative) > 1) s"${r.nominative} (${r.county})" else r.nominative

  /** Every city's venues in page order, paired with the city's display label.
   *  Single source of truth for `all` and for the uptime page's per-city
   *  grouping — add a city here and both pick it up.
   *
   *  LAZY, along with everything derived from it, and that is load-bearing rather than a
   *  micro-optimisation. `GermanRoster` reads `Cinema.polishAndUk` while building itself,
   *  and `UsRoster` reads `polishAndUk` AND `GermanRoster.byCity`, so `Cinema` and both
   *  rosters initialise each other. Forcing the German roster from
   *  a strict field here made the outcome depend on which object the JVM happened to touch
   *  first: enter through `Cinema` and `polishAndUk` is assigned in time, enter through
   *  `GermanRoster` and it reads a null, dying with `ExceptionInInitializerError`. That is
   *  why `CinemaScraperCatalogSpec` could not be run on its own — it passed only inside a
   *  full suite where some earlier spec happened to touch `Cinema` first. Deferring the
   *  roster means entering through `GermanRoster` completes `Cinema`'s own fields without
   *  re-entering the roster, so either entry point works. */
  lazy val byCity: Seq[(String, Seq[Cinema])] =
    polishAndUk ++
      GermanRoster.byCity ++  // Germany: by Bundesland, the level above its 436 town pages (data-driven)
      UsRoster.byCity ++      // USA: 55 states/territories, the level its venues are grouped at
      SpanishRoster.byCity   // Spain: by province, the level above its 240 town pages (data-driven)

  lazy val all: Seq[Cinema] = byCity.flatMap(_._2)

  /** Resolve a cinema from the `displayName` it is stored under — the wire key
   *  every per-cinema row uses (`movie_slots`, `screenings`, `cinema_scrapes`).
   *  Unambiguous by construction: `GermanRoster` qualifies any generated venue
   *  whose name would collide with a Polish or UK one, and `SourceWireKeySpec`
   *  fails the build on a duplicate. */
  lazy val byDisplayName: Map[String, Cinema] = all.map(c => c.displayName -> c).toMap

  /** City display label for each cinema — derived from `byCity`, the single
   *  source of truth. Used by the debug source-data view to disambiguate
   *  same-named chains (e.g. the many "Helios" venues across cities). */
  lazy val cityByCinema: Map[Cinema, String] =
    byCity.flatMap { case (city, cinemas) => cinemas.map(_ -> city) }.toMap

  def cityOf(cinema: Cinema): Option[String] = cityByCinema.get(cinema)
}
