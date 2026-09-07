package controllers

import models.City

/** Translates the URL filter state on `/` into a page title + Open Graph
 *  description, in the deployment's language (Polish for Poland, English for the
 *  UK, …). Filters are written into the URL by the in-page JS — `?date=` on
 *  every day change, and the rest of the panel (`?room=…&country=…&…`) on demand
 *  via `copyFilterLinkToClipboard`. This helper is the server-side inverse so
 *  link-preview crawlers (Facebook in particular) see the filtered phrasing in
 *  the OG tags without running JS.
 *
 *  Default (no filters in the URL) → the brand (`"Kinowo"` in PL, `"Showtimes"`
 *  elsewhere) + a short generic description. With filters → `"Kinowo — filmy
 *  <body>"` (`"Showtimes — films <body>"` in English) where `body` is a
 *  comma-separated list of per-filter phrases. The
 *  title is truncated to `MaxTitle` (FB/Google sweet spot), the description to
 *  `MaxDescription`.
 *
 *  The language is read off the city's country (`city.country.language`) — a
 *  pure locale branch, mirroring [[models.CityGrammar]] / [[DateFormatter]], so
 *  the phrasing stays byte-identical to the pre-i18n Polish and needs no
 *  injected `Messages`.
 *
 *  URL semantics for multi-checkbox filters (room, cinema, country, genre,
 *  director, cast): the values listed are the INCLUDED items (the boxes the user has
 *  ticked). `?room=Sala+5` means "show only Sala 5", matching the user's
 *  mental model when pasting/sharing a URL. The helper picks the smaller of
 *  the included / excluded sets and uses the natural preposition for the
 *  language (`w …` / `bez …` → `in …` / `without …`, …) — so "only Sala 5" lands
 *  as `filmy w sali Sala 5` (`films in screen Sala 5`), "all but Multikino" as
 *  `filmy bez Multikino` (`films without Multikino`).
 *
 *  Cinema URL encodes ENABLED cinemas (matching JS — the LS-backed
 *  `disabledCinemas` is the complement and is recomputed on boot).
 */
object FilterDescription {

  case class Meta(title: String, description: String)

  /** The customer-facing brand for this city's deployment — "Kinowo" in Poland,
   *  "Showtimes" elsewhere ([[models.Country.brandName]]). Used as the prefix of
   *  the FILTERED title ("Kinowo — filmy …" / "Showtimes — films …") and the
   *  suffix of the default city title. */
  private def brand(city: City): String = city.country.brandName

  /** Pick this deployment's literal for a phrase written out in each language.
   *
   *  Every country whose language is not English is spelled out; English is what
   *  the UK and the US read, and it stays the fallback for a country onboarded
   *  before its copy is written.
   *
   *  Leaving a language on that fallback is not merely "untranslated": the city
   *  phrase is assembled from an English caption plus [[models.CityGrammar]]'s
   *  per-language preposition, so Spain once rendered the mixed "Cinema listings
   *  en Madrid", and Germany served English titles under a `lang="de"` document
   *  and a German OG locale — on every page title, every meta description, every
   *  share card and the city's structured data. A new country's phrases belong
   *  here at onboarding, not after. */
  private def tr(city: City)(polish: String, english: String, spanish: String, german: String): String =
    city.country.language.getLanguage match {
      case "pl" => polish
      case "es" => spanish
      case "de" => german
      case _    => english
    }

  /** "Repertuar kin w Poznaniu" / "Cinema listings in London" — the city-scoped
   *  heading shared by the default page title ([[defaultTitle]]) and the city
   *  OG-card overlay ([[MovieController.cityOgImage]]). Reads the declined
   *  locative for Polish, "in {City}" for English, off [[City.locativePhrase]]. */
  def cityHeading(city: City): String = {
    val caption = tr(city)("Repertuar kin", "Cinema listings", "Cartelera de cine", "Kinoprogramm")
    s"$caption ${city.locativePhrase}"
  }

  /** Default (no-filter) `<title>` for a city listing — keyword-first so the tab
   *  and the Google result lead with what people search ("repertuar kin
   *  <miasto>", "godziny seansów" / "cinema listings <city>", "showtimes")
   *  rather than the bare brand. */
  def defaultTitle(city: City): String = {
    val tail = tr(city)("godziny seansów na dziś", "today's showtimes", "sesiones de hoy", "Spielzeiten heute")
    truncate(s"${cityHeading(city)} – $tail | ${brand(city)}", MaxTitle)
  }

  /** Default OG/meta description, parameterized by the city's genitive-plural
   *  label ("…wszystkich poznańskich kin…" / "…all London cinema listings…") and
   *  naming the query-shaped phrases (godziny seansów, na dziś / today's
   *  showtimes) plus the four rating sources. */
  def defaultDescription(city: City): String = {
    val towns = namedPlaces(city)
    // Name as many towns as the budget actually fits, not a fixed number.
    // `truncate` cuts at MaxDescription, so a town too many does not just cost
    // the last town — it cuts one in half and eats everything behind it,
    // which is the rating sources. Poland found this in production: Tarnów's
    // six towns ended the sentence on "Metacritic i Rotten…".
    towns.indices.reverse.map(n => describe(city, towns.take(n + 1)))
      .find(_.length <= MaxDescription)
      .getOrElse(truncate(describe(city, Nil), MaxDescription))
  }

  /** The default description naming exactly these towns, at whatever length that
   *  comes to. [[defaultDescription]] picks how many actually fit. */
  private def describe(city: City, towns: Seq[String]): String = {
    val genitiveLabel = city.genitivePluralLabel
    val locative      = city.locativePhrase
    // A multi-town page names its towns INSTEAD of closing with the "what's on
    // today" sentence: both together run past MaxDescription, and of the two it
    // is the towns that a search for "kino Sopot" can actually match. A one-town
    // city (almost all of them) has no towns to name and keeps the sentence,
    // byte for byte.
    val places = if (towns.isEmpty) "" else towns.mkString(" (", ", ", ")")
    if (city.country.language.getLanguage == "pl")
      s"Repertuar wszystkich $genitiveLabel kin$places – godziny seansów na dziś, " +
        s"oceny IMDb, Filmweb, Metacritic i Rotten Tomatoes." +
        (if (towns.isEmpty) s" Sprawdź, co dziś grają w kinie $locative." else "")
    else if (city.country.language.getLanguage == "es")
      s"La cartelera de todos los cines de $genitiveLabel$places – sesiones de hoy, " +
        s"valoraciones de IMDb, Metacritic y Rotten Tomatoes." +
        (if (towns.isEmpty) s" Mira qué ponen hoy en el cine $locative." else "")
    else if (city.country.language.getLanguage == "de")
      s"Das Kinoprogramm aller Kinos in $genitiveLabel$places – Spielzeiten heute, " +
        s"Bewertungen von IMDb, Metacritic und Rotten Tomatoes." +
        (if (towns.isEmpty) s" Sieh nach, was heute $locative im Kino läuft." else "")
    else
      s"All $genitiveLabel cinema listings$places – today's showtimes, " +
        s"IMDb, Filmweb, Metacritic and Rotten Tomatoes ratings." +
        (if (towns.isEmpty) s" See what's on today $locative." else "")
  }

  /** The page's `<h1>` — the city heading, plus the towns a multi-town listing
   *  covers. The listing has no other heading at all (the design opens straight
   *  onto the card grid), so this is the one place the city is marked up as what
   *  the page is ABOUT rather than only as a `<title>` and a meta tag — and for
   *  `/trojmiasto/` or a US metro, the only heading text a search for one of the
   *  covered towns can match. */
  def pageHeading(city: City): String = namedPlaces(city) match {
    case Nil   => cityHeading(city)
    case towns => s"${cityHeading(city)} – ${towns.mkString(", ")}"
  }

  /** The covered towns this page is willing to spell out. Capped because a
   *  region can cover dozens and both the heading and the description have a
   *  budget; [[models.City.coveredPlaces]] orders them biggest-first, so the cap
   *  keeps the ones worth naming. */
  private def namedPlaces(city: City): Seq[String] = city.otherCoveredPlaces.take(MaxNamedPlaces)

  /** The most covered towns either the heading or the description will name. A
   *  ceiling, not a target: the description then keeps only as many as fit
   *  [[MaxDescription]], which for long Polish names is fewer. */
  val MaxNamedPlaces = 6

  val MaxTitle       = 65
  val MaxDescription = 180

  /** Build the meta for `/{city}/` (the repertoire page) given the active URL
   *  filters and the rendered `schedules`. `schedules` supplies the
   *  universe of options for include/exclude inversion; `city` scopes the
   *  cinema universe, the default description, and the language. */
  def forIndex(city: City, query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Meta =
    filtered(city, query, schedules).getOrElse(Meta(defaultTitle(city), defaultDescription(city)))

  /** The filtered phrasing, or `None` when the URL carries no filter at all and
   *  the caller should fall back to its own default. */
  private def filtered(city: City, query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Option[Meta] = {
    val phrases = buildPhrases(city, query, schedules)
    Option.when(phrases.nonEmpty) {
      val body     = phrases.mkString(", ")
      val filmWord = tr(city)("filmy", "films", "películas", "Filme")
      val joined   = s"${brand(city)} — $filmWord $body"
      Meta(truncate(joined, MaxTitle), truncate(joined, MaxDescription))
    }
  }

  /** Trim at the nearest word boundary below `max`, with an ellipsis when
   *  the string was actually shortened. Word-boundary trimming keeps mid-
   *  word truncations out of the FB preview (`"filmy w Sala…"` reads better
   *  than `"filmy w Sa…"`). */
  private def truncate(s: String, max: Int): String = {
    if (s.length <= max) return s
    val sliced = s.take(max - 1)
    val cut    = sliced.lastIndexOf(' ')
    val head   = if (cut > max / 2) sliced.substring(0, cut) else sliced
    head.stripSuffix(",").stripSuffix(" ") + "…"
  }

  private def buildPhrases(city: City, query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Seq[String] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[String]
    // Keeps every language's literal side by side at the call site rather than
    // forking the whole builder, and leaves the Polish and English output
    // byte-identical. Shadows the outer helper of the same name so the `city`
    // argument isn't repeated thirty times.
    def tr(polish: String, english: String, spanish: String, german: String): String =
      FilterDescription.tr(city)(polish, english, spanish, german)

    // Search query first — it's the most specific filter and the user-typed
    // text deserves prime real estate in the share preview.
    parameterOf(query, "q").filter(_.nonEmpty).foreach { q => out += tr(s"„$q”", s"“$q”", s"«$q»", s"„$q“") }

    parameterOf(query, "date").foreach {
      case "tomorrow" => out += tr("jutro", "tomorrow", "mañana", "morgen")
      case "week"     => out += tr("w tym tygodniu", "this week", "esta semana", "diese Woche")
      // `anytime` is the no-restriction view — the description would otherwise
      // read "filmy kiedykolwiek" which says nothing the bare "Kinowo" doesn't
      // already. Silent, same as `today`.
      case "anytime"  => ()
      case iso if iso.matches("\\d{4}-\\d{2}-\\d{2}") => out += iso
      case "today"    => ()
      case _          => ()
    }

    val allRooms: Set[String] = schedules
      .flatMap(_.showings.flatMap(_._2))
      .flatMap(cs => cs.showtimes.flatMap(_.room.map(r => s"${cs.cinema.displayName}|$r")))
      .toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "room"),
      universe = allRooms,
      // German venues name their own rooms "Saal 3" / "Kino 1", so the German
      // preposition is the bare "in " — "in Saal Saal 3" is what a caption noun
      // would produce here.
      includedSingularPreposition = tr("w sali ", "in screen ", "en la sala ", "in "),
      includedPluralPreposition   = tr("w salach ", "in screens ", "en las salas ", "in "),
      excludedPreposition         = tr("bez sal ", "without screens ", "sin las salas ", "ohne "),
      // Drop the "Cinema|" prefix when describing — the same Sala 5 exists
      // across many cinemas, but a single bare room name still reads cleanly
      // in the title and avoids "Cinema City Kinepolis|Sala 5" walls of text.
      display   = key => key.substring(key.indexOf('|') + 1),
      countNoun = tr("sal", "screens", "salas", "Säle"),
    )

    val allCinemas: Set[String] = city.cinemaDisplayNames.toSet
    val cityPills               = city.cinemaPillMap
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cinema"),
      universe = allCinemas,
      includedSingularPreposition = tr("w ", "at ", "en ", "im "),
      includedPluralPreposition   = tr("w ", "at ", "en ", "in "),
      excludedPreposition         = tr("bez ", "without ", "sin ", "ohne "),
      display   = c => cityPills.getOrElse(c, c),
      countNoun = tr("kin", "cinemas", "cines", "Kinos"),
    )

    parameterOf(query, "dim").foreach { case d @ ("2D" | "3D") => out += d; case _ => () }
    // The two version tokens are the COUNTRY's own (`NAP`/`DUB` in Poland,
    // `VOSE`/`DOB` in Spain, `OmU`/`DF` in Germany) — the same pair the Filtry
    // radios are rendered from, so a token spelled for one country never has to
    // be recognised here for another.
    for {
      selected <- parameterOf(query, "lang")
      tokens   <- city.country.versionTokens
    } {
      if (selected == tokens.subtitled)   out += tr("z napisami", "with subtitles", "subtituladas", "mit Untertiteln")
      if (selected == tokens.dubbed)      out += tr("z dubbingiem", "with dubbing", "dobladas", "synchronisiert")
    }
    if (parameterOf(query, "imax").contains("1")) out += "IMAX"
    parameterOf(query, "from").filter(_.matches("\\d{1,2}:\\d{2}")).foreach(f => out += tr(s"od $f", s"from $f", s"desde las $f", s"ab $f"))

    val allCountries = schedules.flatMap(_.movie.countries).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "country"),
      universe = allCountries,
      includedSingularPreposition = tr("z ", "from ", "de ", "aus "),
      includedPluralPreposition   = tr("z ", "from ", "de ", "aus "),
      excludedPreposition         = tr("bez ", "without ", "sin ", "ohne "),
      display   = identity,
      countNoun = tr("krajów", "countries", "países", "Länder"),
    )

    val allGenres = schedules.flatMap(_.movie.genres).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "genre"),
      universe = allGenres,
      includedSingularPreposition = tr("gatunku ", "genre ", "del género ", "aus dem Genre "),
      includedPluralPreposition   = tr("z gatunków ", "genres ", "de los géneros ", "aus den Genres "),
      excludedPreposition         = tr("bez gatunków ", "without genres ", "sin los géneros ", "ohne die Genres "),
      display   = identity,
      countNoun = tr("gatunków", "genres", "géneros", "Genres"),
    )

    val allDirectors = schedules.flatMap(_.director).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "director"),
      universe = allDirectors,
      includedSingularPreposition = tr("reż. ", "dir. ", "dir. ", "Regie: "),
      includedPluralPreposition   = tr("reż. ", "dir. ", "dir. ", "Regie: "),
      excludedPreposition         = tr("bez reż. ", "without dir. ", "sin dir. ", "ohne Regie: "),
      display   = identity,
      countNoun = tr("reżyserów", "directors", "directores", "Regisseure"),
    )

    val allCast = schedules.flatMap(_.cast).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cast"),
      universe = allCast,
      includedSingularPreposition = tr("z ", "with ", "con ", "mit "),
      includedPluralPreposition   = tr("z ", "with ", "con ", "mit "),
      excludedPreposition         = tr("bez ", "without ", "sin ", "ohne "),
      display   = identity,
      countNoun = tr("aktorów", "actors", "actores", "Schauspieler"),
    )

    out.toSeq
  }

  /** Express a multi-checkbox filter as one phrase using the smaller of the
   *  inclusion / exclusion sets — "tylko te trzy" reads better than "wszystkie
   *  z wyjątkiem tych trzydziestu". `included = None` means "parameter absent
   *  from URL = all checked = no filter"; an empty Set means "parameter present
   *  but with zero values = nothing visible" which we still skip in the
   *  description (the page is empty, the OG would read oddly). */
  private def inclusionPhrase(
    included: Option[Set[String]],
    universe: Set[String],
    includedSingularPreposition: String,
    includedPluralPreposition: String,
    excludedPreposition: String,
    display: String => String,
    countNoun: String,
  ): Option[String] = included.flatMap { inc =>
    if (inc.isEmpty || universe.isEmpty) None
    else {
      // Restrict to items we recognise — a URL listing a stale (dropped from
      // the corpus today) room shouldn't be counted toward "all visible".
      val incInUniverse = inc.intersect(universe)
      val excluded      = universe.diff(incInUniverse)
      if (incInUniverse.isEmpty || excluded.isEmpty) None
      else {
        val pickIncluded = incInUniverse.size <= excluded.size
        val (set, prep) =
          if (pickIncluded) (incInUniverse, if (incInUniverse.size == 1) includedSingularPreposition else includedPluralPreposition)
          else              (excluded,      excludedPreposition)
        val items = set.map(display).filter(_.nonEmpty).toSeq.sorted
        if (items.isEmpty) None
        else if (items.size <= 3) Some(prep + items.mkString(", "))
        else Some(s"${set.size} $countNoun") // too many to enumerate — summarise
      }
    }
  }

  private def parameterOf(query: Map[String, Seq[String]], key: String): Option[String] =
    query.get(key).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)

  /** `None` when the parameter is absent (= no filter). `Some(set)` when present,
   *  tolerating both the per-value shape (`?room=A&room=B`) and the legacy
   *  comma-list (`?room=A,B`) so old shared URLs still narrow correctly. */
  private def maybeListOf(query: Map[String, Seq[String]], key: String): Option[Set[String]] =
    query.get(key).map { values =>
      values.iterator.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSet
    }
}
