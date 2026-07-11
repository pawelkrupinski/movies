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
 *  Default (no filters in the URL) → `"Kinowo"` + a short generic
 *  description. With filters → `"Kinowo — filmy <body>"` (`… — films <body>` in
 *  English) where `body` is a comma-separated list of per-filter phrases. The
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

  /** Brand token, used as the prefix of the FILTERED title ("Kinowo — filmy …")
   *  and the suffix of the default city title. */
  val DefaultTitle       = "Kinowo"

  /** Whether this city's deployment renders in Polish (Poland) vs. another
   *  language (English UK, …). All the copy below branches on it. */
  private def isPolish(city: City): Boolean = city.country.language.getLanguage == "pl"

  /** "Repertuar kin w Poznaniu" / "Cinema listings in London" — the city-scoped
   *  heading shared by the default page title ([[defaultTitle]]) and the city
   *  OG-card overlay ([[MovieController.cityOgImage]]). Reads the declined
   *  locative for Polish, "in {City}" for English, off [[City.locativePhrase]]. */
  def cityHeading(city: City): String =
    if (isPolish(city)) s"Repertuar kin ${city.locativePhrase}"
    else                s"Cinema listings ${city.locativePhrase}"

  /** Default (no-filter) `<title>` for a city listing — keyword-first so the tab
   *  and the Google result lead with what people search ("repertuar kin
   *  <miasto>", "godziny seansów" / "cinema listings <city>", "showtimes")
   *  rather than the bare brand. */
  def defaultTitle(city: City): String = {
    val tail = if (isPolish(city)) "godziny seansów na dziś" else "today's showtimes"
    truncate(s"${cityHeading(city)} – $tail | $DefaultTitle", MaxTitle)
  }

  /** Default OG/meta description, parameterized by the city's genitive-plural
   *  label ("…wszystkich poznańskich kin…" / "…all London cinema listings…") and
   *  naming the query-shaped phrases (godziny seansów, na dziś / today's
   *  showtimes) plus the four rating sources. */
  def defaultDescription(city: City): String = {
    val s =
      if (isPolish(city))
        s"Repertuar wszystkich ${city.genitivePluralLabel} kin – godziny seansów na dziś, " +
          s"oceny IMDb, Filmweb, Metacritic i Rotten Tomatoes. Sprawdź, co dziś grają w kinie ${city.locativePhrase}."
      else
        s"All ${city.genitivePluralLabel} cinema listings – today's showtimes, " +
          s"IMDb, Filmweb, Metacritic and Rotten Tomatoes ratings. See what's on today ${city.locativePhrase}."
    truncate(s, MaxDescription)
  }

  val MaxTitle       = 65
  val MaxDescription = 180

  /** Build the meta for `/{city}/` (the repertoire page) given the active URL
   *  filters and the rendered `schedules`. `schedules` supplies the
   *  universe of options for include/exclude inversion; `city` scopes the
   *  cinema universe, the default description, and the language. */
  def forIndex(city: City, query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Meta = {
    val phrases = buildPhrases(city, query, schedules)
    if (phrases.isEmpty) Meta(defaultTitle(city), defaultDescription(city))
    else {
      val body     = phrases.mkString(", ")
      val filmWord = if (isPolish(city)) "filmy" else "films"
      val joined   = s"$DefaultTitle — $filmWord $body"
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
    // Pick the Polish or the English literal for this deployment. Keeps the two
    // languages side by side at each call site (rather than forking the whole
    // builder) and leaves the Polish output byte-identical.
    val pl = isPolish(city)
    def tr(polish: String, english: String): String = if (pl) polish else english

    // Search query first — it's the most specific filter and the user-typed
    // text deserves prime real estate in the share preview.
    parameterOf(query, "q").filter(_.nonEmpty).foreach { q => out += tr(s"„$q”", s"“$q”") }

    parameterOf(query, "date").foreach {
      case "tomorrow" => out += tr("jutro", "tomorrow")
      case "week"     => out += tr("w tym tygodniu", "this week")
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
      includedSingularPreposition = tr("w sali ", "in screen "),
      includedPluralPreposition   = tr("w salach ", "in screens "),
      excludedPreposition         = tr("bez sal ", "without screens "),
      // Drop the "Cinema|" prefix when describing — the same Sala 5 exists
      // across many cinemas, but a single bare room name still reads cleanly
      // in the title and avoids "Cinema City Kinepolis|Sala 5" walls of text.
      display   = key => key.substring(key.indexOf('|') + 1),
      countNoun = tr("sal", "screens"),
    )

    val allCinemas: Set[String] = city.cinemaDisplayNames.toSet
    val cityPills               = city.cinemaPillMap
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cinema"),
      universe = allCinemas,
      includedSingularPreposition = tr("w ", "at "),
      includedPluralPreposition   = tr("w ", "at "),
      excludedPreposition         = tr("bez ", "without "),
      display   = c => cityPills.getOrElse(c, c),
      countNoun = tr("kin", "cinemas"),
    )

    parameterOf(query, "dim").foreach { case d @ ("2D" | "3D") => out += d; case _ => () }
    parameterOf(query, "lang").foreach {
      case "NAP" => out += tr("z napisami", "with subtitles")
      case "DUB" => out += tr("z dubbingiem", "with dubbing")
      case _     => ()
    }
    if (parameterOf(query, "imax").contains("1")) out += "IMAX"
    parameterOf(query, "from").filter(_.matches("\\d{1,2}:\\d{2}")).foreach(f => out += tr(s"od $f", s"from $f"))

    val allCountries = schedules.flatMap(_.movie.countries).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "country"),
      universe = allCountries,
      includedSingularPreposition = tr("z ", "from "),
      includedPluralPreposition   = tr("z ", "from "),
      excludedPreposition         = tr("bez ", "without "),
      display   = identity,
      countNoun = tr("krajów", "countries"),
    )

    val allGenres = schedules.flatMap(_.movie.genres).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "genre"),
      universe = allGenres,
      includedSingularPreposition = tr("gatunku ", "genre "),
      includedPluralPreposition   = tr("z gatunków ", "genres "),
      excludedPreposition         = tr("bez gatunków ", "without genres "),
      display   = identity,
      countNoun = tr("gatunków", "genres"),
    )

    val allDirectors = schedules.flatMap(_.director).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "director"),
      universe = allDirectors,
      includedSingularPreposition = tr("reż. ", "dir. "),
      includedPluralPreposition   = tr("reż. ", "dir. "),
      excludedPreposition         = tr("bez reż. ", "without dir. "),
      display   = identity,
      countNoun = tr("reżyserów", "directors"),
    )

    val allCast = schedules.flatMap(_.cast).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cast"),
      universe = allCast,
      includedSingularPreposition = tr("z ", "with "),
      includedPluralPreposition   = tr("z ", "with "),
      excludedPreposition         = tr("bez ", "without "),
      display   = identity,
      countNoun = tr("aktorów", "actors"),
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
