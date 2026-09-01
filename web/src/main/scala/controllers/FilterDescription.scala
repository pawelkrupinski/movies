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
   *  Polish and Spanish are spelled out; every other language reads ENGLISH.
   *  That is the right answer for the UK and the US, and it is the honest
   *  fallback for Germany, whose copy here has never been translated — a German
   *  deployment has always served "Cinema listings in Berlin", and this change
   *  deliberately leaves that exactly as it was rather than smuggling a
   *  translation into a Spain rollout.
   *
   *  Spanish had to be spelled out rather than left on the fallback, because the
   *  fallback is NOT merely untranslated here: the city phrase is assembled from
   *  an English caption plus [[models.CityGrammar]]'s per-language preposition,
   *  so a Spanish deployment rendered the mixed "Cinema listings en Madrid" — on
   *  every page title, every OG description and every share card. */
  private def tr(city: City)(polish: String, english: String, spanish: String): String =
    city.country.language.getLanguage match {
      case "pl" => polish
      case "es" => spanish
      case _    => english
    }

  /** "Repertuar kin w Poznaniu" / "Cinema listings in London" — the city-scoped
   *  heading shared by the default page title ([[defaultTitle]]) and the city
   *  OG-card overlay ([[MovieController.cityOgImage]]). Reads the declined
   *  locative for Polish, "in {City}" for English, off [[City.locativePhrase]]. */
  def cityHeading(city: City): String = {
    val caption = tr(city)("Repertuar kin", "Cinema listings", "Cartelera de cine")
    s"$caption ${city.locativePhrase}"
  }

  /** Default (no-filter) `<title>` for a city listing — keyword-first so the tab
   *  and the Google result lead with what people search ("repertuar kin
   *  <miasto>", "godziny seansów" / "cinema listings <city>", "showtimes")
   *  rather than the bare brand. */
  def defaultTitle(city: City): String = {
    val tail = tr(city)("godziny seansów na dziś", "today's showtimes", "sesiones de hoy")
    truncate(s"${cityHeading(city)} – $tail | ${brand(city)}", MaxTitle)
  }

  /** Default OG/meta description, parameterized by the city's genitive-plural
   *  label ("…wszystkich poznańskich kin…" / "…all London cinema listings…") and
   *  naming the query-shaped phrases (godziny seansów, na dziś / today's
   *  showtimes) plus the four rating sources. */
  def defaultDescription(city: City): String = {
    val genitiveLabel = city.genitivePluralLabel
    val locative      = city.locativePhrase
    val s =
      if (city.country.language.getLanguage == "pl")
        s"Repertuar wszystkich $genitiveLabel kin – godziny seansów na dziś, " +
          s"oceny IMDb, Filmweb, Metacritic i Rotten Tomatoes. Sprawdź, co dziś grają w kinie $locative."
      else if (city.country.language.getLanguage == "es")
        s"La cartelera de todos los cines de $genitiveLabel – sesiones de hoy, " +
          s"valoraciones de IMDb, Metacritic y Rotten Tomatoes. Mira qué ponen hoy en el cine $locative."
      else
        s"All $genitiveLabel cinema listings – today's showtimes, " +
          s"IMDb, Filmweb, Metacritic and Rotten Tomatoes ratings. See what's on today $locative."
    truncate(s, MaxDescription)
  }

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
      val filmWord = tr(city)("filmy", "films", "películas")
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
    def tr(polish: String, english: String, spanish: String): String =
      FilterDescription.tr(city)(polish, english, spanish)

    // Search query first — it's the most specific filter and the user-typed
    // text deserves prime real estate in the share preview.
    parameterOf(query, "q").filter(_.nonEmpty).foreach { q => out += tr(s"„$q”", s"“$q”", s"«$q»") }

    parameterOf(query, "date").foreach {
      case "tomorrow" => out += tr("jutro", "tomorrow", "mañana")
      case "week"     => out += tr("w tym tygodniu", "this week", "esta semana")
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
      includedSingularPreposition = tr("w sali ", "in screen ", "en la sala "),
      includedPluralPreposition   = tr("w salach ", "in screens ", "en las salas "),
      excludedPreposition         = tr("bez sal ", "without screens ", "sin las salas "),
      // Drop the "Cinema|" prefix when describing — the same Sala 5 exists
      // across many cinemas, but a single bare room name still reads cleanly
      // in the title and avoids "Cinema City Kinepolis|Sala 5" walls of text.
      display   = key => key.substring(key.indexOf('|') + 1),
      countNoun = tr("sal", "screens", "salas"),
    )

    val allCinemas: Set[String] = city.cinemaDisplayNames.toSet
    val cityPills               = city.cinemaPillMap
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cinema"),
      universe = allCinemas,
      includedSingularPreposition = tr("w ", "at ", "en "),
      includedPluralPreposition   = tr("w ", "at ", "en "),
      excludedPreposition         = tr("bez ", "without ", "sin "),
      display   = c => cityPills.getOrElse(c, c),
      countNoun = tr("kin", "cinemas", "cines"),
    )

    parameterOf(query, "dim").foreach { case d @ ("2D" | "3D") => out += d; case _ => () }
    parameterOf(query, "lang").foreach {
      case "NAP" => out += tr("z napisami", "with subtitles", "subtituladas")
      case "DUB" => out += tr("z dubbingiem", "with dubbing", "dobladas")
      case _     => ()
    }
    if (parameterOf(query, "imax").contains("1")) out += "IMAX"
    parameterOf(query, "from").filter(_.matches("\\d{1,2}:\\d{2}")).foreach(f => out += tr(s"od $f", s"from $f", s"desde las $f"))

    val allCountries = schedules.flatMap(_.movie.countries).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "country"),
      universe = allCountries,
      includedSingularPreposition = tr("z ", "from ", "de "),
      includedPluralPreposition   = tr("z ", "from ", "de "),
      excludedPreposition         = tr("bez ", "without ", "sin "),
      display   = identity,
      countNoun = tr("krajów", "countries", "países"),
    )

    val allGenres = schedules.flatMap(_.movie.genres).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "genre"),
      universe = allGenres,
      includedSingularPreposition = tr("gatunku ", "genre ", "del género "),
      includedPluralPreposition   = tr("z gatunków ", "genres ", "de los géneros "),
      excludedPreposition         = tr("bez gatunków ", "without genres ", "sin los géneros "),
      display   = identity,
      countNoun = tr("gatunków", "genres", "géneros"),
    )

    val allDirectors = schedules.flatMap(_.director).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "director"),
      universe = allDirectors,
      includedSingularPreposition = tr("reż. ", "dir. ", "dir. "),
      includedPluralPreposition   = tr("reż. ", "dir. ", "dir. "),
      excludedPreposition         = tr("bez reż. ", "without dir. ", "sin dir. "),
      display   = identity,
      countNoun = tr("reżyserów", "directors", "directores"),
    )

    val allCast = schedules.flatMap(_.cast).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cast"),
      universe = allCast,
      includedSingularPreposition = tr("z ", "with ", "con "),
      includedPluralPreposition   = tr("z ", "with ", "con "),
      excludedPreposition         = tr("bez ", "without ", "sin "),
      display   = identity,
      countNoun = tr("aktorów", "actors", "actores"),
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
