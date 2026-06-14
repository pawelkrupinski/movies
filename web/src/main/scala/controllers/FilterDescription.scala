package controllers

import models.City

/** Translates the URL filter state on `/` into a Polish page title +
 *  Open Graph description. Filters are written into the URL by the
 *  in-page JS — `?date=` on every day change, and the rest of the panel
 *  (`?room=…&country=…&…`) on demand via `copyFilterLinkToClipboard`.
 *  This helper is the server-side inverse so link-preview crawlers
 *  (Facebook in particular) see the filtered phrasing in the OG tags
 *  without running JS.
 *
 *  Default (no filters in the URL) → `"Kinowo"` + a short generic
 *  description. With filters → `"Kinowo — filmy <body>"` where `body` is a
 *  comma-separated list of per-filter phrases. The title is truncated to
 *  `MaxTitle` (FB/Google sweet spot), the description to `MaxDescription`.
 *
 *  URL semantics for multi-checkbox filters (room, cinema, country, genre,
 *  director, cast): the values listed are the INCLUDED items (the boxes the user has
 *  ticked). `?room=Sala+5` means "show only Sala 5", matching the user's
 *  mental model when pasting/sharing a URL. The helper picks the smaller of
 *  the included / excluded sets and uses the natural Polish preposition
 *  (`w …` / `bez …`, `z …` / `bez krajów …`, …) — so "only Sala 5" lands as
 *  `filmy w sali Sala 5`, "all but Multikino" as `filmy bez Multikino`.
 *
 *  Cinema URL encodes ENABLED cinemas (matching JS — the LS-backed
 *  `disabledCinemas` is the complement and is recomputed on boot).
 */
object FilterDescription {

  case class Meta(title: String, description: String)

  val DefaultTitle       = "Kinowo"
  /** Default OG description, parameterized by the city's genitive-plural label
   *  ("…wszystkich poznańskich kin…"). */
  def defaultDescription(city: City): String =
    s"Repertuar wszystkich ${city.labels.genitivePlural} kin w jednym miejscu — z ocenami IMDb, RT i Filmweb."

  val MaxTitle       = 65
  val MaxDescription = 180

  /** Build the meta for `/{city}/` (the repertoire page) given the active URL
   *  filters and the rendered `schedules`. `schedules` supplies the
   *  universe of options for include/exclude inversion; `city` scopes the
   *  cinema universe and the default description. */
  def forIndex(city: City, query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Meta = {
    val phrases = buildPhrases(city, query, schedules)
    if (phrases.isEmpty) Meta(DefaultTitle, defaultDescription(city))
    else {
      val body  = phrases.mkString(", ")
      val joined = s"$DefaultTitle — filmy $body"
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

    // Search query first — it's the most specific filter and the user-typed
    // text deserves prime real estate in the share preview.
    parameterOf(query, "q").filter(_.nonEmpty).foreach { q => out += s"„$q”" }

    parameterOf(query, "date").foreach {
      case "tomorrow" => out += "jutro"
      case "week"     => out += "w tym tygodniu"
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
      includedSingularPreposition = "w sali ",
      includedPluralPreposition   = "w salach ",
      excludedPreposition         = "bez sal ",
      // Drop the "Cinema|" prefix when describing — the same Sala 5 exists
      // across many cinemas, but a single bare room name still reads cleanly
      // in the title and avoids "Cinema City Kinepolis|Sala 5" walls of text.
      display   = key => key.substring(key.indexOf('|') + 1),
      countNoun = "sal",
    )

    val allCinemas: Set[String] = city.cinemaDisplayNames.toSet
    val cityPills               = city.cinemaPillMap
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cinema"),
      universe = allCinemas,
      includedSingularPreposition = "w ",
      includedPluralPreposition   = "w ",
      excludedPreposition         = "bez ",
      display   = c => cityPills.getOrElse(c, c),
      countNoun = "kin",
    )

    parameterOf(query, "dim").foreach { case d @ ("2D" | "3D") => out += d; case _ => () }
    parameterOf(query, "lang").foreach {
      case "NAP" => out += "z napisami"
      case "DUB" => out += "z dubbingiem"
      case _     => ()
    }
    if (parameterOf(query, "imax").contains("1")) out += "IMAX"
    parameterOf(query, "from").filter(_.matches("\\d{1,2}:\\d{2}")).foreach(f => out += s"od $f")

    val allCountries = schedules.flatMap(_.movie.countries).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "country"),
      universe = allCountries,
      includedSingularPreposition = "z ",
      includedPluralPreposition   = "z ",
      excludedPreposition         = "bez ",
      display   = identity,
      countNoun = "krajów",
    )

    val allGenres = schedules.flatMap(_.movie.genres).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "genre"),
      universe = allGenres,
      includedSingularPreposition = "gatunku ",
      includedPluralPreposition   = "z gatunków ",
      excludedPreposition         = "bez gatunków ",
      display   = identity,
      countNoun = "gatunków",
    )

    val allDirectors = schedules.flatMap(_.director).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "director"),
      universe = allDirectors,
      includedSingularPreposition = "reż. ",
      includedPluralPreposition   = "reż. ",
      excludedPreposition         = "bez reż. ",
      display   = identity,
      countNoun = "reżyserów",
    )

    val allCast = schedules.flatMap(_.cast).toSet
    out ++= inclusionPhrase(
      included = maybeListOf(query, "cast"),
      universe = allCast,
      includedSingularPreposition = "z ",
      includedPluralPreposition   = "z ",
      excludedPreposition         = "bez ",
      display   = identity,
      countNoun = "aktorów",
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
