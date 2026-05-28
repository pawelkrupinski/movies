package controllers

import models.Cinema

/** Translates the URL filter state on `/` into a Polish page title +
 *  Open Graph description. Filters are written by the in-page JS
 *  (`syncFiltersToURL`) as `?date=…&room=…&country=…&…`; this helper is
 *  the server-side inverse so link-preview crawlers (Facebook in particular)
 *  see the filtered phrasing in the OG tags without running JS.
 *
 *  Default (no filters in the URL) → `"Kinowo"` + a short generic
 *  description. With filters → `"Kinowo — filmy <body>"` where `body` is a
 *  comma-separated list of per-filter phrases. The title is truncated to
 *  `MaxTitle` (FB/Google sweet spot), the description to `MaxDescription`.
 *
 *  Inclusion vs exclusion: multi-checkbox filters (room, cinema, country,
 *  director, cast) encode the UNCHECKED items in the URL — convenient when
 *  the user un-ticks a few, awkward when the user ticks only a few. The
 *  helper picks the smaller of the two sets and uses the natural Polish
 *  preposition (`w …` / `bez …`, `z …` / `bez krajów …`, …) so a "only Sala
 *  5" share lands as `filmy w Sala 5`, not `filmy bez 30 sal`.
 */
object FilterDescription {

  case class Meta(title: String, description: String)

  val DefaultTitle       = "Kinowo"
  val DefaultDescription =
    "Repertuar wszystkich poznańskich kin w jednym miejscu — z ocenami IMDb, RT i Filmweb."

  val MaxTitle       = 65
  val MaxDescription = 180

  /** Build the meta for `/` (the repertoire page) given the active URL
   *  filters and the rendered `schedules`. `schedules` supplies the
   *  universe of options for include/exclude inversion. */
  def forIndex(query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Meta = {
    val phrases = buildPhrases(query, schedules)
    if (phrases.isEmpty) Meta(DefaultTitle, DefaultDescription)
    else {
      val body  = phrases.mkString(", ")
      // Em-dash matches the existing "Kino X — Repertuar kinowy Poznań"
      // styling used by the /kina template; reads cleanly in the FB preview
      // strip where the title is the prominent line.
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

  private def buildPhrases(query: Map[String, Seq[String]], schedules: Seq[FilmSchedule]): Seq[String] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[String]

    // Search query first — it's the most specific filter and the user-typed
    // text deserves prime real estate in the share preview.
    paramOf(query, "q").filter(_.nonEmpty).foreach { q => out += s"„$q”" }

    paramOf(query, "date").foreach {
      case "tomorrow" => out += "jutro"
      case "week"     => out += "w tym tygodniu"
      case "anytime"  => out += "kiedykolwiek"
      case iso if iso.matches("\\d{4}-\\d{2}-\\d{2}") => out += iso
      case "today" => () // default — silent
      case _       => ()
    }

    val allRooms: Set[(String, String)] = schedules
      .flatMap(_.showings.flatMap(_._2))
      .flatMap(cs => cs.showtimes.flatMap(_.room.map(r => (cs.cinema.displayName, r))))
      .toSet
    out ++= multiPhrase(
      excluded = listOf(query, "room"),
      universe = allRooms.map { case (c, r) => s"$c|$r" },
      includedPreposition = "w sali ",
      includedPluralPreposition = "w salach ",
      excludedPreposition = "bez sal ",
      // Drop the cinema prefix when describing — the same Sala 5 exists
      // across many cinemas, but a single bare room name still reads cleanly
      // in the title and avoids "Cinema City Kinepolis|Sala 5" walls of text.
      display = key => key.substring(key.indexOf('|') + 1),
      countNoun = "sal"
    )

    val allCinemas: Set[String] = Cinema.all.map(_.displayName).toSet
    out ++= multiPhrase(
      excluded = listOf(query, "cinema"),
      universe = allCinemas,
      includedPreposition = "w ",
      includedPluralPreposition = "w ",
      excludedPreposition = "bez ",
      display = c => Cinema.pillMap.getOrElse(c, c),
      countNoun = "kin"
    )

    paramOf(query, "dim").foreach { case d @ ("2D" | "3D") => out += d; case _ => () }
    paramOf(query, "lang").foreach {
      case "NAP" => out += "z napisami"
      case "DUB" => out += "z dubbingiem"
      case _     => ()
    }
    if (paramOf(query, "imax").contains("1")) out += "IMAX"
    paramOf(query, "from").filter(_.matches("\\d{1,2}:\\d{2}")).foreach(f => out += s"od $f")

    val allCountries  = schedules.flatMap(_.movie.countries).toSet
    out ++= multiPhrase(
      excluded = listOf(query, "country"),
      universe = allCountries,
      includedPreposition  = "z ",
      includedPluralPreposition = "z ",
      excludedPreposition = "bez ",
      display = identity,
      countNoun = "krajów"
    )

    val allDirectors = schedules.flatMap(_.director).toSet
    out ++= multiPhrase(
      excluded = listOf(query, "director"),
      universe = allDirectors,
      includedPreposition = "reż. ",
      includedPluralPreposition = "reż. ",
      excludedPreposition = "bez reż. ",
      display = identity,
      countNoun = "reżyserów"
    )

    val allCast = schedules.flatMap(_.cast).toSet
    out ++= multiPhrase(
      excluded = listOf(query, "cast"),
      universe = allCast,
      includedPreposition = "z ",
      includedPluralPreposition = "z ",
      excludedPreposition = "bez ",
      display = identity,
      countNoun = "aktorów"
    )

    out.toSeq
  }

  /** Express a multi-checkbox filter as one phrase using the smaller set
   *  ("only these three" vs "all but these three"). Returns 0 or 1 phrase. */
  private def multiPhrase(
    excluded: Set[String],
    universe: Set[String],
    includedPreposition: String,
    includedPluralPreposition: String,
    excludedPreposition: String,
    display: String => String,
    countNoun: String,
  ): Option[String] = {
    if (excluded.isEmpty || universe.isEmpty) return None
    val included = universe.diff(excluded)
    if (included.isEmpty) return None
    val pickIncluded = included.size <= excluded.size
    val (set, prep) =
      if (pickIncluded) (included, if (included.size == 1) includedPreposition else includedPluralPreposition)
      else              (excluded, excludedPreposition)
    val items = set.map(display).filter(_.nonEmpty).toSeq.sorted
    if (items.isEmpty) None
    else if (items.size <= 3) Some(prep + items.mkString(", "))
    else Some(s"${set.size} $countNoun")  // user picked many — summarise rather than dump
  }

  private def paramOf(query: Map[String, Seq[String]], key: String): Option[String] =
    query.get(key).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)

  private def listOf(query: Map[String, Seq[String]], key: String): Set[String] =
    paramOf(query, key).map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)
}
