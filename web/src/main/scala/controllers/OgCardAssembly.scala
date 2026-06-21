package controllers

import services.movies.TitleNormalizer

/** View-model / payload assembly for the share-preview surfaces — the film
 *  page's `og:description` text and the two Open Graph card images (per-film
 *  and per-city). Pulled out of [[MovieController]] so the controller is left
 *  with HTTP concerns and this stays a pure, independently-testable mapping
 *  from a [[FilmSchedule]] to the renderer inputs ([[tools.OgCardService]] /
 *  [[tools.CityOgCardService]]). No injected collaborators — every method is a
 *  deterministic function of its arguments. */
object OgCardAssembly {

  /** Build the `og:description` / `twitter:description` text for the film
   * page. Format: rating summary ("IMDb 8.7 · RT 86% · Metacritic 79 ·
   * Filmweb 7.5") prefixed to the synopsis, truncated to keep WhatsApp /
   * Messenger / Telegram previews readable. Skips ratings that aren't set;
   * the whole string may be empty for films with no enrichment + no
   * synopsis. */
  def previewDescription(film: FilmSchedule): String = {
    val ratings  = ratingTokens(film).mkString(" · ")
    // og:description is plain text — drop the markdown emphasis markers.
    val synopsis = tools.SynopsisMarkdown.strip(film.synopsis.getOrElse("")).trim
    val joined =
      if (ratings.nonEmpty && synopsis.nonEmpty) ratings + " — " + synopsis
      else if (ratings.nonEmpty) ratings
      else synopsis
    // 300 chars is the practical cap most preview UIs render before
    // truncating; we add an ellipsis to make truncation visible.
    if (joined.length > 300) joined.take(297) + "…" else joined
  }

  /** The rating summary as individual tokens ("IMDb 8.8", "RT 87%", …),
   *  skipping sources that aren't set. Feeds the text `og:description`
   *  ([[previewDescription]], joined with " · "). */
  def ratingTokens(film: FilmSchedule): Seq[String] = {
    val ratings = film.resolved.ratings
    Seq(
      ratings.imdb.map(x => f"IMDb $x%.1f"),
      ratings.rottenTomatoes.map(s => s"RT $s%"),
      ratings.metascore.map(s => s"Metacritic $s"),
      ratings.filmweb.map(x => f"Filmweb $x%.1f")
    ).flatten
  }

  /** The rating badges for the OG-card image — the same per-source brand-coloured
   *  two-segment pills the web/iOS/Android render (see [[tools.OgCardRenderer.ratingBadges]]). */
  def cardRatingBadges(film: FilmSchedule): Seq[tools.OgCardRenderer.Badge] =
    tools.OgCardRenderer.ratingBadges(
      imdb           = film.resolved.ratings.imdb,
      metascore      = film.resolved.ratings.metascore,
      rottenTomatoes = film.resolved.ratings.rottenTomatoes,
      filmweb        = film.resolved.ratings.filmweb
    )

  /** The "2026 · Dramat, Kryminał" line under the title on the OG card —
   *  release year then genres, each part omitted when absent. */
  def cardSubtitle(film: FilmSchedule): String =
    (film.movie.releaseYear.map(_.toString).toSeq ++ Seq(film.movie.genres.mkString(", ")).filter(_.nonEmpty))
      .mkString(" · ")

  /** The directors as a single "Name, Name" string for the OG card's
   *  "Reżyseria: …" line, or None when no director is known. */
  def cardDirector(film: FilmSchedule): Option[String] =
    Some(film.director.mkString(", ")).filter(_.nonEmpty)

  /** Pick the city OG card's films so no poster shows twice. Drops a row whose
   *  EITHER (a) upstream search key — so a base showing and its "Poranki:" /
   *  accessibility / "+ Q&A" variant collapse to one — OR (b) poster URL — so
   *  unrelated films that share one image (a retrospective on a single generic
   *  placeholder) don't both appear — was already taken. Keeps the first
   *  (earliest-showing) row. */
  def distinctByMovie(schedules: Seq[FilmSchedule]): Seq[FilmSchedule] = {
    val seenKeys, seenPosters = scala.collection.mutable.Set.empty[String]
    schedules.filter { s =>
      val key    = TitleNormalizer.apiQuery(s.movie.title).toLowerCase
      val poster = s.posterUrl.map(_.trim).filter(_.nonEmpty)
      if (seenKeys(key) || poster.exists(seenPosters)) false
      else { seenKeys += key; poster.foreach(seenPosters += _); true }
    }
  }

  /** The `count` films to show in the city card on `epochDay`: dedup, keep the
   *  ones with a poster, then rotate a `count`-per-day window through the FULL
   *  poster-bearing repertoire — so a different (and, day to day, non-overlapping)
   *  set of posters shows each day, cycling through every film over time,
   *  deterministically (stable within a day, fresh the next). */
  def dailyCardFilms(schedules: Seq[FilmSchedule], epochDay: Long, count: Int): Seq[FilmSchedule] = {
    val pool = distinctByMovie(schedules).filter(_.posterUrl.exists(_.nonEmpty))
    if (pool.isEmpty) Nil
    else {
      val start = Math.floorMod(epochDay * count, pool.length.toLong).toInt
      (pool.drop(start) ++ pool.take(start)).take(count)
    }
  }

  /** Build one page-like column ([[tools.CityCardFilm]]) for the dynamic city OG
   *  card: the small meta pills (runtime / year / up to two genres), the rating
   *  pills, and the soonest day's per-cinema showtime chips — capped (2 cinemas
   *  × 6 chips) so the rendered card stays bounded. */
  def toCityCardFilm(film: FilmSchedule): tools.CityCardFilm = {
    val meta =
      film.movie.runtimeMinutes.map(CardFormat.runtimeText).toSeq ++
      film.movie.releaseYear.map(_.toString).toSeq ++
      film.movie.genres.take(2)
    val (dayLabel, cinemas) = film.showings.headOption.map { case (date, css) =>
      CardFormat.date(date) -> css.take(2).map { cs =>
        cs.cinema.displayName -> cs.showtimes.take(6).map { st =>
          val time = CardFormat.time(st.dateTime)
          st.format.headOption.filter(_.nonEmpty).fold(time)(fmt => s"$time $fmt")
        }
      }
    }.getOrElse("" -> Seq.empty)
    // Primary poster first, then the cinema fallbacks — the primary is often a
    // Multikino origin Cloudflare 403s from our Fly IP (see OgCardService).
    val posterUrls = film.posterUrl.toSeq ++ film.resolved.fallbackPosterUrls
    tools.CityCardFilm(film.movie.title, meta, cardRatingBadges(film), posterUrls, dayLabel, cinemas)
  }
}
