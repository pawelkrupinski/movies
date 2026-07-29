package services.movies

import models.Showtime

/**
 * Renders WHAT differs between two corpora, for the clue on a failed
 * whole-corpus assertion.
 *
 * Every spec that compares two corpora needs this and none of them can use a
 * plain `shouldBe` message: the values are thousands of records deep, and
 * ScalaTest's rendering of two unequal `Seq[StoredMovieRecord]`s is a wall of
 * text whose first several hundred characters are identical. Diffing field by
 * field is the difference between "these differ somewhere" and "slot X's runtime
 * is 125 here and 98 there".
 *
 * Shared because three specs ask the same question of three different axes —
 * `ScrapeOrderDeterminismSpec` across arrival orders, `ReScrapeIdempotencySpec`
 * across a settle, `CountryConvergenceBehaviour` across both — and a copy per
 * spec drifts into three different ideas of which fields are worth printing.
 */
object CorpusDiff {

  private def short(x: Any): String = {
    val t = x.toString
    if (t.length > 140) s"${t.take(140)}…(len ${t.length})" else t
  }

  /** Field-by-field difference between two record sets, `labelA`/`labelB` naming
   *  the two sides in the output (e.g. "iter0"/"iter3", "before"/"after"). Empty
   *  string when they match. */
  def records(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord],
              labelA: String = "0", labelB: String = "1"): String = {
    val ka = a.map(r => (r.title, r.year)).toSet
    val kb = b.map(r => (r.title, r.year)).toSet
    if (ka != kb) return s"record keys differ: only$labelA=${ka -- kb} only$labelB=${kb -- ka}"
    val byKey = b.map(r => (r.title, r.year) -> r.record).toMap
    a.flatMap { ra =>
      val ea = ra.record; val eb = byKey((ra.title, ra.year))
      val scalars = Seq[(String, Any, Any)](
        ("tmdbId", ea.tmdbId, eb.tmdbId), ("imdbId", ea.imdbId, eb.imdbId),
        ("imdbRating", ea.imdbRating, eb.imdbRating), ("metascore", ea.metascore, eb.metascore),
        ("rottenTomatoes", ea.rottenTomatoes, eb.rottenTomatoes), ("filmwebRating", ea.filmwebRating, eb.filmwebRating),
        ("filmwebUrl", ea.filmwebUrl, eb.filmwebUrl), ("metacriticUrl", ea.metacriticUrl, eb.metacriticUrl),
        ("rottenTomatoesUrl", ea.rottenTomatoesUrl, eb.rottenTomatoesUrl)
      ).collect { case (n, x, y) if x != y => s"  scalar $n: $labelA=${short(x)} $labelB=${short(y)}" }
      val srcDiffs = (ea.data.keySet ++ eb.data.keySet).toSeq.sortBy(_.displayName).flatMap { src =>
        (ea.data.get(src), eb.data.get(src)) match {
          case (Some(da), Some(db)) if da != db =>
            val fields = Seq[(String, Any, Any)](
              ("title", da.title, db.title),
              ("synopsis.len", da.synopsis.map(_.length), db.synopsis.map(_.length)),
              ("cast", da.cast, db.cast), ("director", da.director, db.director),
              ("posterUrl", da.posterUrl, db.posterUrl), ("releaseYear", da.releaseYear, db.releaseYear),
              ("runtimeMinutes", da.runtimeMinutes, db.runtimeMinutes), ("countries", da.countries, db.countries),
              ("genres", da.genres, db.genres), ("filmUrl", da.filmUrl, db.filmUrl),
              ("showtimes.size", da.showtimes.size, db.showtimes.size), ("showtimes", da.showtimes, db.showtimes)
            ).collect { case (n, x, y) if x != y => s"      $n: $labelA=${short(x)} $labelB=${short(y)}" }
            Some(s"  source '${src.displayName}' differs:\n${fields.mkString("\n")}")
          case (Some(_), None) => Some(s"  source '${src.displayName}': only in $labelA")
          case (None, Some(_)) => Some(s"  source '${src.displayName}': only in $labelB")
          case _               => None
        }
      }
      if (scalars.isEmpty && srcDiffs.isEmpty) Nil
      else Seq(s"record '${ra.title}' (${ra.year}):") ++ scalars ++ srcDiffs
    }.mkString("\n")
  }

  /**
   * The first rendered row that differs, pinpointed at the character where the two
   * renderings part company.
   *
   * A rendered row is one enormous `toString`, and printing its first N characters
   * is worse than useless: the divergence that started all this differed in a
   * poster URL some 700 characters in, so both sides printed an identical-looking
   * 400-character prefix and the clue said, in effect, "these two identical things
   * are different". Seeking to the first differing offset and showing a window
   * around it turns that into the field name and both values.
   */
  def rows(a: Seq[Any], b: Seq[Any], labelA: String = "0", labelB: String = "1"): String = {
    val sizes = if (a.size == b.size) "" else s"  sizes differ: $labelA=${a.size} $labelB=${b.size}\n"
    val firstDiff = a.iterator.zip(b.iterator).zipWithIndex
      .collectFirst { case ((x, y), index) if x != y => index -> pinpoint(String.valueOf(x), String.valueOf(y), labelA, labelB) }
    sizes + firstDiff.map { case (index, detail) => s"  row $index differs:\n$detail" }
      .getOrElse("  no row differs pairwise — the difference is in row COUNT or ORDER alone")
  }

  /** A window around the first character two renderings disagree on, so the clue
   *  shows the difference rather than a common prefix that hides it. */
  private def pinpoint(x: String, y: String, labelA: String, labelB: String): String = {
    val common = x.iterator.zip(y.iterator).takeWhile { case (c, d) => c == d }.size
    val from   = math.max(0, common - 70)
    val to     = math.min(math.max(x.length, y.length), common + 90)
    def window(s: String) = s"${if (from > 0) "…" else ""}${s.slice(from, to)}${if (to < s.length) "…" else ""}"
    s"    common prefix: $common chars; first differs at char $common\n" +
    s"    $labelA=${window(x)}\n" +
    s"    $labelB=${window(y)}"
  }

  /** Films whose per-slot showtimes differ, by slot-key sizes. The screenings
   *  collection is keyed `filmId -> slotKey -> showtimes`. */
  def slots(a: Map[String, Map[String, Seq[Showtime]]],
            b: Map[String, Map[String, Seq[Showtime]]],
            labelA: String = "0", labelB: String = "1"): String = {
    val films = (a.keySet ++ b.keySet).filter(f => a.get(f) != b.get(f))
    s"  ${films.size} film(s) differ; first: " + films.toList.sorted.take(3).map { f =>
      s"$f\n    $labelA=${a.getOrElse(f, Map.empty).view.mapValues(_.size).toMap}" +
      s"\n    $labelB=${b.getOrElse(f, Map.empty).view.mapValues(_.size).toMap}"
    }.mkString("\n  ")
  }
}
