package services.movies

import models.{Showtime, SourceData}

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
   *  string when they match.
   *
   *  Led by WHICH fields differ and on how many films, so the first line says where to look; then the
   *  films only one side made, each with the listings it holds (the fold that differs is the story, and
   *  its title alone does not say which venue's listing went where); then the first `limit` differing
   *  films field by field, and how many more there are. */
  def records(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord],
              labelA: String = "0", labelB: String = "1", limit: Int = 20): String = {
    def keyOf(r: StoredMovieRecord) = (r.title, r.year)
    val namesakesA = a.groupBy(keyOf)
    val namesakes  = b.groupBy(keyOf)
    def only(side: Seq[StoredMovieRecord], other: Map[(String, Option[Int]), Seq[StoredMovieRecord]], label: String) = {
      val alone = side.filterNot(r => other.contains(keyOf(r)))
      if (alone.isEmpty) Nil
      else s"${alone.size} film(s) only in $label:" +: (alone.take(limit).map(r => s"  '${r.title}' (${r.year}) ← ${listingsOf(r)}") ++
        Option.when(alone.sizeIs > limit)(s"  …and ${alone.size - limit} more film(s)"))
    }
    // Each differing film's field names (for the tally) and its lines.
    val differing: Seq[(Seq[String], Seq[String])] = a.filter(r => namesakes.contains(keyOf(r))).flatMap { ra =>
      val ea = ra.record; val eb = counterpart(ra, namesakes(keyOf(ra))).record
      val scalars = Seq[(String, Any, Any)](
        ("tmdbId", ea.tmdbId, eb.tmdbId), ("imdbId", ea.imdbId, eb.imdbId),
        ("imdbRating", ea.imdbRating, eb.imdbRating), ("metascore", ea.metascore, eb.metascore),
        ("rottenTomatoes", ea.rottenTomatoes, eb.rottenTomatoes), ("filmwebRating", ea.filmwebRating, eb.filmwebRating),
        ("filmwebUrl", ea.filmwebUrl, eb.filmwebUrl), ("metacriticUrl", ea.metacriticUrl, eb.metacriticUrl),
        ("rottenTomatoesUrl", ea.rottenTomatoesUrl, eb.rottenTomatoesUrl)
      ).collect { case (n, x, y) if x != y => n -> s"  scalar $n: $labelA=${short(x)} $labelB=${short(y)}" }
      val srcDiffs = (ea.data.keySet ++ eb.data.keySet).toSeq.sortBy(_.displayName).flatMap { src =>
        (ea.data.get(src), eb.data.get(src)) match {
          case (Some(da), Some(db)) if da != db =>
            val fields = Seq[(String, Any, Any)](
              ("title", da.title, db.title),
              ("synopsis.len", da.synopsis.map(_.length), db.synopsis.map(_.length)),
              // Compared as the names it holds: a re-billed cast is no difference.
              ("cast", SourceData.castSet(da.cast), SourceData.castSet(db.cast)), ("director", da.director, db.director),
              ("posterUrl", da.posterUrl, db.posterUrl), ("releaseYear", da.releaseYear, db.releaseYear),
              ("runtimeMinutes", da.runtimeMinutes, db.runtimeMinutes), ("countries", da.countries, db.countries),
              ("genres", da.genres, db.genres), ("filmUrl", da.filmUrl, db.filmUrl),
              ("showtimes.size", da.showtimes.size, db.showtimes.size), ("showtimes", da.showtimes, db.showtimes)
            ).collect { case (n, x, y) if x != y => n -> s"      $n: $labelA=${short(x)} $labelB=${short(y)}" }
            Some(fields.map(field => s"slot ${field._1}") -> s"  source '${src.displayName}' differs:\n${fields.map(_._2).mkString("\n")}")
          case (Some(_), None) => Some(Seq(s"slot only in $labelA") -> s"  source '${src.displayName}': only in $labelA")
          case (None, Some(_)) => Some(Seq(s"slot only in $labelB") -> s"  source '${src.displayName}': only in $labelB")
          case _               => None
        }
      }
      Option.when(scalars.nonEmpty || srcDiffs.nonEmpty)(
        (scalars.map(_._1) ++ srcDiffs.flatMap(_._1)).distinct ->
          (Seq(s"record '${ra.title}' (${ra.year}):") ++ scalars.map(_._2) ++ srcDiffs.map(_._2)))
    }
    val tally = differing.flatMap(_._1).groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortBy { case (field, n) => (-n, field) }
    val header = Option.when(differing.nonEmpty)(
      s"${differing.size} film(s) differ — ${tally.map { case (field, n) => s"$field ×$n" }.mkString(", ")}")
    (header.toSeq ++ only(a, namesakes, labelA) ++ only(b, namesakesA, labelB) ++ differing.take(limit).flatMap(_._2) ++
      Option.when(differing.sizeIs > limit)(s"…and ${differing.size - limit} more film(s) differ")).mkString("\n")
  }

  /** A film's cinema listings as a reader finds them: venue and the title it billed. */
  private def listingsOf(r: StoredMovieRecord): String =
    r.record.cinemaSlots.map { case (source, slot) =>
      s"${models.Source.cinemaOf(source).fold(source.displayName)(_.displayName)}: '${slot.rawTitle.orElse(slot.title).getOrElse("")}'"
    }.sorted.mkString("; ")

  /** `r`'s record among its `namesakes` on the other side (the records of its title and year): where
   *  several films share both (a relay the model keeps apart per venue, two same-titled secret
   *  screenings), the one of its id, so a diff pairs each film with itself rather than with whichever
   *  namesake a map kept last and reports differences no film has. The first when no id matches. */
  private def counterpart(r: StoredMovieRecord, namesakes: Seq[StoredMovieRecord]): StoredMovieRecord =
    namesakes.find(_.id == r.id).getOrElse(namesakes.head)

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
