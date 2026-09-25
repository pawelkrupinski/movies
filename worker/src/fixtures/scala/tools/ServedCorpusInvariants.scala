package tools

import models.{Cinema, CinemaMovie, CinemaShowing, CityScreening, ResolvedMovie, Showtime}
import services.movies.{ScrapeListing, StoredMovieRecord, TitleNormalizer}
import services.scrapes.ArchivedScrape

import java.time.LocalDateTime

/**
 * What a settled corpus SERVES, checked listing by listing against the archive it was
 * built from.
 *
 * The convergence legs' older no-loss check compared SETS: every archived cinema, every
 * archived `(cinema, start time)` pair and every settled film key had to appear somewhere
 * in the rendered rows. A set cannot see WHICH film a showtime is served under, so every
 * mistake that moves a showtime or a whole listing onto the wrong film passed it: a
 * multiplex runs several films at 18:00, so a listing folded onto a different film (the
 * wrong-merge class) or a projection that dropped one film's copy of a shared start time
 * left every pair still present. The settled-key comparison read both sides from the
 * CACHE, so a listing that never reached the cache at all was invisible to it too.
 *
 * These checks follow each archived listing to the one film that holds it and on to the
 * read model's rows for that film:
 *
 *   - HOMED: every listing with showtimes is held by EXACTLY one stored film, in the slot
 *     `CinemaShowing.keyFor` derives for its (cinema, title) — none is a lost listing, two
 *     is one listing duplicated across films.
 *   - SERVED UNDER ITS FILM: every showtime of that listing is in the read model's
 *     screenings for THAT film (or one of its `~variant` cards) at THAT cinema.
 *   - NOTHING INVENTED: every served showtime traces back to an archived listing of the
 *     same film at the same cinema — a showtime copied onto a second film, or kept on a
 *     film its listing left, is served twice.
 *   - NO EMPTY CARDS: every served film has at least one screening, every screening
 *     belongs to a served film, and every film the projector is allowed to publish
 *     (`readyToProject`) is served.
 *   - ONE FILM PER TMDB ID: no two stored films share a `tmdbId`. The repository's
 *     partial unique index makes the second write FAIL rather than land, and a failed
 *     write is swallowed and counted — so the index alone turns a wrong split into a
 *     silently dropped row rather than a failure anyone sees.
 *
 * Pure: it takes what the caller already read, so the leg pays no extra database pass
 * beyond the read-model scan, and it is unit-testable over hand-built inputs.
 */
object ServedCorpusInvariants {

  /** How many offenders each check names before truncating; the count is always given. */
  val Shown = 12

  /** The archive's listings that carry at least one showtime. */
  def listings(archive: Seq[ArchivedScrape]): Seq[(Cinema, CinemaMovie)] =
    archive.flatMap(row => row.films.filter(_.showtimes.nonEmpty).map(row.cinema -> _))

  /** @param listings what the venues last reported, `(venue, listing)` — see [[listings]]
   *  @param from     showtimes before this are ignored on BOTH sides: the web never serves
   *                  them, and a venue whose scrape failed or came back empty keeps its old
   *                  slot on purpose, past starts included. */
  def violations(
    listings:   Seq[(Cinema, CinemaMovie)],
    records:    Seq[StoredMovieRecord],
    served:     Seq[ResolvedMovie],
    screenings: Seq[CityScreening],
    normalizer: TitleNormalizer,
    from:       LocalDateTime = LocalDateTime.MIN
  ): Seq[String] = {
    def upcoming(times: Iterable[Showtime]): Set[LocalDateTime] = times.iterator.map(_.dateTime).filterNot(_.isBefore(from)).toSet
    def base(cardId: String): String = cardId.takeWhile(_ != '~')
    val label: Map[String, String] = records.map(r =>
      r.id.value -> s"'${r.title}' (${r.year.getOrElse("—")}) [${r.key(normalizer)}]").toMap
    def film(id: String): String = label.getOrElse(id, s"<no stored film $id>")

    // (cinema display name, slot key) -> the stored films holding that slot.
    val holders: Map[(String, String), Seq[String]] = records.flatMap { r =>
      r.record.data.keysIterator.collect { case CinemaShowing(c, key) => (c.displayName, key) -> r.id.value }
    }.groupMap(_._1)(_._2)

    // What the read model serves, per (base film id, cinema).
    val servedAt: Map[(String, String), Set[LocalDateTime]] =
      screenings.groupMapReduce(s => (base(s.filmId), s.cinema))(s => upcoming(s.showtimes))(_ ++ _)

    val problems = Seq.newBuilder[String]
    def report(what: String, offenders: Seq[String]): Unit =
      if (offenders.nonEmpty)
        problems += s"${offenders.size} $what:\n  ${offenders.sorted.take(Shown).mkString("\n  ")}" +
                    (if (offenders.size > Shown) s"\n  … and ${offenders.size - Shown} more" else "")

    val homes = listings.filter { case (_, cm) => upcoming(cm.showtimes).nonEmpty }.map { case (cinema, cm) =>
      val key = ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)
      (cinema.displayName, cm, holders.getOrElse((cinema.displayName, key), Nil).distinct)
    }
    report("archived listing(s) held by NO stored film — scraped, then lost before `movies`",
      homes.collect { case (c, cm, Nil) => s"'${cm.movie.title}' at $c (${cm.showtimes.size} showtime(s))" })
    report("archived listing(s) held by SEVERAL stored films — one listing duplicated across films",
      homes.collect { case (c, cm, hs) if hs.sizeIs > 1 => s"'${cm.movie.title}' at $c held by ${hs.map(film).sorted.mkString(", ")}" })

    // The films each (cinema, film) pair may legitimately serve, from the archive.
    val expected: Map[(String, String), Set[LocalDateTime]] = homes.collect { case (c, cm, Seq(id)) =>
      (id, c) -> upcoming(cm.showtimes)
    }.groupMapReduce(_._1)(_._2)(_ ++ _)

    report("archived listing(s) whose showtimes are NOT served under the film that holds them",
      homes.collect { case (c, cm, Seq(id)) =>
        val wanted  = upcoming(cm.showtimes)
        val missing = wanted -- servedAt.getOrElse((id, c), Set.empty)
        Option.when(missing.nonEmpty)(
          s"'${cm.movie.title}' at $c → ${film(id)}: ${missing.size} of ${wanted.size} showtime(s) unserved, " +
          s"first ${missing.toSeq.sorted.head}")
      }.flatten)

    report("served (film, cinema) pair(s) carrying showtimes no archived listing of that film gave it",
      servedAt.toSeq.flatMap { case ((id, c), times) =>
        val invented = times -- expected.getOrElse((id, c), Set.empty)
        Option.when(invented.nonEmpty)(
          s"${film(id)} at $c: ${invented.size} of ${times.size} showtime(s) unaccounted for, first ${invented.toSeq.sorted.head}")
      })

    val servedIds    = served.map(m => base(m._id)).toSet
    // Every screening ROW counts here, past starts or not: a card whose showtimes have all
    // passed is invisible on the site but not empty, and nothing retires it by date.
    val screenedIds  = screenings.iterator.filter(_.showtimes.nonEmpty).map(s => base(s.filmId)).toSet
    report("served film(s) with NO screening — a card with nothing under it",
      (servedIds -- screenedIds).toSeq.map(film))
    report("screening row(s) whose film is not served",
      (screenedIds -- servedIds).toSeq.map(film))
    report("stored film(s) ready to project that the read model does not serve",
      records.filter(r => r.record.readyToProject && holdsShowtimes(r) && !servedIds.contains(r.id.value))
        .map(r => film(r.id.value)))

    report("tmdbId(s) held by several stored films",
      records.filter(_.record.tmdbId.isDefined).groupBy(_.record.tmdbId.get).toSeq.collect {
        case (tmdb, rs) if rs.sizeIs > 1 => s"tmdb $tmdb: ${rs.map(r => film(r.id.value)).sorted.mkString(", ")}"
      })

    problems.result()
  }

  private def holdsShowtimes(r: StoredMovieRecord): Boolean =
    r.record.cinemaSlots.exists { case (_, sd) => services.movies.ShowtimesDigest.slotShowtimeCount(sd) > 0 }
}
