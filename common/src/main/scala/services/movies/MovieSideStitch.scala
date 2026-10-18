package services.movies

import models.{Showtime, Source, SourceData}

/** How [[MongoMovieRepository]] makes a stored `movies` row whole again: its slots from
 *  `movie_slots` and its showtimes from `screenings`, each side collection when it is wired.
 *  Logs under the repository's own logger, so the lines read as they always have. */
private[movies] final class MovieSideStitch(
  screenings: Option[ScreeningsRepository],
  slots:      Option[SlotsRepository],
  normalizer: TitleNormalizer,
  logger:     play.api.Logger
)(using LatestTitleYear) {

  /** Re-inject a stored row's showtimes from `screenings` (its authority under the
   *  split), given that film's `slotKey -> showtimes` map. No-op without a split. */
  def row(r: StoredMovieRecord, scr: Map[String, Seq[Showtime]], storedSlots: Map[String, SourceData]): StoredMovieRecord = {
    val withSlots = slotsInto(r, storedSlots)
    if (screenings.isEmpty) withSlots
    else withSlots.copy(record = withSlots.record.copy(
      data = ScreeningsSplit.stitch(withSlots.record.data, scr)))
  }

  /** The film's slots at `cinemas` as a whole-film read stitches them (see [[VenueSlots]]) —
   *  reading only those venues' `movie_slots` and `screenings` rows. None when either read fails,
   *  when a slot key names no known per-title venue slot, or when a `screenings` row has no slot
   *  row beside it (a whole-film read would take that slot from the `movies` document, which this
   *  does not read): the caller then reads the whole film. */
  def readVenues(filmId: String, cinemas: Set[models.Cinema]): Option[VenueSlots] = {
    val names = cinemas.map(_.displayName)
    for {
      sl                        <- slots
      sc                        <- screenings
      slotRows                  <- sl.findAtCinemasChecked(filmId, names).answered
      showtimeRows              <- sc.findAtCinemasChecked(filmId, names).answered
      if covered(filmId, showtimeRows.keySet, slotRows.keySet)
      placed                     = slotRows.toSeq.map { case (key, slot) => Source.byWireKey(key).collect {
                                     case showing: models.CinemaShowing if cinemas(showing.cinema) => showing -> slot } }
      if placed.forall(_.isDefined)
    } yield {
      val stitched = ScreeningsSplit.stitch(placed.flatten.toMap[Source, SourceData], showtimeRows)
      VenueSlots(FilmId(filmId), cinemas.map(cinema => cinema -> stitched.toSeq.collect {
        case (showing: models.CinemaShowing, slot) if showing.cinema == cinema => showing -> slot }).toMap)
    }
  }

  /** Whether every showtimes row has a slot row beside it — logging, when not, which keys lack one and
   *  which slot keys the film does have, so a write path that leaves showtimes without slots (or files
   *  the two under different keys) can be named from the log. A few a day at most. */
  private def covered(filmId: String, showtimeKeys: Set[String], slotKeys: Set[String]): Boolean = {
    val orphans = showtimeKeys -- slotKeys
    if (orphans.nonEmpty)
      logger.info(s"MovieRepository.readVenues: film $filmId has showtimes without a slot row at " +
        s"${orphans.toSeq.sorted.map(_.replace('\u241f', '|')).mkString(", ")}; slot rows there: " +
        s"${slotKeys.toSeq.sorted.map(_.replace('\u241f', '|')).mkString(", ")}")
    orphans.isEmpty
  }

  /** Union a row's stored `movie_slots` rows with whatever its `movies` document still
   *  embeds — see [[SlotsRepository.merge]] for why a union rather than "stored wins",
   *  and for the prod measurement that forced it.
   *
   *  An EMPTY `storedSlots` still means "fall back to the embedded map", but it no longer
   *  has to carry the weight of distinguishing a genuinely slot-less film from a failed
   *  read: [[SlotsRepository.findForFilmChecked]] answers that directly, and callers
   *  refuse to build a record at all when the read failed.
   *
   *  Re-derives the DISPLAY TITLE from the stitched record, because
   *  `StoredMovieDto.toDomain` could not: it named the film from the `movies`
   *  document's own `sourceData`, which the slot split leaves EMPTY, so
   *  `displayTitle` fell through to its fallback — the sanitized `_id` prefix —
   *  and every hydrated row came back as "Thecabinetofdrcaligari". The cache keyed
   *  the corpus under those mangled spellings (harmless for lookups, since
   *  `CacheKey` compares by `sanitize`), and the first `SettleReaper` pass after
   *  each boot then "re-spelled" every one of them back to the real title: 1240 of
   *  1603 UK rows rewritten under byte-identical `_id`s per deploy (prod,
   *  2026-07-28), plus the change-stream and read-model fan-out behind them. Only
   *  a single-word ASCII title ("Interstellar") round-tripped `sanitize` and stayed
   *  quiet — which is why ~22% of the corpus never churned. */
  def slotsInto(r: StoredMovieRecord, storedSlots: Map[String, SourceData]): StoredMovieRecord =
    if (storedSlots.isEmpty) r
    else {
      val stitched = r.record.copy(data = SlotsRepository.merge(r.record.data, storedSlots))
      StoredMovieRecord.fromStorage(r.id.value, Some(r.key(normalizer)), stitched, normalizer)
    }

  /** Decode one stored row and re-inject its slots from `movie_slots` and its showtimes
   *  from `screenings` — the per-film read-stitch shared by [[MongoMovieRepository.findById]] and the
   *  change-stream fan-out. Slots first: the showtime stitch keys off the slot map.
   *
   *  `None` when the SLOT or the SCREENINGS read failed. A migrated film's `movies`
   *  document holds no cinemas of its own, so a failed slot read would decode to a film
   *  with none at all, and a failed screenings read to one whose every cinema has no
   *  showtimes — and this is precisely the record the change-stream fan-out hands the
   *  read-model projector, whose `diffScreenings` then deletes every `web_screening` the
   *  film has. Declining to produce a record costs one missed re-projection, which the
   *  film's next write repeats; producing an empty one empties a live film off the site.
   *  `withShowtimes = false` skips the `screenings` read entirely: the slots-only row. */
  def decode(dto: StoredMovieDto, withShowtimes: Boolean = true): Option[StoredMovieRecord] =
    MovieSideStitch.read(slots)(_.findForFilmChecked(dto._id)) match {
      case None =>
        logger.warn(s"MovieRepository: skipping ${dto._id} — its movie_slots read failed, and serving the row " +
          "without them would present a live film as having no cinemas.")
        None
      case Some(storedSlots) =>
        MovieSideStitch.read(screenings.filter(_ => withShowtimes))(_.findForFilmChecked(dto._id)) match {
          case None =>
            logger.warn(s"MovieRepository: skipping ${dto._id} — its screenings read failed, and serving the row " +
              "without them would present a live film as having no showtimes.")
            None
          case Some(storedShowtimes) => Some(row(StoredMovieDto.toDomain(dto, normalizer), storedShowtimes, storedSlots))
        }
    }

  /** The slot rows of the films `ids`, by film — None when the read failed. */
  def slotRowsOf(ids: Set[String]): Option[Map[String, Map[String, SourceData]]] =
    MovieSideStitch.read(slots)(_.findForFilmsChecked(ids))

  /** The showtimes rows of the films `ids`, by film — None when the read failed. */
  def screeningRowsOf(ids: Set[String]): Option[Map[String, Map[String, Seq[Showtime]]]] =
    MovieSideStitch.read(screenings)(_.findForFilmsChecked(ids))

  /** Every film's slot rows — None when the read failed. */
  def allSlotRows(): Option[Map[String, Map[String, SourceData]]] =
    MovieSideStitch.read(slots)(_.findAllChecked())
}

private[movies] object MovieSideStitch {
  /** A read of a side collection that may not be wired: an unwired one answers empty, a failed read None. */
  def read[R, A](repository: Option[R])(rows: R => tools.ReadOutcome[Map[String, A]]): Option[Map[String, A]] =
    repository.fold(Option(Map.empty[String, A]))(rows(_).answered)
}
