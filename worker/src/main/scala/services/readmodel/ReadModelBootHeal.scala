package services.readmodel

import play.api.Logging

import scala.util.Try

/** THE BOOT HEAL of the read model: every ready row one of whose cards or venues the read model lacks, projected
 *  before the first prune sweep ([[ReadModelSweeper]]) can delete the card it has under an old id. Asks exactly
 *  which rows are short, with no threshold, and projects only those whole; a row whose venues the read model
 *  serves under another city is a re-file, not a lost write, and is counted apart.
 *
 *  Built by the sweeper over the projection it sweeps; the projector runs it between its boot reads. */
private[readmodel] final class ReadModelBootHeal(projection: ReadModelSweeper.Projection) extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.HealTrigger
  private val movieRepository = projection.movieRepository
  private val reader          = projection.reader
  private val metrics         = projection.metrics

  /** Project, at boot, every ready row one of whose cards is missing — before the
   *  first prune can delete the card it has under an old id.
   *
   *  A row's card id can change under a running read model: the id scheme moved
   *  (2026-09, from `sanitize(title)|resolvedYear` to the row's permanent `FilmId`), or
   *  a database was restored from before that. The scheduled prune then deletes the
   *  old-id cards as orphans, while the change stream re-creates only the rows a
   *  scrape happens to rewrite — so the served site runs short until every venue has
   *  been scraped again. The first version of this check gated a whole-corpus
   *  reproject on a MAJORITY of cards being stale, and the 2026-09-07 rollout came in
   *  under it (509 of 1,156 Polish cards): Poznań served ~10% fewer films for the
   *  better part of an hour. The exact question is cheaper and has no threshold:
   *  which ready rows lack a card under one of their ids? Those are projected whole;
   *  everything else is left to the diffing change-stream path. A row is asked for
   *  EVERY id it projects to — a split row whose plain card survived but whose variant
   *  card did not is still short, and the change stream only revisits it on its next
   *  scrape change. */
  def healMissingCards(study: Option[Seq[BootRow]]): Unit = Try {
    val cardIds = reader.findAllMovieIdsChecked().answered
    val refs    = reader.findAllScreeningRefsChecked().answered
    val (missing, complete) = findMissingAtBoot(study, cardIds, refs)
    if (cardIds.isEmpty) logger.warn("read model: the card ids could not be read at boot — nothing healed; the prune sweep retries.")
    // Each row guarded, as in the sweep: one failed write must not leave every later row unhealed.
    val projected = missing.flatMap { row =>
      continuing(s"read model: healing ${row.id} at boot failed")(projection.exclusively(projection.heal(row.id, row.metadataHash, row.absentCards, row.absentVenues)))
        .flatten.map(written => (row.id.value, written, row.refile)) }
    val (refiledRows, lostRows) = projected.collect { case (id, true, refile) => (id, refile) }.partition(_._2)
    reportBootHeals(lostRows.map(_._1), refiledRows.map(_._1), complete)
  }.recover { case exception => logger.warn(s"read-model missing-card check failed, skipped: ${exception.getMessage}") }

  /** The ready rows one of whose cards or venues the read model lacks, and whether the source read
   *  was complete. The check reads slots only (ids derive from the slot titles, never from a
   *  showtime); the few rows it names are then read whole, showtimes included. An incomplete card
   *  read heals nothing: "no cards" and "could not read" differ. The boot hydrate's rows when it
   *  handed them over (see [[BootCorpusStudy]]); otherwise a read of its own. */
  private def findMissingAtBoot(study: Option[Seq[BootRow]], cardIds: Option[Seq[String]],
                                refs: Option[Seq[ScreeningRef]]): (Seq[MissingAtBoot], Boolean) = {
    val cards   = cardIds.fold(Set.empty[String])(_.toSet)
    val venues  = refs.map(_.map(_._id).toSet)
    // Each (card, venue) the read model serves, whichever city it is filed under: a missing venue row
    // whose card and venue are here under ANOTHER city is a re-file -- a deploy moved the venue's
    // city (2026-10-11: Germany's and the US's town pages, 1,029 and 1,508 rows) -- not a lost write.
    val filed   = refs.fold(Set.empty[(String, String)])(_.iterator.flatMap(_.venueKey).toSet)
    val missing = scala.collection.mutable.ListBuffer.empty[MissingAtBoot]
    def refiled(filmIds: Seq[String], venue: String): Boolean =
      filmIds.filter(card => venue.startsWith(card + "|")).maxByOption(_.length)
        .flatMap(card => ScreeningRef(venue, card).venueKey).exists(filed)
    def check(id: services.movies.FilmId, filmIds: Seq[String], screeningIds: Seq[String], metadataHash: => Int): Unit = {
      projection.recordRowCards(id.value, filmIds.toSet)
      val absentCards  = filmIds.filterNot(cards)
      val absentVenues = venues.fold(Seq.empty[String])(has => screeningIds.filterNot(has))
      if (absentCards.nonEmpty || absentVenues.nonEmpty)
        missing += MissingAtBoot(id, metadataHash, absentCards, absentVenues, absentCards.isEmpty && absentVenues.forall(refiled(filmIds, _)))
    }
    val complete = cardIds.isDefined && study.fold(movieRepository.foreachRecordWithSlots { row =>
      if (row.record.readyToProject) {
        val partition = projection.partition(row)
        check(row.id, partition.filmIds, partition.screeningIds, ReadModelProjection.metadataHash(row))
      }
    }.isComplete) { rows => rows.foreach(row => check(row.id, row.filmIds, row.screeningIds, row.metadataHash)); true }
    (missing.toSeq, complete)
  }

  /** The boot heal's metrics and lines: rows healed of a lost write, and rows whose venues moved city. */
  private def reportBootHeals(healed: Seq[String], refiled: Seq[String], complete: Boolean): Unit = {
    if (healed.nonEmpty) {
      metrics.recordHeal(HealTrigger.Boot, healed.size)
      logger.warn(s"read model: projected ${healed.size} ready row(s) missing a card or a venue at boot" +
        (if (complete) "" else " (source scan incomplete — the rest heal on their next change)") +
        s": ${ReadModelSweeper.idsForLog(healed)}.")
    }
    if (refiled.nonEmpty) {
      metrics.recordHeal(HealTrigger.Refile, refiled.size)
      logger.info(s"read model: projected ${refiled.size} ready row(s) at boot whose venues the read model serves under " +
        s"another city — this code files them elsewhere: ${ReadModelSweeper.idsForLog(refiled)}.")
    }
  }
}
