package services.readmodel

import play.api.Logging

/** THE BOOT SEED of a [[ReadModelProjector]]'s memo: what this process takes as already written from the read model
 *  ([[fromReadModel]]), and the corpus's rows learnt, from the boot hydrate's read ([[learnAtBoot]]) or a read of its
 *  own ([[learnFromCorpus]]). Each runs under the projection's lock, the change stream's applies waiting on it.
 *  [[ReadModelBoot]] decides when; the projector then diffs every projection against what this seeded.
 *
 *  Built over a projector ([[ReadModelBootSeed.apply]]): it fills that projector's own memo and card store. */
final class ReadModelBootSeed private[readmodel] (
  projection: SweptProjection,
  memo:       ProjectionMemo,
  cardStore:  CardStore
) extends Logging {
  private val reader = projection.reader

  /** Seed the last-projection state from the derived collections, so a restart doesn't
   *  rewrite documents that are already correct — and so a document this process did not
   *  write (one left under a renamed city's former slug, say) is one its next projection of
   *  that film can retire. What a worker does as it boots ([[ReadModelBoot]]); public so a harness can model the
   *  boot of the next worker over a read model it did not write. */
  def fromReadModel(): Unit = projection.exclusively {
    // A read that falls short seeds nothing: every card is then rewritten on its first projection,
    // the costly-but-safe direction.
    reader.findAllMoviesChecked().answered.getOrElse {
      logger.warn("read model: the cards could not be read whole at boot — seeding none; each is rewritten on its first projection")
      Seq.empty
    }.foreach(cardStore.seed)
    // Page by page, keeping each row's id and hash only: read whole, US's screenings were
    // ~400 MB live beside the identity take-up. The content is known, the slots it was
    // built from are not: no input hash, so the first projection of each venue rebuilds it
    // rather than trusting it. A read that falls short seeds nothing, as it always has.
    val byFilm = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.Builder[(String, WrittenScreening), Map[String, WrittenScreening]]]
    val complete = reader.foreachScreening(s => byFilm.getOrElseUpdate(s.filmId, Map.newBuilder) += s._id -> WrittenScreening(s.##, input = None))
    if (complete.isComplete) byFilm.foreach { case (fid, rows) => memo.lastScreenings.update(fid, rows.result()) }
    memo.seeded = true
  }

  /** Learn every row of the boot hydrate's complete read. Before the seed the memo has no row to vouch:
   *  learning then would mark each row known with every venue unvouched. */
  private[readmodel] def learnAtBoot(rows: Seq[BootRow]): Unit = projection.exclusively {
    if (memo.seeded) rows.foreach(row => memo.learnFrom(row.id.value, row.lesson))
  }

  /** Learn every ready row from a read of the corpus with its showtimes — what [[learnAtBoot]] takes from the
   *  hydrate's read, when the boot had none to hand over (it failed, or its derivation ran past its wait). One row at
   *  a time under the lock, so the change stream's applies interleave; a change at a row not learned yet meanwhile is
   *  re-read whole, as it would be anyway. */
  private[readmodel] def learnFromCorpus(): Unit = {
    val read = projection.scan(withShowtimes = true) { row =>
      if (row.readyToProject)
        scala.util.Try(Lesson.of(projection.partition(row))).foreach(lesson =>
          projection.exclusively(if (memo.seeded) memo.learnFrom(row.id.value, lesson)))
    }
    logger.info(s"read model: learned the corpus's rows from a read of its own (${read.explain}).")
  }
}

object ReadModelBootSeed {
  /** The seed of `projector`'s own memo and card store. */
  def apply(projector: ReadModelProjector): ReadModelBootSeed =
    new ReadModelBootSeed(projector.projection, projector.memo, projector.cardStore)
}
