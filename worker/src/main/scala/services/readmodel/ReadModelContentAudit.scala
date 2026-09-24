package services.readmodel

import models.{CityScreening, ResolvedMovie}
import services.movies.{FilmId, MovieRepository}

/**
 * STORED CONTENT MATCHES A FRESH RE-READ: a sampled card's `web_movies` document and its
 * `web_screenings` rows, compared field by field with what [[ReadModelProjection]] derives from
 * its source row right now.
 *
 * Every other read-model check compares IDS. The prune removes a card whose row is gone, the heal
 * writes a card or venue that is missing, the served-films rules compare counts — and a row that
 * EXISTS and is WRONG passes all of them. That is how Troy, 2046 and Glastonbury served August
 * showtimes in the UK from 2026-08-29 to 2026-09-08. The projector's rolling content check now
 * repairs such a row within a day, but it compares the source with the projector's own MEMO of
 * what it wrote, not with what the store holds; this reads the store, so a write the memo
 * believes landed and the store does not hold is visible here too.
 *
 * `shareCard` / `shareCardPending` are left out: the projection does not derive them (the
 * projector fills them from the share-card directory, see `ReadModelProjector.gate`), and the
 * share-card audit checks the first against the disk.
 */
object ReadModelContentAudit {

  /** The card's differences, named — `Some(Nil)` when it matches — or None when it cannot be
   *  judged: a read failed, or its row no longer projects this card (unready, re-keyed, gone),
   *  which is the prune's and the heal's business and already has its own signals. */
  def differences(cardId: String, movies: MovieRepository, reader: ReadModelReader): Option[Seq[String]] = {
    val (row, readable) = movies.findByIdChecked(FilmId(rowIdOf(cardId)))
    for {
      stored   <- row.filter(_ => readable).filter(_.record.readyToProject)
      expected <- ReadModelProjection.projectAll(stored, movies.normalizer).find(_._1._id == cardId)
      card     <- reader.findCard(cardId)
      movie    <- card.movie
    } yield differences(expected, (movie, card.screenings))
  }

  /** The source row a card comes from: its id up to a display-title variant's `~` (see
   *  `ReadModelProjection.partition`). */
  private def rowIdOf(cardId: String): String = cardId.takeWhile(_ != '~')

  /** Pure: what differs between the projected card and the stored one, as `field` for the
   *  document and `screenings[<id>].field` / `screenings[<id>] missing|unexpected` for its rows. */
  def differences(expected: (ResolvedMovie, Seq[CityScreening]), stored: (ResolvedMovie, Seq[CityScreening])): Seq[String] = {
    def comparable(m: ResolvedMovie) = m.copy(shareCard = None, shareCardPending = false)
    val (expectedMovie, expectedRows) = expected
    val (storedMovie, storedRows)     = stored
    val storedById                    = storedRows.map(row => row._id -> row).toMap
    val expectedIds                   = expectedRows.map(_._id).toSet
    fields(comparable(expectedMovie), comparable(storedMovie)) ++
      expectedRows.flatMap { row =>
        storedById.get(row._id).fold(Seq(s"screenings[${row._id}] missing"))(fields(row, _).map(f => s"screenings[${row._id}].$f"))
      } ++
      storedRows.map(_._id).filterNot(expectedIds).map(id => s"screenings[$id] unexpected")
  }

  private def fields(expected: Product, stored: Product): Seq[String] =
    expected.productElementNames.zip(expected.productIterator.zip(stored.productIterator))
      .collect { case (name, (want, have)) if want != have => name }.toSeq
}
