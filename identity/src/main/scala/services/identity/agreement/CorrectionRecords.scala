package services.identity.agreement

import services.identity.measures.Film
import services.identity.vocabulary.Answer

/** TMDB's records as a model take's correction reads them ([[CorrectionReading]]): each kind of evidence against the take
 *  — the relay, the posters, the families — names its films by TMDB id, and reads their records, titles and directors
 *  here, through `evidence` ([[AgreementEvidence]]) as the venues' lookups or TMDB's hold them. A record not answered yet
 *  is counted among the `candidates`' blind reads ([[AgreementCandidates.blindReads]]): the correction that met one is
 *  read again rather than kept. */
private[agreement] final class CorrectionRecords(evidence: AgreementEvidence, candidates: AgreementCandidates) {
  import evidence.{looked, venuesLooked}

  /** How many reads of TMDB's answers met one not answered yet ([[AgreementCandidates.blindReads]]). */
  def blindReads: Long = candidates.blindReads

  /** TMDB's record of `id`, as the venues' lookups or TMDB's hold it. */
  def record(id: Int): Option[Film] =
    venuesLooked.film(id).answered.flatten.orElse(looked.flatMap(_.film(id).answered.flatten))

  /** TMDB's record of `id` ([[record]]) — one not answered yet counted among the [[blindReads]]. */
  def named(id: Int): Option[Film] = {
    val found = record(id)
    if (found.isEmpty && looked.fold(venuesLooked.film(id))(_.film(id)) == Answer.Unknown) candidates.blindReads += 1
    found
  }

  /** Is TMDB's record of `id` known ([[named]])? A take whose own record is not is corrected by nothing. */
  def known(id: Int): Boolean = named(id).isDefined

  /** TMDB's record of `id` ([[named]]) as a family's record is compared with it. */
  def source(id: Int): Option[SourceRecord] = named(id).map(SourceRecord.ofTmdb(id, _))

  /** `id`'s title as its record names it, else `tmdb <id>`. */
  def title(id: Int): String = record(id).fold(s"tmdb $id")(_.titled)

  /** Does another database's `other` record contradict the take `model`'s by its facts ([[Correction.contradicts]])? */
  def contradicts(other: SourceRecord, model: Int): Boolean = record(model).exists(Correction.contradicts(other.film, _))

  /** Do `other` and the take `model` share a director ([[Correction.shareDirector]])? `None` where either record credits
   *  none, or is not known. */
  def sharesDirector(other: Int, model: Int): Option[Boolean] =
    named(other).flatMap(o => record(model).flatMap(film => Correction.shareDirector(o.directors.getOrElse(Nil), film.directors.getOrElse(Nil))))
}
