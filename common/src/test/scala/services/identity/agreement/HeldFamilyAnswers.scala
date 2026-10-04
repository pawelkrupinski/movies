package services.identity.agreement

import services.identity.{Answer, CandidateQuery, DetailFacts, Hit, IdentityLookups, IdentityMeasures, Listing}

/** A family answering from what it holds: every title search (by any of a record's titles) and record known, no person
 *  search — or, `unanswered`, nothing known yet. */
final class HeldFamilyAnswers(val family: VoterFamily, records: Map[String, SourceRecord], unanswered: Boolean = false, stale: Boolean = false)
    extends FamilyAnswers {
  override def fresh(question: String): Boolean = !stale
  def titled(text: String): Answer[Seq[SourceHit]] =
    if (unanswered) Answer.Unknown
    else Answer.Known(records.toSeq.sortBy(_._1).collect { case (id, record) if record.film.titles.map(IdentityMeasures.key).contains(IdentityMeasures.key(text)) =>
      SourceHit(id, record.film.title, record.film.originalTitle, record.film.year) })
  def directedBy(name: String): Answer[Seq[SourceHit]] = if (unanswered) Answer.Unknown else Answer.Known(Nil)
  def record(id: String): Answer[Option[SourceRecord]] = if (unanswered) Answer.Unknown else Answer.Known(records.get(id))
}

/** No venue detail page anywhere: the families' answers are all a resolve reads. */
object NoVenueDetails extends IdentityLookups {
  def hasDetail(listing: Listing): Boolean                     = false
  def detail(listing: Listing): Answer[Option[DetailFacts]]    = Answer.Known(None)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = Answer.Known(Nil)
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
}
