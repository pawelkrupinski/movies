package services.identityadapters

import services.identity.agreement.{Showing, SourceHit, SourceRecord}
import services.identity.vocabulary.VoterFamily

/**
 * Another film database FAMILY asked live ([[VoterFamily]]): its title search's films, a credited person's films where
 * it has a person search, and its record of one film — what [[FamilyAnswerStore]] files and the agreement reads. A
 * failed read throws (the fill asks again); an answer naming nothing is empty.
 */
trait FamilySource {
  def family: VoterFamily
  def titled(text: String): Seq[SourceHit]
  def directedBy(name: String): Seq[SourceHit]
  def record(id: String): Option[SourceRecord]
  /** The films the family's site lists a venue screening, and on which days: none where it lists no programmes. */
  def showing(venue: String): Seq[Showing] = Nil
}

object FamilySource {
  /** A source for `family` that answers nothing: what a family this worker has no source for files. */
  def answeringNothing(of: VoterFamily): FamilySource = new FamilySource {
    val family: VoterFamily = of
    def titled(text: String): Seq[SourceHit]     = Nil
    def directedBy(name: String): Seq[SourceHit] = Nil
    def record(id: String): Option[SourceRecord] = None
  }
}

object FamilySources {
  /** How many of a search's films are read: the experiment read the first eight. */
  val SearchedFilms = 8
}
