package services.identity.agreement

import services.identity.ReadEvent
import services.identity.vocabulary.{Answer, VoterFamily}

/** `inner`'s family answers, each answer it gives reported to `onRead` as a [[ReadEvent.FamilyAnswered]] under the
 *  question that names it (`title|…`, `director|…`, `record|…`, `showing|…`). Its freshness is the default's: a
 *  recording is never asked whether an answer is stale; what notes that asks `inner`. */
private[identity] final class RecordingAnswers(inner: FamilyAnswers, onRead: ReadEvent => Unit) extends FamilyAnswers {
  val family: VoterFamily = inner.family
  private def noted[A](question: String, answer: Answer[A]): Answer[A] = { onRead(ReadEvent.FamilyAnswered(question, answer)); answer }
  def titled(text: String): Answer[Seq[SourceHit]]     = noted(s"title|$text", inner.titled(text))
  def directedBy(name: String): Answer[Seq[SourceHit]] = noted(s"director|$name", inner.directedBy(name))
  def record(id: String): Answer[Option[SourceRecord]] = noted(s"record|$id", inner.record(id))
  override def showing(venue: String): Answer[Seq[Showing]] = noted(s"showing|$venue", inner.showing(venue))
}
