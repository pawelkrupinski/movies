package services.identity

import scala.collection.concurrent.TrieMap

import models.ListingKey
import services.identity.measures.{Film, Listing}
import services.identity.vocabulary.{Answer, CandidateQuery, Dependency, DetailFacts, Hit, ProseWords}

/** `inner`'s lookups, each read reported to `onRead` as a [[ReadEvent]] as it is answered; every other question
 *  forwarded as asked. */
private[identity] final class RecordingLookups(inner: IdentityLookups, onRead: ReadEvent => Unit) extends ForwardingIdentityLookups(inner) {
  override def hasDetail(listing: Listing): Boolean = { val has = inner.hasDetail(listing); onRead(ReadEvent.DetailPresence(listing, has)); has }
  override def detail(listing: Listing): Answer[Option[DetailFacts]] = noted(inner.detail(listing))(ReadEvent.DetailAnswered(listing, _))
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = lookup(FamilyIndex.Lookups.search(query), inner.candidates(query))
  override def film(tmdbId: Int): Answer[Option[Film]] = lookup(FamilyIndex.Lookups.record(tmdbId, Dependency.Aspect.Answer), inner.film(tmdbId))
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] =
    lookup(FamilyIndex.Lookups.record(tmdbId, Dependency.Aspect.Release), inner.releaseDay(tmdbId))
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = lookup(FamilyIndex.Lookups.record(tmdbId, Dependency.Aspect.Cast), inner.cast(tmdbId))
  override def overview(tmdbId: Int): Answer[Option[ProseWords]] =
    lookup(FamilyIndex.Lookups.record(tmdbId, Dependency.Aspect.Overview), inner.overview(tmdbId))

  private def lookup[A](dependency: Dependency, answer: Answer[A]): Answer[A] = noted(answer)(ReadEvent.LookupAnswered(dependency, _))
  private def noted[A](answer: Answer[A])(event: Answer[A] => ReadEvent): Answer[A] = { onRead(event(answer)); answer }
}

/** The lookups a family's resolve read, as the family index records them: each read's [[Dependency]] with the answer
 *  and, as a receipt digests it ([[Dependency.digest]]), its digest; and which listings' venues publish a detail page.
 *  [[record]] is what a [[RecordingLookups]] reports to. Safe on any thread. */
private[identity] final class LookupLedger {
  val answers: TrieMap[Dependency, Answer[?]] = TrieMap.empty
  val detailed: TrieMap[ListingKey, Boolean] = TrieMap.empty
  def record(event: ReadEvent): Unit = event match {
    case ReadEvent.DetailAnswered(listing, answer) => answers.update(FamilyIndex.Lookups.detail(listing.key), answer)
    case ReadEvent.DetailPresence(listing, has)    => detailed.update(listing.key, has)
    case ReadEvent.LookupAnswered(dependency, answer) => answers.update(dependency, answer)
    case _: ReadEvent.FamilyAnswered                => ()
  }
  def reads: Map[Dependency, Long] = answers.toMap.map { case (dependency, answer) => dependency -> Dependency.digest(answer) }
}
