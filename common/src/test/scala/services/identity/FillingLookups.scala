package services.identity

import scala.collection.mutable
import scala.util.Random

/** `inner` with some questions not answered yet — a question is a gap the first time it is seen with
 *  probability 0.3 — and `answer` closing about half of the open ones, as a fill round does. */
final class FillingLookups(inner: IdentityLookups, rnd: Random) extends IdentityLookups {
  private val openQueries = mutable.HashSet.empty[CandidateQuery]
  private val openFilms   = mutable.HashSet.empty[Int]
  private val seenQueries = mutable.HashSet.empty[CandidateQuery]
  private val seenFilms   = mutable.HashSet.empty[Int]
  // A question is a gap the first time it is seen with probability 0.3, and stays one until answered.
  private def gapQuery(q: CandidateQuery) = { if (seenQueries.add(q) && rnd.nextDouble() < 0.3) openQueries += q; openQueries(q) }
  private def gapFilm(id: Int)            = { if (seenFilms.add(id) && rnd.nextDouble() < 0.3) openFilms += id; openFilms(id) }
  def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
  def detail(l: Listing): Answer[Option[DetailFacts]] = inner.detail(l)
  def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (gapQuery(q)) Answer.Unknown else inner.candidates(q)
  def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (gapFilm(id)) Answer.Unknown else inner.film(id)
  /** Answer about half of the open questions; what changed. */
  def answer(): AnswersChanged = {
    val queries = openQueries.toSeq.sorted.filter(_ => rnd.nextBoolean()).toSet
    val films   = openFilms.toSeq.sorted.filter(_ => rnd.nextBoolean()).toSet
    openQueries --= queries; openFilms --= films
    AnswersChanged(queries, films)
  }
}

/** One event the identity model takes: listings a scrape saw, listings it no longer sees, answers filed. */
enum IdentityEvent {
  case Seen(listings: Seq[Listing])
  case Gone(keys: Seq[services.movies.ListingKey])
  case Answered(changed: AnswersChanged)

  def label: String = this match {
    case Seen(listings) => s"seen ${listings.size}"
    case Gone(keys)     => s"gone ${keys.size}"
    case Answered(c)    => s"answered ${c.queries.size}+${c.films.size}"
  }
}

/** A random sequence of `steps` events over `listings` — batches arriving, some leaving (to arrive
 *  again later), and the fill answering gaps — with `held`, the listings the model holds after each. */
final class RandomIdentityEvents(listings: Seq[Listing], lookups: FillingLookups, seed: Long, steps: Int = 40) {
  private val rnd     = new Random(seed)
  private val pending = mutable.Queue.from(rnd.shuffle(listings))
  val held = mutable.LinkedHashMap.empty[services.movies.ListingKey, Listing]

  def events: Iterator[IdentityEvent] = Iterator.range(0, steps).map { _ =>
    rnd.nextInt(10) match {
      case 0 | 1 | 2 | 3 if pending.nonEmpty =>
        val batch = Seq.fill(1 + rnd.nextInt(6))(()).flatMap(_ => Option.when(pending.nonEmpty)(pending.dequeue()))
        batch.foreach(listing => held(listing.key) = listing)
        IdentityEvent.Seen(batch)
      case 4 | 5 if held.nonEmpty =>
        val gone = rnd.shuffle(held.keys.toSeq).take(1 + rnd.nextInt(3))
        pending ++= gone.flatMap(held.remove)
        IdentityEvent.Gone(gone)
      case _ => IdentityEvent.Answered(lookups.answer())
    }
  }
}
