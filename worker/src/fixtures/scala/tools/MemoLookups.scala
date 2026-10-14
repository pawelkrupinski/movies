package tools

import services.identity._

import scala.jdk.CollectionConverters._

/** [[IdentityLookups]] with every answer memoised, so the permutation runs pay each lookup
 *  once. The memo changes how often the source is reached, never what the resolver ISSUES. */
final class MemoLookups(inner: IdentityLookups) extends IdentityLookups {
  private val details = new java.util.concurrent.ConcurrentHashMap[(String, String), Answer[Option[DetailFacts]]]()
  private val queries = new java.util.concurrent.ConcurrentHashMap[CandidateQuery, Answer[Seq[Hit]]]()
  private val films   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[IdentityMeasures.Film]]]()
  override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
  override def detail(l: Listing): Answer[Option[DetailFacts]] = details.computeIfAbsent((l.venue, l.page.getOrElse("")), _ => inner.detail(l))
  override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = queries.computeIfAbsent(q, _ => inner.candidates(q))
  override def film(id: Int): Answer[Option[IdentityMeasures.Film]]      = films.computeIfAbsent(id, _ => inner.film(id))
  // What a resolve announces it will ask, answered ahead on a few threads: a live-gap replay
  // (`settings.IdentityLiveGaps`) otherwise asks its live questions one after another.
  override def prefetch(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[Listing]): Unit = {
    inner.prefetch(qs, ids, pages)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(MemoLookups.PrefetchThreads)
    try {
      (qs.toSeq.map(q => () => { candidates(q); () }) ++ ids.toSeq.map(id => () => { film(id); () }))
        .map(task => pool.submit(new java.util.concurrent.Callable[Unit] { def call(): Unit = task() })).foreach(_.get())
    } finally pool.shutdown()
  }
  def sizes: (Int, Int, Int) = (details.size, queries.size, films.size)
  /** Whether `l`'s venue detail was asked through it and had no answer: its page unread. */
  def unreadDetail(l: Listing): Boolean = Option(details.get((l.venue, l.page.getOrElse("")))).exists(!_.isKnown)
  /** Every candidate question and film record asked through it, with the answer it got. */
  def asked: (Map[CandidateQuery, Answer[Seq[Hit]]], Map[Int, Answer[Option[IdentityMeasures.Film]]]) =
    (queries.asScala.toMap, films.asScala.toMap)
  def unknown: (Int, Int, Int) = (details.values.asScala.count(!_.isKnown), queries.values.asScala.count(!_.isKnown),
    films.values.asScala.count(!_.isKnown))
}

object MemoLookups { val PrefetchThreads = 8 }
