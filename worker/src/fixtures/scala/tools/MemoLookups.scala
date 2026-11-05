package tools

import services.identity._

import scala.jdk.CollectionConverters._

/** [[IdentityLookups]] with every answer memoised, so the permutation runs pay each lookup
 *  once. The memo changes how often the source is reached, never what the resolver ISSUES. */
final class MemoLookups(inner: IdentityLookups) extends IdentityLookups {
  private val details = new java.util.concurrent.ConcurrentHashMap[(String, String), Answer[Option[DetailFacts]]]()
  private val queries = new java.util.concurrent.ConcurrentHashMap[CandidateQuery, Answer[Seq[Hit]]]()
  private val films   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[IdentityMeasures.Film]]]()
  private val casts   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[Seq[String]]]]()
  override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
  override def detail(l: Listing): Answer[Option[DetailFacts]] = details.computeIfAbsent((l.venue, l.page.getOrElse("")), _ => inner.detail(l))
  override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = queries.computeIfAbsent(q, _ => inner.candidates(q))
  override def film(id: Int): Answer[Option[IdentityMeasures.Film]]      = films.computeIfAbsent(id, _ => inner.film(id))
  // every other lookup the source answers: left to the trait's defaults, a cast read as unknown and a release day off
  // the record alone, whatever the source knew. Only members the oldest ref a slice runs under has (this file compiles
  // into it); a newer one is passed through by name in the slice's wiring (`OlderRefs.NewerLookups`)
  override def cast(id: Int): Answer[Option[Seq[String]]]               = casts.computeIfAbsent(id, _ => inner.cast(id))
  override def releaseDay(id: Int): Answer[Option[java.time.LocalDate]] = inner.releaseDay(id)
  override def prefetchAnswered(): Unit                                 = inner.prefetchAnswered()
  override def released(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[services.movies.ListingKey]): Unit =
    inner.released(qs, ids, pages)
  // What a resolve announces it will ask, answered ahead on a few threads: a live-gap replay
  // (`settings.IdentityLiveGaps`) otherwise asks its live questions one after another, and a recorded corpus's detail
  // pages -- decompressed and parsed, a third of the US resolve's 285 s (JFR, 2026-10-15) -- are read on the idle cores.
  //
  // Only what the memo does not hold yet reaches the source, one prefetch at a time: a source's prefetch may let go of
  // the last one's documents (StoredTmdbLookups' does), so two family resolves prefetching at once over one memo raced
  // (an NPE in its `load`, FamilyCounterfactual and MirrorRecompute held to one thread), and every family re-announcing
  // answers the corpus's resolve already memoised threw the source's documents away for nothing.
  override def prefetch(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[Listing]): Unit = {
    val newQueries = qs.toSeq.distinct.filterNot(queries.containsKey)
    val newFilms   = ids.toSeq.distinct.filterNot(films.containsKey)
    val newPages   = pages.toSeq.filterNot(l => details.containsKey((l.venue, l.page.getOrElse(""))))
    if (newQueries.nonEmpty || newFilms.nonEmpty || newPages.nonEmpty) prefetching.synchronized {
      inner.prefetch(newQueries, newFilms, newPages)
      val pool = java.util.concurrent.Executors.newFixedThreadPool(MemoLookups.PrefetchThreads)
      try {
        (newQueries.map(q => () => { candidates(q); () }) ++ newFilms.map(id => () => { film(id); () }) ++
          newPages.map(l => () => { detail(l); () }))
          .map(task => pool.submit(new java.util.concurrent.Callable[Unit] { def call(): Unit = task() })).foreach(_.get())
      } finally pool.shutdown()
    }
  }
  private val prefetching = new Object
  def sizes: (Int, Int, Int) = (details.size, queries.size, films.size)
  /** Whether `l`'s venue detail was asked through it and had no answer: its page unread. */
  def unreadDetail(l: Listing): Boolean = Option(details.get((l.venue, l.page.getOrElse("")))).exists(!answered(_))
  /** Every candidate question and film record asked through it, with the answer it got. */
  def asked: (Map[CandidateQuery, Answer[Seq[Hit]]], Map[Int, Answer[Option[IdentityMeasures.Film]]]) =
    (queries.asScala.toMap, films.asScala.toMap)
  // a tool file, dropped into old refs (scripts/identity-slice TOOL_FILES): read through `Known`, which every ref has, not
  // `Answer.isAnswered` (2026-11-06) nor the `isKnown` before it
  private def answered(answer: services.identity.Answer[?]): Boolean = answer.isInstanceOf[services.identity.Answer.Known[?]]
  def unknown: (Int, Int, Int) = (details.values.asScala.count(!answered(_)), queries.values.asScala.count(!answered(_)),
    films.values.asScala.count(!answered(_)))
}

object MemoLookups { val PrefetchThreads = 8 }
