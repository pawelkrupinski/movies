package services.movies

/** A [[MergeMetrics]] that keeps every `recordMerge` it is sent. */
final class RecordingMergeMetrics extends MergeMetrics {
  private val recorded = new java.util.concurrent.ConcurrentLinkedQueue[(MergeReason, Int)]()
  def recordMerge(reason: MergeReason, victims: Int): Unit = { recorded.add(reason -> victims); () }
  /** Every call, in order. */
  def calls: Seq[(MergeReason, Int)] = scala.jdk.CollectionConverters.IteratorHasAsScala(recorded.iterator).asScala.toSeq
  /** Victims merged, all reasons together. */
  def total: Int = calls.map(_._2).sum
  def byReason: Map[MergeReason, Int] = MergeReason.all.map(r => r -> calls.collect { case (`r`, n) => n }.sum).toMap
}
