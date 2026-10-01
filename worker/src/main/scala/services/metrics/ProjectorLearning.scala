package services.metrics

import services.movies.TitleNormalizer
import services.readmodel.ReadModelProjector

/** Teaches the read-model projector, from each corpus census pass, the rows it has not projected since
 *  boot — so a change at a few of a film's venues is applied from those venues alone instead of
 *  re-reading the whole film ([[ReadModelProjector.learn]]). The census already reads every film with
 *  its showtimes and partitions it; this rides that read, and writes nothing. A complete pass tells the
 *  projector every row has been offered. */
final class ProjectorLearning(projector: ReadModelProjector, normalizer: TitleNormalizer) extends CorpusMetricsCollector {
  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    // A row turned away because the projector had not seeded yet was not offered in any real sense.
    private var refused = false
    def accept(row: CorpusRow): Unit = row.partition(normalizer).foreach(p => if (!projector.learn(p)) refused = true)
    def publish(scanComplete: Boolean): Unit = if (scanComplete && !refused) projector.learnedAll()
  }
}
