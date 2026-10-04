package services.movies

/** A [[ListingIntakeMetrics]] that keeps every guard verdict and write skip it is sent. */
final class RecordingListingIntakeMetrics extends ListingIntakeMetrics {
  @volatile var verdicts: Vector[(String, String)] = Vector.empty
  @volatile var skips:    Vector[String]           = Vector.empty
  def recordGuardVerdict(guard: String, verdict: String): Unit = synchronized { verdicts :+= (guard -> verdict) }
  def recordWriteSkipped(reason: String): Unit                 = synchronized { skips :+= reason }
}
