package services.readmodel

/** Where a whole-collection scan counts a document it SKIPPED because it could not decode
 *  it. The skip keeps one malformed document from sinking the page it arrived in, but a
 *  skipped document is a film or screening the reader silently goes without — a WARN line
 *  alone let that pass unnoticed. The worker and the web each wire a Prometheus-backed
 *  counter (`kinowo_{worker,web}_decode_failures_total{country, collection}`, alerted by
 *  `DocumentsUndecodable`); scripts and most tests use [[DecodeFailureMetrics.noop]]. */
trait DecodeFailureMetrics {
  def recordDecodeFailure(collection: String): Unit
}

object DecodeFailureMetrics {
  val noop: DecodeFailureMetrics = (_: String) => ()

  /** Every collection a tolerant scan reads, seeded at 0 so the first skip moves `increase()`. */
  val Collections: Seq[String] = Seq(MongoReadModelRepository.MoviesCollection, MongoReadModelRepository.ScreeningsCollection)
}
