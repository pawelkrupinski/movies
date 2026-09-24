package services.readmodel

/** Where a whole-collection scan counts a document it SKIPPED because it could not decode
 *  it — and where the `movies` repository counts one it could not decode, which fails its read. The skip keeps one malformed document from sinking the page it arrived in, but a
 *  skipped document is a film or screening the reader silently goes without — a WARN line
 *  alone let that pass unnoticed. The worker and the web each wire a Prometheus-backed
 *  counter (`kinowo_{worker,web}_decode_failures_total{country, collection}`, alerted by
 *  `DocumentsUndecodable`); scripts and most tests use [[DecodeFailureMetrics.noop]]. */
trait DecodeFailureMetrics {
  def recordDecodeFailure(collection: String): Unit
}

object DecodeFailureMetrics {
  val noop: DecodeFailureMetrics = (_: String) => ()

  /** The `movies` collection, whose reads do not skip an undecodable document but FAIL on it:
   *  the point read answers "unreadable" and the corpus scan comes back incomplete. */
  val SourceMoviesCollection = "movies"

  /** Every collection counted, seeded at 0 so the first failure moves `increase()`. */
  val Collections: Seq[String] =
    Seq(MongoReadModelRepository.MoviesCollection, MongoReadModelRepository.ScreeningsCollection, SourceMoviesCollection)

  /** Whether a failed READ failed on the document rather than on the way to it. The codec's
   *  refusals take whatever shape the codec throws — a `BSONException`, a
   *  `CodecConfigurationException`, a `ClassCastException` out of a macro codec meeting a string
   *  where it wants a map — so this is read the other way round: anything that is not the driver,
   *  the network or the wait failing (`MongoException`, an `IOException`, a timeout, an
   *  interrupt) happened decoding what did arrive. */
  def isDecodeFailure(failure: Throwable): Boolean =
    !Iterator.iterate(failure)(_.getCause).takeWhile(_ != null).take(10).exists {
      case _: com.mongodb.MongoException | _: java.io.IOException |
           _: java.util.concurrent.TimeoutException | _: InterruptedException => true
      case _ => false
    }
}
