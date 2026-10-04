package services.readmodel

/** Where [[WebReadModel]] says it reopened a change stream that had ended — once per collection
 *  per reopen, so a stream dying over and over (Mongo still down, or a stream that cannot open at
 *  all) is a climbing series rather than a log line. */
trait ReadModelStreamMetrics {
  def reopened(collection: String): Unit
}

object ReadModelStreamMetrics {
  val noop: ReadModelStreamMetrics = (_: String) => ()

  /** The collections a web read model streams — the label values the metric is seeded over. */
  val Collections: Seq[String] = Seq(MongoReadModelRepository.MoviesCollection, MongoReadModelRepository.ScreeningsCollection)
}
