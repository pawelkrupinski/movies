package services.metrics.worker

/** The worker's shared histogram upper bounds: the task pipeline's handle times and the read model's writes read the
 *  same duration buckets, so a panel can set the two side by side. */
object HistogramBuckets {

  /** Fixed histogram upper bounds (seconds), spanning a sub-second freshness
   *  skip up to a slow multi-minute scrape/detail fetch. */
  val DurationSeconds: Seq[Double] = Seq(0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 30.0, 60.0, 120.0)

  /** Upper bounds (seconds) for one pure projectAll — sub-millisecond for a
   *  single-variant row up to the ~0.3s worst-case many-variant film (post
   *  synopsis-memoization; pre-memoization the tail reached ~2.3s). */
  val ProjectSeconds: Seq[Double] = Seq(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5)
}
