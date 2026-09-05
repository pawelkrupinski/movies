package services.metrics

/**
 * Pluck one `{country, cache}`-labelled sample out of a Prometheus text
 * exposition.
 *
 * Both tiers' cache specs need this and neither can use the plain
 * `PrometheusExposition.sample`, which matches a label string EXACTLY — these
 * families carry two labels, and a spec should not have to know which order the
 * client writes them in or what the other one's value is.
 *
 * Absence is meaningful here rather than an inconvenience: a cache that cannot
 * answer a question publishes no series for it (see [[CacheOccupancy]]), so
 * `None` is the assertion those specs make most often.
 */
object CacheMetricSamples {

  /** The value of `name{…cache="<cache>"…}`, or None when the series is absent. */
  def sample(exposition: String, name: String, cache: String): Option[Double] =
    exposition.linesIterator
      .filterNot(_.startsWith("#"))
      .find(line => line.startsWith(s"$name{") && line.contains(s"""cache="$cache""""))
      .map(_.trim.split("\\s+").last.toDouble)
}
