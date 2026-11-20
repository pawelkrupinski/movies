package services.metrics

/** Pluck one sample out of a Prometheus text exposition ([[PrometheusExposition.render]]'s output), so a metrics spec
 *  asserts the value it means, not the parsing. Both tiers' specs share it; [[CacheMetricSamples]] is its two-label
 *  sibling for the cache families. */
object ExpositionSamples {

  /** The value of the `name{labels}` line (labels spelled exactly as the exposition writes them, e.g.
   *  `subset="total"`), or None if absent. */
  def sample(text: String, name: String, labels: String): Option[Double] =
    text.linesIterator
      .find(_.startsWith(s"$name{$labels}"))
      .map(_.trim.split("\\s+").last.toDouble)

  /** The value of a label-less `name value` line (skipping the `# HELP`/`# TYPE` header lines and any labelled
   *  `name{…}` sibling), or None if absent. */
  def value(text: String, name: String): Option[Double] =
    text.linesIterator
      .find(l => l.startsWith(s"$name ") && !l.startsWith("#"))
      .map(_.trim.split("\\s+").last.toDouble)
}
