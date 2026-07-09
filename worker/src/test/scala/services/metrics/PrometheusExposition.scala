package services.metrics

import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry

import java.io.ByteArrayOutputStream

/** Shared test helper for the metrics specs: render a registry to its text
 *  exposition and pluck a single gauge sample by name + labels. Keeps the
 *  render/parse boilerplate in one place so a spec asserts intent, not plumbing. */
object PrometheusExposition {
  def render(registry: PrometheusRegistry): String = {
    val out = new ByteArrayOutputStream()
    PrometheusTextFormatWriter.create().write(out, registry.scrape())
    out.toString("UTF-8")
  }

  /** The value of the `name{labels}` gauge line (labels spelled exactly as the
   *  exposition writes them, e.g. `subset="total"`), or None if absent. */
  def sample(text: String, name: String, labels: String): Option[Double] =
    text.linesIterator
      .find(_.startsWith(s"$name{$labels}"))
      .map(_.trim.split("\\s+").last.toDouble)

  /** The value of a label-less `name value` gauge line (skips the `# HELP`/`# TYPE`
   *  header lines and any `name…{…}` labelled sibling), or None if absent. */
  def value(text: String, name: String): Option[Double] =
    text.linesIterator
      .find(l => l.startsWith(s"$name ") && !l.startsWith("#"))
      .map(_.trim.split("\\s+").last.toDouble)
}
