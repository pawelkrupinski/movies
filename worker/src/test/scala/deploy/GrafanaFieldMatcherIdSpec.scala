package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards dashboard field-override matchers against ids Grafana doesn't know.
 *
 * A `fieldConfig.overrides[].matcher.id` is looked up in a registry at RENDER
 * time, and an unknown id doesn't degrade to "no override" — it throws, and the
 * whole dashboard renders as `An unexpected error happened / Error: "<id>" not
 * found in: anyMatch,allMatch,...`. So one typo in one panel takes down every
 * panel on the page, and nothing in provisioning or the JSON API rejects it on
 * the way in.
 *
 * That is how `byRefID` (the plausible-looking spelling; the real id is
 * `byFrameRefID`) blanked the whole worker-diagnostics dashboard — it was only
 * ever used to dash the constant threshold lines, cosmetic on three panels.
 *
 * The registry below is Grafana's `fieldMatchers` set, as enumerated by the
 * error message the frontend prints when the lookup misses.
 */
class GrafanaFieldMatcherIdSpec extends AnyFlatSpec with Matchers {

  private val KnownMatcherIds = Set(
    "anyMatch",
    "allMatch",
    "invertMatch",
    "alwaysMatch",
    "neverMatch",
    "byType",
    "byTypes",
    "numeric",
    "time",
    "byName",
    "byRegexp",
    "byNames",
    "byRegexpOrNames",
    "byFrameRefID",
    "first",
    "firstTimeField",
    "byValue"
  )

  /** `"matcher": { "id": "byName", ... }` — the id right after a `matcher` key. */
  private val MatcherId = """"matcher"\s*:\s*\{\s*"id"\s*:\s*"([^"]+)"""".r

  private def dashboards(): Seq[java.io.File] =
    Option(new java.io.File("fly/grafana/provisioning/dashboards").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.getName.endsWith(".json"))
      .sortBy(_.getName)
      .toSeq

  "every provisioned dashboard" should "use field-matcher ids Grafana can resolve" in {
    val files = dashboards()
    files.size should be > 1 // otherwise the sweep is vacuous

    val unknown = files.flatMap { file =>
      MatcherId
        .findAllMatchIn(RepoFile.read(file.getPath))
        .map(_.group(1))
        .filterNot(KnownMatcherIds.contains)
        .map(id => s"${file.getName}: $id")
        .toSeq
    }

    withClue(
      s"unknown matcher id(s) — Grafana throws on lookup and the ENTIRE dashboard " +
        s"renders as an error, not just the panel: ${unknown.mkString(", ")}. " +
        s"Valid ids: ${KnownMatcherIds.toSeq.sorted.mkString(", ")}. "
    ) {
      unknown shouldBe empty
    }
  }

  it should "actually carry matchers for this guard to check" in {
    // If overrides ever vanish from every dashboard the check above passes
    // vacuously; keep it honest.
    val ids = dashboards().flatMap(f => MatcherId.findAllMatchIn(RepoFile.read(f.getPath)).map(_.group(1)).toSeq)
    ids should not be empty
  }
}
