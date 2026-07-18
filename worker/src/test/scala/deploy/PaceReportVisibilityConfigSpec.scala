package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the one logback line that makes ThrottledHttpFetch's per-host
 * "pace-report" visible in prod. The class lives in `tools`, which sits at the
 * root WARN level, so the summary — logged at INFO — is silently dropped unless
 * a targeted logger raises just this class. That failure mode is invisible: the
 * worker keeps running, the reports simply never appear, and a rate we're trying
 * to tune (Filmstarts' 429s) can't be read. Guard the config the same way the
 * durable-diagnostics spec guards the Dockerfile/toml lines no running-JVM test
 * layer reaches.
 *
 * Tests run with the repo root as CWD (the fixture specs load `test/resources`
 * the same way), so the resource path resolves directly.
 */
class PaceReportVisibilityConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val logback = RepoFile.read("worker/src/main/resources/logback-base.xml")

  "the worker logback config" should "surface tools.ThrottledHttpFetch at INFO so pace-reports print under root WARN" in {
    logback should include ("""<logger name="tools.ThrottledHttpFetch" level="INFO"/>""")
  }
}
