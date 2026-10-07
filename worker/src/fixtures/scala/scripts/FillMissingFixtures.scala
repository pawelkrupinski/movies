package scripts

import models.Country
import tools.{HttpFetch, InMemoryFleetHostPace, MissingFixtureFill, MissingFixtures, RealHttpFetch, TlsTrust}

import java.nio.file.{Files, Path}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A convergence leg's `fill` row (country-convergence-leg.yml): fetch, within a budget, what the
 * previous hermetic leg's tree could not answer, into a fixture tree of its own for the row to
 * publish beside the pinned pair. See [[tools.MissingFixtureFill]] for the policy and
 * docs/design/convergence-fixture-fill.md for why.
 *
 *   sbt "worker/Fixtures/runMain scripts.FillMissingFixtures <code> <refetch list> <held root> <out root> <budget s> [threads]"
 *
 * `<held root>` holds the fills earlier legs published (never fetched again); `<out root>` receives
 * this fill's — both fixture roots, `enrichment-<code>` under each. Every request goes through the
 * worker's own pacing (`HttpWiring.pacedWire`, `HostPolicies`) over the real wire, on the wall clock.
 * Exits 0 whatever it fetched: a fill is best effort, and what it missed the next leg's fill asks.
 */
object FillMissingFixtures {

  def main(args: Array[String]): Unit = args match {
    case Array(code, list, held, out, budget, rest*) =>
      val country       = Country.all.find(_.code == code).getOrElse(sys.error(s"no country $code"))
      val tree          = s"enrichment-${country.code}"
      val configuration = settings.ProcessConfiguration.resolve()
      val gaps = if (Files.isRegularFile(Path.of(list)))
        Files.readAllLines(Path.of(list)).asScala.toSeq.flatMap(MissingFixtures.Refetch.parse).map(_._2)
      else Nil
      val live: HttpFetch = modules.wiring.HttpWiring.pacedWire(new RealHttpFetch(tls = TlsTrust.newContext()),
        configuration, new InMemoryFleetHostPace, java.time.Clock.systemUTC(), Thread.sleep)
      val fill = new MissingFixtureFill(
        MissingFixtureFill.heldIn(settings.FixtureRoot(Path.of(held)), tree),
        MissingFixtureFill.recordingInto(settings.FixtureRoot(Path.of(out)), tree, live),
        threads = rest.headOption.flatMap(_.toIntOption).getOrElse(8))
      val started = System.nanoTime()
      val outcome = fill.fill(gaps, budget.toInt.seconds)
      val seconds = (System.nanoTime() - started) / 1e9
      println(f"[fill] ${country.displayName}: ${outcome.describe} in $seconds%.0fs " +
        f"(${(outcome.fetched + outcome.failed) / seconds.max(1.0)}%.1f req/s)")
      sys.exit(0)
    case _ =>
      System.err.println("usage: FillMissingFixtures <code> <refetch list> <held root> <out root> <budget seconds> [threads]")
      sys.exit(64)
  }
}
