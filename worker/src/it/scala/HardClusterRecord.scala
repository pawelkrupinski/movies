package integration

import models.Country
import tools.{CommandLine, HardClusters, IntegrationMongoTarget}

import scala.util.Try

/**
 * Re-records the hard-cluster responses file(s) (`hard-clusters-responses-<cc>.json.gz`) and the films checked in for
 * the clusters (`expected-hard-clusters-<cc>.txt`) after the fixture grows or the pipeline asks new questions — what
 * `scripts/hard-clusters.sh record` and the ratchet a red convergence leg runs call:
 *
 * {{{
 *   scripts/it-local.sh "worker/IntegrationTest/runMain integration.HardClusterRecord pl uk --fixtures <dir holding enrichment-<cc>/>"
 * }}}
 *
 * Every pass [[HardClusterConvergenceIntegrationSpec]] replays is run against the countries' enrichment trees, and what
 * the passes do not ask — each spelling alone, the identity resolver's query set — asked too; every request the file
 * does not already answer is asked of the tree, and the file written back with the old answers AND the new. The new
 * clusters are the point here: what the passes find (a divergence, churn, an outage that does not recover) is printed
 * for the fix to answer — the spec judges it, over the recording, once committed — and never stops the recording.
 *
 * `MONGODB_URI` names the throwaway Mongo the passes run in. Exit: 0 recorded; 1 a pass could not run; 2 usage.
 */
object HardClusterRecord {

  val commandLine: CommandLine = CommandLine("HardClusterRecord", "Re-records the hard clusters' responses and expected films.",
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"))
    .withPositionals(CommandLine.Positional("country", "the countries re-recorded (default: every one with hard clusters)", repeatable = true))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val only = Option.when(arguments.positionals.nonEmpty)(arguments.positionals.map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"no country $code (one of ${Country.all.map(_.code).mkString(", ")})"))).toSet)
    val configuration = settings.ProcessConfiguration.resolve()
    val harness       = new settings.HarnessConfiguration(configuration.env)
    val root   = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(harness.fixtureRoot)
    val target = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to run the passes in"))
    target.requireThrowaway()
    val countries = HardClusterPasses.countries(only)
    if (countries.isEmpty) arguments.fail("no hard clusters checked in for the countries named")
    val passes = new HardClusterPasses(target, root, configuration.env, countries, recording = true)
    val recorded = try Try(record(passes)) finally passes.close()
    recorded.failed.foreach { e => System.err.println(s"[hard-clusters] the recording FAILED: $e"); e.printStackTrace(); sys.exit(1) }
  }

  /** Every pass the spec replays, in its order — each country's boots, the expected films written, the outages, the
   *  rescrapes — then the rest recorded and the responses written; what the passes found printed. */
  private def record(passes: HardClusterPasses): Unit = {
    passes.booted
    passes.countries.foreach { country =>
      val path = HardClusters.expectedFilmsPath(country)
      java.nio.file.Files.writeString(path, passes.actualFilms(country))
      println(s"[${country.code}] recording — rewrote $path")
    }
    passes.outages
    val found = passes.countries.map { country =>
      val (diverging, fixed) = passes.divergences(country)
      country -> (diverging ++ fixed.map(key => s"$key no longer diverges on the split arrival") ++ passes.servedProblems(country) ++
        passes.outageDrift(country) ++ passes.churnProblems(country))
    }
    passes.writeRecording()
    found.foreach {
      case (country, Nil)      => println(s"[hard-clusters] ${country.code}: the passes agree")
      case (country, problems) => println(s"[hard-clusters] ${country.code}: ${problems.size} finding(s) for the fix to answer:\n" +
        problems.take(20).mkString("\n"))
    }
  }
}
