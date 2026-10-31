package scripts

import models.Country
import tools.{HttpFetch, InMemoryFleetHostPace, MissingFixtureFill, MissingFixtures, RealHttpFetch, TlsTrust}

import java.nio.file.{Files, Path}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A hermetic leg's convergence row's pre-suite fill (country-convergence-leg.yml), or a `Convergence fill` row: fetch,
 * within a budget, what the previous hermetic leg's tree could not answer, into a fixture tree of its own for the row to
 * publish beside the pinned pair. See [[tools.MissingFixtureFill]] for the policy and
 * docs/design/convergence-fixture-fill.md for why.
 *
 *   sbt "worker/Fixtures/runMain scripts.FillMissingFixtures <code> <refetch list> <held root> <out root> <until> [refused list]"
 *
 * `[refused list]`, when given, receives the gaps the origin refused ([[tools.MissingFixtureFill.writeRefused]]), for
 * the row to publish so the next fills skip them for a while.
 *
 * `<held root>` holds what is already recorded — a leg row's restored tree with the pair's fills over it,
 * or those fills alone — never fetched again; `<out root>` receives
 * this fill's — both fixture roots, `enrichment-<code>` under each. `<until>` (epoch seconds) is when
 * the row stops starting requests: a deadline rather than a duration, so the row's own setup and
 * compile, however long they took, come out of the same minutes. Every request goes through the
 * worker's own pacing (`HttpWiring.pacedWire`, `HostPolicies`) over the real wire, on the wall clock, and a
 * gap listed with its key masked is signed with the process's own (`TMDB_API_KEY`, [[tools.FillCredentials]]).
 * Exits 0 whatever it fetched: a fill is best effort, and what it missed the next leg's fill asks.
 */
object FillMissingFixtures {
  private val clock = java.time.Clock.systemUTC()


  val commandLine: tools.CommandLine = tools.CommandLine("FillMissingFixtures", "Fetches, within a deadline, what the previous hermetic leg's tree could not answer.")
    .withPositionals(tools.CommandLine.Positional("code", "the country filled (required)"),
      tools.CommandLine.Positional("refetch-list", "the gaps to fetch (required; none when the file is absent)"),
      tools.CommandLine.Positional("held-root", "the fixture root already recorded (required)"),
      tools.CommandLine.Positional("out-root", "the fixture root this fill records into (required)"),
      tools.CommandLine.Positional("until", "when the fill stops starting requests, epoch seconds (required)"),
      tools.CommandLine.Positional("refused-list", "where the gaps the origin refused go"))

  /** One fill as its command line asks it — a code no country has, an `until` that is no epoch second, or a part left
   *  out stops it; an argument past the refused list is refused, not ignored as `rest*` did. */
  final case class Run(country: Country, list: Path, held: Path, out: Path, until: Long, refused: Option[Path])

  def runOf(arguments: tools.CommandLine.Arguments): Run = {
    arguments.requiredPositional("code")
    val until = arguments.requiredPositional("until")
    Run(arguments.country("code").get, Path.of(arguments.requiredPositional("refetch-list")), Path.of(arguments.requiredPositional("held-root")),
      Path.of(arguments.requiredPositional("out-root")), until.toLongOption.getOrElse(arguments.fail(s"<until> wants epoch seconds, got $until")),
      arguments.positional("refused-list").map(Path.of(_)))
  }

  def main(args: Array[String]): Unit = {
    val Run(country, list, held, out, until, refusedList) = runOf(commandLine.parseOrExit(args))
    // What the origin refuses directly goes again through the residential proxy, whose Basic challenge on an HTTPS
    // tunnel the JDK refuses by default: every proxied request answered 407 (run 37677032787). The process's to set,
    // first — see ProxyTunnelAuthentication.
    tools.ProxyTunnelAuthentication.BasicAllowed.applyToJvm()
    val tree          = s"enrichment-${country.code}"
    val configuration = settings.ProcessConfiguration.resolve()
    val listed = if (Files.isRegularFile(list))
      Files.readAllLines(list).asScala.toSeq.flatMap(MissingFixtures.Refetch.parse)
    else Nil
    val gaps = listed.map(_._2)
    val tls   = TlsTrust.newContext()
    val pace  = new InMemoryFleetHostPace
    def paced(leaf: HttpFetch): HttpFetch =
      modules.wiring.HttpWiring.pacedWire(leaf, configuration, pace, clock, Thread.sleep)
    // The residential proxy, when the process holds its credentials: what the origin refuses directly is asked again
    // through it, as production reaches those origins (`MissingFixtureFill.route`).
    val proxy = MissingFixtureFill.residentialProxy(configuration, tls).map(paced)
    // Counted as the convergence leg's own wiring counts its wire, under its own phase, for the run's report of what
    // it asked each service (tools.CiHttpCalls).
    val calls = new services.metrics.WorkerHttpMetrics(Seq(country.code), new io.prometheus.metrics.model.registry.PrometheusRegistry())
    tools.CiHttpCalls.track(calls)
    val wire = new RealHttpFetch(tls = tls)
    val live: HttpFetch = new tools.CountingHttpFetch(MissingFixtureFill.route(paced(wire), proxy),
      calls.recorderFor(country.code, "fill"))
    val fill = new MissingFixtureFill(
      MissingFixtureFill.heldIn(settings.FixtureRoot(held), tree),
      MissingFixtureFill.recordingInto(settings.FixtureRoot(out), tree, live),
      threads = 8,
      sign = tools.FillCredentials.from(configuration).sign)
    val started = tools.Stopwatch.start()
    val outcome = fill.fill(gaps, (until - clock.instant().getEpochSecond).max(0L).seconds)
    val seconds = started.seconds
    val line = f"${country.displayName}: ${outcome.describe} in $seconds%.0fs " +
      f"(${(outcome.fetched + outcome.failed) / seconds.max(1.0)}%.1f req/s)"
    println(s"[fill] $line")
    refusedList.foreach { refusedList =>
      val refused = MissingFixtureFill.writeRefused(refusedList, listed, outcome.refused)
      println(s"[fill] $refused refused gap(s) the next fills skip for a while")
    }
    // What the legs after this one replay beyond the pinned tree, where the run's page shows it.
    configuration.stepSummaryFile.foreach(summary => scala.util.Try(Files.writeString(summary.value, s"**fill** — $line\n\n",
      java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)))
    scala.util.Try(tools.CiHttpCalls.write())
    sys.exit(0)
  }
}
