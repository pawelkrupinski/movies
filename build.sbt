// ── Multi-module layout ─────────────────────────────────────────────────────
//
//   geodata — the GENERATED roster and geography tables (`models.*RosterData`, `CitySlugIndexData`,
//             `VenueTowns`): plain tuples and strings, written by data/**/generate_*.py and read
//             only through common's typed rosters. dependsOn nothing.
//   common  — shared domain + data layer (models, MovieRepository/MovieCache, Mongo,
//             UptimeMonitor, the HTTP-fetch family): what more than one module builds on. Code only one
//             app uses lives in that app. Plain Scala lib. dependsOn(geodata).
//   testkit — shared test fakes/helpers (not deployed). dependsOn(common).
//   scrapers — the cinema scrapers (`services.cinemas`): every venue client, its catalogues and the
//             scrape contract the worker drives them by. dependsOn(common, identity). A library, not
//             an app: the worker depends on it and ships its classes.
//   web     — content-serving Play app (controllers, Twirl views). The k3s web tier.
//   worker  — scrape + enrich background app (plain `def main`). The per-country k3s worker pods.
//             worker's `fixtures` sbt config (src/fixtures/scala) holds the fixture
//             pipeline harness (TestWiring/FixtureTestWiring/ReadModelSnapshot).
//             e2e and web/PageTest depend on it via "test->fixtures" / "page->fixtures"
//             so they compile only those 5 sources, not worker's full ~320-spec test tree.
//   e2e     — cross-app end-to-end specs (not deployed). dependsOn(web, worker).
//
// `root` is a pure aggregator — no sources of its own. web and worker each
// depend on common, and their Compile classpaths never see each other: the two
// deployed apps share the Mongo database, not in-process state. The one cross
// edge is test-only — web's PageTest config depends on worker's `fixtures`
// config ("page->fixtures", below) for the fixture pipeline harness. testkit
// and e2e are libraries — only web and worker are ever staged/deployed, so the
// build still ships exactly two apps.

// Every third-party artifact + its version lives in project/Dependencies.scala;
// the modules below reference those vals so no version string is repeated here.
import Dependencies._

ThisBuild / organization := "com.example"
ThisBuild / version      := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.9.0"

// Compiler options shared by every module. Play's sbt plugin adds -unchecked on top
// of these for the web project (and -deprecation a second time, harmlessly).
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  // Every module, not only web (Play's default). Without it a deprecated API use outside
  // web still failed -Werror, but only as "there was 1 deprecation warning; re-run with
  // -deprecation for details" -- no file, no line. With it the error names the call site.
  "-deprecation",
  // Unused imports, locals, privates, parameters, implicits and pattern variables: dead code fails the build. An
  // intentionally unread parameter (a trait default meant to be overridden, a hook) is `@unused`.
  "-Wunused:all",
  // `-Wsafe-init` (initialization order) is added for main sources only: project/MainSourceInitChecks.scala.
  // A type parameter shadowing an outer one, and a constructor parameter shadowing a field the class inherits (a
  // subclass passing `cache` to its parent's `cache` and then reading its own copy): rename the parameter.
  "-Wshadow:all",
  // Warnings are errors: a warning must be fixed (or explicitly silenced via a
  // `-Wconf` rule below), never merged. Keeps the build warning-clean over time.
  "-Werror",
  // Scala 3.9.0 caps `-java-output-version` at 21 (higher values are rejected).
  // JRE 27 loads Java 21 class files unchanged: build on JDK 27, emit 21, run 27.
  "-java-output-version", "21",
  // Twirl-generated warnings come out without a parseable category — filter by
  // path. `app/views/` only holds Twirl templates. (No-op outside web.)
  "-Wconf:src=.*views/.*:silent",
  // Play's generated router binds every route's parameters in patterns it never reads; its warnings point at the
  // `routes` file but come from the generated `routes/main/router` sources. (No-op outside web.)
  "-Wconf:src=.*/routes/main/.*:silent",
  // An outcome that says a read failed, saw only part of a collection, or lost its write to
  // another writer must not be dropped unread: an unused value of one of these types is an error. The warning is on
  // for every type and silenced for all others — dropping an `Int` or a `Future` is
  // not what this guards. See `tools.ScanOutcome`, `tools.ReadOutcome` and `tools.GuardedWrite`.
  // `-Wvalue-discard` is the same rule where the value lands in a Unit-typed position instead of
  // a statement — a `def f(): Unit = repo.scan(...)`, a lambda handed to `foreach` or `boot.step`.
  "-Wnonunit-statement",
  "-Wvalue-discard",
  "-Wconf:msg=(?s)^(?!.*\\b(ScanOutcome|ReadOutcome|GuardedWrite)\\b).*(unused value|[dD]iscarded non-Unit value).*:silent"
)
ThisBuild / javacOptions ++= Seq("--release", "21")

// Nobody reads this build's Scaladoc — there is no published API and no docs
// site — but `stage` built it anyway: Play's dist ships `share/doc/api`, and
// native-packager pulls the same for worker, so a deploy generated docs for
// FOUR modules (web, common, testkit, worker) including a test-support one.
// That put dotty's scaladoc on the deploy critical path, and it fell off it:
// CI's JDK-25 runner NPE'd inside `SignatureBuilder` rendering common's search
// index, on a commit whose only diff was a workflow file. An unread artefact
// must not be able to fail a deploy, so don't build it — emptying `doc`'s
// sources is enough to drop `share/doc` from both dists. To generate docs on
// demand, hand the sources back for one run:
//
//   sbt 'set common/Compile/doc/sources := (common/Compile/sources).value' common/doc
//
// A CI step in ci.yml re-checks that neither staged dist carries a `doc` tree.
lazy val noApiDocs = Seq(
  Compile / doc / sources := Nil,
)

// The heap-dump budget script, shipped in BOTH deployable dists as `bin/heap-dumps.sh`. The
// Dockerfile CMD runs it before the JVM starts (prune this pod's dump directory, then name a
// unique -XX:HeapDumpPath file for this start). It lives under infra/ because the node's
// systemd timer runs the SAME file and the Nix flake root is infra/ -- one copy, two callers.
lazy val heapDumpScript = Seq(
  Universal / mappings +=
    ((LocalRootProject / baseDirectory).value / "infra" / "nix" / "files" / "heap-dumps.sh") -> "bin/heap-dumps.sh",
)

// JVM options baked into each dist's conf/application.ini. The start script puts them AFTER the
// overlay's JAVA_OPTS on the java command line, so for a flag both name these win: each tier's
// collector and JIT shape (worker.options, web.options), which the image's AOT cache is trained under.
// LauncherJvmFlagsSpec reads back the flags a JVM runs under them.
def launcherOptions(files: String*) = Seq(
  Universal / javaOptions ++= files.flatMap { name =>
    IO.readLines((LocalRootProject / baseDirectory).value / "infra" / "jvm" / name)
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .map("-J" + _)
  },
)

// Integration tests (it/scala) and page-regression tests (page/scala) run under
// their own sbt configurations so CI can dispatch them as separate jobs. Both
// `extend Test` to reuse helpers from test/scala. Defined here because both the
// worker and web modules attach the `it` config.
lazy val IntegrationTest = config("it") extend Test
lazy val PageTest = config("page") extend Test
// The handful of classes (TestWiring, FixtureTestWiring, ReadModelSnapshot, the
// SameThread* determinism helpers) that e2e and web's PageTest need from worker's
// test tree to boot the fixture pipeline — but NOT worker's other ~320 unit specs.
// `extend Compile` (not Test) so this config sees only worker's main code, never
// worker's own specs; that's also what breaks the cycle e2e/web would otherwise
// hit by depending on worker's full Test config (which depends on this one too).
lazy val Fixtures = config("fixtures") extend Compile

// Every module writes its JUnit XML into ONE root-level dir per layer, so each
// CI job's single action-junit-report step (`target/test-reports/<layer>/**`)
// annotates failures from every module. Without this, per-module
// `<module>/target/test-reports/...` dirs would escape the root-relative glob
// and the check-run would miss most modules.
lazy val unitReportSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u",
    ((LocalRootProject / baseDirectory).value / "target" / "test-reports" / "unit").toString)
)
// ORDER INDEPENDENCE. Unit specs share one JVM per module, so a spec that only passes
// because another ran first — or never ran first — passes forever in discovery order.
// `TEST_ORDER_SEED=<n>` runs every module's unit suites one at a time in an order shuffled
// by that seed (`reverse` for plain reversed order); unset, nothing changes. The nightly
// order-independence workflow sets it; reproduce a failure with the seed it prints:
//     TEST_ORDER_SEED=<n> sbt testUnitNoE2e
lazy val testOrderSettings: Seq[Setting[?]] = sys.env.get("TEST_ORDER_SEED").toSeq.flatMap { seed =>
  Seq(
    Test / parallelExecution := false,
    Test / definedTests := {
      val suites = (Test / definedTests).value.sortBy(_.name)
      val ordered =
        if (seed == "reverse") suites.reverse
        else new scala.util.Random(seed.toLong).shuffle(suites)
      streams.value.log.info(s"TEST_ORDER_SEED=$seed: ${thisProject.value.id} runs ${ordered.size} suites in that order")
      ordered
    }
  )
}

// CI ROWS. A layer too CPU-heavy for one 4-core runner runs on several, each selecting its suites through a
// filter named by an environment variable: every row runs the same command, so `all` still keeps going past a
// failing module (it takes no `testOnly` arguments). The LAST row of each is "every suite no other row selects",
// so a new suite always lands in exactly one row. Unset or empty, nothing is filtered (local runs).
def rowSettings(variable: String, config: Configuration, rows: Seq[(String, String => Boolean)]): Seq[Setting[?]] =
  sys.env.get(variable).filter(_.nonEmpty).toSeq.map { row =>
    val selected: String => Boolean = rows.toMap.get(row) match {
      case Some(select) => select
      case None if row == "rest" => suite => !rows.exists(_._2(suite))
      case None => sys.error(s"$variable=$row: expected one of ${(rows.map(_._1) :+ "rest").mkString(", ")}")
    }
    config / testOptions += Tests.Filter(selected)
  }

// `itAll` on one runner was CPU-bound (828 s of suites on 4 cores, 2026-10-15): `IT_ROW=identity` runs the hard-cluster
// convergence, whose parallel boots fill a runner's cores; `IT_ROW=rest` every other integration spec.
lazy val IdentityIntegrationSuites = Set(
  "integration.HardClusterConvergenceIntegrationSpec")
lazy val itRowSettings = rowSettings("IT_ROW", IntegrationTest, Seq("identity" -> IdentityIntegrationSuites))

// The worker's ~6,300 unit tests on one runner were CPU-bound too (409 s of suites at 4 cores, 2026-10-15), split into
// rows. THE HEAVY ROWS (2026-10-28, the owner: the suites that replay whole recordings or captures "don't belong in a
// unit stage", but still run before every deploy): the suites that re-decide a recording's slices or re-resolve the
// checked-in captures, ~610 of the worker's ~1,230 suite-seconds, run on two rows of their own, `heavy1` and `heavy2`,
// cut by measured suite time (Forgejo run 3419, test step at load ~10: each row's longest suite 146 s and 120 s). They
// are rows of ci.yml's `test` job, which main.yml's publish jobs need (`needs: ci`), so they gate every deploy as
// before. The unit rows proper: `identity` (the resolver's other specs, the tools' and the identity tooling's) and
// `rest`, which CI runs beside the web and scrapers modules. Every unit row's suites are mock-only either way
// (NoRealDependenciesInUnitSpecsSpec).
lazy val HeavyRows: Map[String, Set[String]] = Map(
  "heavy1" -> Set(
    "services.identity.IdentitySliceCasesSpec",       // every pinned slice of a recording, decided again
    "scripts.IdentityUnifiedFitSpec",                 // the unified model's weights fitted again from every capture's rows
    "services.identity.NonFilmEventsFixtureSpec",     // the non-film events over every capture's unmatched clusters
    "tools.UnmatchedClustersGapsSpec",                // a capture replayed for the lookups it lacks
    "scripts.IdentityDriftSlicesShard1Spec"),         // the drift slices' second shard (IdentityDriftSlicesSpec)
  "heavy2" -> Set(
    "scripts.IdentityRefitMeasureAllSpec",            // the refit's whole-vs-sharded measure over two captures
    "services.identity.UnmatchedClustersRatchetSpec", // the ratchet: the five captures replayed
    "scripts.IdentityRefitCaptureReplaySpec",         // the refit's re-resolve of a capture
    "services.identity.MatchedTakesLabelSpec",        // every capture's matched takes against the labels
    "scripts.IdentityDriftSlicesShard2Spec",          // the drift slices' third shard (IdentityDriftSlicesSpec)
    "services.identity.FamilyIndexCompletenessSpec")) // every slice's family index, against corpora a population apart
lazy val HeavySuites: Set[String] = HeavyRows.values.flatten.toSet
// Identity suites the catch-all `rest` row (CI's `worker`) runs instead: a drift-slice shard (IdentityDriftSlicesSpec).
lazy val WorkerRestSuites: Set[String] = Set("scripts.IdentityDriftSlicesShard3Spec")
lazy val workerRowSettings = rowSettings("WORKER_ROW", Test, Seq(
  "heavy1"   -> HeavyRows("heavy1"),
  "heavy2"   -> HeavyRows("heavy2"),
  "identity" -> (suite => !HeavySuites(suite) && !WorkerRestSuites(suite) &&
    Seq("services.identity.", "scripts.", "tools.").exists(suite.startsWith))))

lazy val itReportSettings = Seq(
  IntegrationTest / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u",
    ((LocalRootProject / baseDirectory).value / "target" / "test-reports" / "it").toString)
)

// ── Generated roster and geography data ──────────────────────────────────────
//
// ~20k lines of generated tables (`data/**/scripts/generate_*.py` write them; never edited by hand) in plain tuples
// and strings, so they need nothing of common: common's typed rosters (`GermanRoster`, `SpanishRoster`, `UsRoster`,
// `City`, `Country`) read them. A module of their own so an edit to common does not recompile them, and a
// regenerated roster recompiles only them and what reads them.
lazy val geodata = (project in file("geodata"))
  .settings(name := "geodata")
  .settings(noApiDocs)

// ── Shared module ────────────────────────────────────────────────────────────

lazy val common = (project in file("common"))
  .dependsOn(geodata)
  .settings(
    name := "common",
    // standard sbt layout: src/main/scala, src/test/scala, src/test/resources
    libraryDependencies ++= Seq(
      play,             // play.api.Logging (the only Play API the shared code uses)
      mongoScalaDriver,
      Dependencies.caffeine,   // qualified: Play's autoImport also defines `caffeine`
      jsoup,
      // Prometheus client. Lives here, not in one app, because BOTH deployed
      // JVMs expose exposition text on their own /metrics: the worker's task
      // pipeline and the web app's uptime/served gauges + JVM resource
      // collectors. `services.metrics.JvmProcessMetrics` /
      // `PrometheusExposition` are the shared pieces they both build on, so the
      // dependency belongs at the shared layer they live in.
      // `-instrumentation-jvm` supplies the standard process/JVM collectors.
      prometheusCore,
      prometheusText,
      prometheusJvm,
      // common's test config covers ONLY its own specs (models, codecs, Mongo,
      // DaemonExecutors, …) — the shared fakes live in `testkit`, not here, so
      // common's test surface stays lean and self-contained.
      scalatestPlay % Test,
      scalatestScalaCheck % Test,
    ),
    // The shared time doubles (`sharedTestDoubles`).
    Test / unmanagedSources ++= sharedTestDoubles.value
  )
  .settings(unitReportSettings, testOrderSettings)
  .settings(ForkedTests.settings(Test, heap = "512m"))
  .settings(UnitClasspathGuard.settings)
  .settings(noApiDocs)

// The hand-stepped time doubles, the polling helper, the recording tick recorder, the one-country title normalizer, the property specs' base,
// the split booking-URL row shapes and the stored-rows repository live ONCE, in testkit, beside every other shared fake. common's
// and identity's own specs need them too, but neither can depend on testkit (testkit depends on both), so those sources
// are compiled into their test configs as well. A test classpath is never on another module's, so the compilations
// never meet.
lazy val sharedTestDoubles = Def.setting(Seq("tools/MutableClock", "tools/ManualScheduler", "tools/Eventually", "tools/SpecClock",
  "tools/SpecTimeouts", "tools/Answered", "tools/costs/CpuMeter", "tools/RecordingTickRecorder", "services/movies/SingleCountryNormalizer", "services/IdentityPropertySpec",
  "services/movies/SplitBookingUrlShapes", "services/movies/StoredRowsRepository").map { name =>
  (LocalRootProject / baseDirectory).value / "testkit" / "src" / "main" / "scala" / s"$name.scala"
})

// The identity stores' in-memory fakes, in testkit beside the rest, compiled into identity's own test config as the
// time doubles are.
lazy val identityTestDoubles = Def.setting(Seq("InMemoryIdentityTraceStore", "InMemoryPinStore").map { name =>
  (LocalRootProject / baseDirectory).value / "testkit" / "src" / "main" / "scala" / "services" / "identity" / s"$name.scala"
})

// ── Identity resolver ────────────────────────────────────────────────────────
//
// The identity model (`services.identity`): the resolver, its agreement stage, its stores and measures. Built on
// common — its models, titles, Mongo — and nothing in common builds on it (it cannot: common does not depend on it); the
// worker runs it, the web app reads its traces and pins.
lazy val identity = (project in file("identity"))
  .dependsOn(common)
  .settings(
    name := "identity",
    // The identity model's rules version (`services.identity.IdentityRules`): a digest of what the resolver is built
    // from — the sources the compiler records the resolver and its store as reaching, here and in common, and the
    // resources none but other code names (`IdentityRulesSources`). A deploy that changes none of them keeps the model's
    // families; one that changes any re-resolves them. The digested paths ship beside it (`identity-rules-sources.txt`).
    // The identity projection's venue slot code is versioned the same way (`VenueSlotMemo.codeVersion`), from what builds
    // a slot (`IdentityRulesSources.VenueSlotRoots`, not following the leaves that reach the resolver): a deploy that
    // changes none of it keeps the fingerprints a restarted worker reuses its stored slots by.
    // Both digest a Scala source's code, not its comments or spare whitespace (`project/SourceDigest.scala`): a
    // comment-only edit moves neither. Paths are the repository's (`identity/src/main/...`, `common/src/main/...`).
    Compile / resourceGenerators += Def.task {
      val repo     = (LocalRootProject / baseDirectory).value
      val modules  = Seq(
        IdentityRulesSources.Module("identity", (Compile / compile).value),
        IdentityRulesSources.Module("common", (common / Compile / compile).value),
        IdentityRulesSources.Module("geodata", (geodata / Compile / compile).value))
      Seq(("identity-rules", IdentityRulesSources.Roots, Set.empty[String]),
          ("venue-slot", IdentityRulesSources.VenueSlotRoots, IdentityRulesSources.VenueSlotLeaves),
          ("agreement-rules", IdentityRulesSources.AgreementRoots, Set.empty[String])).flatMap { case (name, roots, leaves) =>
        val sources   = IdentityRulesSources.of(modules, roots, leaves)
        val resources = IdentityRulesSources.resources(repo, modules.map(_.name), sources.filterNot(leaves))
        val paths     = sources ++ resources
        // under the module's own package (`ClasspathResource.Stamps`): a stale copy elsewhere can never shadow it
        val stamps  = (Compile / resourceManaged).value / "services" / "identity"
        val version = stamps / s"$name-version.txt"
        val listed  = stamps / s"$name-sources.txt"
        IO.write(version, kinowo.build.SourceDigest.of(paths, path => IO.readBytes(repo / path)))
        IO.write(listed, paths.mkString("\n"))
        Seq(version, listed)
      }
    }.taskValue,
    libraryDependencies ++= Seq(
      // ICU's Any-Latin transliteration: the identity measures compare a director credited in
      // another script (TMDB's "毕赣") with a venue's Latin spelling ("Bi Gan"). The JDK has none.
      Dependencies.icu4j,
      scalatestPlay % Test, scalatestScalaCheck % Test),
    Test / unmanagedSources ++= sharedTestDoubles.value ++ identityTestDoubles.value,
    // The build's source digest (the identity rules and venue slot versions), compiled here too so its specs check
    // the very code the build digests with.
    Test / unmanagedSources += (LocalRootProject / baseDirectory).value / "project" / "SourceDigest.scala"
  )
  .settings(unitReportSettings, testOrderSettings)
  .settings(ForkedTests.settings(Test, heap = "1g"))
  .settings(UnitClasspathGuard.settings)
  .settings(noApiDocs)

// ── Shared test support (not deployed) ───────────────────────────────────────
//
// The fakes/helpers every module's tests reuse — FakeHttpFetch, InMemoryMovieRepository,
// RecordingHttpFetch, RoutingHttpFetch, Eventually, ExecutorProbes. Sources live
// in src/main/scala so consumers pull them with a plain `dependsOn(testkit % Test)`
// rather than the old `common % "test->test"` re-export. That keeps common's test
// surface to its own specs and stops worker-only fakes from being forced into
// common. Depends on common for HttpFetch/MovieRepository/models; carries scalatest on
// the compile classpath because Eventually returns an `org.scalatest.Assertion`.
lazy val testkit = (project in file("testkit"))
  .dependsOn(common, identity)
  .settings(testOrderSettings)
  .settings(ForkedTests.settings(Test, heap = "512m"))
  .settings(UnitClasspathGuard.settings)
  .settings(
    name := "testkit",
    publish / skip := true,
    // ScalaCheck too: the property specs' base (`services.IdentityPropertySpec`) is one of the shared doubles.
    libraryDependencies ++= Seq(scalatestPlay, scalatestScalaCheck),
  )
  .settings(noApiDocs)

// ── Integration test support (not deployed) ──────────────────────────────────
//
// The helpers that reach REAL things -- a Mongo cluster (IntegrationMongo, the isolated and corpus databases, query
// plans, oplog writes, run claims), a child JVM (ChildJvm), a TCP forwarder, git (ScratchGitRepository), the JDK's
// keytool, the repository's scripts (ScriptTests). Only the integration (`it`), page and e2e configs depend on it; no
// unit Test config may (the owner, 2026-10-27: "unit tests have all external deps mocked"), which
// UnitClasspathGuard enforces on every unit Test classpath. testkit keeps the fakes, in-memory implementations and
// pure helpers the unit specs use.
lazy val itkit = (project in file("itkit"))
  .dependsOn(common, identity, testkit)
  .settings(
    name := "itkit",
    publish / skip := true,
    libraryDependencies ++= Seq(scalatestPlay),
  )
  .settings(noApiDocs)

// ── Cinema scrapers ──────────────────────────────────────────────────────────
//
// Every venue client (`services.cinemas.{pl,uk,us,de,es,roster}`), the catalogues that list them, and the scrape
// contract the worker drives them through (`services.cinemas.common`). A module of its own so an edit to one scraper
// recompiles the scrapers, not the rest of the worker. Built on common and identity; the worker depends on it.
lazy val scrapers = (project in file("scrapers"))
  .dependsOn(common, identity, testkit % Test)
  .settings(
    name := "scrapers",
    libraryDependencies ++= Seq(scalatestPlay % Test, scalatestScalaCheck % Test),
    // The scraper specs read their captured pages off the shared repo-root `test/resources` tree, by path and
    // through `getResourceAsStream("/fixtures/…")` — referenced in place on the test classpath, as the worker does.
    Test / unmanagedClasspath += Attributed.blank((LocalRootProject / baseDirectory).value / "test" / "resources"),
  )
  .settings(unitReportSettings, testOrderSettings)
  .settings(ForkedTests.settings(Test, heap = "512m"))
  .settings(UnitClasspathGuard.settings)
  .settings(noApiDocs)

// ── Worker app (scrape + enrich) ─────────────────────────────────────────────
//
// Plain Scala app (no Play): its only inbound HTTP is a small ops server (/health, /ready, /metrics, /heapdump, /profile), so it
// skips Play's server/router/Twirl stack — leaner, and a `def main` boot avoids
// the `extends App` init-order hazards with Mongo/Sentry. It writes through
// MovieCache to Mongo; the web app's cache picks those writes up via the Mongo
// change stream, so the two processes share no in-process state.
lazy val worker = (project in file("worker"))
  // The identity-slice tool files (`OlderRefsPackage`) import the identity adapters from both packages a ref may keep
  // them in, under `@nowarn("msg=unused import")`. At this ref neither import is unused, so that `@nowarn` suppresses
  // nothing here — which -Wunused:all reports — while an older ref still needs it.
  .settings(scalacOptions += "-Wconf:msg=@nowarn annotation does not suppress any warnings&src=.*/worker/src/(fixtures|test)/.*:silent")
  // `it` too: the integration specs need the same shared doubles the unit specs do —
  // notably the repositories that report an INCOMPLETE read, which is the only way to
  // test that a caller honours a completeness flag.
  // `test->test` on scrapers: the worker's specs drive its scrape tasks through the scrapers' own test doubles
  // (`StubCinemaScraper`, `ScriptedCinemaScraper`, `FakeDetailEnricher`) and recording tools.
  .dependsOn(common, identity, scrapers % "compile->compile;test->test", testkit % "test,it,fixtures->compile", itkit % "it,fixtures->compile")
  .enablePlugins(JavaAppPackaging)
  .configs(IntegrationTest, Fixtures)
  .settings(
    name := "worker",
    // standard sbt layout: src/main/scala, src/test/scala
    Compile / mainClass := Some("modules.WorkerMain"),
    // the hand judgements of takes, bundled as `/identity-judgements.tsv`: the suspect-take count leaves out the judged
    // ones and counts the takes judged wrong apart (services.identity.TakeJudgements)
    Compile / resourceGenerators += Def.task {
      val bundled = (Compile / resourceManaged).value / "identity-judgements.tsv"
      IO.copyFile((ThisBuild / baseDirectory).value / "test" / "resources" / "fixtures" / "identity-judgements.tsv", bundled)
      Seq(bundled)
    }.taskValue,
    // Integration tests (live scrape + Mongo) run under their own config so CI
    // can dispatch them as a separate job — same pattern as the web module.
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "src" / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "src" / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    // src/fixtures/scala/tools/{TestWiring,FixtureTestWiring,SameThreadExecutor*,
    // ReadModelSnapshot} — see the Fixtures config comment above. Compiled once
    // here; worker's own specs (below) and e2e/web's PageTest (cross-project,
    // via the worker % "...->fixtures" mapping) all reuse this one compile.
    inConfig(Fixtures)(Defaults.configSettings),
    Fixtures / scalaSource := baseDirectory.value / "src" / "fixtures" / "scala",
    // worker's own specs still construct TestWiring/FixtureTestWiring directly —
    // make Fixtures' compiled output visible on worker's Test classpath.
    Test / unmanagedClasspath ++= (Fixtures / exportedProducts).value,
    // …and on `it`'s, for the fixtures-side stores whose Mongo round-trip can only
    // be tested against a real Mongo (MongoEnrichmentCacheStore's TTL index).
    IntegrationTest / unmanagedClasspath ++= (Fixtures / exportedProducts).value,
    // …and the unit specs' own helpers (RepoFile, the recorded fixtures' readers): a spec that moved to `it` for its
    // real dependency (a script, git, a child JVM: itkit) keeps the helpers it was written with.
    IntegrationTest / unmanagedClasspath ++= (Test / fullClasspath).value,
    // …and identity's: ResolverAllocationIntegrationSpec runs identity's ResolverAllocationSpec in a child JVM.
    IntegrationTest / unmanagedClasspath ++= (identity / Test / exportedProducts).value,
    // A handful of enrichment specs (MetacriticClientSpec, RottenTomatoesClientSpec,
    // ImdbClientSpec, ImdbIdResolverSpec) read fixtures off the CLASSPATH via
    // `getResourceAsStream("/fixtures/…")`, but those HTML captures live in the
    // shared repo-root `test/resources` tree, not under worker/src/test/resources.
    // Reference that dir on the test classpath WITHOUT copying it — the tree is
    // ~400MB, so copyResources is out; unmanagedClasspath points at it in place.
    Test / unmanagedClasspath += Attributed.blank((LocalRootProject / baseDirectory).value / "test" / "resources"),
    // sbt runs `Test/runMain` (the recorder, the identity tools) through bgRunMain,
    // which by default snapshots the run classpath into a temp dir. That copy chokes
    // on the ~400MB+ `test/resources` tree above (and any fixtures symlink). Run
    // against the live classpath instead — we never mutate classes mid-run.
    Test / bgCopyClasspath := false,
    libraryDependencies ++= Seq(
      // jsoup + mongo + caffeine arrive transitively via `common`; Sentry is the
      // worker's own error-reporting sink.
      sentryLogback,
      // The SLF4J binding. The web app gets logback-classic via Play's
      // play-logback; this plain `def main` worker does NOT, so without it
      // SLF4J finds no provider, falls back to a NOP logger, and silently drops
      // EVERY log line — WorkerMain lifecycle, scrape ticks, and Sentry error
      // reporting all vanish. (The web app declares the same version — see there.)
      logbackClassic,
      // webp ImageReader for the share-card compositor (tools.OgCardRenderer + PosterDecode): the worker renders
      // the cards, the web only links to them.
      imageioWebp,
      // The Prometheus client the task-pipeline metrics are built on (scraped by
      // Prometheus, scraped over the worker's NodePort) arrives via
      // `common`, which owns it for both apps — see its libraryDependencies.
      scalatestPlay % Test,
      scalatestScalaCheck % Test,
    ),
  )
  .settings(unitReportSettings, testOrderSettings, workerRowSettings)
  .settings(ForkedTests.settings(Test, heap = "2g"))
  .settings(UnitClasspathGuard.settings)
  .settings(itReportSettings, itRowSettings)
  // Fails the it run when a repository write failed and was swallowed into a WARN.
  .settings(WriteFailureTripwire.settings(IntegrationTest))
  .settings(noApiDocs, heapDumpScript)
  .settings(launcherOptions("worker.options"))

// ── Web app (content serving) ────────────────────────────────────────────────

lazy val web = (project in file("web"))
  .enablePlugins(PlayScala, SbtWeb)
  // Disable Play's app/ + conf/ + public/ layout so it falls back to the
  // standard sbt layout — src/main/scala, src/main/resources (routes +
  // application.conf + logback.xml), src/main/twirl (views), src/main/assets —
  // matching common and worker.
  .disablePlugins(PlayLayoutPlugin)
  // The page tests render real pages from a fixture-populated cache that the
  // worker's `FixtureTestWiring` (scrape pipeline) seeds — so the PAGE scope
  // alone reaches into worker's classpath. Depending on worker's narrow
  // `Fixtures` config (not its full `Test`, which is ~320 unrelated specs)
  // keeps every PageTest shard from paying to compile worker's whole test tree.
  // Compile/Test stay independent: the deployed `web/stage` is built from
  // Compile, which never sees worker, so the two apps remain decoupled in
  // production. (Same cross-app shape as e2e.)
  .dependsOn(common, identity, testkit % Test, itkit % "it->compile", worker % "page->fixtures")
  .configs(IntegrationTest, PageTest)
  .settings(
    name := "web",
    // Play forks web's Test JVM, which doesn't read .jvmopts: the same pins as every other test JVM (headless, so AWT
    // rendering never steals focus; production's en_US number format) — project/ForkedTests.scala.
    Test / javaOptions ++= ForkedTests.options(Test, heap = "1g").value,
    // Suites in parallel inside that fork, as every other module's run (ForkedTests.settings): sbt's default for a forked
    // run is one suite at a time, which made web's 198 suites a serial 3.7 min of the `worker` row (CI, 2026-10-27).
    // BOTH keys: sbt's forked run reads `parallelExecution` too, which PlayScala sets false, so with only
    // `testForkedParallel` the fork stayed serial (the `worker` row's tail was web's suites one at a time, ~120 s for
    // ~105 suite-seconds, measure runs 3420-3480). testOrderSettings, after these, still makes the order-independence
    // run sequential.
    Test / parallelExecution := true,
    Test / testForkedParallel := (Test / parallelExecution).value,
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "src" / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "src" / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    // The unit specs' helpers, for a spec that moved to `it` for its real dependency (see worker's).
    IntegrationTest / unmanagedClasspath ++= (Test / fullClasspath).value,
    inConfig(PageTest)(Defaults.testSettings),
    PageTest / scalaSource              := baseDirectory.value / "src" / "page" / "scala",
    PageTest / resourceDirectory        := baseDirectory.value / "src" / "page" / "resources",
    // Suites in parallel: each starts its own Chrome (fresh temp profile, debugging port 0) and its
    // own server on a free port, so they share no browser state or origin. Serially the two big ones
    // (PageJsBehaviourSpec, HiddenFilmsSyncModelSpec, ~100 s each on CI) ran back to back.
    PageTest / parallelExecution        := true,
    // Run PageTest UNFORKED so the JVM's working directory is the repo root, not
    // this submodule's dir. The page specs + FixtureServerMain load fixtures and
    // diff snapshots via repo-root-relative paths (`test/resources/fixtures/…`);
    // Play forks tests by default (CWD = web/ baseDirectory), which the monolith
    // never hit because its baseDirectory WAS the repo root. e2e (also unforked)
    // loads the same fixtures fine.
    PageTest / fork                     := false,
    // Page-regression XML stays under the root report dir too (single web module,
    // but keep the layer convention so the page job's glob matches). Unit + it
    // report paths come from unitReportSettings / itReportSettings below.
    PageTest / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u",
      ((LocalRootProject / baseDirectory).value / "target" / "test-reports" / "page").toString),
    pipelineStages := Seq(digest),
    // Don't let a detached `run` (no controlling terminal — stdin at /dev/null,
    // nohup'd, or the launching terminal closed) busy-spin Play's "press Enter to
    // stop" reader at ~300% CPU. Falls back to Play's default when a real console
    // is attached (see project/HeadlessSafeInteractionMode.scala).
    PlayKeys.playInteractionMode := HeadlessSafeInteractionMode,
    // Eagerly trigger AppLoader on `sbt run` (see project/Warmup.scala). Prod Mongo for a
    // local run comes from `scripts/with-prod-mongo.sh sbt web/run`, which opens the tunnel.
    PlayKeys.playRunHooks += Warmup(),
    // The client-side language pack (`controllers.I18nPacks`) — derives from
    // `messages*` on every compile so the bundles stay the one source of
    // truth for both the server-rendered default page and the client-side
    // language switch. See `I18nPackGenerator`.
    Compile / resourceGenerators += Def.task {
      Seq(I18nPackGenerator.generate(
        (Compile / resourceDirectory).value,
        (Compile / resourceManaged).value,
      ))
    }.taskValue,
    // mongo-scala-driver, caffeine and jsoup come transitively via `common`.
    libraryDependencies ++= Seq(
      jsoup,                 // also used directly in views/helpers
      sentryLogback,         // error reporting
      logbackClassic,        // lifts play-logback's copy to the worker's version (LogbackVersionParitySpec)
      scalatestPlay % Test
    ),
    // Test = src/test/scala (sbt default, now that PlayLayoutPlugin is off).
    // Resources stay in the jar, not in a `conf/` directory on the classpath: the image's AOT cache
    // (tools.ClassArchiveTraining) refuses a classpath holding a non-empty directory.
    PlayKeys.externalizeResources := false,
  )
  .settings(unitReportSettings, testOrderSettings)
  // Play's test kit (play.api.test.Helpers, which web's specs use for FakeRequest and the like) names Selenium's and
  // HtmlUnit's driver classes in its object's initialiser, so they must load; no unit spec drives a browser through
  // them, which NoRealDependenciesInUnitSpecsSpec checks.
  .settings(UnitClasspathGuard.allowing(UnitClasspathGuard.BrowserOrganizations))
  .settings(itReportSettings, itRowSettings)
  .settings(WriteFailureTripwire.settings(IntegrationTest))
  .settings(noApiDocs, heapDumpScript)
  .settings(launcherOptions("web.options"))

// ── End-to-end test module (not deployed) ────────────────────────────────────
//
// The few specs that exercise the full scrape → cache → render contract and so
// need BOTH apps on the classpath: the worker's scrape pipeline (via
// FixtureTestWiring) and the web app's MovieControllerService / FilmSchedule
// transform. Depending on web + worker here means neither app depends on the
// other and no view code has to sink into common. Test-only — `publish / skip`,
// never staged. CI fans its specs out across parallel `e2e` shards (see the
// e2eCorpus/e2eRest aliases below); it ships no deploy
// artifact. Depends on worker's narrow `Fixtures` config, not its full `Test`
// (~320 unrelated specs) — see the Fixtures config comment near IntegrationTest.
lazy val e2e = (project in file("e2e"))
  .dependsOn(web, worker % "test->fixtures", testkit % Test, itkit % Test)
  .settings(
    name := "e2e",
    publish / skip := true,
    libraryDependencies += scalatestPlay % Test,
  )
  .settings(noApiDocs)
  .settings(unitReportSettings, testOrderSettings)
  .settings(WriteFailureTripwire.settings(Test))

// ── Root aggregator (no sources) ─────────────────────────────────────────────

lazy val root = (project in file("."))
  .aggregate(geodata, common, identity, scrapers, testkit, itkit, web, worker, e2e)
  .settings(
    name := "movies",
    publish / skip := true,
  )

// Each unit module's specs run in a forked test JVM, one at a time under the server's JVM slot (two at once on a CI
// runner): project/ForkedTests.scala.
ForkedTests.forkSlots

// One sbt invocation, every module's unit tests, no fail-fast: `all` runs each
// listed task (their forked test JVMs one at a time) and reports all failures
// instead of stopping at the first failing module. `testUnit` is the full local alias; keeping the
// aggregating aliases means new modules join the existing CI jobs rather than
// spawning new ones.
addCommandAlias("testUnit", "; all common/Test/test identity/Test/test scrapers/Test/test testkit/Test/test web/Test/test worker/Test/test ; e2eShared")
// The e2e module MINUS the country-scoped specs. Those install a country's
// TitleRuleSet into the process-global TitleNormalizer, so sharing a JVM with the
// Polish whole-corpus specs (or with each other) silently normalises the wrong
// corpus — see CountryScoped.java. They run one-per-JVM via the convergence* aliases.
addCommandAlias("e2eShared", "e2e/Test/testOnly * -- -l services.movies.CountryScoped")
addCommandAlias("itAll", "all web/IntegrationTest/test worker/IntegrationTest/test")
// `testUnit` is the full local alias. In CI the modules are spread across
// parallel jobs to keep each off the deploy-gating critical path, all under the
// 20-runner cap: `test` runs `testUnitNoE2e` split across rows — `testUnitWorker`
// (the heaviest module, itself split by WORKER_ROW) and `testUnitRest` — `integration-test` runs `itAll` (split by IT_ROW),
// and the `e2e` module is itself FANNED OUT across four parallel shards (each
// tagged heavy spec boots the full ~110s pipeline one or more times, so on one
// runner the module was the long pole). See .github/workflows/ci.yml.
addCommandAlias("testUnitNoE2e", "all common/Test/test identity/Test/test scrapers/Test/test testkit/Test/test web/Test/test worker/Test/test")
addCommandAlias("testUnitWorker", "worker/Test/test")
addCommandAlias("testUnitWorkerAndWeb", "all worker/Test/test web/Test/test scrapers/Test/test")
addCommandAlias("testUnitRest", "all common/Test/test identity/Test/test testkit/Test/test")

// e2e CI shards. The two heavy whole-corpus determinism specs (tagged
// @CorpusReplay) run by name, one per runner; `e2eRest` runs EVERYTHING ELSE in
// the module via tag-exclusion (-l) so a newly-added e2e spec can never be
// silently dropped from CI — see CorpusReplay.java.
//
// ReScrapeIdempotencySpec had a third by-name shard, on the grounds that it was
// the single heaviest spec. It is back in `e2eRest`: that shard ran ~2 min under
// the build's long pole (a WebKit page-test row) with a whole runner to itself,
// and ci.yml is at its 20-runner cap, so the slot buys more as another WebKit
// page-test shard than as a fourth e2e one.
addCommandAlias("e2eCorpus",   "e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec")
addCommandAlias("e2eRest",     "e2e/Test/testOnly * -- -l services.movies.CorpusReplay -l services.movies.CountryScoped")

// Per-country convergence legs, one per country, run by `.github/workflows/
// identity-model-convergence.yml` — NOT by ci.yml, which is already at its 20-runner
// cap. They carry @CountryScoped, which `e2eShared` and `e2eRest` both exclude,
// so they never share a JVM with the Polish corpus specs or with each other; the
// dedicated workflow is what guarantees they run at all. Each
// needs a throwaway MONGODB_URI (the corpus round-trips through a real
// `cinema_scrapes`) and its own JVM: each spec installs its country's rules
// into `TitleNormalizer`, which is process-global, so two countries in one JVM
// would overwrite each other's normalisation. Hence no `convergenceAll` alias —
// running them together is exactly the thing that would silently mis-test.
// The SAMPLE: the same claims over the ~100-film sample. No leg runs it (`country-convergence-leg.yml`):
// the sample corpus is a slice of the full one, so beside the full suite it only failed faster. A red
// hermetic leg's auto-bisect replays it per commit (`convergence-bisect.yml`), its suite alone.
//
// The corpus-shape specs (`CorpusShapeSpec`) hold a country's RECORDED corpus — the one the leg has
// restored or recorded into test/resources/fixtures/corpus — to every cinema slot's field shapes and
// every listing's title search; `-z recorded:<cc>` selects that country's tests alone, so the Polish
// fixture boot those specs also cover stays in `e2eRest`. A hermetic leg's takes row runs them at the
// base and the head and a recording's convergence row ahead of its suite, through these aliases.
def corpusShape(code: String): String =
  s"e2e/Test/testOnly services.movies.CinemaSlotInvariantsSpec services.movies.SearchQueryMarkersSpec -- -z recorded:$code"
addCommandAlias("corpusShapePoland",  corpusShape("pl"))
addCommandAlias("corpusShapeGermany", corpusShape("de"))
addCommandAlias("corpusShapeUk",      corpusShape("uk"))
addCommandAlias("corpusShapeSpain",   corpusShape("es"))
addCommandAlias("corpusShapeUs",      corpusShape("us"))
addCommandAlias("convergencePolandSample",  "e2e/Test/testOnly services.movies.PolandSampleConvergenceSpec")
addCommandAlias("convergenceGermanySample", "e2e/Test/testOnly services.movies.GermanySampleConvergenceSpec")
addCommandAlias("convergenceUkSample",      "e2e/Test/testOnly services.movies.UnitedKingdomSampleConvergenceSpec")
addCommandAlias("convergenceUsSample",      "e2e/Test/testOnly services.movies.UnitedStatesSampleConvergenceSpec")
addCommandAlias("convergenceSpainSample",   "e2e/Test/testOnly services.movies.SpainSampleConvergenceSpec")
addCommandAlias("convergencePoland",  "e2e/Test/testOnly services.movies.PolandConvergenceSpec")
addCommandAlias("convergenceGermany", "e2e/Test/testOnly services.movies.GermanyConvergenceSpec")
addCommandAlias("convergenceUk",      "e2e/Test/testOnly services.movies.UnitedKingdomConvergenceSpec")
// The UK RECORDING splits the same way, in two rows of its job (`Record scrape fixtures`
// passes both): its three replays were ~6 of the recording's 14 minutes and the longest leg
// of the run (run 37105119296), behind a boot they never touch. The hermetic UK legs keep
// `convergenceUk`, whole — a second row there would be a runner on every push.
addCommandAlias("convergenceUkWithoutOrder", "e2e/Test/testOnly services.movies.UnitedKingdomConvergenceSpec -- -l services.movies.OrderIndependence")
addCommandAlias("convergenceUkOrder",        "e2e/Test/testOnly services.movies.UnitedKingdomConvergenceSpec -- -n services.movies.OrderIndependence")
// The US full leg runs everything EXCEPT the whole-corpus order-independence replay,
// and `convergenceUsOrder` runs only that. Split because the two do not fit in one job:
// the boot is 167 minutes and the three concurrent replays project to ~4h15m, against a
// 315-minute suite step and GitHub's hard 360-minute job cancellation. No other country
// splits in the pipeline lane — Poland's replays cost 287s and Germany's 439s, so theirs stay
// inline there, and every sample leg keeps the ~100-film version of the claim. See `OrderIndependence`.
addCommandAlias("convergenceUs",      "e2e/Test/testOnly services.movies.UnitedStatesConvergenceSpec -- -l services.movies.OrderIndependence")
// The identity lane's US leg, split once more: the next-day test on a boot of its own (`NextDay`).
addCommandAlias("convergenceUsWithoutNextDay", "e2e/Test/testOnly services.movies.UnitedStatesConvergenceSpec -- -l services.movies.OrderIndependence -l services.movies.NextDay")
addCommandAlias("convergenceUsNextDay",        "e2e/Test/testOnly services.movies.UnitedStatesConvergenceSpec -- -n services.movies.NextDay")
addCommandAlias("convergenceUsOrder", "e2e/Test/testOnly services.movies.UnitedStatesConvergenceSpec -- -n services.movies.OrderIndependence")
// The IDENTITY lane splits Germany the same way, for wall clock rather than to fit a job: its
// three replays were ~5 of the slowest row's 11.3 minutes (run 37148209974, three lockstep passes
// behind a ~2-minute boot), which bounded the whole lane. Two rows, one runner more per push.
addCommandAlias("convergenceGermanyWithoutOrder", "e2e/Test/testOnly services.movies.GermanyConvergenceSpec -- -l services.movies.OrderIndependence")
addCommandAlias("convergenceGermanyOrder",        "e2e/Test/testOnly services.movies.GermanyConvergenceSpec -- -n services.movies.OrderIndependence")
// And Poland (2026-11-02): its three replays beside the boot kept the row on all four cores, 8.7-10.6 minutes on hosted
// runners (docs/investigations/2026-11-01-twenty-runners.md section 9). The same two rows of the order passes.
addCommandAlias("convergencePolandWithoutOrder", "e2e/Test/testOnly services.movies.PolandConvergenceSpec -- -l services.movies.OrderIndependence")
addCommandAlias("convergencePolandOrder",        "e2e/Test/testOnly services.movies.PolandConvergenceSpec -- -n services.movies.OrderIndependence")
addCommandAlias("convergenceSpain",    "e2e/Test/testOnly services.movies.SpainConvergenceSpec")

