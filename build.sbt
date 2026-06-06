// ── Multi-module layout ─────────────────────────────────────────────────────
//
//   common  — shared domain + data layer (models, MovieRepo/MovieCache, Mongo,
//             EventBus, UptimeMonitor, the HTTP-fetch family). Plain Scala lib.
//   testkit — shared test fakes/helpers (not deployed). dependsOn(common).
//   web     — content-serving Play app (controllers, Twirl views). Fly: kinowo.
//   worker  — scrape + enrich background app (plain `def main`). Fly: <worker>.
//   e2e     — cross-app end-to-end specs (not deployed). dependsOn(web, worker).
//
// `root` is a pure aggregator — no sources of its own. web and worker each
// depend on common and never on each other; they share the Mongo database, not
// in-process state. testkit and e2e are libraries — only web and worker are
// ever staged/deployed, so the build still ships exactly two Fly apps.

// Every third-party artifact + its version lives in project/Dependencies.scala;
// the modules below reference those vals so no version string is repeated here.
import Dependencies._

ThisBuild / organization := "com.example"
ThisBuild / version      := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.3"

// Compiler options shared by every module. Play's sbt plugin adds -deprecation
// and -unchecked on top of these for the web project.
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Wunused:imports",
  // Scala 3.8.3 caps `-java-output-version` at 21 (higher values are rejected).
  // JRE 25 loads Java 21 class files unchanged: build on JDK 25, emit 21, run 25.
  "-java-output-version", "21",
  // Twirl-generated warnings come out without a parseable category — filter by
  // path. `app/views/` only holds Twirl templates. (No-op outside web.)
  "-Wconf:src=.*views/.*:silent"
)
ThisBuild / javacOptions ++= Seq("--release", "21")

// Integration tests (it/scala) and page-regression tests (page/scala) run under
// their own sbt configurations so CI can dispatch them as separate jobs. Both
// `extend Test` to reuse helpers from test/scala. Defined here because both the
// worker and web modules attach the `it` config.
lazy val IntegrationTest = config("it") extend Test
lazy val PageTest = config("page") extend Test

// Every module writes its JUnit XML into ONE root-level dir per layer, so each
// CI job's single action-junit-report step (`target/test-reports/<layer>/**`)
// annotates failures from every module. Without this, per-module
// `<module>/target/test-reports/...` dirs would escape the root-relative glob
// and the check-run would miss most modules.
lazy val unitReportSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u",
    ((LocalRootProject / baseDirectory).value / "target" / "test-reports" / "unit").toString)
)
lazy val itReportSettings = Seq(
  IntegrationTest / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u",
    ((LocalRootProject / baseDirectory).value / "target" / "test-reports" / "it").toString)
)

// ── Shared module ────────────────────────────────────────────────────────────

lazy val common = (project in file("common"))
  .settings(
    name := "common",
    // standard sbt layout: src/main/scala, src/test/scala, src/test/resources
    libraryDependencies ++= Seq(
      play,             // play.api.Logging (the only Play API the shared code uses)
      mongoScalaDriver,
      Dependencies.caffeine,   // qualified: Play's autoImport also defines `caffeine`
      jsoup,
      // common's test config covers ONLY its own specs (models, codecs, Mongo,
      // DaemonExecutors, …) — the shared fakes live in `testkit`, not here, so
      // common's test surface stays lean and self-contained.
      scalatestPlay % Test,
    )
  )
  .settings(unitReportSettings)

// ── Shared test support (not deployed) ───────────────────────────────────────
//
// The fakes/helpers every module's tests reuse — FakeHttpFetch, InMemoryMovieRepo,
// RecordingHttpFetch, RoutingHttpFetch, Eventually, ExecutorProbes. Sources live
// in src/main/scala so consumers pull them with a plain `dependsOn(testkit % Test)`
// rather than the old `common % "test->test"` re-export. That keeps common's test
// surface to its own specs and stops worker-only fakes from being forced into
// common. Depends on common for HttpFetch/MovieRepo/models; carries scalatest on
// the compile classpath because Eventually returns an `org.scalatest.Assertion`.
lazy val testkit = (project in file("testkit"))
  .dependsOn(common)
  .settings(
    name := "testkit",
    publish / skip := true,
    libraryDependencies += scalatestPlay,
  )

// ── Worker app (scrape + enrich) ─────────────────────────────────────────────
//
// Plain Scala app (no Play): its only inbound HTTP is a Fly health check, so it
// skips Play's server/router/Twirl stack — leaner, and a `def main` boot avoids
// the `extends App` init-order hazards with Mongo/Sentry. It writes through
// MovieCache to Mongo; the web app's cache picks those writes up via the Mongo
// change stream, so the two processes share no in-process state.
lazy val worker = (project in file("worker"))
  .dependsOn(common, testkit % Test)
  .enablePlugins(JavaAppPackaging)
  .configs(IntegrationTest)
  .settings(
    name := "worker",
    // standard sbt layout: src/main/scala, src/test/scala
    Compile / mainClass := Some("modules.WorkerMain"),
    // Integration tests (live scrape + Mongo) run under their own config so CI
    // can dispatch them as a separate job — same pattern as the web module.
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "src" / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "src" / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    // A handful of enrichment specs (MetacriticClientSpec, RottenTomatoesClientSpec,
    // ImdbClientSpec, ImdbIdResolverSpec) read fixtures off the CLASSPATH via
    // `getResourceAsStream("/fixtures/…")`, but those HTML captures live in the
    // shared repo-root `test/resources` tree, not under worker/src/test/resources.
    // Reference that dir on the test classpath WITHOUT copying it — the tree is
    // ~400MB, so copyResources is out; unmanagedClasspath points at it in place.
    Test / unmanagedClasspath += Attributed.blank((LocalRootProject / baseDirectory).value / "test" / "resources"),
    libraryDependencies ++= Seq(
      // jsoup + mongo + caffeine arrive transitively via `common`; Sentry is the
      // worker's own error-reporting sink.
      sentryLogback,
      // The SLF4J binding. The web app gets logback-classic via Play's
      // play-logback; this plain `def main` worker does NOT, so without it
      // SLF4J finds no provider, falls back to a NOP logger, and silently drops
      // EVERY log line — WorkerMain lifecycle, scrape ticks, and Sentry error
      // reporting all vanish. (Version pinned in Dependencies.scala to match Play.)
      logbackClassic,
      scalatestPlay % Test,
    )
  )
  .settings(unitReportSettings)
  .settings(itReportSettings)

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
  // alone reaches into worker's test classpath. Compile/Test stay independent:
  // the deployed `web/stage` is built from Compile, which never sees worker, so
  // the two apps remain decoupled in production. (Same cross-app shape as e2e.)
  .dependsOn(common, testkit % Test, worker % "page->test")
  .configs(IntegrationTest, PageTest)
  .settings(
    name := "web",
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "src" / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "src" / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    inConfig(PageTest)(Defaults.testSettings),
    PageTest / scalaSource              := baseDirectory.value / "src" / "page" / "scala",
    PageTest / resourceDirectory        := baseDirectory.value / "src" / "page" / "resources",
    PageTest / parallelExecution        := false,
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
    // Package `tools.FixtureServerMain` (PageTest scope) + its full runtime
    // classpath into one self-contained jar. CI's page-test rows `java -jar`
    // this instead of `sbt PageTest/runMain …`, dropping the sbt-target cache
    // restore + sbt/Play boot (~3min/job). The server is a raw JDK HttpServer
    // (no Play app boot), so the jar just needs the worker test-wiring +
    // controllers + Twirl views on its classpath — exactly PageTest/fullClasspath.
    assembly / mainClass        := Some("tools.FixtureServerMain"),
    assembly / fullClasspath    := (PageTest / fullClasspath).value,
    assembly / assemblyJarName  := "fixture-server.jar",
    // Stable, scala-version-independent path so CI can reference it directly.
    assembly / assemblyOutputPath := target.value / "fixture-server.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF")              => MergeStrategy.discard
      case PathList("META-INF", "versions", xs @ _*)        => MergeStrategy.first
      case PathList("META-INF", "services", xs @ _*)        => MergeStrategy.concat
      case PathList("META-INF", "native-image", xs @ _*)    => MergeStrategy.first
      case PathList("org", "apache", "commons", "logging", xs @ _*) => MergeStrategy.first
      case "module-info.class"                              => MergeStrategy.discard
      case "reference.conf"                                 => MergeStrategy.concat
      case "application.conf"                                => MergeStrategy.concat
      case x if x.endsWith("reference-overrides.conf")      => MergeStrategy.concat
      case x if x.endsWith("logback.xml")                   => MergeStrategy.first
      case x if x.endsWith(".proto")                        => MergeStrategy.first
      case x => (assembly / assemblyMergeStrategy).value(x)
    },
    pipelineStages := Seq(digest),
    // Eagerly trigger AppLoader on `sbt run` (see project/Warmup.scala) and start
    // the Mongo proxy for local dev (see project/MongoProxy.scala).
    PlayKeys.playRunHooks += Warmup(),
    // `.env.local` lives at the repo root, not under web/, so point the proxy
    // hook at the build root rather than this module's baseDirectory.
    PlayKeys.playRunHooks += MongoProxy((LocalRootProject / baseDirectory).value),
    // mongo-scala-driver, caffeine and jsoup come transitively via `common`.
    libraryDependencies ++= Seq(
      jsoup,                 // also used directly in views/helpers
      sentryLogback,         // error reporting
      scalatestPlay % Test
    ),
    // Test = src/test/scala (sbt default, now that PlayLayoutPlugin is off).
    // ── Coverage (sbt-scoverage) ──────────────────────────────────────────────
    coverageExcludedPackages := Seq(
      "<empty>",
      "router\\..*",          // play-routes-compiler output (generated)
      "controllers\\.javascript\\..*",
      "controllers\\.routes.*",
      "views\\.html\\..*",    // Twirl-generated `.scala.html` → `.scala`
    ).mkString(";"),
    coverageExcludedFiles := ".*/test/scala/scripts/.*",
    coverageFailOnMinimum := false,
  )
  .settings(unitReportSettings)
  .settings(itReportSettings)

// ── End-to-end test module (not deployed) ────────────────────────────────────
//
// The few specs that exercise the full scrape → cache → render contract and so
// need BOTH apps on the classpath: the worker's scrape pipeline (via
// FixtureTestWiring) and the web app's MovieControllerService / FilmSchedule
// transform. Depending on web + worker here means neither app depends on the
// other and no view code has to sink into common. Test-only — `publish / skip`,
// never staged. CI runs its specs inside the existing unit `test` job via
// aggregation, so it adds no deploy artifact and no CI job.
lazy val e2e = (project in file("e2e"))
  .dependsOn(web, worker % "test->test", testkit % Test)
  .settings(
    name := "e2e",
    publish / skip := true,
    libraryDependencies += scalatestPlay % Test,
  )
  .settings(unitReportSettings)

// ── Root aggregator (no sources) ─────────────────────────────────────────────

lazy val root = (project in file("."))
  .aggregate(common, testkit, web, worker, e2e)
  .settings(
    name := "movies",
    publish / skip := true,
  )

// One sbt invocation, every module's unit tests, no fail-fast: `all` runs each
// listed task in parallel and reports all failures instead of stopping at the
// first failing module. CI's unit job calls `sbt testUnit`; the integration
// job `sbt itAll`. Keeping both as aggregating aliases means new modules join
// the existing CI jobs rather than spawning new ones.
addCommandAlias("testUnit", "all common/Test/test testkit/Test/test web/Test/test worker/Test/test e2e/Test/test")
addCommandAlias("itAll", "all web/IntegrationTest/test worker/IntegrationTest/test")
