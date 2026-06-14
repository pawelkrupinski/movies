// ── Multi-module layout ─────────────────────────────────────────────────────
//
//   common  — shared domain + data layer (models, MovieRepository/MovieCache, Mongo,
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
ThisBuild / scalaVersion := "3.8.4"

// Compiler options shared by every module. Play's sbt plugin adds -deprecation
// and -unchecked on top of these for the web project.
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Wunused:imports",
  // Warnings are errors: a warning must be fixed (or explicitly silenced via a
  // `-Wconf` rule below), never merged. Keeps the build warning-clean over time.
  "-Werror",
  // Scala 3.8.4 caps `-java-output-version` at 21 (higher values are rejected).
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
// The fakes/helpers every module's tests reuse — FakeHttpFetch, InMemoryMovieRepository,
// RecordingHttpFetch, RoutingHttpFetch, Eventually, ExecutorProbes. Sources live
// in src/main/scala so consumers pull them with a plain `dependsOn(testkit % Test)`
// rather than the old `common % "test->test"` re-export. That keeps common's test
// surface to its own specs and stops worker-only fakes from being forced into
// common. Depends on common for HttpFetch/MovieRepository/models; carries scalatest on
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
    // `sbt localStack` background-runs LocalFixtureWorkerMain via bgRunMain, which
    // by default snapshots the run classpath into a temp dir. That copy chokes on
    // the ~400MB+ `test/resources` tree above (and any fixtures symlink). Run the
    // bg job against the live classpath instead — we never mutate classes mid-run.
    Test / bgCopyClasspath := false,
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
    commands += localStack,
  )

// ── Local dev stack: `sbt localStack` ────────────────────────────────────────
//
// Runs `web` + `worker` together against ONE shared local Mongo (distinct from
// the prod db `.env.local` reaches), where the WORKER replays every HTTP fetch
// from test/resources/fixtures/today while WEB hits the real internet (posters,
// etc.) and serves the corpus the worker projects into the local read model.
//
//   - worker: `worker/Test/bgRunMain modules.LocalFixtureWorkerMain` (background,
//     forked). Fetches via FakeHttpFetch("today"); real local Mongo + projector.
//   - web: `web/run` (foreground, Play dev on :9000). Loaded in THIS sbt JVM, so
//     a JVM property is how it picks up the local Mongo — Env precedence is
//     process-env > -D > .env.local, so this beats .env.local's prod URI.
//
// Override via env: KINOWO_LOCAL_MONGO_URI, KINOWO_LOCAL_MONGO_DB, KINOWO_FIXTURE_DIR.
// Needs a local Mongo on port 27018 (NOT 27017 — that's usually the `flyctl
// proxy` to prod Mongo; this stack must never touch prod), e.g.:
//   docker run -d -p 27018:27017 --name kinowo-local-mongo mongo --replSet rs0
//   docker exec kinowo-local-mongo mongosh --quiet --eval 'rs.initiate()'
// (A single-node replica set gives web instant change-stream updates; a plain
// standalone also works — web falls back to its periodic full reload.)
//
// Stop: quit `web/run` (Enter), then `bgStop <id>` for the worker (`jobs` lists
// it), or just exit sbt.
lazy val localStack = Command.command("localStack") { state =>
  val uri        = sys.env.getOrElse("KINOWO_LOCAL_MONGO_URI", "mongodb://127.0.0.1:27018")
  val db         = sys.env.getOrElse("KINOWO_LOCAL_MONGO_DB", "kinowo_local")
  val fixtureDir = sys.env.getOrElse("KINOWO_FIXTURE_DIR", "today")
  ensureLocalMongo(state, uri)
  // The worker fork can't see these JVM props; LocalFixtureWorkerMain re-forces
  // the SAME defaults itself, so both land on the same db.
  System.setProperty("MONGODB_URI", uri)
  System.setProperty("MONGODB_DB", db)
  state.log.info(s"[localStack] worker replays fixtures/$fixtureDir, web hits the internet — both on Mongo $uri db=$db")
  "worker/Test/bgRunMain modules.LocalFixtureWorkerMain" :: "web/run" :: state
}

/** Make sure a Mongo is listening for the local stack. For a LOCAL uri whose
 *  port is closed, start a single-node-replica-set docker mongo (replica set so
 *  web's change streams deliver live, not just the 30-min reload backstop) and
 *  wait for it. Never touches a remote/custom uri; degrades to a clear hint if
 *  docker is absent. */
def ensureLocalMongo(state: State, uri: String): Unit = {
  val log = state.log
  val (host, port) = """mongodb://([^:/]+):(\d+)""".r.findFirstMatchIn(uri)
    .map(m => (m.group(1), m.group(2).toInt)).getOrElse(("127.0.0.1", 27018))
  val isLocal = host == "127.0.0.1" || host == "localhost"
  def portOpen: Boolean =
    try { val s = new java.net.Socket(); s.connect(new java.net.InetSocketAddress(host, port), 500); s.close(); true }
    catch { case _: Throwable => false }
  if (!isLocal || portOpen) return
  def sh(cmd: String): Int = scala.sys.process.Process(Seq("sh", "-c", cmd)).!(scala.sys.process.ProcessLogger(_ => ()))
  val haveDocker = sh("command -v docker >/dev/null 2>&1") == 0
  if (!haveDocker) {
    log.warn(s"[localStack] no Mongo on $host:$port and docker not found. Start one yourself: " +
      s"docker run -d -p $port:27017 --name kinowo-local-mongo mongo --replSet rs0  &&  " +
      s"docker exec kinowo-local-mongo mongosh --quiet --eval 'rs.initiate()'")
    return
  }
  val name = "kinowo-local-mongo"
  log.info(s"[localStack] no Mongo on $host:$port — starting docker '$name'…")
  val exists = scala.sys.process.Process(Seq("sh", "-c", s"docker ps -aq -f name=^$name$$")).!!.trim.nonEmpty
  if (exists) sh(s"docker start $name")
  else sh(s"docker run -d -p $port:27017 --name $name mongo --replSet rs0")
  var waited = 0
  while (!portOpen && waited < 40) { Thread.sleep(1000); waited += 1 }
  // Idempotent: a fresh container needs rs.initiate(); a restarted one already
  // has it (try/catch swallows "already initialized").
  sh(s"docker exec $name mongosh --quiet --eval 'try { rs.initiate() } catch (e) {}'")
  Thread.sleep(2000) // let the single node elect itself primary before web connects
  if (portOpen) log.info(s"[localStack] local mongo ready on $host:$port")
  else log.warn(s"[localStack] mongo didn't come up on $host:$port — check 'docker logs $name'")
}

// One sbt invocation, every module's unit tests, no fail-fast: `all` runs each
// listed task in parallel and reports all failures instead of stopping at the
// first failing module. `testUnit` is the full local alias; keeping the
// aggregating aliases means new modules join the existing CI jobs rather than
// spawning new ones.
addCommandAlias("testUnit", "all common/Test/test testkit/Test/test web/Test/test worker/Test/test e2e/Test/test")
addCommandAlias("itAll", "all web/IntegrationTest/test worker/IntegrationTest/test")
// `testUnit` is the full local alias. In CI the modules are spread across THREE
// parallel jobs to keep each off the deploy-gating critical path, all under the
// 20-runner cap: `test` runs `testUnitNoE2e`, `integration-test` runs `itAll`,
// and a dedicated `e2e` job runs `e2e/Test/test` (its whole-corpus staging specs
// are ~6 min on their own). See .github/workflows/ci.yml.
addCommandAlias("testUnitNoE2e", "all common/Test/test testkit/Test/test web/Test/test worker/Test/test")
