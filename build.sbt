// ── Multi-module layout ─────────────────────────────────────────────────────
//
// `common` holds the shared domain + data layer (models, MovieRepo/MovieCache,
// Mongo connection, EventBus, UptimeMonitor) that both the content-serving app
// and the (forthcoming) scrape/enrich worker depend on. It is a plain Scala
// library — no Play plugin — so it stays cheap to depend on from a worker that
// doesn't serve HTTP.
//
// `root` is the Play web app (content serving). For now it still wires the full
// pipeline (scrape + enrich) exactly as before — this commit only extracts
// `common`, with no behaviour change. The read/write split lands in a later
// phase, when a `worker` module takes the scrape/enrich half.

ThisBuild / organization := "com.example"
ThisBuild / version      := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.3"

// Compiler options shared by every module. Play's sbt plugin adds -deprecation
// and -unchecked on top of these for the web project.
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Wunused:imports",
  // Scala 3.8.3 caps `-java-output-version` at 21 (higher values are rejected
  // with "not a valid choice"). JRE 25 loads Java 21 class files unchanged, so
  // the toolchain is consistent: build on JDK 25, emit Java 21 bytecode, run on
  // JRE 25.
  "-java-output-version", "21",
  // Twirl rewrites the source position of generated-code warnings back to the
  // .scala.html origin, but the warnings come out without a parseable category
  // — filter them by source path. `app/views/` only contains Twirl templates,
  // so silencing the whole tree is safe. (No-op in `common`, which has no views.)
  "-Wconf:src=.*views/.*:silent"
)
// javac uses `--release N` to pin both language level and API surface to JDK N.
ThisBuild / javacOptions ++= Seq("--release", "21")

// ── Shared module ────────────────────────────────────────────────────────────

lazy val common = (project in file("common"))
  .settings(
    name := "kinowo-common",
    // Mirror the Play `app/` source layout so files keep their package paths
    // when they move here from the monolith.
    Compile / scalaSource := baseDirectory.value / "app",
    libraryDependencies ++= Seq(
      // play.api.Logging (the only Play API the shared code uses)
      "org.playframework"             %% "play"                % "3.0.10",
      "org.mongodb.scala"             %% "mongo-scala-driver"  % "5.7.0",
      "com.github.ben-manes.caffeine" %  "caffeine"            % "3.2.4",
      "org.jsoup"                     %  "jsoup"               % "1.22.2",
    )
  )

// ── Worker app (scrape + enrich) ─────────────────────────────────────────────
//
// Plain Scala app (no Play): its only inbound HTTP is a Fly health check, so it
// skips Play's server/router/Twirl stack entirely — leaner, and a `def main`
// boot avoids the `extends App` init-order hazards with Mongo/Sentry. It writes
// through MovieCache to Mongo; the web app's cache picks those writes up via the
// Mongo change stream, so the two processes share no in-process state.
lazy val worker = (project in file("worker"))
  .dependsOn(common)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "kinowo-worker",
    Compile / scalaSource := baseDirectory.value / "app",
    Test / scalaSource    := baseDirectory.value / "test" / "scala",
    Compile / mainClass   := Some("modules.WorkerMain"),
    libraryDependencies ++= Seq(
      // jsoup + mongo + caffeine arrive transitively via `common`; Sentry is the
      // worker's own error-reporting sink.
      "io.sentry"              %  "sentry-logback"     % "8.42.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test,
    )
  )

// ── Web app (content serving) ────────────────────────────────────────────────

// Integration tests live in `it/scala/` and run under a separate sbt
// configuration so CI can dispatch `sbt test` and `sbt IntegrationTest/test`
// in parallel jobs. `extend Test` lets the integration specs reuse helpers
// from `test/scala/` (`tools.TestWiring`, fakes, etc.) without duplicating.
lazy val IntegrationTest = config("it") extend Test

// Page-regression tests live in `page/scala/` and run under their own
// configuration so CI can dispatch `sbt PageTest/test` on a runner that
// has Chrome installed (PageJsBehaviourSpec drives a real browser over
// CDP). Pulling them out of `sbt test` keeps the main unit job
// browser-free and gives the page checks a discrete CI status.
lazy val PageTest = config("page") extend Test

lazy val root = (project in file("."))
  .enablePlugins(PlayScala, SbtWeb)
  .dependsOn(common)
  .aggregate(common, worker)
  .configs(IntegrationTest, PageTest)
  .settings(
    name := "movies",
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    inConfig(PageTest)(Defaults.testSettings),
    PageTest / scalaSource              := baseDirectory.value / "page" / "scala",
    PageTest / resourceDirectory        := baseDirectory.value / "page" / "resources",
    PageTest / parallelExecution        := false,
    // Emit JUnit-style XML alongside the console reporter so CI can turn
    // a failure into an inline check-run annotation + job summary
    // (mikepenz/action-junit-report) instead of a raw log scroll. `-o`
    // keeps ScalaTest's normal console output; `-u <dir>` adds the XML.
    // Distinct dirs per config so a local full run doesn't intermix them.
    Test / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/unit"),
    IntegrationTest / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/it"),
    PageTest / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/page"),
    pipelineStages := Seq(digest),
    // Eagerly trigger AppLoader on `sbt run` instead of waiting for the
    // first request. See project/Warmup.scala.
    PlayKeys.playRunHooks += Warmup(),
    // Start `flyctl proxy 27017:27017 --app kinowo-mongo` for the duration
    // of `sbt run` when .env.local points MONGODB_URI at 127.0.0.1.
    // No-op in prod (playRunHooks don't run inside the Docker image) and
    // no-op when the port's already listening. See project/MongoProxy.scala.
    PlayKeys.playRunHooks += MongoProxy(baseDirectory.value),
    // mongo-scala-driver, caffeine and jsoup come transitively via `common`.
    libraryDependencies ++= Seq(
      // HTML parsing — also used directly by the scrapers that still live here
      "org.jsoup" % "jsoup" % "1.22.2",
      // Error reporting
      "io.sentry" % "sentry-logback" % "8.42.0",
      // Testing
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test
    ),
    Test / scalaSource       := baseDirectory.value / "test" / "scala",
    Test / resourceDirectory := baseDirectory.value / "test" / "resources",
    // ── Coverage (sbt-scoverage) ──────────────────────────────────────────────
    // `sbt clean coverage test coverageReport` instruments the bytecode, runs
    // the unit suite, then writes `target/scala-*/scoverage-report/`. Templates
    // and one-shot scripts aren't production code — exclude them so the number
    // reflects the app, not generated Twirl or ad-hoc harnesses.
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
