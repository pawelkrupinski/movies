// ── Multi-module layout ─────────────────────────────────────────────────────
//
//   common  — shared domain + data layer (models, MovieRepo/MovieCache, Mongo,
//             EventBus, UptimeMonitor, the HTTP-fetch family). Plain Scala lib.
//   web     — content-serving Play app (controllers, Twirl views). Fly: kinowo.
//   worker  — scrape + enrich background app (plain `def main`). Fly: <worker>.
//
// `root` is a pure aggregator — no sources of its own. web and worker each
// depend on common and never on each other; they share the Mongo database, not
// in-process state.

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

// ── Shared module ────────────────────────────────────────────────────────────

lazy val common = (project in file("common"))
  .settings(
    name := "common",
    // standard sbt layout: src/main/scala, src/test/scala, src/test/resources
    libraryDependencies ++= Seq(
      // play.api.Logging (the only Play API the shared code uses)
      "org.playframework"             %% "play"                % "3.0.10",
      "org.mongodb.scala"             %% "mongo-scala-driver"  % "5.7.0",
      "com.github.ben-manes.caffeine" %  "caffeine"            % "3.2.4",
      "org.jsoup"                     %  "jsoup"               % "1.22.2",
      // Shared test helpers/fakes live in common/test and are re-exported to web
      // and worker via `dependsOn(common % "test->test")`.
      "org.scalatestplus.play"        %% "scalatestplus-play"  % "7.0.2" % Test,
    )
  )

// ── Worker app (scrape + enrich) ─────────────────────────────────────────────
//
// Plain Scala app (no Play): its only inbound HTTP is a Fly health check, so it
// skips Play's server/router/Twirl stack — leaner, and a `def main` boot avoids
// the `extends App` init-order hazards with Mongo/Sentry. It writes through
// MovieCache to Mongo; the web app's cache picks those writes up via the Mongo
// change stream, so the two processes share no in-process state.
lazy val worker = (project in file("worker"))
  .dependsOn(common % "compile->compile;test->test")
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
    Test / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/unit"),
    IntegrationTest / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/it"),
    libraryDependencies ++= Seq(
      // jsoup + mongo + caffeine arrive transitively via `common`; Sentry is the
      // worker's own error-reporting sink.
      "io.sentry"              %  "sentry-logback"     % "8.42.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test,
    )
  )

// ── Web app (content serving) ────────────────────────────────────────────────

lazy val web = (project in file("web"))
  .enablePlugins(PlayScala, SbtWeb)
  // Disable Play's app/ + conf/ + public/ layout so it falls back to the
  // standard sbt layout — src/main/scala, src/main/resources (routes +
  // application.conf + logback.xml), src/main/twirl (views), src/main/assets —
  // matching common and worker.
  .disablePlugins(PlayLayoutPlugin)
  .dependsOn(common % "compile->compile;test->test")
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
    // Emit JUnit-style XML alongside the console reporter so CI can turn a
    // failure into an inline check-run annotation instead of a raw log scroll.
    Test / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/unit"),
    IntegrationTest / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/it"),
    PageTest / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports/page"),
    pipelineStages := Seq(digest),
    // Eagerly trigger AppLoader on `sbt run` (see project/Warmup.scala) and start
    // the Mongo proxy for local dev (see project/MongoProxy.scala).
    PlayKeys.playRunHooks += Warmup(),
    // `.env.local` lives at the repo root, not under web/, so point the proxy
    // hook at the build root rather than this module's baseDirectory.
    PlayKeys.playRunHooks += MongoProxy((LocalRootProject / baseDirectory).value),
    // mongo-scala-driver, caffeine and jsoup come transitively via `common`.
    libraryDependencies ++= Seq(
      "org.jsoup" % "jsoup" % "1.22.2",            // also used directly in views/helpers
      "io.sentry" % "sentry-logback" % "8.42.0",   // error reporting
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test
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

// ── Root aggregator (no sources) ─────────────────────────────────────────────

lazy val root = (project in file("."))
  .aggregate(common, web, worker)
  .settings(
    name := "movies",
    publish / skip := true,
  )
