name := "movies"

organization := "com.example"

version := "1.0-SNAPSHOT"

scalaVersion := "3.8.3"

// Integration tests live in `it/scala/` and run under a separate sbt
// configuration so CI can dispatch `sbt test` and `sbt IntegrationTest/test`
// in parallel jobs. `extend Test` lets the integration specs reuse helpers
// from `test/scala/` (`tools.TestWiring`, fakes, etc.) without duplicating.
lazy val IntegrationTest = config("it") extend Test

// Page-regression tests live in `page/scala/` and run under their own
// configuration so CI can dispatch `sbt PageTest/test` on a runner that
// has Chrome installed (PageJsBehaviourSpec drives a real browser over
// CDP). Pulling them out of `sbt test` keeps the main unit job
// browser-free and gives the page checks a discrete CI status. Same
// `extend Test` trick the IT config uses — page specs reuse
// `FixtureTestWiring`, fakes, and other helpers from `test/scala/`.
lazy val PageTest = config("page") extend Test

lazy val root = (project in file("."))
  .enablePlugins(PlayScala, SbtWeb)
  .configs(IntegrationTest, PageTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "it" / "resources",
    IntegrationTest / parallelExecution := true,
    inConfig(PageTest)(Defaults.testSettings),
    PageTest / scalaSource              := baseDirectory.value / "page" / "scala",
    PageTest / resourceDirectory        := baseDirectory.value / "page" / "resources",
    PageTest / parallelExecution        := false,
    pipelineStages := Seq(digest),
  )

// ── Dependencies ──────────────────────────────────────────────────────────────

libraryDependencies ++= Seq(

  // MongoDB official Scala driver
  "org.mongodb.scala" %% "mongo-scala-driver" % "5.7.0",

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.22.2",

  // In-memory caching
  "com.github.ben-manes.caffeine" % "caffeine" % "3.2.4",

  // Error reporting
  "io.sentry" % "sentry-logback" % "8.42.0",

  // Testing
  "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test
)

// ── Test layout ───────────────────────────────────────────────────────────────

Test / scalaSource :=
  baseDirectory.value / "test" / "scala"

Test / resourceDirectory :=
  baseDirectory.value / "test" / "resources"

// ── Compiler options ──────────────────────────────────────────────────────────

// Play's sbt plugin already sets -deprecation and -unchecked; only add
// what it doesn't supply. Twirl-generated .scala.html files trip
// -Wunused:imports with false positives (the template parameter / import
// is used by the rendered HTML, not by code the compiler can see), so
// silence that category for them.
scalacOptions ++= Seq(
  "-feature",
  "-Wunused:imports",
  // Scala 3.8.3 caps `-java-output-version` at 21 (higher values are
  // rejected with "not a valid choice"). JRE 25 loads Java 21 class
  // files unchanged, so the toolchain is consistent: build on JDK 25,
  // emit Java 21 bytecode, run on JRE 25.
  "-java-output-version", "21",
  // Twirl rewrites the source position of generated-code warnings back to
  // the .scala.html origin, but the warnings come out without a parseable
  // category — filter them by source path. `app/views/` only contains
  // Twirl templates, so silencing the whole tree is safe.
  "-Wconf:src=.*views/.*:silent"
)

// Match the Scala output above. javac uses `--release N` to pin both
// the language level and the API surface to JDK N.
javacOptions ++= Seq("--release", "21")

// ── Coverage (sbt-scoverage) ──────────────────────────────────────────────────
//
// `sbt clean coverage test coverageReport` instruments the bytecode,
// runs the unit suite, then writes `target/scala-*/scoverage-report/`.
// CI uploads the report as an artifact and prints the summary text.
//
// Templates and one-shot scripts under `test/scala/scripts/` aren't
// production code — exclude them so the coverage number reflects the
// app, not generated Twirl or the ad-hoc backfill harnesses.
coverageExcludedPackages := Seq(
  "<empty>",
  "router\\..*",          // play-routes-compiler output (generated)
  "controllers\\.javascript\\..*",
  "controllers\\.routes.*",
  "views\\.html\\..*",    // Twirl-generated `.scala.html` → `.scala`
).mkString(";")
coverageExcludedFiles := ".*/test/scala/scripts/.*"

// No minimum floor yet — the report is uploaded as a CI artifact for
// inspection. Once a few runs land we know the actual baseline, then
// `coverageMinimumStmtTotal` / `coverageMinimumBranchTotal` get set
// just under it with `coverageFailOnMinimum := true` so a slip fails
// the job. Setting a floor today would either be (a) so low it gives
// no signal or (b) a guess that flakes the first time it runs.
coverageFailOnMinimum := false

