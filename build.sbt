name := "movies"

organization := "com.example"

version := "1.0-SNAPSHOT"

scalaVersion := "3.8.3"

// Integration tests live in `it/scala/` and run under a separate sbt
// configuration so CI can dispatch `sbt test` and `sbt IntegrationTest/test`
// in parallel jobs. `extend Test` lets the integration specs reuse helpers
// from `test/scala/` (`tools.TestWiring`, fakes, etc.) without duplicating.
lazy val IntegrationTest = config("it") extend Test

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .configs(IntegrationTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.testSettings),
    IntegrationTest / scalaSource       := baseDirectory.value / "it" / "scala",
    IntegrationTest / resourceDirectory := baseDirectory.value / "it" / "resources",
    IntegrationTest / parallelExecution := true,
  )

// ── Dependencies ──────────────────────────────────────────────────────────────

libraryDependencies ++= Seq(

  // MongoDB official Scala driver
  "org.mongodb.scala" %% "mongo-scala-driver" % "5.7.0",

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.22.2",

  // In-memory caching
  "com.github.ben-manes.caffeine" % "caffeine" % "3.2.4",

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
  // rejected with "not a valid choice"). JRE 26 loads Java 21 class
  // files unchanged, so the toolchain is consistent: build on JDK 26,
  // emit Java 21 bytecode, run on JRE 26.
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

