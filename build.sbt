name := "movies"

organization := "com.example"

version := "1.0-SNAPSHOT"

scalaVersion := "3.3.7"

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
  // Twirl rewrites the source position of generated-code warnings back to
  // the .scala.html origin, but the warnings come out without a parseable
  // category — filter them by source path. `app/views/` only contains
  // Twirl templates, so silencing the whole tree is safe.
  "-Wconf:src=.*views/.*:silent"
)

// `javacOptions --release 21` stays — it's the *Java* source language
// level, not a Scala→bytecode pinning. Play's routes compiler emits Java
// for the JavaScript reverse router, and the generated source uses
// modern API; dropping this breaks `Compile/compileIncremental`.
javacOptions ++= Seq("--release", "21")

// ── Runtime JVM options ───────────────────────────────────────────────────────

// Scala 3.3.7's LazyVals runtime helper still calls
// `sun.misc.Unsafe.objectFieldOffset`; JDK 24+'s default warns on every
// such call. The 3.3 LTS line hasn't backported the VarHandle migration
// that landed in 3.4+, so until we move off 3.3 the warning is noise.
// `allow` silences it; the production Dockerfile has the same flag.
Compile / run / javaOptions += "--sun-misc-unsafe-memory-access=allow"