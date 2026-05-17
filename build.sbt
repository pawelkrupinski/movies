name := "movies"

organization := "com.example"

version := "1.0-SNAPSHOT"

scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)

// ── Versions ──────────────────────────────────────────────────────────────────

val playVersion        = "3.0.8"
val mongoDriverVersion = "5.7.0"

// ── Dependencies ──────────────────────────────────────────────────────────────

libraryDependencies ++= Seq(

  // MongoDB official Scala driver
  "org.mongodb.scala" %% "mongo-scala-driver" % mongoDriverVersion,

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.17.2",

  // In-memory caching
  "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8",

  // Testing
  "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test
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