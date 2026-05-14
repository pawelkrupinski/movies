name := "movies"

organization := "com.example"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)

// ── Versions ──────────────────────────────────────────────────────────────────

val playVersion        = "3.0.8"
val mongoDriverVersion = "5.1.1"

// ── Dependencies ──────────────────────────────────────────────────────────────

libraryDependencies ++= Seq(

  // MongoDB official Scala driver
  "org.mongodb.scala" %% "mongo-scala-driver" % mongoDriverVersion,

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.17.2",

  // In-memory caching
  "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8",

  // Testing
  "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
  "org.mockito"            %% "mockito-scala"      % "1.17.31" % Test
)

// ── Test layout ───────────────────────────────────────────────────────────────

Test / scalaSource :=
  baseDirectory.value / "test" / "scala"

Test / resourceDirectory :=
  baseDirectory.value / "test" / "resources"

// ── Compiler options ──────────────────────────────────────────────────────────

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-unused:imports"
)