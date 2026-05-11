name := "movies"
organization := "com.example"
version := "1.0-SNAPSHOT"

scalaVersion := "2.13.14"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

// ── Dependencies ──────────────────────────────────────────────────────────────

val mongoDriverVersion = "5.1.1"
val playVersion        = "2.9.5"

libraryDependencies ++= Seq(
  guice,

  // MongoDB official Scala driver
  "org.mongodb.scala" %% "mongo-scala-driver" % mongoDriverVersion,

  // JSON (Play's built-in)
  "com.typesafe.play" %% "play-json" % "2.10.5",

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.17.2",

  // In-memory caching
  "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8",

  // Testing
  "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
  "org.mockito"            %% "mockito-scala"       % "1.17.31" % Test
)

// ── Compiler options ──────────────────────────────────────────────────────────

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-unused:imports"
)
