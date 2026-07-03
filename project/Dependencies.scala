import sbt._

/** Single source of truth for every third-party dependency in the build.
  *
  * Versions are declared once below; build.sbt references the artifacts as
  * `Dependencies.caffeine`, `Dependencies.scalatestPlay`, … so a version bump
  * happens in exactly one place and every module that uses the lib follows.
  * No version string lives in build.sbt — they all live here.
  *
  * The test artifact (scalatestPlay) is declared WITHOUT a configuration so
  * each call site picks the scope it needs: `scalatestPlay % Test` in a normal
  * module, plain `scalatestPlay` on testkit's compile classpath.
  */
object Dependencies {
  // ── Versions ───────────────────────────────────────────────────────────────
  private val playVersion          = "3.0.11"
  private val mongoScalaVersion    = "5.8.0"
  private val caffeineVersion      = "3.2.4"
  private val jsoupVersion         = "1.22.2"
  // Pure-Java webp ImageReader (no native libs), so the OG-card compositor can
  // decode the webp posters cinema CDNs now serve. imageio-core is the shared
  // runtime the format plugin needs.
  private val twelveMonkeysVersion = "3.13.1"
  private val sentryVersion        = "8.47.0"
  // Pinned to the version Play uses so logback.xml + the sentry-logback appender
  // stay compatible across the web app (gets it via play-logback) and the
  // plain-`def main` worker (declares it directly — see build.sbt).
  private val logbackVersion       = "1.5.22"
  private val scalatestPlayVersion = "7.0.2"
  // Official Prometheus Java client (client_java 1.x) — the worker builds its
  // task-pipeline metrics with it (counters, gauges, a bucketed duration
  // histogram) and renders the text exposition via the exposition-formats
  // module, rather than hand-rolling the `0.0.4` format.
  private val prometheusVersion    = "1.8.0"

  // ── Artifacts ──────────────────────────────────────────────────────────────
  val play             = "org.playframework"             %% "play"               % playVersion
  val mongoScalaDriver = "org.mongodb.scala"             %% "mongo-scala-driver" % mongoScalaVersion
  val caffeine         = "com.github.ben-manes.caffeine" %  "caffeine"           % caffeineVersion
  val jsoup            = "org.jsoup"                      %  "jsoup"              % jsoupVersion
  val imageioWebp      = "com.twelvemonkeys.imageio"      %  "imageio-webp"       % twelveMonkeysVersion
  val sentryLogback    = "io.sentry"                      %  "sentry-logback"     % sentryVersion
  val logbackClassic   = "ch.qos.logback"                %  "logback-classic"    % logbackVersion
  val scalatestPlay    = "org.scalatestplus.play"        %% "scalatestplus-play" % scalatestPlayVersion
  val prometheusCore   = "io.prometheus"                  %  "prometheus-metrics-core"               % prometheusVersion
  val prometheusText   = "io.prometheus"                  %  "prometheus-metrics-exposition-formats" % prometheusVersion
  val prometheusJvm    = "io.prometheus"                  %  "prometheus-metrics-instrumentation-jvm" % prometheusVersion
}
