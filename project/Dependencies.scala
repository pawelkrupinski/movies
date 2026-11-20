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
  private val playVersion          = "3.0.12"
  private val mongoScalaVersion    = "5.15.0"
  private val caffeineVersion      = "3.3.1"
  private val jsoupVersion         = "1.24.1"
  // Pure-Java webp ImageReader (no native libs), so the OG-card compositor can
  // decode the webp posters cinema CDNs now serve. imageio-core is the shared
  // runtime the format plugin needs.
  private val twelveMonkeysVersion = "3.15.3"
  private val sentryVersion        = "8.65.0"
  // One logback for both apps, so logback.xml + the sentry-logback appender
  // behave the same in each: the worker and the web app both declare it (see
  // build.sbt), and eviction lifts the web app's play-logback copy to it. Keep
  // it at or above Play's own (LogbackVersionParitySpec fails otherwise).
  private val logbackVersion       = "1.6.8"
  private val scalatestPlayVersion = "7.0.2"
  // scalatest's ScalaCheck bridge (`ScalaCheckPropertyChecks`), for the
  // `*PropertySpec`s that state the identity core's invariants as properties
  // rather than one prod incident at a time. Test-only. The scalatestplus line
  // is versioned after the scalatest it was built against (3.2.19); the 3.2.20
  // scalatestplus-play pulls in evicts it upward, which the 3.2.x line allows.
  private val scalatestScalaCheckVersion = "3.2.20.0"
  // Official Prometheus Java client (client_java 1.x) — the worker builds its
  // task-pipeline metrics with it (counters, gauges, a bucketed duration
  // histogram) and renders the text exposition via the exposition-formats
  // module, rather than hand-rolling the `0.0.4` format.
  private val prometheusVersion    = "1.10.0"
  // THE CODE-QUALITY RATCHETS' tools (the `quality` module, not deployed): PMD's copy-paste detector with its Scala
  // tokenizer, and the scalameta parser the complexity and length measures read the production sources with -- the
  // SAME scalameta PMD's Scala module is built on (pmd-scala-common's `scalameta.version`), both its 2.13 build, so one
  // classpath holds one scalameta. Bump them together.
  private val pmdVersion           = "7.29.0"
  private val scalametaVersion     = "4.17.4"

  // ── Artifacts ──────────────────────────────────────────────────────────────
  val play             = "org.playframework"             %% "play"               % playVersion
  val mongoScalaDriver = "org.mongodb.scala"             %% "mongo-scala-driver" % mongoScalaVersion
  val caffeine         = "com.github.ben-manes.caffeine" %  "caffeine"           % caffeineVersion
  val jsoup            = "org.jsoup"                      %  "jsoup"              % jsoupVersion
  val imageioWebp      = "com.twelvemonkeys.imageio"      %  "imageio-webp"       % twelveMonkeysVersion
  val icu4j            = "com.ibm.icu"                    %  "icu4j"              % "79.1"
  val sentryLogback    = "io.sentry"                      %  "sentry-logback"     % sentryVersion
  val logbackClassic   = "ch.qos.logback"                %  "logback-classic"    % logbackVersion
  // Without the browser drivers it pulls in for its browser suites (OneBrowserPerSuite & co.): nothing here drives a
  // browser through them (the page tests speak CDP to a real Chrome), and a browser driver on a unit classpath is what
  // UnitClasspathGuard refuses.
  val scalatestPlay    = ("org.scalatestplus.play"        %% "scalatestplus-play" % scalatestPlayVersion)
    .excludeAll(ExclusionRule("org.seleniumhq.selenium"), ExclusionRule("org.htmlunit"), ExclusionRule("net.sourceforge.htmlunit"))
  // (web's specs need them back for Play's own test kit: see web's UnitClasspathGuard.allowing in build.sbt)
  val scalatestScalaCheck = "org.scalatestplus"          %% "scalacheck-1-20"    % scalatestScalaCheckVersion
  val prometheusCore   = "io.prometheus"                  %  "prometheus-metrics-core"               % prometheusVersion
  val prometheusText   = "io.prometheus"                  %  "prometheus-metrics-exposition-formats" % prometheusVersion
  val pmdScala         = "net.sourceforge.pmd"             %  "pmd-scala_2.13"     % pmdVersion
  val scalametaParsers = ("org.scalameta"                 %% "parsers"            % scalametaVersion).cross(CrossVersion.for3Use2_13)
  val prometheusJvm    = "io.prometheus"                  %  "prometheus-metrics-instrumentation-jvm" % prometheusVersion
}
