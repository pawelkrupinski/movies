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
  // Netty 4.1.x NATIVE-EPOLL transport for the Mongo reactive driver. The worker's
  // recurring CPU-credit floor is a busy-spin in the JDK `sun.nio.ch` epoll selector
  // (`EPollSelectorImpl.doSelect`) — PROVEN on-box: one selector thread burns ~31cc
  // while making only ~1.5 voluntary context switches/sec (a healthy selector parks
  // thousands of times/sec — one per `epoll_wait` block), i.e. it never sleeps, it
  // loops; and the CPU is decoupled from real work (change-events ~0.1/s). It is NOT
  // a wedged/half-closed Mongo socket (0 CLOSE_WAIT on-box, all Mongo conns healthy) —
  // the trigger FD is a spurious selector readiness, the classic JDK NIO epoll spin.
  // Neither the driver's default NIO2 transport NOR Netty's default NIO transport
  // avoids it — both sit on `sun.nio.ch`, and Netty's select-auto-rebuild only trips on
  // ZERO-key wakeups, not this one (the spin re-formed after we moved to Netty NIO).
  // Netty's NATIVE epoll runs its OWN JNI epoll loop, not the JDK selector, so the
  // sun.nio.ch spin can't occur. Native lib is per-arch (classifier jars below); off
  // Linux / missing lib → `Epoll.isAvailable` false → fall back to Netty NIO.
  private val nettyVersion         = "4.1.118.Final"
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
  // The Mongo driver declares netty as an OPTIONAL dependency (it isn't pulled
  // transitively), so the three modules its NettyStreamFactory needs are declared
  // explicitly here: buffer + transport for the socket I/O, handler for TLS. See
  // `nettyVersion` above for why we route Mongo I/O through Netty at all.
  val nettyBuffer      = "io.netty"                       %  "netty-buffer"       % nettyVersion
  val nettyTransport   = "io.netty"                       %  "netty-transport"    % nettyVersion
  val nettyHandler     = "io.netty"                       %  "netty-handler"      % nettyVersion
  // Native-epoll transport: the Java classes (arch-independent, so compiles on macOS)
  // plus the native `.so` for each Linux arch the worker might run on (Fly = x86_64
  // today, aarch64 kept for safety — whichever matches loads, the other is inert).
  // Only these reap EPOLLRDHUP on a wedged half-closed FD instead of spinning on it.
  val nettyEpollClasses = "io.netty" % "netty-transport-classes-epoll" % nettyVersion
  val nettyEpollX86     = "io.netty" % "netty-transport-native-epoll"  % nettyVersion classifier "linux-x86_64"
  val nettyEpollArm     = "io.netty" % "netty-transport-native-epoll"  % nettyVersion classifier "linux-aarch_64"
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
