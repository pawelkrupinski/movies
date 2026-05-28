import play.sbt.PlayRunHook

import scala.util.Try

/**
 * Eagerly trigger Play's `AppLoader.load()` on `sbt run` instead of
 * waiting for the first HTTP request. Play dev-mode lazy-loads the
 * application by design (so code changes between requests can swap
 * cleanly), but in practice that means the first manual page hit
 * pays the full boot cost — connect to Mongo, hydrate caches, start
 * workers — before any HTML comes back.
 *
 * `afterStarted()` runs once the Pekko HTTP listener is bound (port
 * accepting connections, but no application materialised yet). We
 * fire-and-forget a /health request from a daemon thread; the
 * response itself is ignored, the side effect is what we want: the
 * loader constructs `AppComponents`, hydrate threads launch, and by
 * the time the user types a URL in the browser the boot is already
 * underway (or done).
 *
 * No effect on prod — `playRunHooks` only fire under `sbt run`.
 */
object Warmup {
  def apply(): PlayRunHook = new PlayRunHook {
    override def afterStarted(): Unit = {
      val t = new Thread(() => {
        // Give the listener a moment to be fully ready (small race
        // against the `Listening for HTTP` log line).
        Thread.sleep(500)
        Try {
          val conn = new java.net.URI("http://localhost:9000/health").toURL.openConnection()
          conn.setConnectTimeout(5000)
          conn.setReadTimeout(60000)  // first load can take ~15s under load
          conn.getInputStream.close()
        }
        ()
      }, "play-eager-warmup")
      t.setDaemon(true)
      t.start()
    }
  }
}
