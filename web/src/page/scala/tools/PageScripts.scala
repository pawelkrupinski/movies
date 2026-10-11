package tools

import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

/** The page-side JavaScript the page specs run — CDP fetch stubs, harness helpers —
 *  kept as `.js` files under `web/src/page/resources/` rather than in Scala strings,
 *  where it would be unlinted, unhighlighted and escape-prone. Each file exposes a small
 *  `window.__x` API, so a spec only makes one-line calls (`__panes.release('de')`).
 *  Installed into a tab with [[CdpPage.installScript]] / [[CdpPage.useScript]]. */
object PageScripts {
  private val sources = new ConcurrentHashMap[String, String]()

  /** The text of the resource at `path` (relative to the resources root, e.g.
   *  `landing/hold-picker-panes.js`), read once per JVM. */
  def source(path: String): String =
    sources.computeIfAbsent(path, p => {
      val in = Option(getClass.getResourceAsStream("/" + p))
        .getOrElse(throw new IllegalArgumentException(s"no page script at web/src/page/resources/$p"))
      try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
    })
}
