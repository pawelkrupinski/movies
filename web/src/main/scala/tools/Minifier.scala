package tools

import java.util.concurrent.ConcurrentHashMap

import play.api.Mode

/** Rewrites an HTML fragment's inline `<script>` / `<style>` blocks — what
 *  `views/_minified.scala.html` applies to every block it wraps. Chosen once
 *  at the web composition root ([[Minifier.forMode]]) and handed to the
 *  templates by the controllers. */
trait Minifier {
  def process(html: String): String
}

object Minifier {
  /** Production traffic gets a fresh [[MemoisingMinifier]]; every other mode
   *  gets [[PassThroughMinifier]] so the inline source stays readable in
   *  DevTools while running `sbt run` locally. */
  def forMode(mode: Mode): Minifier =
    if (mode == Mode.Prod) new MemoisingMinifier else PassThroughMinifier
}

/** Leaves the markup untouched — the dev-mode and `/debug/tune` choice. */
object PassThroughMinifier extends Minifier {
  def process(html: String): String = html
}

/** [[Minify]] memoised per input, so the per-request render cost is one
 *  `ConcurrentHashMap` lookup after the first hit. The caches are unbounded
 *  but the input set is bounded — every distinct template-rendered block is
 *  one entry, ~dozens total even with interpolated values — and they live
 *  exactly as long as the instance that owns them. */
class MemoisingMinifier extends Minifier {
  private val blockCache = new ConcurrentHashMap[String, String]
  private val jsCache    = new ConcurrentHashMap[String, String]
  private val cssCache   = new ConcurrentHashMap[String, String]

  private val minifyJs:  String => String = src => jsCache.computeIfAbsent(src, Minify.minifyJs)
  private val minifyCss: String => String = src => cssCache.computeIfAbsent(src, Minify.minifyCss)

  def process(html: String): String =
    blockCache.computeIfAbsent(html, Minify.process(_, minifyJs, minifyCss))

  /** How many distinct blocks this instance has memoised. */
  def cachedBlocks: Int = blockCache.size
}
