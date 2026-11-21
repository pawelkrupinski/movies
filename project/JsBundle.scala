package kinowo.build

import sbt._

/**
 * The shared browser scripts' bundle (`js/shared.bundle.js`): the scripts `web/src/main/js-bundles/shared.txt` lists,
 * concatenated in its order, with a source map back to each one. Every city page loads it with ONE request
 * (`_sharedScripts.scala.html`) where it took one per module; the modules stay the source, one concern each.
 *
 * Concatenation keeps most of what the separate classic scripts meant: they shared one global scope already (each
 * top-level function and `const` was the next script's), none says `"use strict"`, and each runs in the listed order.
 * A `;` line between two scripts keeps one's unterminated last statement from running into the next one's first. What
 * it changes is what one script is: every function the bundle declares exists from its first line, and an error
 * thrown at load stops the rest -- as when the modules were one `shared.js`, which is why only they are bundled (the
 * list says why `i18n.js` is not).
 *
 * The bundle goes through the asset pipeline like any source asset (`Assets / resourceGenerators`), so the digest
 * stage versions it; its map names each module by its own published path, so DevTools shows and steps through the
 * modules as written.
 */
object JsBundle {

  /** `order`'s scripts (paths under `assets`, one a line; `#` comments and blank lines skipped). */
  def scripts(order: File): Seq[String] =
    IO.readLines(order).map(_.trim).filterNot(line => line.isEmpty || line.startsWith("#"))

  /** Writes `<name>.js` and `<name>.js.map` under `out` (beside the scripts' own directory) and returns both. */
  def write(assets: File, order: File, out: File, name: String): Seq[File] = {
    val listed  = scripts(order)
    val missing = listed.filterNot(path => (assets / path).isFile)
    require(missing.isEmpty, s"$order lists scripts that are not under $assets: ${missing.mkString(", ")}")
    val bundle = out / s"$name.js"
    val map    = out / s"$name.js.map"
    val bundleDirectory = (assets / name).getParentFile
    val sources = listed.map(path => IO.relativize(bundleDirectory, assets / path).getOrElse(path))
    val text = new StringBuilder
    val mappings = new StringBuilder
    var previousSource, previousLine = 0
    def line(content: String, segment: String): Unit = {
      text.append(content).append('\n')
      mappings.append(segment).append(';')
    }
    listed.zipWithIndex.foreach { case (path, source) =>
      line(s"// $path", "")
      IO.readLines(assets / path).zipWithIndex.foreach { case (content, sourceLine) =>
        line(content, Vlq.encode(0) + Vlq.encode(source - previousSource) + Vlq.encode(sourceLine - previousLine) + Vlq.encode(0))
        previousSource = source
        previousLine = sourceLine
      }
      line(";", "")
    }
    text.append(s"//# sourceMappingURL=${map.getName}\n")
    IO.write(bundle, text.toString)
    val quoted = sources.map(source => "\"" + source + "\"").mkString(",")
    IO.write(map, s"""{"version":3,"file":"${bundle.getName}","sources":[$quoted],"names":[],"mappings":"${mappings.toString}"}""")
    Seq(bundle, map)
  }

  /** A source map's base64 VLQ: the sign in the lowest bit, then five bits a digit, low first. */
  private object Vlq {
    private val Digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    def encode(value: Int): String = {
      var rest = if (value < 0) (-value << 1) | 1 else value << 1
      val out = new StringBuilder
      do {
        val digit = rest & 31
        rest >>>= 5
        out.append(Digits(if (rest > 0) digit | 32 else digit))
      } while (rest > 0)
      out.toString
    }
  }
}
