package controllers

import scala.io.Source

/** The build-time-generated language pack — `messages`/`messages.en`/
 *  `messages.de`/`messages.es`, flattened to one JSON object, produced by
 *  `project/I18nPackGenerator.scala` (wired into the `web` project's
 *  `Compile / resourceGenerators` in `build.sbt`). Embedded once per page
 *  (`_sharedJsConfig.scala.html`) so `shared.js` can swap the visible UI
 *  language in place, with no server round trip and no extra request.
 *
 *  `messages.*` stays the only place a translator edits; this is derived,
 *  never hand-maintained. See `I18nPackGenerator`'s doc for the exact
 *  `{0}`/`''` handling client-side substitution relies on.
 */
object I18nPacks {
  lazy val json: String = {
    val stream = getClass.getResourceAsStream("/i18n-packs.generated.json")
    try Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }
}
