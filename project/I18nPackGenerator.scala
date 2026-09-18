import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Properties

/**
 * Generates the client-side language pack — all four `messages`/`messages.en`/
 * `messages.de`/`messages.es` bundles, flattened into one JSON object — from
 * `web/src/main/resources/messages*` at build time, so those bundles stay the
 * single source of truth for both the server-rendered default-language page
 * and the client-side language switch (`controllers.I18nPacks`, `shared.js`'s
 * `applyLanguage`).
 *
 * Values keep their `{0}`/`{1}` placeholders literal — Java `MessageFormat`
 * substitution only ever runs server-side, for the deployment-default render;
 * the client does its own positional substitution against
 * `data-i18n-arg0`/`data-i18n-arg1`. The one thing this DOES undo is
 * `MessageFormat`'s doubled-apostrophe escape (`''` -> `'`), since the client
 * never runs these strings through `MessageFormat` itself.
 */
object I18nPackGenerator {

  private val Bundles: Seq[(String, String)] = Seq(
    "pl" -> "messages",
    "en" -> "messages.en",
    "de" -> "messages.de",
    "es" -> "messages.es",
  )

  /** Reads the four bundles out of `resourceDir` and writes the flattened
   *  JSON pack to `targetDir/i18n-packs.generated.json`, returning that file
   *  (the shape sbt's `resourceGenerators` contract expects). */
  def generate(resourceDir: File, targetDir: File): File = {
    val packs = Bundles.map { case (lang, fileName) => lang -> loadBundle(new File(resourceDir, fileName)) }
    val out = new File(targetDir, "i18n-packs.generated.json")
    targetDir.mkdirs()
    Files.write(out.toPath, toJson(packs).getBytes(StandardCharsets.UTF_8))
    out
  }

  private def loadBundle(file: File): Seq[(String, String)] = {
    val props = new Properties()
    // `Properties.load(InputStream)` decodes as ISO-8859-1 (the classic Java
    // properties-file convention) — WRONG here, since `messages*` are real
    // UTF-8 files (same convention Play's own `MessagesApi` reads them
    // under), so a `File.pathSeparator`-agnostic non-ASCII value like
    // Spanish "España" silently corrupted to "EspaÃ±a". `load(Reader)`
    // skips that decoding entirely — the Reader has already decoded the
    // bytes, so `Properties` just consumes characters.
    val reader = Files.newBufferedReader(file.toPath, StandardCharsets.UTF_8)
    try props.load(reader) finally reader.close()
    val keys = props.stringPropertyNames().iterator()
    val entries = Seq.newBuilder[(String, String)]
    while (keys.hasNext) {
      val key = keys.next()
      entries += key -> props.getProperty(key).replace("''", "'")
    }
    entries.result().sortBy(_._1)
  }

  // Hand-rolled rather than pulling a JSON library into the build: every
  // value here is a plain string, so a minimal escaper is simpler than wiring
  // play-json (or similar) onto the sbt meta-build's own classpath/Scala
  // version just for this.
  private def toJson(packs: Seq[(String, Seq[(String, String)])]): String = {
    def quoted(s: String): String = {
      val sb = new StringBuilder("\"")
      s.foreach {
        case '"'          => sb.append("\\\"")
        case '\\'         => sb.append("\\\\")
        case '\n'         => sb.append("\\n")
        case '\r'         => sb.append("\\r")
        case '\t'         => sb.append("\\t")
        case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
        case c            => sb.append(c)
      }
      sb.append('"').toString
    }
    val langObjects = packs.map { case (lang, entries) =>
      val body = entries.map { case (k, v) => s"${quoted(k)}:${quoted(v)}" }.mkString(",")
      s"${quoted(lang)}:{$body}"
    }
    s"{${langObjects.mkString(",")}}"
  }
}
