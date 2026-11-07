package tools

/** Runs of whitespace a page or feed spaces text with, as one space: what every scraper does to a title or a text node
 *  before reading it. Java's `\s`, so a no-break space stays (a caller folding it says so first). */
object Whitespace {
  private val Run = java.util.regex.Pattern.compile("""\s+""")

  extension (text: String)
    /** `text` with each whitespace run one space, trimmed. */
    def collapsedSpaces: String = Run.matcher(text).replaceAll(" ").trim
}
