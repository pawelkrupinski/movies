package services.cinemas.common

/** Reads `ranges` (flattened `[from, until)` pairs) of `text` back to back, copying in bulk: a page streamed to the
 *  parser as the parts of it a parse reads ([[SlimmedPage.slimmedReader]]), since
 *  building the slimmed string cost a third of what slimming saved. */
private[common] final class RangesReader(text: String, ranges: Array[Int]) extends java.io.Reader {
  private var range = 0
  private var at    = if (ranges.isEmpty) 0 else ranges(0)
  override def read(buffer: Array[Char], offset: Int, length: Int): Int = {
    while (range < ranges.length && at >= ranges(range + 1)) { range += 2; if (range < ranges.length) at = ranges(range) }
    if (range >= ranges.length) -1
    else {
      val count = math.min(length, ranges(range + 1) - at)
      text.getChars(at, at + count, buffer, offset)
      at += count
      count
    }
  }
  override def close(): Unit = ()
}

private[common] object RangesReader {
  /** What a [[RangesReader]] over `text` streams, as one string — for a spec to read. */
  def text(text: String, ranges: Array[Int]): String = {
    val out = new java.io.StringWriter(text.length); new RangesReader(text, ranges).transferTo(out); out.toString
  }
}

/** A page streamed to its parser as only the parts the parse reads ([[keptRanges]]), so the parser does not tokenise
 *  the rest: building the slimmed string instead cost a third of what slimming saved. */
trait SlimmedPage {
  /** The `[from, until)` ranges of `html` the parse keeps, in order, flattened. */
  protected def keptRanges(html: String): Array[Int]

  private[common] def slimmedReader(html: String): java.io.Reader = new RangesReader(html, keptRanges(html))

  /** [[slimmedReader]]'s text as a string — for the spec. */
  private[common] def slimmed(html: String): String = RangesReader.text(html, keptRanges(html))
}
