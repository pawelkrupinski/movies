package services.cinemas.common

/** Reads `ranges` (flattened `[from, until)` pairs) of `text` back to back, copying in bulk: a page streamed to the
 *  parser as the parts of it a parse reads ([[FlicksClient.slimmedReader]], [[FlicksFilmPage.slimmedReader]]), since
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
