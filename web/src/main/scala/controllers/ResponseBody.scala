package controllers

import java.io.{OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPOutputStream

import org.apache.pekko.util.ByteString
import play.twirl.api.Html

/**
 * A response body as the bytes it goes out as — gzipped for the shared cache and the
 * clients that take it, plain for the rest — produced WITHOUT first holding the whole
 * page as one `String` where it can avoid it.
 *
 * WHY. A city listing is a Twirl tree of tens of thousands of fragments. `Html.body`
 * flattens it into one `String` through a `StringBuilder` that doubles as it grows,
 * and the page is not Latin-1 (`↗`, the translation packs), so every char is two
 * bytes: New York's 7.6 MB page cost ~78 MB for its `String` and ~33 MB more for the
 * `getBytes` after it — per render, in a 1 GiB heap, a third of what a cache miss
 * allocated. [[html]] walks the tree instead and writes each fragment's UTF-8 straight
 * into the stream that needs it.
 */
sealed trait ResponseBody {
  def gzipped: ByteString
  def plain: ByteString
}

object ResponseBody {

  /** A body that already is one `String` — a JSON payload, say. */
  def text(render: => String): ResponseBody = new ResponseBody {
    def gzipped: ByteString = gzip(out => out.write(render.getBytes(StandardCharsets.UTF_8)))
    def plain: ByteString   = ByteString.fromString(render, StandardCharsets.UTF_8)
  }

  /** `body`, recording to `record` how much heap producing its bytes allocated on this
   *  thread — the render it wraps included, since a body renders when its bytes are
   *  first asked for. Where the JVM cannot say, it records nothing. */
  def measured(body: ResponseBody, record: Long => Unit): ResponseBody = new ResponseBody {
    def gzipped: ByteString = tools.ThreadAllocation.measure(record)(body.gzipped)
    def plain: ByteString   = tools.ThreadAllocation.measure(record)(body.plain)
  }

  /** A rendered Twirl page, written fragment by fragment. */
  def html(render: => Html): ResponseBody = new ResponseBody {
    def gzipped: ByteString = gzip(out => write(render, out))
    def plain: ByteString = {
      val out = new ChunkedOutput
      write(render, out)
      out.result
    }
  }

  private def gzip(body: OutputStream => Unit): ByteString = {
    val out = new ChunkedOutput
    val gz  = new GZIPOutputStream(out, 1 << 16)
    try body(gz) finally gz.close()
    out.result
  }

  /** Bytes in fixed 64 KB chunks, joined into one `ByteString` rope without a copy.
   *  A `ByteArrayOutputStream` doubles one array and copies it at every growth (~3.6x
   *  the page allocated to hold it once), and Pekko's `ByteStringBuilder` copies what
   *  it is handed into buffers of its own; this allocates each byte's chunk once. */
  private final class ChunkedOutput extends OutputStream {
    private val ChunkSize = 1 << 16
    private var done  = ByteString.empty
    private var chunk = new Array[Byte](ChunkSize)
    private var used  = 0
    private def seal(): Unit = {
      done = done ++ ByteString.fromArrayUnsafe(chunk, 0, used)
      chunk = new Array[Byte](ChunkSize); used = 0
    }
    override def write(b: Int): Unit = {
      if (used == ChunkSize) seal()
      chunk(used) = b.toByte; used += 1
    }
    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
      var from = offset; var left = length
      while (left > 0) {
        if (used == ChunkSize) seal()
        val n = math.min(left, ChunkSize - used)
        System.arraycopy(bytes, from, chunk, used, n)
        used += n; from += n; left -= n
      }
    }
    def result: ByteString = done ++ ByteString.fromArrayUnsafe(chunk, 0, used)
  }

  /** `html`'s UTF-8 into `out`, exactly the bytes `html.body` would encode to.
   *
   *  Each LEAF is still turned into text by its own `buildString`, through one reused
   *  buffer: a leaf may be an `HtmlFormat.escape` of user text whose escaping Twirl
   *  applies only there, so writing a leaf's raw `text` would emit it unescaped. The
   *  walk only replaces the concatenation of the leaves, never their rendering. */
  private[controllers] def write(html: Html, out: OutputStream): Unit = {
    val writer  = new OutputStreamWriter(out, StandardCharsets.UTF_8)
    val scratch = new scala.collection.mutable.StringBuilder(FlushAt * 2)
    var chars   = new Array[Char](FlushAt * 2)
    def flush(): Unit = {
      val length = scratch.length
      if (length > chars.length) chars = new Array[Char](math.max(length, chars.length * 2))
      scratch.underlying.getChars(0, length, chars, 0)
      writer.write(chars, 0, length)
      scratch.clear()
    }
    val flushIfFull: () => Unit = () => if (scratch.length >= FlushAt) flush()
    def writeChunked(text: String): Unit = {
      var from = 0
      while (from < text.length) {
        val n = math.min(chars.length, text.length - from)
        text.getChars(from, from + n, chars, 0)
        writer.write(chars, 0, n)
        from += n
      }
    }
    // ONE function value for the whole walk: `children.foreach(walk)` eta-expands a
    // fresh one at every node, ~1 MB a page across a listing's fragments.
    lazy val walk: Html => Unit = {
      case streamed: StreamedHtml => streamed.renderInto(scratch.underlying, flushIfFull)
      // Encoded straight from the string it already is, a buffer's worth at a time — not
      // copied through `scratch`, and not handed to `writer.write(String)`, which copies
      // the whole string into a fresh array first.
      case written: PrewrittenHtml => flush(); writeChunked(written.whole)
      case node =>
        val children = TwirlTree.children(node)
        if (children.nonEmpty) children.foreach(walk)
        else { TwirlTree.renderLeaf(node, scratch); flushIfFull() }
    }
    walk(html)
    flush()
    writer.flush()
  }

  /** How much markup the walk buffers before handing it to the stream. */
  private val FlushAt = 32 * 1024

}

/** A fragment that writes itself into the body as the body is written, instead of
 *  being rendered into a `String` first: [[ResponseBody.html]] hands it the buffer it
 *  is filling and a `flush` to call between pieces, so the fragment is never held
 *  whole. Rendered any other way — `Html.body`, a template nesting it — it writes into
 *  that builder and nothing changes, so the bytes are identical either way.
 *
 *  For the one fragment large enough to matter: a city listing's showings
 *  (`ShowingsMarkup.days`), 7 MB of New York's 7.7 MB page.
 *
 *  ⚠️ NEVER HAND ONE TO A TEMPLATE BARE. Twirl's `_display_` passes a value through
 *  only when its class is exactly `Html`; a subclass is escaped as text. Wrap it:
 *  `new Html(List(streamed))`. */
final class StreamedHtml(val renderInto: (java.lang.StringBuilder, () => Unit) => Unit) extends Html(Nil) {
  override protected def buildString(builder: scala.collection.mutable.StringBuilder): Unit =
    renderInto(builder.underlying, () => ())
}

/** A fragment that already exists as one string — a cached film card — which
 *  [[ResponseBody.html]] encodes straight from that string instead of copying it
 *  through its buffer (~1 MB of copying a New York render, a card at a time). Rendered
 *  any other way it appends itself, so the bytes are identical either way. Wrap it in a
 *  plain `Html` before a template sees it, for [[StreamedHtml]]'s reason. */
final class PrewrittenHtml(val whole: String) extends Html(Nil) {
  override protected def buildString(builder: scala.collection.mutable.StringBuilder): Unit = builder.append(whole)
}
