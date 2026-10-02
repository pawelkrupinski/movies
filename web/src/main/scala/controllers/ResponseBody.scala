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
    val scratch = new scala.collection.mutable.StringBuilder(1024)
    var chars   = new Array[Char](1024)
    def walk(node: Html): Unit = {
      val children = TwirlTree.children(node)
      if (children.nonEmpty) children.foreach(walk)
      else {
        scratch.clear()
        TwirlTree.renderLeaf(node, scratch)
        val length = scratch.length
        if (length > chars.length) chars = new Array[Char](math.max(length, chars.length * 2))
        scratch.underlying.getChars(0, length, chars, 0)
        writer.write(chars, 0, length)
      }
    }
    walk(html)
    writer.flush()
  }
}
