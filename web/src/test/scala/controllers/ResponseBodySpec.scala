package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.pekko.util.ByteString
import play.twirl.api.{Html, HtmlFormat}

import java.io.ByteArrayInputStream
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream

/**
 * [[ResponseBody.html]] writes a Twirl tree's UTF-8 without flattening it to one
 * `String` first. What it must never change is a single byte of the page — above all
 * the escaping Twirl applies to a leaf at render time — and what it is for is the
 * heap: a New-York-sized listing cost ~111 MB of `String` + `getBytes` per render.
 */
class ResponseBodySpec extends AnyFlatSpec with Matchers {

  private def gunzip(bytes: ByteString): ByteString =
    ByteString(new GZIPInputStream(new ByteArrayInputStream(bytes.toArrayUnsafe())).readAllBytes())

  /** A tree with every leaf kind a template produces: raw markup, escaped user text
   *  (escaped only when the leaf renders), non-Latin-1 text, and nesting. */
  private val tree = new Html(Seq(
    Html("<div class=\"x\">"),
    HtmlFormat.escape("""<script>alert("x")</script> & 'q'"""),
    new Html(Seq(Html("Kraków ↗ — "), HtmlFormat.escape("Łódź <b>"), Html(""))),
    Html("</div>")
  ))

  "a rendered page" should "go out byte for byte as Html.body encodes it, escaping included" in {
    val expected = ByteString(tree.body, StandardCharsets.UTF_8)
    ResponseBody.html(tree).plain shouldBe expected
    gunzip(ResponseBody.html(tree).gzipped) shouldBe expected
    expected.utf8String should include ("&lt;script&gt;")
  }

  // A streamed fragment writes into the walk's own buffer and flushes as it goes; the
  // bytes must still be exactly what `Html.body` renders the same tree to — here
  // across many flushes, with a multi-byte char straddling them.
  it should "write a streamed fragment's bytes exactly, across flushes" in {
    val streamed = new StreamedHtml((out, flush) =>
      for (i <- 0 until 5000) { out.append("<i>").append(i).append(" Łódź ↗</i>"); flush() })
    val page = new Html(Seq(Html("<ul>"), new Html(List(streamed)), Html("</ul>")))
    val expected = ByteString(page.body, StandardCharsets.UTF_8)
    expected.length should be > 64 * 1024
    ResponseBody.html(page).plain shouldBe expected
    gunzip(ResponseBody.html(page).gzipped) shouldBe expected
  }

  it should "say the same of a text body" in {
    val json = """{"title":"Łódź ↗"}"""
    ResponseBody.text(json).plain.utf8String shouldBe json
    gunzip(ResponseBody.text(json).gzipped).utf8String shouldBe json
  }

  // ── The heap ──────────────────────────────────────────────────────────────────

  private val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def allocatedBy(f: => Any): Long = {
    val id = Thread.currentThread.threadId
    val before = threads.getThreadAllocatedBytes(id)
    f
    threads.getThreadAllocatedBytes(id) - before
  }

  /** ~6 MB of listing-shaped markup in 40k leaves, non-Latin-1 like the real page. */
  private def bigPage: Html = new Html((0 until 40000).map(i =>
    new Html(Seq(Html(s"""<a data-s="ho000$i?date=2026-10-02&amp;site=1929&amp;id=$i">"""),
             HtmlFormat.escape(f"${i % 24}%02d:${i % 60}%02d"), Html(" Regal ↗</a>"),
             Html("<div class=\"cinema-group\" data-u=\"https://www.regmovies.com/movies/forgotten-island-\">")))))

  /** What a body cost before [[ResponseBody]]: the whole page as one `String`, then
   *  its bytes, then gzip — the route `EncodedResponseCache.gzip(html.body)` took. */
  private def oldRoute(page: Html): Int = {
    val out = new java.io.ByteArrayOutputStream()
    val gz  = new java.util.zip.GZIPOutputStream(out)
    gz.write(page.body.getBytes(StandardCharsets.UTF_8)); gz.close()
    out.toByteArray.length
  }

  it should "write a listing-sized page in a fraction of what flattening it to one String costs" in {
    // A fresh tree per measurement: `Html.body` is a lazy val, so a tree whose body was
    // already flattened would make the old route look free.
    val size = bigPage.body.getBytes(StandardCharsets.UTF_8).length.toLong
    for (_ <- 1 to 3) { oldRoute(bigPage); ResponseBody.html(bigPage).plain; ResponseBody.html(bigPage).gzipped }   // warm the JIT
    val (p1, p2, p3) = (bigPage, bigPage, bigPage)
    val flattened = allocatedBy(oldRoute(p1))
    val streamed  = allocatedBy(ResponseBody.html(p2).gzipped)
    val plain     = allocatedBy(ResponseBody.html(p3).plain)
    withClue(s"page ${size / 1024} KB; String route ${flattened / 1024} KB, streamed gzip ${streamed / 1024} KB, plain ${plain / 1024} KB: ") {
      streamed should be < flattened / 3
      plain    should be < 3 * size
    }
  }
}
