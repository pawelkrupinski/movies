package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.io.ByteArrayInputStream
import java.nio.file.Files
import javax.imageio.ImageIO

class ShareCardPostersSpec extends AnyFlatSpec with Matchers {

  private val url = "https://cdn.example/poster-a.jpg"

  private val film1 = "f0123456789abcd"

  "The poster cache" should "fetch a film's poster once, cache it slot-sized, and serve the next render from disk" in {
    val rig = new Rig
    rig.posters.load(film1, Seq(url)).map { case (chosen, i) => (chosen, i.getWidth, i.getHeight) } shouldBe Some((url, 420, 630))
    rig.download.total shouldBe 1
    val cached = rig.store.posterPath(film1)
    rig.store.version(cached) shouldBe Some(ShareCardPosters.key(url))
    val slot = ImageIO.read(cached.toFile)
    (slot.getWidth, slot.getHeight) shouldBe ((420, 630))

    rig.posters.load(film1, Seq(url)) shouldBe defined
    rig.download.total shouldBe 1                                  // no fetch for a cached poster
  }

  it should "overwrite the film's one poster when its poster changes" in {
    val rig = new Rig
    rig.posters.load(film1, Seq(url))
    rig.posters.load(film1, Seq("https://cdn.example/new.jpg")).map(_._1) shouldBe Some("https://cdn.example/new.jpg")
    rig.download.total shouldBe 2
    rig.store.version(rig.store.posterPath(film1)) shouldBe Some(ShareCardPosters.key("https://cdn.example/new.jpg"))
    Files.list(rig.store.root.resolve(ShareCardStore.PosterDir)).count() shouldBe 1
  }

  it should "use a cached fallback rather than refetch a primary that keeps failing" in {
    val primary = "https://multikino.example/403.jpg"
    val rig = new Rig(download = new CountingDownload(failing = Set(primary)))
    rig.posters.load(film1, Seq(primary, url)).map(_._1) shouldBe Some(url)
    rig.posters.load(film1, Seq(primary, url)).map(_._1) shouldBe Some(url)
    rig.download.calls.get(primary).get shouldBe 1
    rig.download.calls.get(url).get shouldBe 1
  }

  "A film's poster load" should "count once per film: a failing primary whose fallback works is no failure" in {
    val primary  = "https://multikino.example/403.jpg"
    val series   = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val download = new CountingDownload(failing = Set(primary, "https://gone.example/a.jpg", "https://gone.example/b.jpg"))
    val posters  = new ShareCardPosters(tempStore(), download, newJavaShrinker(), series.forCountry("pl"))

    posters.load(film1, Seq(primary, url)) shouldBe defined
    posters.load("fother", Seq("https://gone.example/a.jpg", "https://gone.example/b.jpg")) shouldBe None
    (series.posterLoadCount("pl", ok = true, retry = false), series.posterLoadCount("pl", ok = false, retry = false)) shouldBe ((1.0, 1.0))
    PosterFailure.all.map(series.posterFetchCount("pl", _)).sum shouldBe 3.0   // the per-URL detail stays
  }

  "A failed poster's log line" should "be rate-limited per poster cache, not across every cache in the process" in {
    val gone = "https://gone.example/a.jpg"
    def failOnce(): Unit = new Rig(download = new CountingDownload(failing = Set(gone))).posters.load(film1, Seq(gone))
    val events = tools.LogCapture.thisThread(classOf[ShareCardPosters].getName, Some(ch.qos.logback.classic.Level.DEBUG)) {
      failOnce(); failOnce()
    }
    events.map(_.getLevel.toString) shouldBe Seq("INFO", "INFO")
  }

  // The kit's shrinker was once one object-level instance, so every rig — and every simulated
  // worker in the it/ specs — queued behind ONE decode gate, where production has one per process.
  "Two rigs' shrinkers" should "each shrink behind their own decode gate" in {
    import scala.concurrent.{Await, Future, Promise}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.*
    val (busy, idle) = (new Rig(), new Rig())
    val file    = Files.write(Files.createTempFile("poster-", ".jpg"), posterJpeg)
    val holding = Promise[Unit](); val release = Promise[Unit]()
    val holder  = Future(busy.shrinker.gate.withPermit { holding.success(()); Await.ready(release.future, 30.seconds) })
    try {
      Await.ready(holding.future, 10.seconds)
      Await.result(Future(idle.shrinker.coverSlot(file)), 10.seconds).map(_.getWidth) shouldBe Right(420)
    } finally { release.success(()); Await.ready(holder, 30.seconds); Files.delete(file) }
  }

  "A poster download" should "be abandoned at the byte cap instead of written out" in {
    val body = new Array[Byte](10 * 1024)
    PosterPipeline.copyCapped(new ByteArrayInputStream(body), maxBytes = 4096) shouldBe Left(PosterFailure.TooLarge)
    val kept = PosterPipeline.copyCapped(new ByteArrayInputStream(body), maxBytes = 64 * 1024)
    kept.map(Files.size) shouldBe Right(10L * 1024)
    kept.foreach(Files.delete)
  }

  "The shrinker" should "decode with the bounded JDK reader when vips is not installed" in {
    val file = Files.write(Files.createTempFile("poster-", ".jpg"), posterJpeg)
    try new VipsPosterShrinker(binary = None).coverSlot(file).map(i => (i.getWidth, i.getHeight)) shouldBe Right((420, 630))
    finally Files.delete(file)
  }

  it should "refuse a real 8000×12000 progressive JPEG before decoding it, with the worker's memory untouched" in {
    val vips = VipsPosterShrinker.locate(settings.ProcessConfiguration.resolve().executableSearchPath)
    assume(vips.isDefined, "vips is not installed")
    val dir  = Files.createTempDirectory("giant-poster-")
    val big  = dir.resolve("big.jpg")
    // Generated by vips itself: a 96-megapixel raster built in the JVM would be the very allocation
    // this path exists to avoid.
    def run(args: String*): Unit = new ProcessBuilder((vips.get +: args)*).inheritIO().start().waitFor() shouldBe 0
    run("gaussnoise", dir.resolve("n.v").toString, "8000", "12000")
    run("cast", dir.resolve("n.v").toString, dir.resolve("u.v").toString, "uchar")
    run("jpegsave", dir.resolve("u.v").toString, big.toString, "--interlace", "--Q", "80")
    val threads   = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val thread    = Thread.currentThread().threadId()
    val before    = threads.getThreadAllocatedBytes(thread)
    new VipsPosterShrinker(binary = vips).coverSlot(big) shouldBe Left(PosterFailure.ProgressiveEstimate)
    val allocated = threads.getThreadAllocatedBytes(thread) - before
    info(f"JVM allocation for refusing the 8000×12000 progressive: ${allocated / 1e6}%.2f MB")
    allocated should be < (4L * 1024 * 1024)
    Files.list(dir).forEach(Files.delete(_)); Files.delete(dir)
  }
}
