package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.awt.Color
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

/**
 * Posters come in every colour model a cinema site can produce — transparent PNGs, CMYK and
 * grayscale JPEGs, palette PNGs — and each must become a card. On 2026-09-24 a PNG with an alpha
 * channel reached the JPEG writer untouched ("Bogus input colorspace"), the exception escaped the
 * render, and the task failed over and over without trying the film's other posters. The cases that render through
 * a real vips child are in [[PosterColourSpaceIntegrationSpec]].
 */
class PosterColourSpaceSpec extends AnyFlatSpec with Matchers {

  import PosterColourSpaceSpec.{file, image, posters, rendersThrough, serving}

  for ((what, poster) <- posters) {
    s"$what" should "render through the JDK decode" in rendersThrough(new VipsPosterShrinker(binary = None), poster)
  }

  "A poster that cannot be turned into a card" should "fall through to the film's next poster, counted, without throwing" in {
    val broken = "https://cdn.example/broken.jpg"
    val good   = "https://cdn.example/good.jpg"
    val download = new PosterDownload {
      def fetch(url: String): Either[String, Path] = Right(Files.write(Files.createTempFile("dl-", ".img"), url.getBytes("UTF-8")))
    }
    val shrinker = new PosterShrinker {
      def coverSlot(f: Path): Either[String, BufferedImage] =
        if (new String(Files.readAllBytes(f), "UTF-8") == broken) throw new javax.imageio.IIOException("Bogus input colorspace")
        else Right(image(BufferedImage.TYPE_INT_RGB, 420, 630))
    }
    val series = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val store = tempStore()
    val loaded = new ShareCardPosters(store, download, shrinker, series.forCountry("pl"), _root_.tools.Stopwatch.System).load("f1", Seq(broken, good))
    loaded.map(_._1) shouldBe Some(good)
    series.posterFetchCount("pl", PosterFailure.EncodeError) shouldBe 1
  }

  it should "leave the film a card without a poster, not an exception, when every poster fails" in {
    val shrinker = new PosterShrinker { def coverSlot(f: Path): Either[String, BufferedImage] = throw new javax.imageio.IIOException("Bogus input colorspace") }
    val store = tempStore()
    val posters = new ShareCardPosters(store, serving(() => file(image(BufferedImage.TYPE_INT_RGB), "jpg")), shrinker, ShareCardMetrics.noop, _root_.tools.Stopwatch.System)
    val service = new ShareCardService(models.Country.default, store, posters, new services.tasks.InMemoryTaskQueue, ShareCardRescrapes.disabled(ShareCardMetrics.noop), ShareCardMetrics.noop, clockAt(T0))
    service.render(service.inputs(film()), Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.RenderedNoPoster
  }
}

object PosterColourSpaceSpec extends Matchers {

  def image(kind: Int, w: Int = 600, h: Int = 900): BufferedImage = {
    val img = new BufferedImage(w, h, kind)
    val g = img.createGraphics()
    g.setColor(new Color(40, 120, 200, 120)); g.fillRect(0, 0, w, h)
    g.setColor(Color.ORANGE); g.fillOval(w / 6, h / 4, w / 2, w / 2); g.dispose()
    img
  }

  def file(img: BufferedImage, format: String): Path = {
    val f = Files.createTempFile("poster-", s".$format")
    ImageIO.write(img, format, f.toFile) shouldBe true
    f
  }

  val posters: Seq[(String, () => Path)] = Seq(
    "a PNG with transparency" -> (() => file(image(BufferedImage.TYPE_INT_ARGB), "png")),
    "a grayscale JPEG"        -> (() => file(image(BufferedImage.TYPE_BYTE_GRAY), "jpg")),
    "an indexed-colour PNG"   -> (() => file(image(BufferedImage.TYPE_BYTE_INDEXED), "png")),
    "a slot-sized transparent PNG" -> (() => file(image(BufferedImage.TYPE_INT_ARGB, 420, 630), "png")))

  /** Download serving `poster` for every URL. */
  def serving(poster: () => Path): PosterDownload = new PosterDownload {
    def fetch(url: String): Either[String, Path] = Right(Files.copy(poster(), Files.createTempFile("dl-", ".img"), java.nio.file.StandardCopyOption.REPLACE_EXISTING))
  }

  def rendersThrough(shrinker: PosterShrinker, poster: () => Path) = {
    val store = tempStore()
    val posters = new ShareCardPosters(store, serving(poster), shrinker, ShareCardMetrics.noop, _root_.tools.Stopwatch.System)
    val movie = film()
    val service = new ShareCardService(models.Country.default, store, posters, new services.tasks.InMemoryTaskQueue, ShareCardRescrapes.disabled(ShareCardMetrics.noop), ShareCardMetrics.noop, clockAt(T0))
    service.render(service.inputs(movie), Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.Rendered
    ImageIO.read(store.cardPath(movie._id).toFile).getWidth shouldBe 1200
    ImageIO.read(store.posterPath(movie._id).toFile).getWidth shouldBe 420
    store.version(store.cardPath(movie._id)) should not be Some(service.inputs(movie).version(None))
  }
}
