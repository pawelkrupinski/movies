package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters.*

/** One card, one base and one poster per film, each at a stable name and overwritten in place:
 *  nothing old is ever left behind, and `/share-cards/<cc>/<film>.jpg` is always the latest card. */
class ShareCardStableFilesSpec extends AnyFlatSpec with Matchers {

  private def files(dir: Path): Seq[String] =
    if (!Files.isDirectory(dir)) Nil else Files.list(dir).iterator.asScala.filter(Files.isRegularFile(_)).map(_.getFileName.toString).toSeq.sorted

  private def everything(rig: Rig): Seq[String] =
    Seq("" -> rig.store.root, ".base/" -> rig.store.root.resolve(ShareCardStore.BaseDir), ".posters/" -> rig.store.root.resolve(ShareCardStore.PosterDir))
      .flatMap { case (prefix, dir) => files(dir).map(prefix + _) }

  private def rated(movie: models.ResolvedMovie, imdb: Double) = movie.copy(ratings = movie.ratings.copy(imdb = Some(imdb)))

  "Re-rendering a film" should "overwrite its one card in place, leaving exactly one card, base and poster" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    val first = rig.service.current(movie).get
    val bytesBefore = Files.readAllBytes(rig.store.cardPath(movie._id))
    rig.service.render(rig.service.inputs(rated(movie, 8.4)), Seq(ShareCardReason.Ratings))
    rig.service.render(rig.service.inputs(rated(movie, 8.4).copy(title = "Diuna 2", posterUrl = Some("https://cdn.example/b.jpg"))), Seq(ShareCardReason.Title))

    everything(rig).sorted shouldBe Seq("f0123456789abcd.jpg", ".base/f0123456789abcd.jpg", ".posters/f0123456789abcd.jpg").sorted
    val latest = rig.service.current(rated(movie, 8.4).copy(title = "Diuna 2", posterUrl = Some("https://cdn.example/b.jpg"))).get
    latest should startWith ("f0123456789abcd.jpg?v=")
    latest should not be first
    Files.readAllBytes(rig.store.cardPath(movie._id)) should not equal bytesBefore
  }

  "A request for a stale version" should "get the latest card: the version names the URL, not the file" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    val stale = rig.service.current(movie).get
    rig.service.render(rig.service.inputs(rated(movie, 8.4)), Seq(ShareCardReason.Ratings))
    // The file behind `<film>.jpg?v=<stale>` is `<film>.jpg` — the new card.
    rig.store.cardPath(stale.takeWhile(_ != '?').stripSuffix(".jpg")) shouldBe rig.store.cardPath(movie._id)
    rig.store.version(rig.store.cardPath(movie._id)) shouldBe Some(rig.service.inputs(rated(movie, 8.4)).version(Some(movie.posterUrl.get)))
  }

  "A film dropped from the read model" should "lose its card, base and poster at once, unless they are brand new" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.onRetired(movie._id)
    everything(rig) should have size 3                                    // just written: another replica may be publishing it

    Seq(rig.store.cardPath(movie._id), rig.store.basePath(movie._id), rig.store.posterPath(movie._id))
      .foreach(Files.setLastModifiedTime(_, FileTime.from(T0.minusSeconds(7200))))
    rig.service.onRetired(movie._id)
    everything(rig) shouldBe empty
  }
}
