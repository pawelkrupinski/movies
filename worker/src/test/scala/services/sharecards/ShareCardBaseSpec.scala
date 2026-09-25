package services.sharecards

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters.*

/** The card BASE cache: a ratings change draws only the badges onto a cached base; any other
 *  change rebuilds the base from the cached poster. Same budget, grace and prune as the rest. */
class ShareCardBaseSpec extends AnyFlatSpec with Matchers {

  private def bases(store: ShareCardStore): Seq[String] =
    store.list().filter(_.kind == ShareCardStore.Kind.Base).map(_.name)

  private def rated(movie: models.ResolvedMovie, imdb: Double) = movie.copy(ratings = movie.ratings.copy(imdb = Some(imdb)))

  private final class PathRig extends Rig {
    val series = new ShareCardMetrics.Series(Seq("pl"), new PrometheusRegistry)
    override val metrics = series.forCountry("pl")
    def path(p: String): Double = series.pathCount("pl", p)
  }

  "A first card" should "rebuild its base from the poster and keep it" in {
    val rig = new PathRig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.Rendered
    bases(rig.store) shouldBe Seq("f0123456789abcd.jpg")
    rig.store.version(rig.store.basePath(movie._id)) shouldBe Some(rig.service.inputs(movie).baseVersion(Some(movie.posterUrl.get)))
    rig.path(ShareCardMetrics.Path.BaseRebuild) shouldBe 1
  }

  "A ratings change" should "draw the badges on the cached base without touching the poster" in {
    val rig = new PathRig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    // The poster cache is gone: a base_hit must not need it.
    Files.list(rig.store.root.resolve(ShareCardStore.PosterDir)).iterator.asScala.foreach(Files.delete(_))
    rig.service.render(rig.service.inputs(rated(movie, 8.4)), Seq(ShareCardReason.Ratings)) shouldBe ShareCardMetrics.Outcome.Rendered
    rig.path(ShareCardMetrics.Path.BaseHit) shouldBe 1
    rig.download.total shouldBe 1
    rig.service.existing(rig.service.inputs(rated(movie, 8.4))) shouldBe defined
    bases(rig.store) should have size 1
  }

  "A title change" should "rebuild the base from the cached poster, not from the old base or card, in place" in {
    val rig = new PathRig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.render(rig.service.inputs(movie.copy(title = "Diuna 2")), Seq(ShareCardReason.Title))
    rig.path(ShareCardMetrics.Path.BaseRebuild) shouldBe 2
    rig.download.total shouldBe 1                                       // the poster came from the cache
    bases(rig.store) should have size 1
    rig.store.version(rig.store.basePath(movie._id)) shouldBe
      Some(rig.service.inputs(movie.copy(title = "Diuna 2")).baseVersion(Some(movie.posterUrl.get)))
  }

  "A posterless film" should "be drawn whole, keeping no base" in {
    val rig = new PathRig
    rig.service.render(rig.service.inputs(film().copy(posterUrl = None)), Seq(ShareCardReason.NewFilm))
    rig.path(ShareCardMetrics.Path.Full) shouldBe 1
    bases(rig.store) shouldBe empty
  }

  "The budget" should "count a film's base, and never evict the base of a current card" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.readModel.upsertMovie(movie.copy(shareCard = rig.service.current(movie)))
    def age(dir: Path): Unit = Files.list(dir).iterator.asScala.foreach(Files.setLastModifiedTime(_, FileTime.from(T0.minusSeconds(7200))))
    age(rig.store.root); age(rig.store.root.resolve(ShareCardStore.BaseDir)); age(rig.store.root.resolve(ShareCardStore.PosterDir))
    val tight = new ShareCardJanitor(rig.store, rig.readModel, 1L, rig.metrics, rig.clock, _ => ()).enforceBudget()
    tight.currentBytes shouldBe rig.store.list().map(_.bytes).sum
    bases(rig.store) should have size 1
  }
}
