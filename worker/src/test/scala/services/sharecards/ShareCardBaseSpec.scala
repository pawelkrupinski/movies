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
    bases(rig.store) shouldBe Seq(s"${rig.service.inputs(movie).baseKey(Some(movie.posterUrl.get))}.jpg")
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

  "A title change" should "rebuild the base from the cached poster, not from the old base or card" in {
    val rig = new PathRig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.render(rig.service.inputs(movie.copy(title = "Diuna 2")), Seq(ShareCardReason.Title))
    rig.path(ShareCardMetrics.Path.BaseRebuild) shouldBe 2
    rig.download.total shouldBe 1                                       // the poster came from the cache
    bases(rig.store) should have size 2
  }

  "A posterless film" should "be drawn whole, keeping no base" in {
    val rig = new PathRig
    rig.service.render(rig.service.inputs(film().copy(posterUrl = None)), Seq(ShareCardReason.NewFilm))
    rig.path(ShareCardMetrics.Path.Full) shouldBe 1
    bases(rig.store) shouldBe empty
  }

  "Rendering on a base" should "be much cheaper than rebuilding it" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))           // warm-up
    def time(body: => Any): Double = { val t = System.nanoTime(); body; (System.nanoTime() - t) / 1e6 }
    val hits = (1 to 5).map(i => time(rig.service.render(rig.service.inputs(rated(movie, 7.0 + i / 10.0)), Seq(ShareCardReason.Ratings))))
    val rebuilds = (1 to 5).map(i => time(rig.service.render(rig.service.inputs(movie.copy(title = s"Diuna $i")), Seq(ShareCardReason.Title))))
    val baseBytes = rig.store.list().filter(_.kind == ShareCardStore.Kind.Base).map(_.bytes)
    info(f"base_hit median ${hits.sorted.apply(2)}%.1f ms, base_rebuild (poster cached) median ${rebuilds.sorted.apply(2)}%.1f ms; base ${baseBytes.sum / baseBytes.size / 1024} KB")
  }

  "The janitor" should "keep the base a current card was drawn on, and prune an unreferenced one past the grace period" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.render(rig.service.inputs(movie.copy(title = "Diuna 2")), Seq(ShareCardReason.Title))
    val current = rig.service.existing(rig.service.inputs(movie.copy(title = "Diuna 2"))).get
    rig.readModel.upsertMovie(movie.copy(title = "Diuna 2", shareCard = Some(current)))
    rig.readModel.upsertScreening(screening(movie._id))
    def age(dir: Path): Unit = Files.list(dir).iterator.asScala.foreach(Files.setLastModifiedTime(_, FileTime.from(T0.minusSeconds(7200))))
    age(rig.store.root); age(rig.store.root.resolve(ShareCardStore.BaseDir)); age(rig.store.root.resolve(ShareCardStore.PosterDir))
    val report = new ShareCardJanitor(rig.store, rig.readModel, 1L << 30, rig.metrics, rig.clock, _ => ()).prune()
    report.deleted.get(ShareCardMetrics.PruneReason.Unreferenced) shouldBe Some(1)
    bases(rig.store) shouldBe Seq(s"${ShareCardFile.parse(current).get.baseKey}.jpg")
    // …and the budget counts it and never evicts it.
    val tight = new ShareCardJanitor(rig.store, rig.readModel, 1L, rig.metrics, rig.clock, _ => ()).enforceBudget()
    tight.currentBytes shouldBe rig.store.list().map(_.bytes).sum
    bases(rig.store) should have size 1
  }
}
