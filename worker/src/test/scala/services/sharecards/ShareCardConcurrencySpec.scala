package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.InMemoryReadModelRepository
import ShareCardTestKit.*

import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*

/** Two replicas' renderers and janitors over ONE directory, interleaved. */
class ShareCardConcurrencySpec extends AnyFlatSpec with Matchers {

  "Two replicas" should "render and prune one directory at once without losing a card or leaving a partial file" in {
    val store = tempStore()
    val first = new Rig(store = store)
    val second = new Rig(store = store)
    val readModel = new InMemoryReadModelRepository
    val janitors = Seq(first, second).map(rig => new ShareCardJanitor(store, readModel, budgetBytes = 1, rig.metrics, rig.clock, _ => ()))
    val films = (1 to 12).map(i => film(id = f"frace$i%02d", poster = s"https://cdn.example/p${i % 3}.jpg"))

    val pool  = Executors.newFixedThreadPool(4)
    val start = new CountDownLatch(1)
    val renders = Seq(first, second).map { rig =>
      pool.submit[Unit](() => { start.await(); films.foreach(m => rig.service.render(rig.service.inputs(m), Seq(ShareCardReason.NewFilm))) })
    }
    val prunes = janitors.map { janitor =>
      pool.submit[Unit](() => { start.await(); (1 to 30).foreach(_ => { janitor.prune(); janitor.enforceBudget() }) })
    }
    start.countDown()
    (renders ++ prunes).foreach(_.get(2, TimeUnit.MINUTES))
    pool.shutdown()

    // Every card either replica wrote is there — young and unrecorded, so neither janitor may touch
    // it, even at a one-byte budget — and every one is a whole JPEG.
    films.foreach { m =>
      val name = first.service.existing(first.service.inputs(m))
      name shouldBe defined
      ImageIO.read(store.cardPath(name.get).toFile).getWidth shouldBe 1200
    }
    store.list().filter(_.temp) shouldBe empty
    // Each distinct poster was cached, whole, however the two replicas raced on it.
    store.list().count(_.kind == ShareCardStore.Kind.Poster) shouldBe 3

    // Once past the grace period, the budget holds: nothing is referenced, so it all may go.
    Files.list(store.root).iterator.asScala.toSeq.filter(Files.isRegularFile(_))
      .foreach(Files.setLastModifiedTime(_, FileTime.from(T0.minusSeconds(7200))))
    Files.list(store.root.resolve(ShareCardStore.PosterDir)).iterator.asScala.toSeq
      .foreach(Files.setLastModifiedTime(_, FileTime.from(T0.minusSeconds(7200))))
    janitors.head.enforceBudget().bytes should be <= 1L
  }
}
