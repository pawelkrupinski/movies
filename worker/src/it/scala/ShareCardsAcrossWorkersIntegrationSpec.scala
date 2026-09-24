package integration

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.sharecards.ShareCardTestKit._
import services.sharecards.{RenderShareCardHandler, ShareCardMetrics, ShareCardPosters, ShareCardService, ShareCardStore}
import services.tasks.MongoTaskQueue
import tools.ConcurrentInstances
import tools.ConcurrentInstances.{race, rounds, successes}

import java.nio.file.Files
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.util.Try

/**
 * Two workers rendering ONE film's share card into ONE directory, from one task queue: the
 * store's doc promises a reader (Caddy) only ever sees the old file or the new one, never a part,
 * and the service's that a render of inputs a newer request replaced never lands over the newer
 * card. Each worker is its own Mongo client and its own `ShareCardService`; both saw the film's
 * two versions asked in the same order, as both read the same read model.
 */
class ShareCardsAcrossWorkersIntegrationSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  /** A JPEG as a reader finds it: starting at its start-of-image and ending at its end-of-image. */
  private def whole(bytes: Array[Byte]): Boolean =
    bytes.length > 4 && (bytes(0) & 0xff) == 0xff && (bytes(1) & 0xff) == 0xd8 &&
      (bytes(bytes.length - 2) & 0xff) == 0xff && (bytes(bytes.length - 1) & 0xff) == 0xd9

  "two workers rendering one film's card at once" should "leave the latest version, and never let a reader see a torn file" in
    ConcurrentInstances.withInstances("share-cards-two-workers") { instances =>
      val store = tempStore()
      val services = instances.map { instance =>
        val posters = new ShareCardPosters(store, new CountingDownload(), javaShrinker, ShareCardMetrics.noop)
        val queue   = new MongoTaskQueue(Some(instance.database))
        (new ShareCardService(Country.default, store, posters, queue, ShareCardMetrics.noop, clockAt(T0)), queue)
      }
      rounds(4) { round =>
        val id     = f"frolling${round.number}%02d"
        val older  = film(id = id)
        val newer  = film(id = id, ratings = ratings.copy(imdb = Some(8.4)))
        services.foreach { case (service, _) =>
          service.request(service.inputs(older), askedAt = T0)
          service.request(service.inputs(newer), askedAt = T0.plusSeconds(1))
        }

        val card     = store.cardPath(id)
        val renders  = new CountDownLatch(services.size * 2)
        val samples  = new AtomicInteger()
        val torn     = new AtomicInteger()
        val sampler  = () => {
          while (renders.getCount > 0) Try(Files.readAllBytes(card)).foreach { bytes =>
            samples.incrementAndGet(); if (!whole(bytes)) torn.incrementAndGet()
          }
        }
        val drainers = for { ((service, queue), w) <- services.zipWithIndex; thread <- 1 to 2 } yield () => {
          try {
            val handler = new RenderShareCardHandler(service)
            Iterator.continually(queue.claim(s"worker-$w-$thread", 1.minute, T0.plusSeconds(365L * 86400)))
              .takeWhile(_.isDefined).flatten.foreach { task => handler.handle(task); queue.complete(task.id, s"worker-$w-$thread") }
            // A backfill sweep on each worker that read the film BEFORE its newer version, landing
            // after the queue drained: superseded, it must not put the older card back.
            service.renderIfLatest(service.inputs(older), Seq("backfill"))
            ()
          } finally renders.countDown()
        }
        successes(race(drainers :+ sampler, Some(round), joinTimeout = 2.minutes))

        withClue(s"$torn of $samples reads found a partial card: ") { torn.get shouldBe 0 }
        val (service, _) = services.head
        withClue("the card on disk must be the NEWER inputs' version: ") {
          store.version(card) shouldBe service.existing(service.inputs(newer))
          service.existing(service.inputs(newer)) shouldBe defined
        }
        store.list().filter(_.temp) shouldBe empty
        store.list().count(_.kind == ShareCardStore.Kind.Card) shouldBe round.number   // one card per film
      }
    }
}
