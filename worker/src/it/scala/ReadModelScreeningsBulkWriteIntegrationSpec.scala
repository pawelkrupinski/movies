package integration

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent}
import models.CityScreening
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelRepository

import scala.concurrent.Await
import scala.jdk.CollectionConverters._

/** A card's screenings go to Mongo as one write: a wide US release has one document per venue, and
 *  an awaited `replaceOne` each was most of a country's first projection. */
class ReadModelScreeningsBulkWriteIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "upsertScreenings" should "write a card's screenings in one round trip, each replacing its own document" in {
    val commands = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit = { commands.add(event.getCommandName); () }
      }).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "readmodel-bulk-screenings"))
    val rm = new MongoReadModelRepository(Some(db))
    def screening(i: Int, cinema: String) =
      CityScreening(_id = s"film|venue-$i", filmId = "film", city = "poznan", cinema = cinema, filmUrl = None, showtimes = Nil)
    try {
      rm.upsertScreenings((1 to 3).map(screening(_, "Before")))
      commands.clear()
      rm.upsertScreenings((1 to 40).map(screening(_, "After")))
      commands.asScala.count(_ == "update") shouldBe 1
      rm.findAllScreenings().map(s => s._id -> s.cinema).toMap shouldBe (1 to 40).map(i => s"film|venue-$i" -> "After").toMap
    } finally { rm.close(); Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }
}
