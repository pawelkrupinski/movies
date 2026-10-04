package services.readmodel

import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/**
 * The web read model over a REAL Mongo that goes away and comes back: the outage cut at the TCP
 * layer ([[tools.TcpForwarder]]) between the web's client and the local test Mongo, which itself
 * is never touched. What production must do through it:
 *
 *  - keep serving the last good corpus while every read fails — a failed reload evicts nothing;
 *  - notice its change streams ended (an outage outlasting the driver's one resume ends them);
 *  - once Mongo answers, catch up on what was written meanwhile and stream again — without a
 *    restart and without waiting on the 30-minute backstop.
 */
class WebReadModelMongoOutageIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def movie(id: String) = models.ResolvedMovie(_id = id, title = id, originalTitle = None, posterUrl = None,
    fallbackPosterUrls = Nil, runtimeMinutes = None, releaseYear = Some(2026), genres = Nil, countries = Nil,
    directors = Nil, cast = Nil, synopsis = None, trailerUrls = Nil,
    ratings = models.ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0)
  private def screening(id: String, film: String) =
    models.CityScreening(_id = id, filmId = film, city = "poznan", cinema = "Kino Muza", filmUrl = None, showtimes = Nil)

  "the web read model" should "serve its last good corpus through a Mongo outage, and catch up and stream again after it" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "web-readmodel-outage") { db =>
      val writer = new MongoReadModelRepository(Some(db))
      writer.upsertMovie(movie("before"))
      writer.upsertScreening(screening("before|poznan", "before"))

      val target    = URI.create(mongoTarget.uri.value.replace("mongodb://", "http://"))
      val forwarder = tools.TcpForwarder.start(target.getHost, target.getPort)
      val client    = MongoClient(s"mongodb://127.0.0.1:${forwarder.port}/?directConnection=true" +
        "&serverSelectionTimeoutMS=1000&connectTimeoutMS=500&socketTimeoutMS=3000")
      val reader = new MongoReadModelRepository(Some(client.getDatabase(db.name)), findAllBatchAttempts = 1,
        findAllBatchBackoff = scala.concurrent.duration.Duration.Zero)
      val model  = new WebReadModel(reader, clock = _root_.tools.SpecClock.Pinned)
      try {
        model.start()
        model.hydrated shouldBe true
        model.movie("before") shouldBe defined

        forwarder.sever()
        withClue("the change streams never noticed the outage: ")(tools.Eventually.poll(tools.SpecTimeouts.Settle.toMillis, 200)(!model.streamsLive) shouldBe true)
        writer.upsertMovie(movie("during"))          // lands while the web cannot see Mongo
        model.reload()                               // fails: must evict nothing
        model.movie("before") shouldBe defined
        model.screeningsForCity("poznan").map(_._id) shouldBe Seq("before|poznan")
        model.coldRetryTick()                        // a reopen into the outage: still serving, still not live
        model.movie("before") shouldBe defined

        forwarder.restore()
        withClue("the read model never caught up after Mongo came back: ")(tools.Eventually.poll(tools.SpecTimeouts.Settle.toMillis, 500) {
          model.coldRetryTick()
          model.movie("during").isDefined && model.streamsLive
        } shouldBe true)
        writer.upsertMovie(movie("after"))           // streamed: no tick, no reload
        withClue("the reopened stream did not deliver: ")(tools.Eventually.poll(tools.SpecTimeouts.Settle.toMillis, 100)(model.movie("after").isDefined) shouldBe true)
      } finally { model.stop(); client.close(); forwarder.close() }
    }
}
