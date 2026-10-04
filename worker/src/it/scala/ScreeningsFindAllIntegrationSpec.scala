package integration

import tools.SpecTimeouts

import org.mongodb.scala.{Document, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoScreeningsRepository, ScreeningsRepository}

import scala.concurrent.Await

/** `ScreeningsRepository.findAll` against real MongoDB: a scan that cannot read the whole collection
 *  throws. It answered an empty map — "no film has a screening" — for a collection it could not read.
 *  A row the codec refuses stops the scan on its page, as a page still failing after its retries does. */
class ScreeningsFindAllIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "MongoScreeningsRepository.findAll" should "throw, not answer no screenings, when its scan stops short" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "screenings-find-all") { db =>
      Await.result(db.getCollection[Document](ScreeningsRepository.Collection)
        .insertOne(Document("_id" -> 42, "filmId" -> 7)).toFuture(), SpecTimeouts.Io)
      val repository = new MongoScreeningsRepository(Some(db), findAllBatchAttempts = 1)
      an[Exception] should be thrownBy repository.findAll()
    }
  }
}
