package integration

import org.mongodb.scala.MongoDatabase
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelProjector}
import tools.{Env, IntegrationCorpusDatabase}

/** The production storage split — `movies` + `screenings` + `movie_slots` — with the real
 *  read model and a projector over it, in a database of the suite's own (the projector reads
 *  and writes `web_*` whole, so a co-running spec's rows would be indistinguishable).
 *
 *  The projector is NOT subscribed: each spec attaches it to the repository's shared cursor
 *  itself (`repository.watchUpserts(projector.onMovieUpsert)`), next to whatever other
 *  listener it is asserting through. */
final class ProjectedMongoCorpus(db: MongoDatabase) {
  /** The corpus database's name — what an oplog count ([[tools.OplogWrites]]) is scoped to. */
  val databaseName: String = db.name
  val screenings = new MongoScreeningsRepository(Some(db))
  val slots      = new MongoSlotsRepository(Some(db))
  val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots),
    normalizer = titleNormalizer)
  val readModel  = new MongoReadModelRepository(Some(db))
  val projector  = new ReadModelProjector(repository, readModel, readModel)

  private def close(): Unit = { readModel.close(); repository.close(); slots.close(); screenings.close() }
}

object ProjectedMongoCorpus {
  /** Run `body` over a fresh corpus database named for `suite`, dropped afterwards. */
  def withCorpus[A](suite: String)(body: ProjectedMongoCorpus => A): A =
    IntegrationCorpusDatabase.withDatabase(Env.get("MONGODB_URI").get, suite) { db =>
      val corpus = new ProjectedMongoCorpus(db)
      try body(corpus) finally corpus.close()
    }
}
