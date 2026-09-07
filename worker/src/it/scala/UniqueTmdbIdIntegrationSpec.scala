package services.movies

import models.{Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.mongodb.scala.{Document, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/** One document per film, enforced by the store. The write-time fold merges a
 *  same-tmdbId duplicate before it is written; the unique sparse `tmdbId` index is what
 *  refuses the one a race lets through, so the settle never has a pair to merge. */
class UniqueTmdbIdIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri = Env.get("MONGODB_URI").get

  private def row(title: String): MovieRecord =
    MovieRecord(tmdbId = Some(4242), data = Map[Source, SourceData](
      Tmdb      -> SourceData(title = Some(title), releaseYear = Some(2026)),
      Multikino -> SourceData(title = Some(title), releaseYear = Some(2026))))

  "the movies collection" should "refuse a second document claiming a tmdbId another one holds, and leave unresolved rows alone" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "unique-tmdbid") { db =>
      val repository = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
      try {
        repository.enabled shouldBe true
        val indexes = Await.result(db.getCollection[Document]("movies").listIndexes().toFuture(), 10.seconds)
        withClue(s"indexes: ${indexes.map(_.toJson())}\n") {
          indexes.exists(i => i.get("key").exists(_.asDocument().containsKey("tmdbId")) &&
                              i.get("unique").exists(_.asBoolean().getValue) &&
                              i.contains("partialFilterExpression")) shouldBe true
        }

        repository.upsert("__unique-probe-a__", Some(2026), row("__unique-probe-a__"))
        repository.upsert("__unique-probe-b__", Some(2026), row("__unique-probe-b__"))   // same film, second document

        val stored = repository.findAll().filter(_.record.tmdbId.contains(4242))
        withClue(s"stored: ${stored.map(r => (r.id, r.title))}\n")(stored.map(_.title) shouldBe Seq("__unique-probe-a__"))

        // Unresolved rows carry no tmdbId and are not constrained (sparse).
        repository.upsert("__unique-probe-c__", None, MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("__unique-probe-c__")))))
        repository.upsert("__unique-probe-d__", None, MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("__unique-probe-d__")))))
        repository.findAll().count(_.record.tmdbId.isEmpty) shouldBe 2
      } finally repository.close()
    }

  // The stored key is the lookup identity (see `FilmId`): one document per key, the
  // cache refuses a second at write time and the store refuses the one a race lets by.
  it should "refuse a second document under a key another film already holds" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "unique-key") { db =>
      val repository = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
      try {
        repository.enabled shouldBe true
        val indexes = Await.result(db.getCollection[Document]("movies").listIndexes().toFuture(), 10.seconds)
        withClue(s"indexes: ${indexes.map(_.toJson())}\n") {
          indexes.exists(i => i.get("key").exists(_.asDocument().containsKey("key")) &&
                              i.get("unique").exists(_.asBoolean().getValue)) shouldBe true
        }
        val key = CacheKey("__unique-key-probe__", Some(2026), titleNormalizer)
        repository.upsert(FilmId("f-first"),  key, MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Multikino -> SourceData(title = Some("__unique-key-probe__")))))
        repository.upsert(FilmId("f-second"), key, MovieRecord(tmdbId = Some(2), data = Map[Source, SourceData](Multikino -> SourceData(title = Some("__unique-key-probe__")))))

        val stored = repository.findAll()
        withClue(s"stored: ${stored.map(r => (r.id, r.storedKey))}\n")(stored.map(_.id) shouldBe Seq(FilmId("f-first")))
      } finally repository.close()
    }
}
