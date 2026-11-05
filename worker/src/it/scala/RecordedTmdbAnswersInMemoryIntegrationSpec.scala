package integration

import org.bson.BsonDocument
import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{InMemoryTmdbDocuments, MongoTmdbDocuments, TmdbDocuments, TmdbKind, TmdbNormalizer, TmdbStore}
import tools.{IntegrationMongoSuite, IsolatedMongoDatabase, SliceHttp, SpecTimeouts}

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Await
import scala.jdk.CollectionConverters._
import scala.util.Success

/**
 * A convergence replay keeps its TMDB and IMDb answers in memory (`ArchiveReplayWiring.identityTmdbLayer`), where it
 * kept them in the run's Mongo. Captured responses, replayed: every recorded TMDB / IMDb answer of real identity slices
 * (one per country) normalized through `TmdbNormalizer` into a store over Mongo and into one in memory, and every
 * document each holds read back -- whole, as an answer (projected), and as a film's hit -- must come out the same.
 */
class RecordedTmdbAnswersInMemoryIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite with BeforeAndAfterAll {
  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "tmdbmem")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val Slices = Paths.get("test/resources/fixtures/identity-slices")
  private val clock  = java.time.Clock.fixed(java.time.Instant.parse("2026-11-01T00:00:00Z"), java.time.ZoneOffset.UTC)

  /** The first slice of each country, by name: recorded answers of four languages' TMDB and IMDb questions. */
  private lazy val slices: Seq[Path] =
    Files.list(Slices).iterator.asScala.filter(d => Files.exists(d.resolve("http.jsonl.gz"))).toSeq.sortBy(_.getFileName.toString)
      .groupBy(_.getFileName.toString.takeWhile(_ != '-')).values.map(_.head).toSeq.sortBy(_.getFileName.toString)

  /** Every recorded answer of `slice` filed into `docs` as the replay files it: through the normalizer, in a fixed order. */
  private def file(slice: Path, docs: TmdbDocuments): Unit = {
    val normalizer = new TmdbNormalizer(new TmdbStore(docs, clock))
    SliceHttp.read(slice.resolve("http.jsonl.gz")).toSeq.sortBy(_._1.render).foreach {
      case (SliceHttp.Request("GET", url, None), SliceHttp.Outcome.Body(body))        => normalizer.filed("GET", url, Success(body))
      case (SliceHttp.Request("POST", url, Some(sent)), SliceHttp.Outcome.Body(body)) => normalizer.filedPost(url, sent, Success(body))
      case _                                                                           => ()
    }
  }

  private def idsInMongo(kind: TmdbKind): Seq[String] =
    Await.result(isolated.database.getCollection[BsonDocument](kind.collection).find().toFuture(), SpecTimeouts.Io)
      .map(_.getString("_id").getValue).sorted

  "a replay's recorded answers" should "read back the same from memory as from Mongo, whole, as answers and as hits" in {
    slices.map(_.getFileName.toString.takeWhile(_ != '-')).distinct.size should be >= 3
    slices.foreach { slice =>
      withClue(s"${slice.getFileName}: ") {
        TmdbKind.values.foreach(k => Await.result(isolated.database.getCollection(k.collection).drop().toFuture(), SpecTimeouts.Io))
        val mongo  = new MongoTmdbDocuments(isolated.database)
        val memory = new InMemoryTmdbDocuments
        file(slice, mongo)
        file(slice, memory)
        val filed = TmdbKind.values.toSeq.map(kind => kind -> idsInMongo(kind))
        filed.map(_._2.size).sum should be > 0
        filed.foreach { case (kind, ids) =>
          withClue(s"$kind: ") {
            memory.size(kind) shouldBe ids.size
            memory.get(kind, ids) shouldBe mongo.get(kind, ids)
            memory.answers(kind, ids) shouldBe mongo.answers(kind, ids)
            memory.answersReadThrough(kind, ids) shouldBe mongo.answersReadThrough(kind, ids)
            if (kind == TmdbKind.Film) memory.filmHits(ids) shouldBe mongo.filmHits(ids)
          }
        }
      }
    }
  }

  "a convergence replay over a real database" should "keep its TMDB answers in memory, none in the run's Mongo" in {
    val country = models.Country.Germany
    val storage = tools.ConvergenceStorage.mongo(mongoTarget, "tmdbmem-wiring", services.movies.TitleNormalizer.forCountry(country),
      services.movies.MovieChangeStream.Debounce.forCountry(country))
    try {
      val wiring = new tools.ArchiveReplayWiring(country, storage.archive, None, storage, s"enrichment-${country.code}",
        settings.FixtureRoot.RepositoryRelative)
      wiring.identityTmdbDocuments.put(TmdbKind.Film, Seq("1018" -> new BsonDocument("record", new BsonDocument("title", new org.bson.BsonString("Mulholland Drive")))))
      wiring.identityTmdbDocuments.get(TmdbKind.Film, Seq("1018")).keySet shouldBe Set("1018")
      val database = storage.connection.database.getOrElse(fail("the storage holds no database"))
      Await.result(database.getCollection(TmdbKind.Film.collection).countDocuments().toFuture(), SpecTimeouts.Io) shouldBe 0L
    } finally storage.close()
  }
}
