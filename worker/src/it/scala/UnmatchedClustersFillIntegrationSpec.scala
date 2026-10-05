package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.identity.agreement.VoterFamily
import tools._

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Fills what [[UnmatchedClusters]]' checked-in fixture lacks after a change that asks the agreement stage new questions
 * of the SAME clusters — a family's question, an agreed IMDb id's TMDB find, a poster's hash — answered as the capture
 * answers them (the experiment's cache, else live; posters as [[CachedPosters]] keeps them), without resolving the whole
 * corpus again. The model's decisions and TMDB's answers stay as captured: a change to those is a re-capture
 * (`UnmatchedClustersCaptureIntegrationSpec`).
 *
 * Opt-in: runs when `KINOWO_IDENTITY_UNMATCHED_FILL` names the fixture directory, with `KINOWO_IDENTITY_AGREEMENT_CACHE`
 * and `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` (the finds) set.
 */
class UnmatchedClustersFillIntegrationSpec extends AnyFlatSpec with Matchers {

  private val configuration = settings.ProcessConfiguration.resolve()
  private val fixtures = for {
    dir     <- configuration.identityUnmatchedFill.toSeq
    _       <- configuration.identityAgreementCache.toSeq
    country <- models.Country.all
    path     = dir.value.resolve(s"${country.code}.json.gz")
    if java.nio.file.Files.exists(path)
  } yield country -> path

  fixtures.foreach { case (country, path) =>
    "The unmatched-cluster fill" should s"answer every question ${country.code}'s fixture lacks" in {
      val capture  = UnmatchedClusters.read(path)
      val clock    = new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z"))
      val docs     = new InMemoryTmdbDocuments
      docs.put(TmdbKind.Family, capture.families.toSeq)
      val store    = new FamilyAnswerStore(docs, clock)
      val cache    = new ExperimentCacheFetch(configuration.identityAgreementCache.get.value)
      val families = UnmatchedClusters.familiesOf(country)
      val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(cache)),
        new WikiFamily(new services.enrichment.WikidataClient(cache), country.language.getLanguage),
        new FilmwebFamily(new services.enrichment.FilmwebClient(cache)), new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(cache)),
        new MetacriticFamily(new services.enrichment.MetacriticClient(cache))).filter(s => families.contains(s.family)).map(s => s.family -> s).toMap
      val tmdb  = configuration.identityLiveGaps.map(key => new clients.TmdbClient(new RealHttpFetch(), apiKey = Some(settings.TmdbApiKey(key.tmdbKey)),
        language = country.language, retrySleep = (_: Long) => ()))
      val finds = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]](capture.finds.asJava)
      val tmdbOf: String => Answer[Option[Int]] = imdb => Option(finds.get(imdb)).map(Answer.Known(_)).getOrElse {
        tmdb.flatMap(client => Try(client.findByImdbId(imdb).map(_.id)).toOption)
          .fold[Answer[Option[Int]]](Answer.Unknown) { found => finds.put(imdb, found); Answer.Known(found) }
      }
      val posterStore = new PosterAnswerStore(store, clock)
      val posters     = CachedPosters.of(configuration, country)
      val lookups     = new UnmatchedClusters.Replay(capture)
      def agree() = UnmatchedClusters.agree(capture.decisions, capture.listings, lookups, families.map(f => f -> store.answers(f)).toMap,
        store.version, tmdbOf, services.movies.TitleNormalizer.forCountry(country), posterStore,
        modules.wiring.IdentityCutoverWiring.identities(country.code))
      var outcome = agree()
      var rounds  = 0
      while ((outcome.stage.wanted.nonEmpty || outcome.stage.wantedPosters.nonEmpty) && rounds < 12) {
        rounds += 1
        println(s"[${country.code}] fill round $rounds: ${outcome.stage.wanted.size} family question(s), ${outcome.stage.wantedPosters.size} poster(s)")
        posters.file(posterStore, outcome.stage.wantedPosters.toSeq)
        outcome.stage.wanted.toSeq.groupBy(_._1).toSeq.map { case (family, asks) =>
          java.util.concurrent.CompletableFuture.runAsync { () =>
            asks.map(_._2).grouped(4).foreach(_.map(question => java.util.concurrent.CompletableFuture.runAsync(() =>
              { Try(AgreementQuestions.file(store, family, sources(family), question)); () })).foreach(_.join()))
          }
        }.foreach(_.join())
        outcome = agree()
      }
      outcome.model.unknownQueries shouldBe 0
      val filed = docs.get(TmdbKind.Family, docs.fetchedBefore(TmdbKind.Family, Long.MaxValue).map(_._1))
      UnmatchedClusters.write(path, capture.copy(families = filed, finds = finds.asScala.toMap))
      val replayed = UnmatchedClusters.replay(UnmatchedClusters.read(path))
      replayed.stage.wanted shouldBe empty
      replayed.stage.wantedFinds shouldBe empty
      replayed.stage.wantedPosters shouldBe empty
      println(s"[${country.code}] filled after $rounds round(s): ${filed.size} family answers, ${finds.size} finds")
    }
  }
}
