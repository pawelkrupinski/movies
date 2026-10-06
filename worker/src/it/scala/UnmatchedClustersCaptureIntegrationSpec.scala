package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.movies.ListingKey
import services.identity.agreement.{FamilyAnswers, VoterFamily}
import tools._

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Records [[UnmatchedClusters]]' fixture: the clusters the resolver leaves with no film over a recorded full corpus, and
 * the model's decisions on them, and every answer the agreement stage read for them — TMDB's from
 * the recording (its gaps asked live with `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`), the families' from the
 * prod's filed answers (`KINOWO_IDENTITY_FAMILY_SEED`), else the signal-combination experiment's answer cache
 * (`KINOWO_IDENTITY_AGREEMENT_CACHE`), else live; and the venue and TMDB posters' hashes, of the images in
 * `KINOWO_IDENTITY_POSTER_CACHE` else downloaded live ([[CachedPosters]]). A re-capture after a
 * change that asks new questions answers them the same way.
 *
 * Opt-in: runs when `KINOWO_IDENTITY_UNMATCHED_CAPTURE` names the directory to write `<cc>.json.gz` to
 * (`test/resources/fixtures/identity-unmatched` re-captures the checked-in fixture), beside the resolver-only replay's
 * variables (`KINOWO_IDENTITY_FULL`, `KINOWO_IDENTITY_CORPUS_DIR`, `KINOWO_FIXTURE_ROOT`, `KINOWO_IDENTITY_AGREEMENT_CACHE`).
 */
class UnmatchedClustersCaptureIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]
  private val corpora: Seq[Corpus] = for {
    _      <- configuration.identityUnmatchedCapture.toSeq
    _      <- configuration.identityAgreementCache.toSeq
    dir    <- configuration.identityCorpusDirectory.toSeq
    corpus <- IdentityShadow.full(configuration.identityFullCorpora.value, dir.value, configuration.fixtureRoot, configuration.identityLiveGaps)
  } yield corpus

  corpora.foreach { c =>
    "The unmatched-cluster capture" should s"record ${c.label}'s unmatched clusters and every answer read for them" in {
      val w        = wiring(mongoTarget, c, storages, configuration.fixtureRoot, configuration.env)
      val listings = listingsOf(w, c.normalizer)
      val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps))
      val whole     = IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver)
      val decisions = whole.decisions.filter(d => d.film.isEmpty && d.fallback.isEmpty && d.unanswered == 0)
      val unmatched = decisions.flatMap(_.members).toSet
      val subset    = listings.filter(l => unmatched(l.key))

      val recording = new UnmatchedClusters.Recording(lookups)
      val cache     = new ExperimentCacheFetch(configuration.identityAgreementCache.get.value)
      val docs      = new InMemoryTmdbDocuments
      // prod's own family answers first (`KINOWO_IDENTITY_FAMILY_SEED`: `<db>.jsonl` exports of identity_family_answers), the rest filled
      configuration.identityFamilySeed.foreach { seed =>
        val file = seed.value.resolve(s"${if (c.country.code == "pl") "kinowo" else s"kinowo_${c.country.code}"}.jsonl")
        if (java.nio.file.Files.exists(file)) docs.put(TmdbKind.Family, java.nio.file.Files.readAllLines(file).asScala.toSeq.filter(_.nonEmpty).map { line =>
          val d = org.bson.BsonDocument.parse(line)
          d.remove("_id").asString.getValue -> d
        })
      }
      val store     = new FamilyAnswerStore(docs, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
      val families  = UnmatchedClusters.familiesOf(c.country)
      val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(cache)),
        new WikiFamily(new services.enrichment.WikidataClient(cache), c.country.language.getLanguage),
        new FilmwebFamily(new services.enrichment.FilmwebClient(cache), services.cinemas.pl.FilmwebProgrammes.resolving(cache, () => new models.VenueClock(java.time.Clock.systemUTC()).todayInPoland).of),
        new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(cache)),
        new MetacriticFamily(new services.enrichment.MetacriticClient(cache))).filter(s => families.contains(s.family)).map(s => s.family -> s).toMap
      val tmdb  = new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language, retrySleep = (_: Long) => ())
      val finds = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]]()
      val tmdbOf: String => Answer[Option[Int]] = imdb => Try(tmdb.findByImdbId(imdb).map(_.id)).toOption
        .fold[Answer[Option[Int]]](Answer.Unknown) { found => finds.put(imdb, found); Answer.Known(found) }
      val answers: Map[VoterFamily, FamilyAnswers] = families.map(f => f -> store.answers(f)).toMap
      val posterStore = new PosterAnswerStore(store, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
      val posters     = CachedPosters.of(configuration, c.country)
      val catalogue   = new CatalogueFill(store, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")), cache)

      // the records of the films the model leaned to and weighed best: what a take names, and what pooled facts read
      (decisions.flatMap(_.leaning.map(_.film)) ++ decisions.flatMap(_.trace.nodes.values.flatMap(_.candidate))).distinct.foreach(recording.film)
      var outcome = UnmatchedClusters.agree(decisions, subset, recording, answers, store.version, tmdbOf, c.normalizer, posterStore,
        modules.wiring.IdentityCutoverWiring.identities(c.country.code), catalogue.answers,
          modules.wiring.IdentityCutoverWiring.listedOn(c.country.code))
      var rounds  = 0
      while ((outcome.stage.wanted.nonEmpty || outcome.stage.wantedPosters.nonEmpty || outcome.stage.wantedCatalogue.nonEmpty) && rounds < 12) {
        rounds += 1
        val open = outcome.stage.wanted.toSeq
        val unhashed = outcome.stage.wantedPosters.toSeq
        println(s"[${c.label}] capture round $rounds: ${open.size} open family question(s), ${unhashed.size} poster(s)")
        posters.file(posterStore, unhashed)
        catalogue.file(outcome.stage.wantedCatalogue)
        open.groupBy(_._1).toSeq.map { case (family, asks) =>
          java.util.concurrent.CompletableFuture.runAsync { () =>
            asks.map(_._2).grouped(4).foreach(_.map(question => java.util.concurrent.CompletableFuture.runAsync(() =>
              { Try(AgreementQuestions.file(store, family, sources(family), question)); () })).foreach(_.join()))
          }
        }.foreach(_.join())
        outcome = UnmatchedClusters.agree(decisions, subset, recording, answers, store.version, tmdbOf, c.normalizer, posterStore,
        modules.wiring.IdentityCutoverWiring.identities(c.country.code), catalogue.answers,
          modules.wiring.IdentityCutoverWiring.listedOn(c.country.code))
      }
      val filed = docs.get(TmdbKind.Family, docs.fetchedBefore(TmdbKind.Family, Long.MaxValue).map(_._1))
      val capture = UnmatchedClusters.Capture(c.country, subset, decisions, recording.queries.asScala.toMap, recording.films.asScala.toMap,
        recording.details.asScala.toMap, recording.withDetail.asScala.toSet, filed, finds.asScala.toMap, recording.casts.asScala.toMap)
      val written = configuration.identityUnmatchedCapture.get.value.resolve(s"${c.country.code}.json.gz")
      UnmatchedClusters.write(written, capture)

      // the capture must replay to exactly what was captured
      val replayed = UnmatchedClusters.replay(UnmatchedClusters.read(written))
      replayed.stage.wanted shouldBe empty
      replayed.stage.wantedPosters shouldBe empty
      replayed.stage.wantedCatalogue shouldBe empty
      UnmatchedClusters.takes(capture, replayed) shouldBe UnmatchedClusters.takes(capture, outcome)
      println(s"[${c.label}] captured ${subset.size} listings in ${decisions.size} clusters; ${capture.queries.size} queries, ${capture.films.size} films, " +
        s"${filed.size} family answers after $rounds round(s); takes ${UnmatchedClusters.takes(capture, outcome).size}")
      // the captured listings' venue pages the recorded tree lacks — the page, and every gap met that ends in its slug (a
      // chain reading its show's detail off its API: Alamo Drafthouse's `…/presentation/<slug>`) — read live and kept, for
      // the next run to answer
      val unread = subset.filter(l => capture.withDetail(ListingKey.serialised(l.key)) && !capture.details.contains(ListingKey.serialised(l.key))).flatMap(_.page)
      val slugs  = unread.map(_.trim.stripSuffix("/").split('/').last).filter(_.length >= 4).toSet
      val gaps   = c.missedKeys().collect { case key if key.startsWith("GET ") => key.drop(4) }
        .filter(url => slugs.exists(slug => url.stripSuffix("/").endsWith(s"/$slug")))
      if (unread.nonEmpty && configuration.identityLiveGaps.isDefined)
        println(s"[${c.label}] ${unread.size} listing(s) whose venue page the tree lacks: read ${IdentityShadow.LiveGapLeaf.readPages(unread ++ gaps)} " +
          "page(s) live — capture again to read them")
    }
  }

  override protected def afterAll(): Unit = {
    storages.foreach(s => Try(s.close()))
    super.afterAll()
  }
}
