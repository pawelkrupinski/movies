package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsObject, JsString, Json}
import services.identity._
import services.movies.ListingKey
import tools._

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Try

/**
 * The resolver alone over a recorded full corpus, every listing's decision written out — the fast loop for
 * a resolver change: seconds per country, no pipeline boot, against the same recording the CI measure
 * replays. `decisions-<cc>.jsonl` holds one line per listing (its serialised key, venue, raw title, the
 * film its cluster takes and the decision's explanation); with `KINOWO_IDENTITY_FOCUS` the focused titles'
 * explanations and candidates are written beside it, as the CI measure's focus mode prints them.
 *
 * Opt-in: runs when `KINOWO_IDENTITY_DUMP` (the output directory), `KINOWO_IDENTITY_FULL`,
 * `KINOWO_IDENTITY_CORPUS_DIR` and `KINOWO_FIXTURE_ROOT` are set. With `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`
 * what the recording cannot answer is asked of TMDB and IMDb live — the loop for a change asking new questions. Nothing is judged here — the CI measure
 * judges; this says what moved.
 */
class IdentityResolveDumpIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]
  private val out      = configuration.identityDump
  private val corpora: Seq[Corpus] = for {
    _      <- out.toSeq
    dir    <- configuration.identityCorpusDirectory.toSeq
    corpus <- IdentityShadow.full(configuration.identityFullCorpora.value, dir.value, configuration.fixtureRoot, configuration.identityLiveGaps)
  } yield corpus

  corpora.foreach { c =>
    "The resolver" should s"write every listing's decision on ${c.label}" in {
      val dir      = out.get.value
      val w        = wiring(mongoTarget, c, storages, configuration.fixtureRoot, configuration.env)
      val listings = listingsOf(w, c.normalizer)
      val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers,
        c.gaps))
      val (resolution, seconds) = timed(IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver))
      val clusterOf = resolution.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
      Files.createDirectories(dir)
      val lines = listings.map { l =>
        val d = resolution.decisionOf(l.key)
        Json.stringify(JsObject(Seq(
          "key"         -> JsString(ListingKey.serialised(l.key)),
          "venue"       -> JsString(l.key.venue),
          "rawTitle"    -> JsString(l.key.rawTitle),
          "cluster"     -> JsNumber(clusterOf.getOrElse(l.key, -1)),
          "film"        -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
          "filmTitle"   -> d.film.flatMap(resolution.films.get).fold[play.api.libs.json.JsValue](JsNull)(f => JsString(s"${f.title} (${f.year.getOrElse("?")})")),
          "confidence"  -> JsNumber(BigDecimal(d.confidence).setScale(4, BigDecimal.RoundingMode.HALF_UP)),
          "basis"       -> JsString(d.basis.toString),
          "explanation" -> JsArray(d.explanation.take(4).map(JsString(_))),
          "rules"       -> JsArray((d.trace.rulesOf(l.key) ++ c.normalizer.firedRules(l.cinema, l.rawTitle)).map(JsString(_))),
          // why not: what stopped a listing left with no film, what it searched, what it weighed
          "blocker"     -> (if (d.film.isDefined) JsNull else JsString(d.trace.nodes.get(l.key).flatMap(_.blocker).getOrElse("pooled:no-film"))),
          "searched"    -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.searched).map(JsString(_))),
          "candidates"  -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.candidates).map(JsString(_))),
          "refusals"    -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.refusals).map(r =>
            JsString(s"${r.rule}: ${r.why}${r.film.fold("")(id => s" [tmdb $id]")}${if (r.detail.nonEmpty) s" — ${r.detail}" else ""}"))))))
      }
      Files.writeString(dir.resolve(s"decisions-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
      configuration.identityFocus.foreach { f =>
        val tokens  = (l: Listing) => services.movies.TitleContainment.tokens(l.rawTitle).toSet ++ services.movies.TitleContainment.tokens(l.cleanTitle).toSet
        val focused = listings.filter(l => f.covers(tokens(l))).map(_.key).toSet
        val trace   = mutable.ListBuffer.empty[String]
        IdentityResolver.explain(listings, lookups, c.normalizer, IdentityCalibration.resolver)(focused).foreach(_.render.foreach(trace += _))
        IdentityResolver.candidatesOf(listings, lookups, c.normalizer, IdentityCalibration.resolver)(l => focused(l.key)).foreach { node =>
          trace += node.label
          node.banners.foreach(b => trace += s"  $b")
          node.candidates.take(8).foreach(cand => trace += s"  ${cand.render}")
        }
        Files.writeString(dir.resolve(s"focus-${c.country.code}.txt"), trace.mkString("", "\n", "\n"))
      }
      configuration.identityAgreementCache.foreach(cache => agreed(c, w, lookups, listings, resolution, cache, dir))
      println(f"[${c.label}] ${listings.size} listings → ${resolution.decisions.size} clusters " +
        f"(${resolution.decisions.count(_.film.isDefined)} with a film) in $seconds%.1fs; unanswered queries ${resolution.unknownQueries}")
    }
  }

  /** The agreement stage over the resolution's no-matches, the families answered from the experiment's cache, else live —
   *  asked until nothing is open — and every cluster it takes written to `agreed-<cc>.jsonl`. */
  private def agreed(c: Corpus, w: ArchiveReplayWiring, lookups: IdentityLookups, listings: Seq[Listing], resolution: Resolution,
                     cache: settings.IdentityAgreementCache, dir: java.nio.file.Path): Unit = {
    import services.identity.agreement.{AgreementStage, VoterFamily}
    val fetch    = new ExperimentCacheFetch(cache.value)
    val store    = new FamilyAnswerStore(new InMemoryTmdbDocuments, new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val families = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(Set("pl", "de", "es")(c.country.code))(VoterFamily.Filmweb)
    val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(fetch)),
      new WikiFamily(new services.enrichment.WikidataClient(fetch), c.country.language.getLanguage),
      new FilmwebFamily(new services.enrichment.FilmwebClient(fetch)), new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(fetch)),
      new MetacriticFamily(new services.enrichment.MetacriticClient(fetch))).filter(source => families.contains(source.family)).map(s => s.family -> s).toMap
    val tmdb  = new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language, retrySleep = (_: Long) => ())
    // the venue and TMDB posters' hashes, of the images KINOWO_IDENTITY_POSTER_CACHE keeps, else downloaded
    val posterStore = new PosterAnswerStore(store, new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val posters     = CachedPosters.of(configuration, c.country)
    val stage = new AgreementStage(families.map(family => family -> store.answers(family)).toMap, lookups, c.normalizer,
      IdentityCalibration.resolver, tmdbOf = imdb => Answer.Known(Try(tmdb.findByImdbId(imdb).map(_.id)).toOption.flatten),
      stored = new services.identity.agreement.InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, posters = posterStore, tmdb = Some(lookups))
    val byKey = listings.map(l => l.key -> l).toMap
    var rounds = 0
    var taken  = stage.apply(resolution, byKey.get, store.version)
    while ((stage.wanted.nonEmpty || stage.wantedPosters.nonEmpty) && rounds < 12) {
      rounds += 1
      val open = stage.wanted.toSeq
      println(s"[${c.label}] agreement round $rounds: ${open.size} open question(s), ${stage.wantedPosters.size} poster(s)")
      posters.file(posterStore, stage.wantedPosters.toSeq)
      // each family's questions four at a time, as the experiment read the sites unblocked
      open.groupBy(_._1).toSeq.map { case (family, asks) =>
        java.util.concurrent.CompletableFuture.runAsync { () =>
          asks.map(_._2).grouped(4).foreach(_.map(question => java.util.concurrent.CompletableFuture.runAsync(() =>
            { Try(AgreementQuestions.file(store, family, sources(family), question)); () })).foreach(_.join()))
        }
      }.foreach(_.join())
      taken = stage.apply(resolution, byKey.get, store.version)
    }
    val lines = taken.decisions.filter(d => d.basis == ResolverDecision.Basis.Agreed || d.basis == ResolverDecision.Basis.Poster).flatMap { d =>
      d.members.flatMap(byKey.get).map(l => Json.stringify(JsObject(Seq(
        "venue" -> JsString(l.key.venue), "rawTitle" -> JsString(l.key.rawTitle), "basis" -> JsString(d.basis.toString),
        "film" -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
        "fallback" -> d.fallback.fold[play.api.libs.json.JsValue](JsNull)(f => JsString(f.id)),
        "agreement" -> JsString(d.explanation.lastOption.getOrElse(""))))))
    }
    Files.writeString(dir.resolve(s"agreed-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
    println(s"[${c.label}] agreement: ${taken.decisions.count(_.basis == ResolverDecision.Basis.Agreed)} cluster(s) agreed, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Poster)} taken by their poster after $rounds round(s); " +
      s"${stage.wanted.size} question(s), ${stage.wantedPosters.size} poster(s) still open")
  }

  override protected def afterAll(): Unit = {
    storages.foreach(s => Try(s.close()))
    super.afterAll()
  }
}
