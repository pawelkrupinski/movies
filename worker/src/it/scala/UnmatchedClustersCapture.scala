package integration

import services.identity._
import services.identity.adapters.{FamilyAnswerStore, InMemoryTmdbDocuments, PosterAnswerStore, TmdbIdentityLookups, TmdbKind}
import services.identity.agreement.{FamilyAnswers, VoterFamily}
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Records [[UnmatchedClusters]]' fixture for one country: the clusters the resolver leaves with no film over its recorded
 * full corpus, and the model's decisions on them, and every answer the agreement stage read for them — TMDB's from the
 * recording (its gaps asked live with `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`), the families' from prod's filed answers
 * (`--family-seed`: `<db>.jsonl` exports of `identity_family_answers`), else the signal-combination experiment's answer
 * cache (`--agreement-cache`), else live; and the venue and TMDB posters' hashes, of the images `--poster-cache` keeps
 * else downloaded live ([[CachedPosters]]). A re-capture after a change that asks new questions answers them the same way.
 * `scripts.IdentityCapture` (`scripts/identity-capture.sh`) runs it, one JVM a country:
 *
 * {{{
 *   java -cp <it classpath> integration.UnmatchedClustersCapture uk --out test/resources/fixtures/identity-unmatched \
 *     --corpus-dir <dir> --agreement-cache <dir> [--fixtures <dir>] [--family-seed <dir>] [--poster-cache <dir>]
 * }}}
 *
 * `MONGODB_URI` (and `MONGODB_DB`) name the throwaway Mongo the corpus is seeded into. A capture that read venue pages the
 * recorded tree lacks says "capture again" — the next run answers them from the pages it kept.
 *
 * Exit: 0 written, and replayed to exactly what was captured; 1 not — no corpus, a recording whose venue-detail reads
 * were refused (nothing is written then: the checked-in capture stands), a capture that does not replay; 2 usage.
 */
object UnmatchedClustersCapture {

  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("UnmatchedClustersCapture",
    "Captures one country's unmatched clusters and every answer the agreement stage reads for them.",
    CommandLine.Value("out", "dir", "where <cc>.json.gz is written (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("agreement-cache", "dir", "the families' answer cache, <host>/<sha256 of \"METHOD url body\"> (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"),
    CommandLine.Value("family-seed", "dir", "prod's identity_family_answers as <db>.jsonl, filed before any family is asked"),
    CommandLine.Value("poster-cache", "dir", "the posters kept between runs (default: KINOWO_IDENTITY_POSTER_CACHE, else target/identity-posters)"))
    .withPositionals(CommandLine.Positional("country", "the country captured (required)"))

  /** The one country a capture or a fill works on, or the tool stops. */
  def countryOf(arguments: CommandLine.Arguments): models.Country = {
    val code = arguments.positional("country").getOrElse(arguments.fail("a country code is required"))
    models.Country.byCode(code).getOrElse(arguments.fail(s"no country $code (one of ${models.Country.all.map(_.code).mkString(", ")})"))
  }

  /** A capture that cannot stand: why, said whole. */
  final class Refused(why: String) extends RuntimeException(why)
  def check(holds: Boolean, why: => String): Unit = if (!holds) throw new Refused(why)

  def main(args: Array[String]): Unit = {
    val arguments     = commandLine.parseOrExit(args)
    val country       = countryOf(arguments)
    val out           = arguments.requiredPath("out")
    val corpusDir     = arguments.requiredPath("corpus-dir")
    val cache         = arguments.requiredPath("agreement-cache")
    val configuration = settings.ProcessConfiguration.resolve()
    val root          = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target        = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to seed the corpus into"))
    target.requireThrowaway()
    val storages      = mutable.ListBuffer.empty[ConvergenceStorage]
    val done = try IdentityShadow.full(Set(country), corpusDir, root, configuration.identityLiveGaps).headOption match {
      case None    => System.err.println(s"[UnmatchedClustersCapture] no cinema-scrapes-${country.code}.json.gz in $corpusDir"); false
      case Some(c) => Try(capture(configuration, target, root, c, out, cache, arguments.path("family-seed"), arguments.path("poster-cache"), storages))
        .fold({ e => System.err.println(s"[${c.label}] FAILED: ${e.getMessage}"); if (!e.isInstanceOf[Refused]) e.printStackTrace(); false }, _ => true)
    } finally storages.foreach(s => Try(s.close()))
    if (!done) sys.exit(1)
  }

  private def capture(configuration: settings.ProcessConfiguration, target: IntegrationMongoTarget, root: settings.FixtureRoot, c: Corpus,
                      out: Path, agreementCache: Path, familySeed: Option[Path], posterCache: Option[Path],
                      storages: mutable.ListBuffer[ConvergenceStorage]): Unit = {
    val w        = wiring(target, c, storages, root, configuration.env)
    val listings = listingsOf(w, c.normalizer)
    val source   = new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
      language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps)
    val lookups  = new Memo(source)
    val whole     = IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver)
    val decisions = whole.decisions.filter(d => d.film.isEmpty && d.fallback.isEmpty && d.unanswered == 0)
    val unmatched = decisions.flatMap(_.members).toSet
    val subset    = listings.filter(l => unmatched(l.key))

    val recording = new UnmatchedClusters.Recording(lookups, described = Some(source))
    val cache     = new ExperimentCacheFetch(agreementCache)
    val docs      = new InMemoryTmdbDocuments
    // prod's own family answers first (`--family-seed`: `<db>.jsonl` exports of identity_family_answers), the rest filled
    familySeed.foreach { seed =>
      val file = seed.resolve(s"${if (c.country.code == "pl") "kinowo" else s"kinowo_${c.country.code}"}.jsonl")
      if (Files.exists(file)) docs.put(TmdbKind.Family, Files.readAllLines(file).asScala.toSeq.filter(_.nonEmpty).map { line =>
        val d = org.bson.BsonDocument.parse(line)
        d.remove("_id").asString.getValue -> d
      })
    }
    val store     = new FamilyAnswerStore(docs, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val families  = UnmatchedClusters.familiesOf(c.country)
    val sources   = LiveFamilySources.of(cache, c.country, families)
    val tmdb  = new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language, retrySleep = (_: Long) => ())
    val finds = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]]()
    val tmdbOf: String => Answer[Option[Int]] = imdb => Try(tmdb.findByImdbId(imdb).map(_.id)).toOption
      .fold[Answer[Option[Int]]](Answer.Unknown) { found => finds.put(imdb, found); Answer.Known(found) }
    val answers: Map[VoterFamily, FamilyAnswers] = families.map(f => f -> store.answers(f)).toMap
    val posterStore = new PosterAnswerStore(store, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val posters     = CachedPosters.of(configuration, c.country, posterCache)
    val catalogue   = new CatalogueFill(store, new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")), cache)

    // the records of the films the model leaned to and weighed best: what a take names, and what pooled facts read
    (decisions.flatMap(_.leaning.map(_.film)) ++ decisions.flatMap(_.trace.nodes.values.flatMap(_.candidate))).distinct.foreach(recording.film)
    def agree() = UnmatchedClusters.agree(decisions, subset, recording, answers, store.version, tmdbOf, c.normalizer, posterStore,
      modules.wiring.IdentityProjectionWiring.identities(c.country.code), catalogue.answers,
        modules.wiring.IdentityProjectionWiring.listedOn(c.country.code))
    // a question its host never answers is given up on, kept in the fixture as unreachable, and asked again next capture
    val asked = new AgreementQuestionRounds(store, sources, c.label).run(() => agree())(o => Question.families(o.stage.wanted),
      pending = o => o.stage.wanted.exists { case Question.Poster(_) | Question.Catalogue(_) => true; case _ => false },
      beside = { o =>
        posters.file(posterStore, o.stage.wanted.toSeq.collect { case Question.Poster(q) => q }); catalogue.file(o.stage.wanted.collect { case Question.Catalogue(q) => q })
      })
    val (outcome, rounds) = (asked.outcome, asked.rounds)
    val filed = docs.get(TmdbKind.Family, docs.fetchedBefore(TmdbKind.Family, Long.MaxValue).map(_._1))
    val capture = UnmatchedClusters.Capture(c.country, subset, decisions, recording.queries.asScala.toMap, recording.films.asScala.toMap,
      recording.details.asScala.toMap, recording.withDetail.asScala.toSet, filed, finds.asScala.toMap, recording.casts.asScala.toMap, asked.unreachable,
      // what the whole resolve read LIVE (an IMDb suggestion its recording lacked): the captured model decisions stand on
      // it, and a slice re-deciding them without it decides otherwise (identity-slice --faithfulness capture)
      c.liveAnswers(), recording.overviews.asScala.toMap)
    val written = out.resolve(s"${c.country.code}.json.gz")
    // never a capture its recording's detail reads were refused for: unwritten, the run's PR keeps the checked-in one
    val refused = UnmatchedClusters.hostsMissingDetail(capture)
    check(refused.isEmpty, s"listings marked as having venue detail, none of their host's stored — the recording's detail reads " +
      s"were refused; re-record the corpus once the host answers (host -> listings): $refused")
    UnmatchedClusters.write(written, capture)

    // the capture must replay to exactly what was captured
    val reread   = UnmatchedClusters.read(written)
    val replayed = UnmatchedClusters.replay(reread)
    val unasked  = UnmatchedClusters.unasked(reread, Question.families(replayed.stage.wanted))
    check(unasked.isEmpty, s"the capture replays asking what it never answered: ${unasked.take(5).mkString("; ")}")
    val wantedPosters   = replayed.stage.wanted.collect { case Question.Poster(q) => q }
    val wantedCatalogue = replayed.stage.wanted.collect { case Question.Catalogue(q) => q }
    check(wantedPosters.isEmpty, s"the capture replays wanting posters: ${wantedPosters.take(5).mkString("; ")}")
    check(wantedCatalogue.isEmpty, s"the capture replays wanting catalogue answers: ${wantedCatalogue.take(5).mkString("; ")}")
    check(UnmatchedClusters.takes(capture, replayed) == UnmatchedClusters.takes(capture, outcome), "the capture replays to other takes than it captured")
    println(s"[${c.label}] captured ${subset.size} listings in ${decisions.size} clusters; ${capture.queries.size} queries, ${capture.films.size} films, " +
      s"${filed.size} family answers after $rounds round(s), ${asked.unreachable.size} unreachable; takes ${UnmatchedClusters.takes(capture, outcome).size}")
    // the venue pages the recorded tree lacks of the captured listings and the model's pooled takes (`LiveGapLeaf.readAhead`)
    // — the page, and every gap met that holds its detail beside it (`LiveGapLeaf.gapsOf`: by its page's slug or its
    // catalogue id) — read live and kept, for the next run to answer; what reading every unread page would cost beside it
    val missed = c.missedKeys().collect { case key if key.startsWith("GET ") => key.drop(4) }
    def reads(ls: Seq[Listing]) = (ls.flatMap(_.page) ++ LiveGapLeaf.gapsOf(ls, missed)).distinct
    val unreadDetail = (l: Listing) => lookups.hasDetail(l) && lookups.unreadDetail(l)
    val ahead = reads(LiveGapLeaf.readAhead(whole.decisions, unmatched, listings, unreadDetail))
    println(s"[${c.label}] unread venue pages: ${ahead.size} read(s) ahead; every unread page of the corpus ${reads(listings.filter(unreadDetail)).size}")
    if (ahead.nonEmpty && configuration.identityLiveGaps.isDefined) {
      val read = LiveGapLeaf.readPages(ahead)
      println(s"[${c.label}] read $read of ${ahead.size} venue page(s) the tree lacks live" + (if (read > 0) " — capture again to read them" else ""))
    }
  }
}
