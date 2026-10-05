package settings

import java.util.Locale

import models.Country
import services.MongoAddress
import tools.Env

import java.nio.file.Path
import scala.concurrent.duration.*

/**
 * THE one resolver of this process's configuration. It is the only code that reads a
 * configuration key: every environment variable / system property the codebase consults is
 * an accessor here, and each comes out as a type of its own (see `ConfigurationValues`) —
 * never a bare String, Int or Duration — so the compiler, not a reviewer, keeps a TMDB key
 * out of an OMDb client and a settle interval out of the enrichment reaper.
 *
 * It reads through an [[Env]]: [[ProcessConfiguration.resolve]] binds that to the real
 * process (`Env.fromProcess` — env vars, then system properties, then `.env.local`), a spec
 * builds one over `Env.of(...)`. Accessors are `def`s that read the Env on every call, so an
 * `/admin/config` override (installed into the Env) reaches a value read per use without a
 * restart, and each numeric knob still self-registers on the admin page with its default.
 *
 * A `main` (AppLoader, WorkerMain, a tool's `main`) resolves ONE of these and hands it — or
 * the values it yields — down. `ProcessAccessLintSpec` keeps every other `src/main` file from
 * reading the process or an Env key; `ProcessConfigurationSpec` keeps every accessor typed.
 *
 * Facts about the PROCESS rather than tuning knobs (the commit, the port, PATH) are read with
 * `currentValue`, which does not list them on the admin page.
 */
final class ProcessConfiguration(val env: Env) {

  private def text(key: String): Option[String] = env.get(key).map(_.trim).filter(_.nonEmpty)
  private def fact(key: String): Option[String] = env.currentValue(key).map(_.trim).filter(_.nonEmpty)

  // ── Deployment ──────────────────────────────────────────────────────────────
  /** `KINOWO_COUNTRY` — the country a single-country process (the web tier) serves. */
  def country: Country = text("KINOWO_COUNTRY").flatMap(Country.byCode).getOrElse(Country.default)

  /** `KINOWO_COUNTRIES` — the countries a worker runs: comma-separated codes, unknown ones
   *  skipped (named in `unknownCountryCodes`), the default country when none is usable. */
  def workerCountries: WorkerCountries = {
    val resolved = countryCodes.flatMap(Country.byCode).distinct
    WorkerCountries(if (resolved.isEmpty) Seq(Country.default) else resolved)
  }

  /** The `KINOWO_COUNTRIES` codes that name no country — for the boot to warn about. */
  def unknownCountryCodes: Seq[UnknownCountryCode] = countryCodes.filter(Country.byCode(_).isEmpty).map(UnknownCountryCode(_))

  private def countryCodes: Seq[String] =
    text("KINOWO_COUNTRIES").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)

  /** `COMMIT_SHA`, or `unknown`. */
  def commit: CommitSha = CommitSha(fact("COMMIT_SHA").getOrElse("unknown"))

  /** `GITHUB_SHA`, or `unknown`. */
  def githubCommit: GithubCommitSha = GithubCommitSha(fact("GITHUB_SHA").getOrElse("unknown"))

  /** `PORT`, else `default`. */
  def healthPort(default: HealthPort): HealthPort =
    fact("PORT").flatMap(_.toIntOption).map(HealthPort(_)).getOrElse(default)

  /** `APP_MODE`, when set to one of `dev|development`, `test`, `prod|production`. An
   *  unrecognised value is refused rather than guessed. */
  def applicationMode: Option[ApplicationMode] = fact("APP_MODE").map(_.toLowerCase(Locale.ROOT)).map {
    case "prod" | "production"  => ApplicationMode.Production
    case "test"                 => ApplicationMode.Test
    case "dev" | "development"  => ApplicationMode.Development
    case other                  => throw new IllegalArgumentException(s"Unknown APP_MODE: $other (expected dev|test|prod)")
  }

  /** `PATH`, split into its directories. */
  def executableSearchPath: ExecutableSearchPath =
    ExecutableSearchPath(fact("PATH").toSeq.flatMap(_.split(java.io.File.pathSeparator)).filter(_.nonEmpty).map(Path.of(_)))


  // ── Mongo ───────────────────────────────────────────────────────────────────
  /** `MONGODB_URI` / `MONGODB_DB`. */
  def mongoAddress: MongoAddress = MongoAddress(text("MONGODB_URI").map(MongoUri(_)), text("MONGODB_DB").map(MongoDatabaseName(_)))

  /** `MONGODB_USERS_DB`. */
  def usersDatabase: Option[UsersDatabaseName] = text("MONGODB_USERS_DB").map(UsersDatabaseName(_))

  /** `MONGODB_MOVIES_MIRROR_URI`. */
  def mirrorMongoUri: Option[MirrorMongoUri] = text("MONGODB_MOVIES_MIRROR_URI").map(MirrorMongoUri(_))

  // ── Third-party credentials and ids ─────────────────────────────────────────
  def tmdbApiKey: Option[TmdbApiKey]                         = text("TMDB_API_KEY").map(TmdbApiKey(_))
  def omdbApiKey: Option[OmdbApiKey]                         = text("OMDB_API_KEY").map(OmdbApiKey(_))
  def anthropicApiKey: Option[AnthropicApiKey]               = text("ANTHROPIC_API_KEY").map(AnthropicApiKey(_))
  def zyteApiKey: Option[ZyteApiKey]                         = text("ZYTE_API_KEY").map(ZyteApiKey(_))
  def proxyUser: Option[ProxyUser]                           = text("KINOWO_PROXY_USER").map(ProxyUser(_))
  def proxyPassword: Option[ProxyPassword]                   = text("KINOWO_PROXY_PASS").map(ProxyPassword(_))
  def facebookAppId: Option[FacebookAppId]                   = text("FACEBOOK_APP_ID").map(FacebookAppId(_))
  def facebookAppSecret: Option[FacebookAppSecret]           = text("FACEBOOK_APP_SECRET").map(FacebookAppSecret(_))
  def githubDispatchToken: Option[GitHubDispatchToken]       = text("KINOWO_GITHUB_DISPATCH_TOKEN").map(GitHubDispatchToken(_))
  def facebookPageAppId: Option[FacebookPageAppId]           = text("FB_APP_ID").map(FacebookPageAppId(_))
  def googleAnalyticsId: Option[GoogleAnalyticsMeasurementId] = text("GA_MEASUREMENT_ID").map(GoogleAnalyticsMeasurementId(_))
  def sentryLoaderUrl: Option[SentryLoaderUrl]               = text("SENTRY_LOADER_URL").map(SentryLoaderUrl(_))
  def googleClientId: Option[GoogleClientId]                 = text("GOOGLE_CLIENT_ID").map(GoogleClientId(_))
  def googleClientSecret: Option[GoogleClientSecret]         = text("GOOGLE_CLIENT_SECRET").map(GoogleClientSecret(_))

  /** `APPLE_BUNDLE_ID`, else the app's own bundle id. */
  def appleBundleId: AppleBundleId = AppleBundleId(text("APPLE_BUNDLE_ID").getOrElse("dev.kinowo.Kinowo"))

  /** `ADMIN_ALLOWLIST` — comma-separated emails; empty (nobody) when unset. */
  def adminAllowlist: AdminAllowlist =
    AdminAllowlist(text("ADMIN_ALLOWLIST").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSet)

  // ── Alert routes ────────────────────────────────────────────────────────────
  /** Where an alerter posts, or the settings whose absence leaves it unrouted. */
  def telegramRoute(route: AlertRoute): Either[Seq[MissingSetting], TelegramRoute] = {
    val token = text(AlertRoute.TokenKey).map(TelegramBotToken(_))
    val chat: Either[String, TelegramChatId] = text(route.chatKey) match {
      case None      => Left(route.chatKey)
      case Some(raw) => raw.toLongOption.map(TelegramChatId(_)).toRight(s"${route.chatKey} (not a number)")
    }
    (token, chat) match {
      case (Some(bot), Right(chatId)) => Right(TelegramRoute(bot, chatId, text(route.topicKey).flatMap(_.toLongOption).map(TelegramTopicId(_))))
      case _                          => Left((token.fold(Seq(AlertRoute.TokenKey))(_ => Nil) ++ chat.left.toSeq).map(MissingSetting(_)))
    }
  }

  /** The keys `feature` needs that this configuration lacks — empty when it is on. */
  def missingFor(feature: GatedIntegration): Seq[MissingSetting] = feature.keys.filter(text(_).isEmpty).map(MissingSetting(_))

  // ── Worker storage ──────────────────────────────────────────────────────────
  /** `KINOWO_SHARE_CARD_DIR`, else `/share-cards`. */
  def shareCardDirectory: ShareCardDirectoryTemplate =
    ShareCardDirectoryTemplate(text("KINOWO_SHARE_CARD_DIR").getOrElse("/share-cards"))

  /** `KINOWO_HEAP_DUMP_DIR`, else `/data/heapdumps`. */
  def heapDumpDirectory: HeapDumpDirectory = HeapDumpDirectory(Path.of(text("KINOWO_HEAP_DUMP_DIR").getOrElse("/data/heapdumps")))

  /** `KINOWO_SCRAPE_CITIES` — lowercased slugs; None when unset or naming nothing. */
  def scrapeCitySlugs: Option[ScrapeCitySlugs] =
    text("KINOWO_SCRAPE_CITIES").map(_.split(",").iterator.map(_.trim.toLowerCase(Locale.ROOT)).filter(_.nonEmpty).toSet)
      .filter(_.nonEmpty).map(ScrapeCitySlugs(_))

  // ── Tuning knobs ────────────────────────────────────────────────────────────
  // Each takes its compiled-in default from the caller, and — like every numeric knob — falls
  // back to it for an unset, unparseable or non-positive value rather than crash or disable
  // the work. The default is what the admin page lists.
  private def count(key: String, default: Int): Int = env.positiveInt(key, default)
  private def seconds(key: String, default: FiniteDuration): FiniteDuration = env.positiveLong(key, default.toSeconds).seconds
  private def minutes(key: String, default: FiniteDuration): FiniteDuration = env.positiveLong(key, default.toMinutes).minutes
  private def millis(key: String, default: FiniteDuration): FiniteDuration = env.positiveLong(key, default.toMillis).millis

  def mongoProbeTimeout(default: MongoProbeTimeout): MongoProbeTimeout =
    MongoProbeTimeout(seconds("MONGODB_PROBE_TIMEOUT_SECONDS", default.value))
  def mongoMaxPoolSize(default: MongoMaxPoolSize): MongoMaxPoolSize = MongoMaxPoolSize(count("KINOWO_MONGO_MAX_POOL_SIZE", default.value))
  def mongoOptional: MongoOptional = MongoOptional(env.flag("MONGODB_OPTIONAL"))


  def backgroundConcurrency(default: BackgroundConcurrency): BackgroundConcurrency =
    BackgroundConcurrency(count("KINOWO_BG_CONCURRENCY", default.value))
  def workerPoolSize(default: WorkerPoolSize): WorkerPoolSize = WorkerPoolSize(count("KINOWO_WORKER_POOL_SIZE", default.value))
  def livenessStaleAfter(default: LivenessStaleAfter): LivenessStaleAfter =
    LivenessStaleAfter(minutes("KINOWO_WORKER_LIVENESS_STALE_MINUTES", default.value))
  def configRefreshInterval(default: ConfigRefreshInterval): ConfigRefreshInterval =
    ConfigRefreshInterval(seconds("KINOWO_CONFIG_REFRESH_SECONDS", default.value))

  def scrapeChunkSpread(default: ScrapeChunkSpread): ScrapeChunkSpread =
    ScrapeChunkSpread(minutes("KINOWO_SCRAPE_CHUNK_SPREAD_MINUTES", default.value))
  def scrapeInitialDelay(default: ScrapeInitialDelay): ScrapeInitialDelay =
    ScrapeInitialDelay(seconds("KINOWO_SCRAPE_INITIAL_DELAY_SECONDS", default.value))
  def scrapeMaxEnqueuePerTick(default: ScrapeMaxEnqueuePerTick): ScrapeMaxEnqueuePerTick =
    ScrapeMaxEnqueuePerTick(count("KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK", default.value))
  def scrapeBootRamp(default: ScrapeBootRamp): ScrapeBootRamp = ScrapeBootRamp(minutes("KINOWO_SCRAPE_BOOT_RAMP_MINUTES", default.value))
  def scrapeEnqueueSpreadSlices(default: ScrapeEnqueueSpreadSlices): ScrapeEnqueueSpreadSlices =
    ScrapeEnqueueSpreadSlices(count("KINOWO_SCRAPE_ENQUEUE_SPREAD_SLICES", default.value))
  def scrapeMaxOutstandingTasks(default: ScrapeMaxOutstandingTasks): ScrapeMaxOutstandingTasks =
    ScrapeMaxOutstandingTasks(count("KINOWO_SCRAPE_MAX_OUTSTANDING_TASKS", default.value))
  def scrapeTasksPerVenue(default: ScrapeTasksPerVenue): ScrapeTasksPerVenue =
    ScrapeTasksPerVenue(count("KINOWO_SCRAPE_TASKS_PER_VENUE", default.value))
  def scrapeFreshness(default: ScrapeFreshness): ScrapeFreshness = ScrapeFreshness(minutes("KINOWO_SCRAPE_FRESHNESS_MINUTES", default.value))

  def enrichmentMaxEnqueuePerTick(default: EnrichmentMaxEnqueuePerTick): EnrichmentMaxEnqueuePerTick =
    EnrichmentMaxEnqueuePerTick(count("KINOWO_ENRICHMENT_MAX_ENQUEUE_PER_TICK", default.value))
  def enrichmentTickInterval(default: EnrichmentTickInterval): EnrichmentTickInterval =
    EnrichmentTickInterval(seconds("KINOWO_ENRICHMENT_TICK_INTERVAL_SECONDS", default.value))
  def detailMaxEnqueuePerTick(default: DetailMaxEnqueuePerTick): DetailMaxEnqueuePerTick =
    DetailMaxEnqueuePerTick(count("KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK", default.value))
  def detailTickInterval(default: DetailTickInterval): DetailTickInterval =
    DetailTickInterval(seconds("KINOWO_DETAIL_TICK_INTERVAL_SECONDS", default.value))
  def omdbBackfillInterval(default: OmdbBackfillInterval): OmdbBackfillInterval =
    OmdbBackfillInterval(seconds("KINOWO_OMDB_BACKFILL_INTERVAL_SECONDS", default.value))
  def filmwebDropThreshold(default: FilmwebDropThreshold): FilmwebDropThreshold =
    FilmwebDropThreshold(count("KINOWO_FILMWEB_DROP_THRESHOLD", default.value))
  def zyteSessionTtl(default: ZyteSessionTtl): ZyteSessionTtl = ZyteSessionTtl(seconds("KINOWO_ZYTE_SESSION_TTL_SECONDS", default.value))

  def cacheRehydrateInterval(default: CacheRehydrateInterval): CacheRehydrateInterval =
    CacheRehydrateInterval(seconds("KINOWO_CACHE_REHYDRATE_SECONDS", default.value))
  /** `KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS` — how many times the boot hydrate retries an empty read;
   *  0 (the default, and what a non-positive value means) hydrates once and does not retry. */
  def bootHydrateMaxAttempts: BootHydrateMaxAttempts =
    BootHydrateMaxAttempts(count("KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS", 0))
  def bootHydrateRetryInterval(default: BootHydrateRetryInterval): BootHydrateRetryInterval =
    BootHydrateRetryInterval(millis("KINOWO_BOOT_HYDRATE_RETRY_MS", default.value))
  def readModelPruneInterval(default: ReadModelPruneInterval): ReadModelPruneInterval =
    ReadModelPruneInterval(seconds("KINOWO_READMODEL_PRUNE_SECONDS", default.value))
  def readModelPruneBootDelay(default: ReadModelPruneBootDelay): ReadModelPruneBootDelay =
    ReadModelPruneBootDelay(seconds("KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS", default.value))
  def readModelReloadInterval(default: ReadModelReloadInterval): ReadModelReloadInterval =
    ReadModelReloadInterval(seconds("KINOWO_READMODEL_RELOAD_SECONDS", default.value))
  def readModelColdRetryInterval(default: ReadModelColdRetryInterval): ReadModelColdRetryInterval =
    ReadModelColdRetryInterval(seconds("KINOWO_READMODEL_COLD_RETRY_SECONDS", default.value))
  /** `KINOWO_IDENTITY_RATING_GATE` (`1` or `true`) — confidence-gated ratings; off unless set. */
  def identityRatingGate: IdentityRatingGateEnabled = IdentityRatingGateEnabled(env.flag("KINOWO_IDENTITY_RATING_GATE"))
  def identityShadowInterval(default: IdentityShadowInterval): IdentityShadowInterval =
    IdentityShadowInterval(seconds("KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS", default.value))
  def identityShadowInitialDelay(default: IdentityShadowInitialDelay): IdentityShadowInitialDelay =
    IdentityShadowInitialDelay(seconds("KINOWO_IDENTITY_SHADOW_INITIAL_DELAY_SECONDS", default.value))
  def identityShadowLookupRate(default: IdentityShadowLookupRate): IdentityShadowLookupRate =
    IdentityShadowLookupRate(count("KINOWO_IDENTITY_SHADOW_LOOKUP_RATE", default.perMinute))
  def readModelAuditSample(default: ReadModelAuditSample): ReadModelAuditSample =
    ReadModelAuditSample(count("KINOWO_READMODEL_AUDIT_SAMPLE", default.value))

  def shareCardStorageBudget(default: ShareCardStorageBudget): ShareCardStorageBudget =
    ShareCardStorageBudget(env.positiveLong("KINOWO_SHARE_CARD_BUDGET_MB", default.bytes / (1024 * 1024)) * 1024 * 1024)
  def shareCardBackfillBatch(default: ShareCardBackfillBatch): ShareCardBackfillBatch =
    ShareCardBackfillBatch(count("KINOWO_SHARE_CARD_BACKFILL_BATCH", default.value))
  def shareCardBackfillMaxBacklog(default: ShareCardBackfillMaxBacklog): ShareCardBackfillMaxBacklog =
    ShareCardBackfillMaxBacklog(count("KINOWO_SHARE_CARD_BACKFILL_MAX_BACKLOG", default.value))
  def posterDecodeMemoryCap(default: PosterDecodeMemoryCap): PosterDecodeMemoryCap =
    PosterDecodeMemoryCap(env.positiveLong("KINOWO_SHARE_CARD_DECODE_MEMORY_MB", default.megabytes))
  def shareCardFirstHold(default: ShareCardFirstHold): ShareCardFirstHold =
    ShareCardFirstHold(seconds("KINOWO_SHARE_CARD_FIRST_HOLD_SECONDS", default.value))
  def shareCardAuditSample(default: ShareCardAuditSample): ShareCardAuditSample =
    ShareCardAuditSample(count("KINOWO_SHARE_CARD_AUDIT_SAMPLE", default.value))

  /** `knob`'s live pace, else `default` — read per request so an admin flip takes effect at
   *  once. A non-positive or unparseable value keeps the default rather than unpacing a host. */
  def hostPace(knob: PaceKnob, default: HostPace): HostPace =
    HostPace(java.time.Duration.ofMillis(env.positiveLong(knob.key, default.value.toMillis)))

  // ── Test and replay harness ─────────────────────────────────────────────────
  /** `KINOWO_LOCAL_MONGO_URI` — the local stack's Mongo (`sbt localStack`). */
  def localStackMongoUri: Option[LocalStackMongoUri] = text("KINOWO_LOCAL_MONGO_URI").map(LocalStackMongoUri(_))
  /** `KINOWO_LOCAL_MONGO_DB` — the local stack's database. */
  def localStackDatabase: Option[LocalStackDatabaseName] = text("KINOWO_LOCAL_MONGO_DB").map(LocalStackDatabaseName(_))
  /** `KINOWO_FIXTURE_DIR` — the fixture tree the local stack's worker replays. */
  def localStackFixtureDirectory: Option[LocalStackFixtureDirectory] = text("KINOWO_FIXTURE_DIR").map(LocalStackFixtureDirectory(_))

  /** `KINOWO_FIXTURE_ROOT`, else the repository's own tree. */
  def fixtureRoot: FixtureRoot = text("KINOWO_FIXTURE_ROOT").map(root => FixtureRoot(Path.of(root))).getOrElse(FixtureRoot.RepositoryRelative)

  /** `KINOWO_ALLOW_REMOTE_IT` (`1` or `true`). */
  def remoteIntegrationAllowed: RemoteIntegrationAllowed = RemoteIntegrationAllowed(env.flag("KINOWO_ALLOW_REMOTE_IT"))

  /** `KINOWO_SPEC_TIME_SCALE` — a whole-number multiplier on every test wait bound (`tools.SpecTimeouts`),
   *  for a runner slower than the defaults allow for; 1 when unset. */
  def specTimeScale: SpecTimeScale = SpecTimeScale(count("KINOWO_SPEC_TIME_SCALE", 1))

  /** `KINOWO_RACE_SEED`, else `default`. */
  def raceSeed(default: RaceSeed): RaceSeed = text("KINOWO_RACE_SEED").flatMap(_.toLongOption).map(RaceSeed(_)).getOrElse(default)

  /** `KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES`, when a run points at a particular tree. */
  def enrichmentFixtureTree: Option[EnrichmentFixtureTree] = text("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES").map(EnrichmentFixtureTree(_))

  /** `KINOWO_CONVERGENCE_HERMETIC` (`1` or `true`). */
  def hermeticReplay: HermeticReplay = HermeticReplay(env.flag("KINOWO_CONVERGENCE_HERMETIC"))
  /** `KINOWO_CONVERGENCE_FILL_ONLY` (`1` or `true`). */
  def gapFill: GapFill = GapFill(env.flag("KINOWO_CONVERGENCE_FILL_ONLY"))

  /** `KINOWO_IDENTITY_LOOKUPS` (`1` or `true`). */
  def identityLookupSweep: IdentityLookupSweepEnabled = IdentityLookupSweepEnabled(env.flag("KINOWO_IDENTITY_LOOKUPS"))

  def corpusRunId: Option[CorpusRunId]                     = text("KINOWO_CONVERGENCE_CORPUS_RUN").map(CorpusRunId(_))
  def corpusRecordedAt: Option[CorpusRecordedAt]           = text("KINOWO_CONVERGENCE_CORPUS_RECORDED_AT").map(CorpusRecordedAt(_))
  def greenCorpusRunId: Option[GreenCorpusRunId]           = text("KINOWO_CONVERGENCE_GREEN_CORPUS_RUN").map(GreenCorpusRunId(_))
  def greenCorpusRecordedAt: Option[GreenCorpusRecordedAt] = text("KINOWO_CONVERGENCE_GREEN_CORPUS_RECORDED_AT").map(GreenCorpusRecordedAt(_))
  def greenCorpusDirectory: Option[GreenCorpusDirectory]   = text("KINOWO_CONVERGENCE_GREEN_CORPUS_DIR").map(dir => GreenCorpusDirectory(Path.of(dir)))

  /** `KINOWO_CONVERGENCE_SCRAPES_URI` / `KINOWO_CONVERGENCE_SCRAPES_DB` — a production dump a
   *  convergence leg (or the corpus recorder) reads its real scrapes from, read-only. */
  def convergenceScrapesUri: Option[ConvergenceScrapesUri] = text("KINOWO_CONVERGENCE_SCRAPES_URI").map(ConvergenceScrapesUri(_))
  def convergenceScrapesDatabase: Option[ConvergenceScrapesDatabaseName] =
    text("KINOWO_CONVERGENCE_SCRAPES_DB").map(ConvergenceScrapesDatabaseName(_))

  /** `GITHUB_STEP_SUMMARY` — the file a CI step appends its markdown summary to. */
  def stepSummaryFile: Option[StepSummaryFile] = fact("GITHUB_STEP_SUMMARY").map(file => StepSummaryFile(Path.of(file)))

  /** `KINOWO_HARD_CLUSTERS_RECORD` (`1` or `true`) — the hard-cluster spec re-records its responses. */
  def hardClusterRecording: HardClusterRecording = HardClusterRecording(env.flag("KINOWO_HARD_CLUSTERS_RECORD"))
  /** `KINOWO_HARD_CLUSTERS_COUNTRIES` — comma-separated codes narrowing a hard-cluster run; codes
   *  naming no country are dropped. */
  def hardClusterCountries: Option[HardClusterCountries] =
    text("KINOWO_HARD_CLUSTERS_COUNTRIES").map(codes => HardClusterCountries(codes.split(",").iterator.map(_.trim).flatMap(Country.byCode).toSet))
  /** `KINOWO_HARD_CLUSTERS_DUMP` (`1` or `true`) — print every pass's films. */
  def hardClusterDump: HardClusterDump = HardClusterDump(env.flag("KINOWO_HARD_CLUSTERS_DUMP"))

  /** `KINOWO_IDENTITY_CORPUS_DIR` — recorded corpora the listing-key spec widens its sweep to. */
  def identityCorpusDirectory: Option[IdentityCorpusDirectory] =
    text("KINOWO_IDENTITY_CORPUS_DIR").map(dir => IdentityCorpusDirectory(Path.of(dir)))
  /** `KINOWO_IDENTITY_GATE=strict` — the identity query-coverage gate fails on any gap, not just reports it. */
  def identityGateStrict: IdentityGateStrict = IdentityGateStrict(text("KINOWO_IDENTITY_GATE").contains("strict"))
  /** `KINOWO_IDENTITY_FULL` — comma-separated codes of the full recorded corpora the identity gate
   *  measures besides the hard clusters; codes naming no country are dropped. */
  def identityFullCorpora: IdentityFullCorpora =
    IdentityFullCorpora(text("KINOWO_IDENTITY_FULL").toSeq.flatMap(_.split(",")).map(_.trim.toLowerCase(Locale.ROOT)).flatMap(Country.byCode).toSet)
  /** `KINOWO_IDENTITY_OUT` — where the identity shadow run writes its reports and calibration
   *  dataset, else `target/identity-shadow`. */
  def identityShadowOutput: IdentityShadowOutput =
    IdentityShadowOutput(Path.of(text("KINOWO_IDENTITY_OUT").getOrElse("target/identity-shadow")))
  /** `KINOWO_IDENTITY_DUMP` — a directory the resolver-only run writes every listing's decision to
   *  (`IdentityResolveDumpIntegrationSpec`): the fast local loop for a resolver change. */
  def identityDump: Option[IdentityDump] = text("KINOWO_IDENTITY_DUMP").map(dir => IdentityDump(Path.of(dir)))
  /** `KINOWO_IDENTITY_AGREEMENT_CACHE` — with it, the resolver-only replay also runs the agreement stage over its no-matches
   *  (`agreement.AgreementStage`), the families answered from this directory (`<host>/<sha256 of "METHOD url body">`, the
   *  signal-combination experiment's cache), else live — a measuring loop, never in CI. */
  def identityAgreementCache: Option[IdentityAgreementCache] = text("KINOWO_IDENTITY_AGREEMENT_CACHE").map(dir => IdentityAgreementCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_UNMATCHED_CAPTURE` — a directory the unmatched-cluster capture (`UnmatchedClustersCaptureIntegrationSpec`)
   *  writes each country's fixture to (`<cc>.json.gz`): the checked-in `test/resources/fixtures/identity-unmatched` to
   *  re-capture it. Never in CI. */
  def identityUnmatchedCapture: Option[IdentityUnmatchedCapture] =
    text("KINOWO_IDENTITY_UNMATCHED_CAPTURE").map(dir => IdentityUnmatchedCapture(Path.of(dir)))
  /** `KINOWO_IDENTITY_POSTER_CACHE` — posters and TMDB poster lists the unmatched-cluster capture hashes before fetching
   *  any live (and files what it fetched into), for the agreement's poster evidence. Never in CI. */
  def identityPosterCache: Option[IdentityPosterCache] = text("KINOWO_IDENTITY_POSTER_CACHE").map(dir => IdentityPosterCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_FAMILY_SEED` — a directory of `<db>.jsonl` exports of prod's `identity_family_answers` (one EJSON
   *  document a line) the unmatched-cluster capture files before asking the families anything. */
  def identityFamilySeed: Option[IdentityFamilySeed] = text("KINOWO_IDENTITY_FAMILY_SEED").map(dir => IdentityFamilySeed(Path.of(dir)))
  /** `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` — a TMDB key with which the resolver-only replay
   *  (`IdentityResolveDumpIntegrationSpec`) answers what its recording cannot from TMDB and IMDb LIVE,
   *  instead of as gaps: the local loop for a change that asks new questions. Never in CI. */
  def identityLiveGaps: Option[IdentityLiveGaps] = text("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY").map(IdentityLiveGaps(_))
  /** `KINOWO_IDENTITY_SEED_FILMS` — a directory of `films-<cc>.json`, today's films as sets of listing
   *  keys (`scripts.ListingKeyBackfill --export`), that the ID-seeding review assigns ids from. */
  def identitySeedFilms: Option[IdentitySeedFilms] =
    text("KINOWO_IDENTITY_SEED_FILMS").map(dir => IdentitySeedFilms(Path.of(dir)))
  /** `KINOWO_IDENTITY_PERMUTATIONS` — arrival orders the identity shadow run replays a FULL corpus
   *  in (the hard clusters always take 21). */
  def identityShadowPermutations: IdentityShadowPermutations =
    IdentityShadowPermutations(text("KINOWO_IDENTITY_PERMUTATIONS").flatMap(_.toIntOption).getOrElse(3))
  /** `KINOWO_IDENTITY_PIPELINE_CACHE` — a directory of booted pipeline answers per corpus: read
   *  when a corpus's file is there, written after a boot when it is not. */
  def identityPipelineCache: Option[IdentityPipelineCache] =
    text("KINOWO_IDENTITY_PIPELINE_CACHE").map(dir => IdentityPipelineCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_BOOT_ONLY` (`1` or `true`) — boot each corpus's pipeline into the cache and measure
   *  nothing: the CI measure boots the BASE commit's projection this way before applying a variant. */
  def identityPipelineBootOnly: IdentityPipelineBootOnly =
    IdentityPipelineBootOnly(env.flag("KINOWO_IDENTITY_BOOT_ONLY"))
  /** `KINOWO_IDENTITY_ROBUSTNESS=off` — skip the robustness measures that resolve a corpus again. */
  def identityShadowRobustness: IdentityShadowRobustness =
    IdentityShadowRobustness(!text("KINOWO_IDENTITY_ROBUSTNESS").contains("off"))
  /** `KINOWO_IDENTITY_FOCUS=pieśni lasu,vincent` — resolve only listings whose titles hold every word
   *  of one of these comma-separated phrases, print their decisions and stop: a fast check. */
  def identityFocus: Option[IdentityFocus] =
    text("KINOWO_IDENTITY_FOCUS").map(v => IdentityFocus(v.split(",").toSeq
      .map(p => services.movies.TitleContainment.tokens(p).toSet).filter(_.nonEmpty), alone = env.flag("KINOWO_IDENTITY_FOCUS_ALONE")))
      .filter(_.phrases.nonEmpty)
  /** `KINOWO_IDENTITY_RECORD_CHECK` — the country whose recording pass the identity gate checks
   *  against a scratch fixture root. */
  def identityRecordCheck: Option[IdentityRecordCheck] =
    text("KINOWO_IDENTITY_RECORD_CHECK").map(_.trim.toLowerCase(Locale.ROOT)).flatMap(Country.byCode).map(IdentityRecordCheck(_))

  /** `CDP_BROWSER_BIN` — the Chrome/Edge binary the page tests drive, over the usual install paths. */
  def cdpBrowserBinary: Option[CdpBrowserBinary] = fact("CDP_BROWSER_BIN").map(bin => CdpBrowserBinary(Path.of(bin)))

  /** `KINOWO_OG_BASE` — the origin the share-card generator screenshots, over the country's own. */
  def ogCardBaseUrl: Option[OgCardBaseUrl] = text("KINOWO_OG_BASE").map(OgCardBaseUrl(_))
  /** `KINOWO_OG_OUT`, else the web assets' image directory. */
  def ogCardOutputDirectory: OgCardOutputDirectory =
    OgCardOutputDirectory(Path.of(text("KINOWO_OG_OUT").getOrElse("web/src/main/assets/img")))
  /** `KINOWO_OG_HOME_CITY` — the city slug the landing card screenshots. */
  def ogCardHomeCity: Option[OgCardHomeCity] = text("KINOWO_OG_HOME_CITY").map(OgCardHomeCity(_))
  /** `KINOWO_OG_PROXY_PORT` — one residential proxy port pinned instead of the rotation. */
  def ogCardProxyPort: Option[OgCardProxyPort] = text("KINOWO_OG_PROXY_PORT").flatMap(_.toIntOption).map(OgCardProxyPort(_))
  /** `KINOWO_OG_PROXY_HOST`, else the residential proxy's own host. */
  def ogCardProxyHost: OgCardProxyHost = OgCardProxyHost(text("KINOWO_OG_PROXY_HOST").getOrElse("isp.decodo.com"))

  // ── The JVM itself ──────────────────────────────────────────────────────────
  /** `java.home` — the running JDK, whose `bin/` a spec launches a child `java` / `keytool` from. */
  def javaHome: JavaHome = JavaHome(Path.of(fact("java.home").getOrElse(throw new IllegalStateException("java.home is not set"))))
  /** `java.class.path` — the running JVM's class path, split into its entries. */
  def javaClassPath: JavaClassPath =
    JavaClassPath(fact("java.class.path").toSeq.flatMap(_.split(java.io.File.pathSeparator)).filter(_.nonEmpty).map(Path.of(_)))
}

object ProcessConfiguration {

  /** This process's configuration: environment variables → system properties → `localFile`
   *  (`.env.local` by default). Call once, from a `main`. */
  def resolve(localFile: java.io.File = new java.io.File(".env.local")): ProcessConfiguration =
    new ProcessConfiguration(Env.fromProcess(localFile))

  /** The process's own environment variables and system properties alone, WITHOUT `.env.local`
   *  — for a root that must tell a value the user exported from one only that file carries (the
   *  local stack, whose `.env.local` MONGODB_URI is prod's). */
  def resolveExported(): ProcessConfiguration = resolve(new java.io.File("/nonexistent/.env.local"))
}

/** Where an alerter's Telegram route is configured: its chat and optional topic. */
enum AlertRoute(val chatKey: String, val topicKey: String) {
  case FilmwebFallback extends AlertRoute("KINOWO_FALLBACK_TG_CHAT_ID", "KINOWO_FALLBACK_TG_TOPIC_ID")
  case FilmwebDrop     extends AlertRoute("KINOWO_FILMWEB_DROP_TG_CHAT_ID", "KINOWO_FILMWEB_DROP_TG_TOPIC_ID")
}

object AlertRoute {
  val TokenKey = "TELEGRAM_BOT_TOKEN"
}

/** A resolved alert route. */
final case class TelegramRoute(token: TelegramBotToken, chatId: TelegramChatId, topicId: Option[TelegramTopicId])

/** An external integration a missing secret switches off without failing the boot — see
 *  `WorkerIntegrations`. `keys` are the settings it needs, all of them. */
enum GatedIntegration(val featureName: String, val keys: Seq[String]) {
  case Tmdb             extends GatedIntegration("tmdb", Seq("TMDB_API_KEY"))
  case Omdb             extends GatedIntegration("omdb", Seq("OMDB_API_KEY"))
  case IdentityProposals extends GatedIntegration("identity_proposals", Seq("ANTHROPIC_API_KEY"))
  case ResidentialProxy extends GatedIntegration("residential_proxy", Seq("KINOWO_PROXY_USER", "KINOWO_PROXY_PASS"))
  case Zyte             extends GatedIntegration("zyte", Seq("ZYTE_API_KEY"))
  case Sentry           extends GatedIntegration("sentry", Seq("SENTRY_DSN"))
  case FacebookRescrape extends GatedIntegration("facebook_rescrape", Seq("FACEBOOK_APP_ID", "FACEBOOK_APP_SECRET"))
}
