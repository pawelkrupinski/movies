package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, read, scalaFiles}

import scala.util.matching.Regex

/**
 * A failure is never data.
 *
 * The class of bug: a fetch, read or decode fails, and the code answers with the value an
 * empty-but-healthy source would have given — `Seq.empty`, `0`, `false`, `None`. The caller
 * cannot tell the two apart, so it acts on the empty one. Seventeen fixes of that shape so
 * far, among them: a whole cinema listing unreachable yet reported as a "0 showtimes"
 * scrape (ecc6d7f63); an unreadable task backlog read as an empty one, so the reaper
 * admitted its whole budget on top of it (eebd3eef9); a failed enqueue reported as
 * "already queued" (c417f36ec); a failed repository write reported as landed, which froze
 * Kino Aurum's screenings for five days (880863c58); and a Mongo DTO decode abort swallowed
 * into `Seq.empty`.
 *
 * Two shapes, in main sources, each named file:line:
 *
 *  1. `Try(...)` answered with `<empty>` on failure: `.getOrElse(<empty>)`,
 *     `.toOption.getOrElse(<empty>)`, `.toOption.fold(<empty>)(…)`, `.fold(_ => <empty>, …)`.
 *  2. An exception handler whose answer is `<empty>`: a `case` for `NonFatal(_)`,
 *     `_: Throwable`/`Exception`/`…Exception`, or `_` inside `catch`/`recover`, whose body
 *     ENDS in `<empty>` — logging first does not make it any less of a swallow.
 *
 *  3. In client code (cinema scrapers, enrichment clients, `TmdbClient`, common `tools`):
 *     a `Try(...)` around an HTTP read or a parse — `http.get`, `fetch.post`, `httpGet`,
 *     `Json.parse`, `Jsoup.parse`, any `parse…(` — answered with `.toOption` or
 *     `.getOrElse(…)`, whatever the fallback. A failed read or a changed page format is
 *     not "no data"; read through `tools.HttpRead` / `ReadOutcome` instead. (`"[]"` and
 *     `"{}"` count as empty for shapes 1 and 2: they are an empty JSON body.)
 *
 *  4. In repository code (a file that holds a `MongoCollection`/`MongoDatabase`): a `Try(...)`
 *     around an awaited Mongo READ — `find`, `first`/`headOption`, a count, `listIndexes`, an
 *     `aggregate`, a `findOneAnd…` — answered with `.toOption`, `.getOrElse`, `.recover` or
 *     `.fold`, whatever the fallback. A read that timed out is not a missing document: read
 *     through `tools.MongoRead` (a `ReadOutcome`), let the failure propagate, or — for a cache,
 *     where a miss is the safe answer — allowlist it in [[RepositoryReadSwallows]] with WHY.
 *     Whole-collection reads answer a `tools.ScanOutcome`, which the build will not let a
 *     caller drop.
 *
 * Where the empty answer is genuinely right — an optional field's default while decoding,
 * a probe whose failure means "not available here", a retry loop's "not yet" — add the
 * site to [[Allowlist]] with WHY. The reason is the review: "defensive" is not one. Where
 * it is not right, propagate the failure, or give the result a type that can say "unknown"
 * (as `TaskQueue.waitingCount` and `EnqueueResult.Failed` now do).
 */
class NoSwallowedFailureSpec extends AnyFlatSpec with Matchers {

  // A type argument may nest one level (`Map.empty[String, Seq[Int]]`).
  private val TypeArgs = """(?:\[(?:[^\[\]]|\[[^\]]*\])*\])?"""
  private val BareEmpty: String =
    """(?:(?:Seq|List|Vector|Map|Set|Iterable|Option)\.empty""" + TypeArgs + """|Nil|(?:Seq|List|Vector|Map|Set)\(\)|""" +
      """0|0L|0\.0|false|None|""|"\[\]"|"\{\}"|Json\.obj\(\))"""
  // …or that value already completed, as a `recoverWith` answers: `Future.successful(Nil)`.
  private val Empty: String = """(?:""" + BareEmpty + """|Future\.successful\(\s*""" + BareEmpty + """\s*\))"""

  private val TryOpen: Regex     = """\bTry\s*[({]""".r
  // What a `Try(...)` may be followed by to answer its failure with `<empty>`: `.getOrElse`,
  // `.toOption.getOrElse`, `.toOption.fold(<empty>)(…)` (eebd3eef9's exact shape), and
  // `.fold(_ => <empty>, …)`.
  // Any chain of calls may sit between the `Try(...)` and the answer — `.map(f).getOrElse(Nil)`
  // is the same swallow as `.getOrElse(Nil)` — so the prefix is walked with [[afterChain]];
  // the answer may be braced (`.getOrElse { Nil }`).
  private val EmptyOnFailure     = ("""^\s*(?:(?:\.toOption\s*)?\.getOrElse\s*[({]\s*""" + Empty + """\s*[)}]""" +
    """|\.toOption\s*\.fold\s*\(\s*""" + Empty + """\s*\)""" +
    """|\.fold\s*\(\s*\w+\s*=>\s*""" + Empty + """\s*,)""").r
  // `_` and a bare binder (`case e =>`) are handlers only inside catch/recover; `Failure(_)` is
  // one wherever it matches a `Try`.
  private val HandlerCase: Regex = """\bcase\s+(_|[a-z]\w*|Failure\(\s*\w+\s*\)|NonFatal\(\s*\w+\s*\)|\w+\s*:\s*(?:Throwable|Exception|\w+Exception))\s*=>""".r
  private val BlockOnlyHandler: Regex = """_|[a-z]\w*""".r
  private val HandlerBlock       = """(?:\bcatch|\brecover|\brecoverWith)\s*$""".r
  private val EmptyLine          = ("""^""" + Empty + """$""").r

  /** Site (repository-relative file, the nearest `def` at or above it, the flagged line
   *  trimmed) → why the empty answer is right. Keyed by the method rather than a line
   *  number so an entry survives unrelated edits above it, and a new swallow elsewhere in
   *  the file is not covered by an old entry that happens to share its text. */
  private val Allowlist: Map[(String, String, String), String] = Map(
    ("worker/src/main/scala/services/identity/IdentityProjection.scala", "quietly",
      "catch { case NonFatal(e) =>") ->
      "not a failure read as data: a projection that throws is logged, counted as a Failed refusal and answers 'not settled', which is what makes `EventTrigger` run it again after a backoff — the stored films keep serving meanwhile",
    ("common/src/main/scala/tools/DaemonExecutors.scala", "run",
      "catch { case _: InterruptedException => cancelUnrun(command); Thread.currentThread().interrupt(); false }") ->
      "not a failure read as data: an interrupt while parked for a permit is `shutdownNow` stopping a task that never started — the task is CANCELLED (its waiter sees a CancellationException) and the interrupt flag restored; false only says 'do not run it'",
    ("web/src/main/scala/controllers/MetricsController.scala", "metrics",
      "val fallback = scala.util.Try(fallbackStore.findAll()).fold(_ => \"\",") ->
      "not an empty value but an ABSENT one: an unreadable fallback store leaves its two gauge families out of the exposition (no series, never a false 0), so the rest of /metrics — the web tier's only request-rate, latency and disk signals — is still served",
    ("web/src/main/scala/controllers/DebugSnapshot.scala", "load",
      "case Failure(exception) =>") ->
      "a dev-only CACHE of a /debug read, never data: None means 'nothing usable stored' and the caller reads the source at once, exactly as with no file — a stale or corrupt file must not fail the page; logged",
    ("worker/src/main/scala/services/cinemas/common/GatsbyBoxOfficeClient.scala", "ask",
      "case NonFatal(e) =>") ->
      "the film details are optional credits beside the schedule, which is the scrape: after a second attempt (a response naming none of the batch counts as a failure), an empty map lists those films without a credit or runtime, exactly as every scrape before the details request did, and the failure is logged",
    ("worker/src/main/scala/services/identity/TmdbNormalizer.scala", "normalize",
      "case Failure(_)                                          => None") ->
      "None is 'write nothing': a transient failure is not an answer, so the store keeps what it held and the model's question stays a gap — the opposite of turning it into data",
    ("worker/src/main/scala/services/cinemas/pl/BiletynaClient.scala", "fromNationalFeed",
      "catch { case e: Exception =>") ->
      "None is not 'no screenings' but 'read the venue's own place page instead', which still propagates its own failure",
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "Try(document.getList(\"errors\", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("worker/src/main/scala/services/sharecards/ShareCardBackfill.scala", "enqueueWithinBacklog",
      "Try(queue.waitingCount(TaskType.RenderShareCard)).toOption.fold(0) { backlog =>") ->
      "0 is the number of renders ENQUEUED, not the backlog: an unreadable backlog admits nothing until the next finished render or prune pass (eebd3eef9's rule)",
    ("worker/src/main/scala/services/sharecards/ShareCardStore.scala", "stamp",
      "Try(Using.resource(Files.newInputStream(path))(_.readNBytes(StampReadBytes))).toOption.fold(Map.empty[String, String]) { head =>") ->
      "an absent or unreadable card reads as unstamped, which re-renders it — the costly-but-safe direction",
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "Try(document.get(\"durationSumMs\").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/UptimeSync.scala", "flag",
      "Try(document.getBoolean(field, false)).getOrElse(false)") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/UptimeSync.scala", "hydrate",
      "bucket.durationSumMs.addAndGet(Try(document.get(\"durationSumMs\").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L))") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "active              = Try(document.getBoolean(\"active\", false)).getOrElse(false),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "consecutiveFailures = Try(document.getInteger(\"consecutiveFailures\", 0)).getOrElse(0),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "alerted             = Try(document.getBoolean(\"alerted\", false)).getOrElse(false),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "failedRuns          = Try(document.getInteger(\"failedRuns\", 0)).getOrElse(0),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/freshness/FreshnessStore.scala", "hydrateInPhases",
      "var loaded  = Try(loadScrape()).getOrElse(false)") ->
      "a boot retry loop: false is \"not loaded yet\" and is retried up to maxScrapeAttempts; readiness is released either way",
    ("common/src/main/scala/services/freshness/FreshnessStore.scala", "hydrateInPhases",
      "loaded = Try(loadScrape()).getOrElse(false)") ->
      "a boot retry loop: false is \"not loaded yet\" and is retried up to maxScrapeAttempts; readiness is released either way",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "amendWaiting",
      "case exception: Throwable =>") ->
      "false files the re-try as not upgraded — which a failed amend is (the mode never reached the task); logged at WARN",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "claim",
      "case Failure(exception) =>") ->
      "None is an idle poll: the worker claims again on its next poll, and nothing is decided from it",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "reapExpiredLeases",
      "case exception: Throwable =>") ->
      "the count only decides whether to ring the doorbell early; a failed reap is retried on the next tick",
    ("common/src/main/scala/services/titlerules/TitleRule.scala", "compiled",
      "try Some(new Regex(pattern)) catch { case _: Throwable => None }") ->
      "an invalid (half-typed) pattern disables only its own rule, and is surfaced to the editor through patternValid",
    ("common/src/main/scala/tools/Env.scala", "readVarsFile",
      "Try {") ->
      ".env.local is a developer convenience: absent or unreadable means no local overrides",
    ("common/src/main/scala/tools/MonitoringHttpFetch.scala", "classify",
      "case _: Exception => None") ->
      "a URL whose host does not parse gets no uptime row; the call itself still goes out and fails on its own",
    ("common/src/main/scala/tools/ThrottledHttpFetch.scala", "isOverload",
      "case _: CircuitOpenException               => false") ->
      "a classifier over an exception already caught, not a handler: a CircuitOpenException is not an overload signal",
    ("web/src/main/scala/services/users/UserRepository.scala", "revokeSessions",
      ".recover { case exception: Throwable =>") ->
      "None is \"not revoked\", and the caller answers 503 (AuthController.revokeAllSessions)",
    ("worker/src/main/scala/modules/wiring/ScrapeWiring.scala", "filmwebFallbackIds",
      "else scala.util.Try(new FilmwebCinemaIdResolver(httpFetch).resolveAll())") ->
      "boot must not fail on Filmweb: with no fallback ids every SourceFallbackScraper serves its primary's real outcome, never an empty success",
    ("worker/src/main/scala/services/cinemas/pl/FilmwebCinemaIdResolver.scala", "resolveAll",
      "Try(parseTowns(HttpRead.page(http, TownsUrl))).getOrElse(Nil).groupMap(_.name)(_.id)") ->
      "no town list leaves every fuzzy cinema Unmatched (reported, never a wrong id) while the pinned overrides — Kinoteka's fallback — still resolve",
    ("worker/src/main/scala/services/cinemas/pl/BokClient.scala", "fetch",
      "Try(HttpRead.page(http, url)).toOption.getOrElse(\"\")") ->
      "a later day's page: the first is fetched outside the Try, so a dead source fails the scrape (ScraperOutageSpec); one failed day is tolerated as ListingPages does",
    ("worker/src/main/scala/services/cinemas/pl/Cinema1Client.scala", "parseScreenHeads",
      "private[cinemas] def parseScreenHeads(raw: String): Map[String, String] = Try {") ->
      "per-film screen names: a malformed answer loses room names, not screenings",
    ("worker/src/main/scala/services/cinemas/pl/Cinema1Client.scala", "parseMovie",
      "private[cinemas] def parseMovie(raw: String): Option[MovieInfo] = Try {") ->
      "per-film detail: None drops one malformed film; the listing itself throws on an unparseable body (ListingParseFailureSpec)",
    ("worker/src/main/scala/services/cinemas/pl/HeliosNuxt.scala", "extractIifeBody",
      "val parameterMap = Try {") ->
      "a NUXT page whose IIFE values do not parse is the degraded redirect page, which HeliosClient.fetch reads as film-less and hands to the empty-scrape guard",
    ("worker/src/main/scala/services/cinemas/pl/KinoPromienClient.scala", "fetch",
      "val (title, dateTimes) = parseDetail(Try(HttpRead.page(http, url)).getOrElse(\"\"), today)") ->
      "one film's detail page: the listing is fetched outside the Try, so a dead source fails the scrape (ScraperOutageSpec); a failed detail loses only its film",
    ("worker/src/main/scala/services/cinemas/uk/OdeonAuthHarvester.scala", "jwtExpiryMillis",
      "} catch { case NonFatal(_) => None }") ->
      "a JWT without a readable exp is treated as expired, so a fresh token is harvested",
    ("worker/src/main/scala/services/cinemas/uk/OdeonAuthHarvester.scala", "meteredBrowserHtml",
      "case NonFatal(e) =>") ->
      "no token: the failure is metered here, and Odeon then throws for want of a token, which falls back to flicks",
    ("worker/src/main/scala/services/metrics/JvmVitalsSampler.scala", "sample",
      "val committed = parseCommittedByCategory(Try(readNmtSummary()).getOrElse(\"\"))") ->
      "/proc and NMT exist only on Linux with -XX:NativeMemoryTracking; elsewhere the gauges stay unset",
    ("worker/src/main/scala/services/metrics/JvmVitalsSampler.scala", "sample",
      "val rss = parseVmRssBytes(Try(readProcStatus()).getOrElse(\"\"))") ->
      "/proc and NMT exist only on Linux with -XX:NativeMemoryTracking; elsewhere the gauges stay unset",
    ("worker/src/main/scala/services/metrics/JvmVitalsSampler.scala", "readProcSelfStatus",
      "Try(new String(Files.readAllBytes(Paths.get(\"/proc/self/status\")), StandardCharsets.UTF_8)).getOrElse(\"\")") ->
      "/proc and NMT exist only on Linux with -XX:NativeMemoryTracking; elsewhere the gauges stay unset",
    ("worker/src/main/scala/services/schedule/ScheduledRunStore.scala", "claim",
      "case exception: Throwable =>") ->
      "fail-closed on purpose: an unclaimable occurrence is skipped rather than risk a double run; the next occurrence claims afresh",
    ("worker/src/main/scala/services/sharecards/PosterPipeline.scala", "vips",
      "if (VipsPosterShrinker.knownFormat(file)) Left(VipsPosterShrinker.failure(child.exitValue(), Try(Files.readString(log)).getOrElse(\"\")))") ->
      "the vips log only decorates a failure already being returned",
    ("worker/src/main/scala/services/sharecards/PosterPipeline.scala", "knownFormat",
      "private[sharecards] def knownFormat(file: Path): Boolean = Try {") ->
      "false routes the file to the JDK decoder, which reports its own failure",
    ("worker/src/main/scala/services/sharecards/ShareCardStore.scala", "usable",
      "def usable: Boolean = Try {") ->
      "a JVM with no share-card mount runs without share cards — the documented meaning of false",
    ("worker/src/main/scala/services/sharecards/ShareCardStore.scala", "deleteFilm",
      "case (path, kind) if Try(Files.getLastModifiedTime(path).toInstant.isBefore(olderThan)).getOrElse(false) && deletePath(path) => kind") ->
      "a file whose age cannot be read is not deleted — the safe direction for a janitor",
    ("worker/src/main/scala/services/sharecards/ShareCardStore.scala", "deletePath",
      "catch { case _: NoSuchFileException => false; case _: IOException => false }") ->
      "a file that could not be deleted is reported as not deleted, which is what happened",
    // ── found once the lint saw chained calls, braces, `Failure(_)`, bare binders and `;` bodies ──
    ("common/src/main/scala/services/MirrorFreshness.scala", "newestIn",
      ".recover { case exception => logger.debug(s\"Mirror freshness read failed: ${exception.getMessage}\"); Seq.empty }") ->
      "the local mirror's age for a dev page: None renders as unknown age, and nothing is decided from it",
    ("common/src/main/scala/services/readmodel/MongoReadModelRepository.scala", "findCard",
      "case Failure(exception) =>") ->
      "read only by ReadModelContentAudit, which skips a card it could not read and audits it on the next pass",
    ("common/src/main/scala/services/readmodel/MongoReadModelRepository.scala", "streamCheckpoint",
      "case Failure(exception) =>") ->
      "None is the documented \"watch from now\"; the projector's boot reconcile covers the gap either way",
    ("common/src/main/scala/services/resolution/ResolutionStore.scala", "removeForFilm",
      "}.recover { case exception =>") ->
      "the count only feeds an INFO line; a forget that failed leaves the entry, which the next forget retries",
    ("common/src/main/scala/services/scrapes/MongoScrapeArchiveRepository.scala", "guard",
      "case Failure(e)     =>") ->
      "the archive is a record of a scrape that already happened: None is \"not archived\", never read as an empty scrape",
    ("common/src/main/scala/services/scrapes/MongoScrapeArchiveRepository.scala", "scanLean",
      "case Failure(e) =>") ->
      "a failed read of the stage relays' days is not empty days: each relay's are UNKNOWN (LeanListing.unread), which the broadcast take waits on and the next read asks again — counted and logged, the rest of the read standing",
    ("common/src/main/scala/services/scrapes/MongoScrapeArchiveRepository.scala", "read",
      "case Failure(exception) =>") ->
      "a row the codec refuses holds no listing to keep: read as absent (logged at WARN) the next scrape replaces it, where a failed read would leave the venue's every scrape undecided",
    ("common/src/main/scala/services/scrapes/MongoScrapeGuardLedger.scala", "attempt",
      "case Failure(e)     =>") ->
      "None IS the ledger's typed \"could not read\" (distinct from Some(Fresh)); ScrapeLanding never writes back over it",
    ("common/src/main/scala/services/tasks/MongoBulkTaskResultStore.scala", "latest",
      ".recover { case exception =>") ->
      "the /tasks page's last-results panel: an unreadable store shows no results, and nothing is decided from it",
    ("common/src/main/scala/tools/HeapDumper.scala", "dump",
      "case Failure(e) =>") ->
      "None is \"no dump was written\", which is what happened; the restart proceeds either way",
    ("web/src/main/scala/services/auth/MongoAuthExchangeCodeStore.scala", "remove",
      ".recover { case exception =>") ->
      "an exchange code that cannot be redeemed fails the sign-in, which the user retries — the safe direction for auth",
    ("worker/src/main/scala/services/cinemas/common/DetailFetchOutcome.scala", "transientToNone",
      "case Failure(_)     => None") ->
      "its contract: a TRANSIENT failure is \"no detail this time\" and retried next tick; a durable one is rethrown above",
    ("worker/src/main/scala/services/cinemas/pl/HeliosClient.scala", "parseApiScreenings",
      "Try(Json.parse(body).as[JsArray]).map { array =>") ->
      "room/format enrichment of screenings the NUXT listing already carries: a malformed body loses rooms, not screenings",
    ("worker/src/main/scala/services/cinemas/pl/HeliosClient.scala", "parseEventScreenings",
      "Try(Json.parse(body).as[JsArray]).map { array =>") ->
      "room/format enrichment of screenings the NUXT listing already carries: a malformed body loses rooms, not screenings",
    ("worker/src/main/scala/services/tasks/MongoChunkScrapeStore.scala", "startRun",
      "case e: Throwable => logger.warn(s\"startRun insert for $cinema failed: ${e.getMessage}\"); false") ->
      "false is \"not inserted\"; the supersede step below then decides, and a run that cannot start is skipped this tick",
    ("worker/src/main/scala/services/tasks/MongoChunkScrapeStore.scala", "startRun",
      "}.recover { case e => logger.warn(s\"startRun replace for $cinema failed: ${e.getMessage}\"); None }") ->
      "None is \"no run started\": the chunked scrape is skipped this tick and the next reaper tick tries again",
    // ── moved off the TODO-HttpRead backlog, each read and judged ──
    ("worker/src/main/scala/services/cinemas/common/GatsbyBoxOfficeParser.scala", "parseDetails",
      "Try(Json.parse(json)).toOption.flatMap(_.asOpt[Seq[JsValue]]).getOrElse(Nil).flatMap { n =>") ->
      "the film details are optional credits beside the schedule (GatsbyBoxOfficeClient.ask's own allowlist entry): an unparseable answer lists the films without credits, never without screenings",
    ("worker/src/main/scala/services/cinemas/es/OcineParser.scala", "filmIds",
      "val root = Try(Json.parse(json)).getOrElse(JsNull)") ->
      "not a swallow into data: a body that is not JSON yields None, which OcineClient turns into a failed scrape; only a parsed pelicules array is an answer",
    ("worker/src/main/scala/services/cinemas/pl/BiletynaClient.scala", "parseEvents",
      "Try(Json.parse(block)).toOption.toSeq.flatMap { json =>") ->
      "one of several JSON-LD blocks on a page that answered; a block that is not JSON (a malformed CMS embed) is skipped, the page's other blocks stand",
    ("worker/src/main/scala/services/cinemas/pl/BiletynaClient.scala", "pageEventCount",
      "Try(Json.parse(block)).toOption.flatMap(json => (json \\ \"events\").asOpt[JsArray]).fold(0)(_.value.size)") ->
      "counts the events the same blocks parseEvents reads, for the paging cap; a block it cannot parse is one parseEvents skipped too",
    ("worker/src/main/scala/services/cinemas/pl/CharlieClient.scala", "parseEvent",
      "Try(Json.parse(block)).toOption") ->
      "one JSON-LD block of many on a page that answered — most are not ScreeningEvents at all; a malformed one is skipped",
    ("worker/src/main/scala/services/cinemas/pl/CharlieMonroeClient.scala", "parseHtml",
      ".flatMap(element => Try(Json.parse(element.data())).toOption)") ->
      "the page's JSON-LD blocks, of which only ScreeningEvents count; a malformed block is skipped, the page that carried it answered",
    ("worker/src/main/scala/services/cinemas/pl/CinemaCityClient.scala", "parseDetails",
      "val js  = FilmDetailsRe.findFirstMatchIn(html).flatMap(m => Try(Json.parse(m.group(1))).toOption)") ->
      "optional fields (cast, runtime) off a film-details page; a missing or malformed embedded JSON leaves them empty, the listing comes from the dates/events API",
    ("worker/src/main/scala/services/cinemas/pl/FilmwebShowtimesClient.scala", "parseCinemaInfo",
      "Try(Json.parse(body)).toOption.flatMap { js =>") ->
      "only feeds resolveSourceUrl, the /uptime link; None keeps the /cinema/-<id> fallback link, and nothing is decided from it",
    ("worker/src/main/scala/services/cinemas/pl/HeliosClient.scala", "window",
      "Try((Json.parse(body) \\ \"name\").asOpt[String]).toOption.flatten.map(id -> _)") ->
      "a screen's display name for room enrichment; an unparseable screen body loses the room name, not a screening",
    ("worker/src/main/scala/services/cinemas/pl/HeliosClient.scala", "parseApiMovieBody",
      "Try(Json.parse(body)).toOption.flatMap { js =>") ->
      "per-film REST metadata enriching films the NUXT listing already carries; a malformed body loses those fields, not the film",
    ("worker/src/main/scala/services/cinemas/uk/CineworldParser.scala", "parseMovieDetail",
      "Try(Json.parse(json)).toOption") ->
      "a deferred detail read: None is DetailFetchOutcome.Failed, which retries the detail next tick; it is never stored as 'no fields'",
    ("worker/src/main/scala/services/cinemas/us/AmcParser.scala", "items",
      "val parsed = Try(Json.parse(json)).getOrElse(") ->
      "not a swallow: the getOrElse THROWS (\"AMC response was not JSON\"), turning a parse failure into a named failed read",
    ("worker/src/main/scala/services/cinemas/us/RegalParser.scala", "body",
      "Try(Json.parse(json)).getOrElse(") ->
      "not a swallow: the getOrElse THROWS (\"…not JSON\"), turning a parse failure into a named failed read",
    ("worker/src/main/scala/services/enrichment/scraping/JsonLdAggregateRating.scala", "of",
      "def of(html: String): JsonLd = JsonLd(scripts(html).flatMap(raw => Try(Json.parse(raw)).toOption))") ->
      "the JSON-LD blocks of a rating page that answered (read through HttpRead); a malformed block is skipped and the page's other blocks still give the score",
    ("worker/src/main/scala/services/enrichment/scraping/RottenTomatoesScorecard.scala", "criticsScore",
      "Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>") ->
      "a data island inside a page that answered (read through HttpRead); JSON-LD is the fallback parseScore tries next, so a malformed island is not 'no score' by itself",
    ("worker/src/main/scala/tools/ParallelDetailFetch.scala", "timed",
      "case _: TimeoutException     => attempt.cancel(true); timedOut.add(url); None") ->
      "a timed-out fetch is recorded in `timedOut`, which the caller reports and meters; None only drops it from the results"
  )

  /** The source with every comment blanked to spaces (newlines kept, so offsets and line
   *  numbers survive) — a Scaladoc quoting the bad shape must not trip the lint. */
  private def withoutComments(src: String): String = {
    val out = new StringBuilder(src)
    var i = 0
    var inString = false
    def blank(from: Int, until: Int): Unit =
      (from until until).foreach(k => if (out(k) != '\n') out(k) = ' ')
    while (i < src.length) {
      val c = src(i)
      if (inString) {
        if (c == '\\') i += 1
        else if (c == '"' || c == '\n') inString = false
        i += 1
      } else if (src.startsWith("\"\"\"", i)) {
        val end = src.indexOf("\"\"\"", i + 3)
        i = if (end < 0) src.length else end + 3
      } else if (c == '"') { inString = true; i += 1 }
      else if (src.startsWith("//", i)) {
        val end = src.indexOf('\n', i)
        val stop = if (end < 0) src.length else end
        blank(i, stop); i = stop
      } else if (src.startsWith("/*", i)) {
        val end = src.indexOf("*/", i + 2)
        val stop = if (end < 0) src.length else end + 2
        blank(i, stop); i = stop
      } else if (c == '\'' && i + 2 < src.length && src(i + 2) == '\'') i += 3   // a char literal such as '(' or '"'
      else i += 1
    }
    out.toString
  }

  /** Index of the bracket closing the one at `open`, skipping string literals. */
  private def closing(src: String, open: Int): Int = {
    var depth = 0
    var i = open
    while (i < src.length) {
      src(i) match {
        case '"' =>
          if (src.startsWith("\"\"\"", i)) { val e = src.indexOf("\"\"\"", i + 3); i = if (e < 0) src.length else e + 2 }
          else { i += 1; while (i < src.length && src(i) != '"') { if (src(i) == '\\') i += 1; i += 1 } }
        case '\'' if i + 2 < src.length && src(i + 2) == '\'' => i += 2
        case '(' | '{' | '[' => depth += 1
        case ')' | '}' | ']' =>
          depth -= 1
          if (depth == 0) return i
        case _ =>
      }
      i += 1
    }
    -1
  }

  /** Index of the `{` of the block enclosing `at`, or -1. */
  private def enclosingBrace(src: String, at: Int): Int = {
    var depth = 0
    var k = at - 1
    while (k >= 0) {
      src(k) match {
        case '}' => depth += 1
        case '{' => if (depth == 0) return k else depth -= 1
        case _ =>
      }
      k -= 1
    }
    -1
  }

  /** A `case` body: from after `=>` to the next `case` at the same depth, or the block's end. */
  private def caseBody(src: String, from: Int): String = {
    var depth = 0
    var i = from
    while (i < src.length) {
      src(i) match {
        case '(' | '{' | '[' => depth += 1
        case ')' | '}' | ']' => if (depth == 0) return src.substring(from, i) else depth -= 1
        case 'c' if depth == 0 && src.startsWith("case ", i) && (i == 0 || !src(i - 1).isLetterOrDigit) =>
          return src.substring(from, i)
        case _ =>
      }
      i += 1
    }
    src.substring(from)
  }

  /** Skip the `.name(args)` / `.name` calls chained after a `Try(...)`, stopping at the answer
   *  this lint looks for (`getOrElse`, `toOption`, `fold`) or a `recover` — the offset it starts at. */
  private def afterChain(src: String, from: Int): Int = {
    val Call = """^\s*\.\s*(\w+)""".r
    var i = from
    var more = true
    while (more) Call.findPrefixMatchOf(src.substring(i)) match {
      // …and at a `recover`: its handler is judged by the handler rule, once, not twice.
      case Some(m) if !Set("getOrElse", "toOption", "fold", "recover", "recoverWith")(m.group(1)) =>
        val after = i + m.end
        val next  = src.indexWhere(c => !c.isWhitespace, after)
        i = if (next >= 0 && (src(next) == '(' || src(next) == '{')) closing(src, next) + 1 else after
      case _ => more = false
    }
    i
  }

  private final case class Site(file: String, line: Int, owner: String, text: String) {
    def key: (String, String, String) = (file, owner, text)
    override def toString = s"$file:$line  (in $owner)  $text"
  }

  // A site's owner: the method it is in, or the member `val`/`lazy val` it initialises (a
  // local `val` inside a method is not an owner — the method is). Member-level is judged by
  // indentation: at most one level in from its class.
  private val Def: Regex = """(?m)(?:\bdef\s+([\w$]+)|^ {0,2}(?:(?:private|protected|override|final|lazy|implicit)(?:\[\w+\])?\s+)*(?:val|var)\s+([\w$]+))""".r

  private def siteAt(file: String, src: String, lines: Array[String], at: Int): Site = {
    val before = src.substring(0, at)
    val n      = before.count(_ == '\n')
    // The site's method, near enough to key on: the `def` its own line declares
    // (`def usable: Boolean = Try {`), else the last one above it.
    val owner  = Def.findFirstMatchIn(lines(n)).orElse(Def.findAllMatchIn(before).toSeq.lastOption)
      .map(m => Option(m.group(1)).getOrElse(m.group(2))).getOrElse("<class body>")
    Site(file, n + 1, owner, lines(n).trim)
  }

  private def swallows(file: String, raw: String): Seq[Site] = {
    val src = withoutComments(raw)
    val lines = raw.split("\n", -1)
    def site(at: Int): Site = siteAt(file, src, lines, at)
    val tries = TryOpen.findAllMatchIn(src).flatMap { m =>
      val close = closing(src, m.end - 1)
      Option.when(close > 0 && EmptyOnFailure.findPrefixOf(src.substring(afterChain(src, close + 1))).isDefined)(site(m.start))
    }
    val handlers = HandlerCase.findAllMatchIn(src).flatMap { m =>
      val brace   = enclosingBrace(src, m.start)
      val handler = !BlockOnlyHandler.matches(m.group(1)) || (brace > 0 && HandlerBlock.findFirstIn(src.substring(0, brace)).isDefined)
      // The body's LAST statement, whether statements are split by lines or by `;`.
      val last    = caseBody(src, m.end).split("[\n;]").map(_.trim).filter(_.nonEmpty).lastOption
      Option.when(handler && last.exists(l => EmptyLine.matches(l)))(site(m.start))
    }
    (tries ++ handlers ++ swallowedReads(file, raw) ++ swallowedRepositoryReads(file, raw)).toSeq.distinct.sortBy(_.line)
  }

  private lazy val found: Seq[Site] = scalaFiles(MainRoots).flatMap { p =>
    swallows(p.toString, read(p))
  }

  /** Where shape 3 applies: code that reads an upstream. */
  private val ClientRoots: Seq[String] = Seq(
    "worker/src/main/scala/services/cinemas/", "worker/src/main/scala/services/enrichment/",
    "worker/src/main/scala/services/TmdbClient.scala", "common/src/main/scala/tools/")
  private def isClient(file: String): Boolean = ClientRoots.exists(file.startsWith)

  // An HTTP read (a receiver named like a fetch: `http`, `httpFetch`, `bnFetch`…), TmdbClient's
  // `httpGet`, or a parse.
  private val ReadCall: Regex =
    """\b\w*(?:[Hh]ttp|[Ff]etch)\w*\s*\.\s*(?:get|getBytes|post|getAsync)\s*\(|\bhttpGet\s*\(|\b(?:Json|Jsoup)\s*\.\s*parse\w*\s*\(|\bparse\w*\s*\(""".r
  // Parses of one scalar field — a date, a number — are a field's default, not a read.
  private val ScalarParse: Regex =
    """\b(?:LocalDate|LocalTime|LocalDateTime|Instant|ZonedDateTime|OffsetDateTime|YearMonth|Year|MonthDay|Duration|Period)\s*\.\s*parse\s*\(|\bparse(?:Int|Long|Double|Float|Boolean)\s*\(""".r
  private val SwallowingAnswer: Regex = """^\s*\.\s*(?:toOption|getOrElse)\b""".r

  /** Shape 3: a `Try` around a read or parse, answered with `.toOption` / `.getOrElse`. */
  private def swallowedReads(file: String, raw: String): Seq[Site] =
    if (!isClient(file)) Nil
    else {
      val src = withoutComments(raw)
      val lines = raw.split("\n", -1)
      TryOpen.findAllMatchIn(src).flatMap { m =>
        val close = closing(src, m.end - 1)
        val body  = if (close > 0) ScalarParse.replaceAllIn(src.substring(m.end, close), "") else ""
        Option.when(close > 0 && ReadCall.findFirstIn(body).isDefined &&
          SwallowingAnswer.findPrefixOf(src.substring(afterChain(src, close + 1))).isDefined)(siteAt(file, src, lines, m.start))
      }.toSeq
    }

  // An awaited Mongo read: a find, one document, a count, the index list, an aggregate, a findOneAnd….
  private val MongoReadCall: Regex =
    """\.\s*(?:find|first|headOption|countDocuments|estimatedDocumentCount|listIndexes|aggregate|distinct|findOneAndDelete|findOneAndUpdate|findOneAndReplace)\b\s*[\[(]""".r
  private val AnyAnswer: Regex = """^\s*\.\s*(?:toOption|getOrElse|recover|recoverWith|fold)\b""".r

  /** Shape 4: a `Try` around an awaited Mongo read in repository code, answered with anything but
   *  the failure itself. */
  private def swallowedRepositoryReads(file: String, raw: String): Seq[Site] =
    if (!raw.contains("MongoCollection") && !raw.contains("MongoDatabase")) Nil
    else {
      val src = withoutComments(raw)
      val lines = raw.split("\n", -1)
      TryOpen.findAllMatchIn(src).flatMap { m =>
        val close = closing(src, m.end - 1)
        val body  = if (close > 0) src.substring(m.end, close) else ""
        Option.when(close > 0 && body.contains("Await.result(") && MongoReadCall.findFirstIn(body).isDefined &&
          AnyAnswer.findPrefixOf(src.substring(afterChain(src, close + 1))).isDefined)(siteAt(file, src, lines, m.start))
      }.toSeq
    }

  private val Allowed: Map[(String, String, String), String] = Allowlist ++ HttpReadBacklog.Swallows ++ RepositoryReadSwallows.Swallows

  "Main sources" should "not answer a failed fetch, read or decode with an empty value, outside the allowlist" in {
    val offenders = found.filterNot(s => Allowed.contains(s.key))
    withClue(s"${offenders.size} site(s) turn a failure into data. Propagate it, or give the result a way to " +
      s"say 'unknown': read HTTP through tools.HttpRead (a ReadOutcome), a venue detail page through " +
      s"DetailFetchOutcome.page, Mongo through tools.MongoRead / a ScanOutcome. If empty really is right here, " +
      s"allowlist the site as (file, enclosing method, trimmed line) -> WHY — Allowlist here for HTTP/parse, " +
      s"RepositoryReadSwallows for a Mongo read (HttpReadBacklog only shrinks):\n  " +
      offenders.mkString("\n  ") + "\n") {
      offenders shouldBe empty
    }
  }

  "The allowlist" should "name only sites that still exist" in {
    val live  = found.map(_.key).toSet
    val stale = Allowed.keySet.filterNot(live)
    withClue(s"stale entries (the site was fixed or moved — drop or re-key them): ${stale.mkString(", ")} — ") {
      stale shouldBe empty
    }
  }

  "The owner of a site" should "be the member val it initialises, not the def above it" in {
    val src =
      """object A {
        |  def before(): Int = 1
        |  private lazy val vars: Map[String, String] =
        |    Try {
        |      read()
        |    }.getOrElse(Map.empty)
        |  def after(): Int = {
        |    val local = Try(x()).getOrElse(0)
        |    local
        |  }
        |}""".stripMargin
    swallows("A.scala", src).map(_.owner) shouldBe Seq("vars", "after")
  }

  "The lint" should "flag both shapes, and nothing in a comment" in {
    val src =
      """object A {
        |  val a = Try(http.get(url)).getOrElse(Seq.empty)
        |  val b = Try { read() }.toOption.getOrElse(0)
        |  val c = try count() catch { case NonFatal(e) =>
        |    log(e)
        |    0L
        |  }
        |  val d = f.recover { case _ => None }
        |  val e = x match { case _ => None }
        |  // Try(http.get(url)).getOrElse(Nil)
        |  val f = Try(parse(s)).getOrElse(fallback)
        |  val g = Try(count()).toOption.fold(0)(_.toInt)
        |  val h = Try(count()).fold(_ => Nil, rows => rows)
        |  val i = Try(http.get(u)).map(identity).getOrElse("")
        |  val j = Try(rows()).getOrElse { Nil }
        |  val k = Try(rows()) match { case Failure(_) => Seq.empty; case Success(r) => r }
        |  val l = f.recover { case e => Nil }
        |  val m = f.recover { case e: Exception => log(e); Nil }
        |  val n = f.recoverWith { case NonFatal(e) => Future.successful(Nil) }
        |  val o = Try(index()).getOrElse(Map.empty[String, Seq[Int]])
        |  val p = Try(pick()).getOrElse(Option.empty[String])
        |  val q = rows match { case row => row }
        |}""".stripMargin
    swallows("A.scala", src).map(_.line) shouldBe Seq(2, 3, 4, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
  }

  "Shape 4" should "flag an awaited Mongo read answered with anything but its failure, in repository code only" in {
    val src =
      """object A {
        |  val c: MongoCollection[Document] = ???
        |  val a = Try(Await.result(c.find(f).headOption(), t)).toOption.flatten
        |  val b = Try(Await.result(c.countDocuments().toFuture(), t)).recover { case e => log(e); -1L }.get
        |  val d = Try(Await.result(c.find(f).toFuture(), t)).map(_.size).getOrElse(fallback)
        |  val e = Try(Await.result(c.insertOne(doc).toFuture(), t)).recover { case e => log(e) }
        |  val g = Try(Await.result(c.find(f).toFuture(), t))
        |  val h = MongoRead.one("row", t)(c.find(f).headOption())
        |  val i = Try(Await.result(c.find[Document](f).first().toFuture(), t)).fold(_ => None, Option(_))
        |}""".stripMargin
    swallows("common/src/main/scala/services/A.scala", src).map(_.line) shouldBe Seq(3, 4, 5, 9)
    // Without a collection in the file only shape 1 (the `fold` to None) is left.
    swallows("common/src/main/scala/services/A.scala", src.replace("MongoCollection", "Store")).map(_.line) shouldBe Seq(9)
  }

  "Shape 3" should "flag a read or parse inside a swallowing Try in client code, and only there" in {
    val src =
      """object A {
        |  val a = Try(http.get(url)).toOption
        |  val b = Try(Json.parse(body)).getOrElse(JsNull)
        |  val c = Try(parseInfo(b)).toOption.flatten
        |  val d = Try(LocalDate.parse(s)).toOption
        |  val e = Try(s.toInt).toOption
        |  val f = Try(Jsoup.parse(http.get(u))).toOption
        |  val g = Try(bnFetch.post(u, b)).map(identity).getOrElse(fallback)
        |  val h = Try(http.get(url))
        |  val i = Try(http.get(url)).getOrElse("[]")
        |  val j = Try(Integer.parseInt(s)).getOrElse(fallback)
        |}""".stripMargin
    swallows("worker/src/main/scala/services/cinemas/pl/A.scala", src).map(_.line) shouldBe Seq(2, 3, 4, 7, 8, 10)
    // Outside client code only the empty-JSON-body answer (shape 1) is flagged.
    swallows("worker/src/main/scala/services/movies/A.scala", src).map(_.line) shouldBe Seq(10)
  }
}
