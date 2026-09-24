package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
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
 * Where the empty answer is genuinely right — an optional field's default while decoding,
 * a probe whose failure means "not available here", a retry loop's "not yet" — add the
 * site to [[Allowlist]] with WHY. The reason is the review: "defensive" is not one. Where
 * it is not right, propagate the failure, or give the result a type that can say "unknown"
 * (as `TaskQueue.waitingCount` and `EnqueueResult.Failed` now do).
 */
class NoSwallowedFailureSpec extends AnyFlatSpec with Matchers {

  private val MainRoots = Seq("common/src/main", "web/src/main", "worker/src/main")

  private val Empty: String =
    """(?:(?:Seq|List|Vector|Map|Set|Iterable)\.empty(?:\[[^\]]*\])?|Nil|(?:Seq|List|Vector|Map|Set)\(\)|""" +
      """0|0L|0\.0|false|None|""|Json\.obj\(\))"""

  private val TryOpen: Regex     = """\bTry\s*[({]""".r
  // What a `Try(...)` may be followed by to answer its failure with `<empty>`: `.getOrElse`,
  // `.toOption.getOrElse`, `.toOption.fold(<empty>)(…)` (eebd3eef9's exact shape), and
  // `.fold(_ => <empty>, …)`.
  private val EmptyOnFailure     = ("""^\s*(?:(?:\.toOption\s*)?\.getOrElse\s*\(\s*""" + Empty + """\s*\)""" +
    """|\.toOption\s*\.fold\s*\(\s*""" + Empty + """\s*\)""" +
    """|\.fold\s*\(\s*\w+\s*=>\s*""" + Empty + """\s*,)""").r
  private val HandlerCase: Regex = """\bcase\s+(_|NonFatal\(\s*\w+\s*\)|\w+\s*:\s*(?:Throwable|Exception|\w+Exception))\s*=>""".r
  private val HandlerBlock       = """(?:\bcatch|\brecover|\brecoverWith)\s*$""".r
  private val EmptyLine          = ("""^""" + Empty + """$""").r

  /** Site (repository-relative file, the nearest `def` at or above it, the flagged line
   *  trimmed) → why the empty answer is right. Keyed by the method rather than a line
   *  number so an entry survives unrelated edits above it, and a new swallow elsewhere in
   *  the file is not covered by an old entry that happens to share its text. */
  private val Allowlist: Map[(String, String, String), String] = Map(
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "Try(document.getList(\"errors\", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("worker/src/main/scala/services/sharecards/ShareCardBackfill.scala", "tick",
      "Try(queue.waitingCount(TaskType.RenderShareCard)).toOption.fold(0) { backlog =>") ->
      "0 is the number of renders ENQUEUED, not the backlog: an unreadable backlog admits nothing this tick (eebd3eef9's rule)",
    ("worker/src/main/scala/services/sharecards/ShareCardStore.scala", "stamp",
      "Try(Using.resource(Files.newInputStream(path))(_.readNBytes(StampReadBytes))).toOption.fold(Map.empty[String, String]) { head =>") ->
      "an absent or unreadable card reads as unstamped, which re-renders it — the costly-but-safe direction",
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "Try(document.get(\"durationSumMs\").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "Try(document.getBoolean(\"fallback\", false)).getOrElse(false)") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/UptimeSync.scala", "hydrate",
      "bucket.durationSumMs.addAndGet(Try(document.get(\"durationSumMs\").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L))") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/UptimeSync.scala", "hydrate",
      "if (Try(document.getBoolean(\"fallback\", false)).getOrElse(false)) bucket.fallback.set(true)") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "active              = Try(document.getBoolean(\"active\", false)).getOrElse(false),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "consecutiveFailures = Try(document.getInteger(\"consecutiveFailures\", 0)).getOrElse(0),") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/fallback/FallbackStore.scala", "instant",
      "alerted             = Try(document.getBoolean(\"alerted\", false)).getOrElse(false)") ->
      "an optional field's default while decoding one document: absent on documents written before the field existed",
    ("common/src/main/scala/services/freshness/FreshnessStore.scala", "hydrateInPhases",
      "var loaded  = Try(loadScrape()).getOrElse(false)") ->
      "a boot retry loop: false is \"not loaded yet\" and is retried up to maxScrapeAttempts; readiness is released either way",
    ("common/src/main/scala/services/freshness/FreshnessStore.scala", "hydrateInPhases",
      "loaded = Try(loadScrape()).getOrElse(false)") ->
      "a boot retry loop: false is \"not loaded yet\" and is retried up to maxScrapeAttempts; readiness is released either way",
    ("common/src/main/scala/services/staging/MongoStagingFolder.scala", "landed",
      "staging: MongoCollection[StoredMovieDto]): Boolean = Try {") ->
      "false is \"not landed\": the fold reschedules and re-verifies — the fail-safe direction, never a false \"done\"",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "amendWaiting",
      "case exception: Throwable =>") ->
      "false files the re-try as not upgraded — which a failed amend is (the mode never reached the task); logged at WARN",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "claim",
      "case exception: Throwable =>") ->
      "None is an idle poll: the worker claims again on its next poll, and nothing is decided from it",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "reapExpiredLeases",
      "case exception: Throwable =>") ->
      "the count only decides whether to ring the doorbell early; a failed reap is retried on the next tick",
    ("common/src/main/scala/services/titlerules/TitleRule.scala", "appliesTo",
      "try Some(new Regex(pattern)) catch { case _: Throwable => None }") ->
      "an invalid (half-typed) pattern disables only its own rule, and is surfaced to the editor through patternValid",
    ("common/src/main/scala/tools/Env.scala", "positiveLong",
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
    ("worker/src/main/scala/modules/wiring/ScrapeWiring.scala", "scrapeAttemptCeiling",
      "else scala.util.Try(new FilmwebCinemaIdResolver(httoFetch).resolveAll())") ->
      "boot must not fail on Filmweb: with no fallback ids every SourceFallbackScraper serves its primary's real outcome, never an empty success",
    ("worker/src/main/scala/services/cinemas/pl/BokClient.scala", "fetch",
      "Try(http.get(url)).toOption.getOrElse(\"\")") ->
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
      "val (title, dateTimes) = parseDetail(Try(http.get(url)).getOrElse(\"\"), today)") ->
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
    ("worker/src/main/scala/services/tasks/ScrapeReaper.scala", "venuesWithin",
      "val n = Try(enqueueUpTo(group, group.size)).getOrElse(0)") ->
      "the count only feeds an INFO line; a failed enqueue is reported by the queue itself (EnqueueResult.Failed, logged and metered)"
  )

  private def scalaFiles(roots: Seq[String]): Seq[Path] =
    roots.map(Paths.get(_)).filter(Files.isDirectory(_)).flatMap { root =>
      Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq
    }.sortBy(_.toString)

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

  private final case class Site(file: String, line: Int, owner: String, text: String) {
    def key: (String, String, String) = (file, owner, text)
    override def toString = s"$file:$line  (in $owner)  $text"
  }

  private val Def: Regex = """\bdef\s+([\w$]+)""".r

  private def swallows(file: String, raw: String): Seq[Site] = {
    val src = withoutComments(raw)
    val lines = raw.split("\n", -1)
    def site(at: Int): Site = {
      val before = src.substring(0, at)
      val n      = before.count(_ == '\n')
      // The site's method, near enough to key on: the `def` its own line declares
      // (`def usable: Boolean = Try {`), else the last one above it.
      val owner  = Def.findFirstMatchIn(lines(n)).orElse(Def.findAllMatchIn(before).toSeq.lastOption)
        .map(_.group(1)).getOrElse("<class body>")
      Site(file, n + 1, owner, lines(n).trim)
    }
    val tries = TryOpen.findAllMatchIn(src).flatMap { m =>
      val close = closing(src, m.end - 1)
      Option.when(close > 0 && EmptyOnFailure.findPrefixOf(src.substring(close + 1)).isDefined)(site(m.start))
    }
    val handlers = HandlerCase.findAllMatchIn(src).flatMap { m =>
      val brace   = enclosingBrace(src, m.start)
      val handler = m.group(1) != "_" || (brace > 0 && HandlerBlock.findFirstIn(src.substring(0, brace)).isDefined)
      val last    = caseBody(src, m.end).split("\n").map(_.trim).filter(_.nonEmpty).lastOption
      Option.when(handler && last.exists(l => EmptyLine.matches(l)))(site(m.start))
    }
    (tries ++ handlers).toSeq.sortBy(_.line)
  }

  private lazy val found: Seq[Site] = scalaFiles(MainRoots).flatMap { p =>
    swallows(p.toString, new String(Files.readAllBytes(p), StandardCharsets.UTF_8))
  }

  "Main sources" should "not answer a failed fetch, read or decode with an empty value, outside the allowlist" in {
    val offenders = found.filterNot(s => Allowlist.contains(s.key))
    withClue(s"${offenders.size} site(s) turn a failure into data. Propagate it, or give the result a way to " +
      s"say 'unknown' — or, if empty really is right here, allowlist the site with the reason:\n  " +
      offenders.mkString("\n  ") + "\n") {
      offenders shouldBe empty
    }
  }

  "The allowlist" should "name only sites that still exist" in {
    val live  = found.map(_.key).toSet
    val stale = Allowlist.keySet.filterNot(live)
    withClue(s"stale entries (the site was fixed or moved — drop or re-key them): ${stale.mkString(", ")} — ") {
      stale shouldBe empty
    }
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
        |}""".stripMargin
    swallows("A.scala", src).map(_.line) shouldBe Seq(2, 3, 4, 8, 12, 13)
  }
}
