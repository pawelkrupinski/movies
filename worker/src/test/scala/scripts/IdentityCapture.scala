package scripts

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import org.mongodb.scala.ObservableFuture
import scala.util.Try

/**
 * The unmatched-cluster fixture's recapture in ONE command (`scripts/identity-capture.sh [cc...]`, which compiles the
 * integration classpath and runs this on it): the newest successful "Record scrape fixtures" run's corpora and the
 * enrichment trees recorded with them, fetched unless already here; prod's `identity_family_answers` exported read-only;
 * then each country's `UnmatchedClustersCaptureIntegrationSpec` in a JVM of its own, every variable it reads defaulted
 * ([[environment]]), and a report of each phase's time and throughput.
 *
 * Everything that touches the network, prod, git or a child process sits behind [[Effects]]: `--dry-run` swaps in
 * [[DryRun]], which prints what the real run would do and touches nothing.
 *
 * {{{
 *   scripts/identity-capture.sh [--dry-run] [--heap 12g] [cc...]
 * }}}
 */
object IdentityCapture {

  /** The countries the recording records, LARGEST FIRST — by their enrichment tree's size (2026-10-06: us 293 MB,
   *  de 244, uk 211, pl 127, es 42), the capture's work growing with the corpus it resolves. */
  val Countries: Seq[String] = Seq("us", "de", "uk", "pl", "es")

  /** The spec each country's JVM runs. */
  val CaptureSpec = "integration.UnmatchedClustersCaptureIntegrationSpec"

  /** The prod Mongo the family answers are exported from (read-only): set by the shell script, after its tunnel. */
  val FamilyUri = "KINOWO_IDENTITY_FAMILY_URI"

  /** The local, THROWAWAY Mongo the capture's pipeline seeds (the it/ one). */
  val LocalMongo = "mongodb://127.0.0.1:28017/?directConnection=true"

  /** Where everything the capture reads is kept between runs. */
  final case class Layout(repo: Path, work: Path) {
    def corpusDir: Path      = work.resolve("corpus")
    def trees: Path          = work.resolve("trees")
    def fixtureRoot: Path    = trees.resolve("test/resources/fixtures")
    def agreementCache: Path = work.resolve("agreement-cache")
    def families: Path       = work.resolve("families")
    def posters: Path        = work.resolve("posters")
    def logs: Path           = work.resolve("logs")
    def fixtures: Path       = repo.resolve("test/resources/fixtures/identity-unmatched")
    /** The recording run a country's downloaded corpus and tree came from. */
    def recorded(cc: String): Path = work.resolve(s"recorded-$cc.run")
  }

  // ── arguments ────────────────────────────────────────────────────────────────────────────

  final case class Options(countries: Seq[String] = Countries, dryRun: Boolean = false, heap: String = "12g", forced: Option[Mode] = None)

  def parse(args: Seq[String]): Either[String, Options] = {
    @scala.annotation.tailrec def go(rest: List[String], o: Options, named: List[String]): Either[String, Options] = rest match {
      case Nil                     => Right(if (named.isEmpty) o else o.copy(countries = Countries.filter(named.contains)))
      case "--dry-run" :: tail     => go(tail, o.copy(dryRun = true), named)
      case "--heap" :: heap :: tail => go(tail, o.copy(heap = heap), named)
      case ("--capture" | "--fill") :: _ if o.forced.isDefined => Left("--capture and --fill: one or the other")
      case "--capture" :: tail     => go(tail, o.copy(forced = Some(Mode.Capture)), named)
      case "--fill" :: tail        => go(tail, o.copy(forced = Some(Mode.Fill)), named)
      case flag :: _ if flag.startsWith("-") => Left(s"unknown option $flag")
      case cc :: tail =>
        val code = cc.toLowerCase(java.util.Locale.ROOT)
        if (Countries.contains(code)) go(tail, o, code :: named) else Left(s"no recorded corpus for country '$cc' (one of ${Countries.mkString(", ")})")
    }
    go(args.toList, Options(), Nil)
  }

  // ── capture or fill ──────────────────────────────────────────────────────────────────────

  /** A CAPTURE resolves the whole corpus again and records the clusters left unmatched and every answer read for them
   *  (`UnmatchedClustersCaptureIntegrationSpec`: hours, prod's family answers, the recording). A FILL keeps the
   *  fixture's clusters and decisions and only answers the questions a change newly asks of them
   *  (`UnmatchedClustersFillIntegrationSpec`: minutes). */
  enum Mode { case Capture, Fill }

  /** What a fixture's decisions are a function of: the recording whose corpus was resolved, and the code that decides
   *  them ([[isDecisionInput]]), hashed. Stamped beside the fixture as `<cc>.inputs` by every capture. */
  final case class Inputs(corpusRun: String, code: String) {
    def render: String = s"recording\t$corpusRun\ncode\t$code\n"
  }
  object Inputs {
    def parse(text: String): Option[Inputs] = {
      val fields = text.linesIterator.map(_.split("\t", 2)).collect { case Array(k, v) => k -> v.trim }.toMap
      for (run <- fields.get("recording"); code <- fields.get("code")) yield Inputs(run, code)
    }
  }

  final case class Choice(cc: String, mode: Mode, why: String)

  /** Fill when the fixture exists and its decisions are unchanged under the current code — the same recording, the same
   *  decision code — else capture; `forced` (`--capture` / `--fill`) overrides, but a fill needs a fixture to fill. */
  def choose(cc: String, fixture: Boolean, stamped: Option[Inputs], current: Option[Inputs], forced: Option[Mode]): Choice =
    forced match {
      case Some(Mode.Fill) if !fixture => throw new IllegalArgumentException(s"--fill: $cc has no $cc.json.gz to fill — capture it first")
      case Some(mode)                  => Choice(cc, mode, s"--${mode.toString.toLowerCase(java.util.Locale.ROOT)} given")
      case None if !fixture            => Choice(cc, Mode.Capture, s"no $cc.json.gz yet")
      case None => (stamped, current) match {
        case (None, _) => Choice(cc, Mode.Capture, s"$cc.inputs is missing: the fixture's inputs are unknown")
        case (_, None) => Choice(cc, Mode.Capture, "no recording at hand to compare the fixture's with")
        case (Some(was), Some(now)) if was.corpusRun != now.corpusRun =>
          Choice(cc, Mode.Capture, s"the corpus moved: captured from recording ${was.corpusRun}, now ${now.corpusRun}")
        case (Some(was), Some(now)) if was.code != now.code =>
          Choice(cc, Mode.Capture, s"the resolver's code changed since the capture (${was.code} → ${now.code})")
        case (_, Some(now)) =>
          Choice(cc, Mode.Fill, s"the fixture's decisions are current: recording ${now.corpusRun}, resolver code ${now.code}")
      }
    }

  /** The code a capture's DECISIONS are a function of: the resolver and its calibration, the title rules and
   *  normaliser that key its listings, its TMDB lookups, and the corpus replay that feeds it — never the agreement stage,
   *  whose new questions a fill answers. A change elsewhere that moves a decision anyway is what `--capture` is for. */
  val DecisionInputs: Seq[String] = Seq(
    "common/src/main/scala/services/identity/", "common/src/main/resources/identity-", "common/src/main/scala/services/titlerules/",
    "common/src/main/scala/services/movies/TitleNormalizer.scala", "worker/src/main/scala/services/identity/TmdbIdentityLookups.scala",
    "worker/src/main/scala/services/TmdbClient.scala", "worker/src/it/scala/IdentityShadow.scala")
  val NotDecisionInputs: Seq[String] = Seq("common/src/main/scala/services/identity/agreement/")

  def isDecisionInput(path: String): Boolean = DecisionInputs.exists(path.startsWith) && !NotDecisionInputs.exists(path.startsWith)

  /** The decision code's hash in the working tree: git blob ids (`git hash-object`) of every [[isDecisionInput]] file,
   *  tracked or not, so the committed and the edited tree hash alike for the same bytes. Twelve hex digits. */
  def decisionCode(repo: Path): String = {
    def git(input: Option[String], args: String*): String = {
      val p = new ProcessBuilder(("git" +: args)*).directory(repo.toFile).start()
      input.foreach { text => p.getOutputStream.write(text.getBytes(StandardCharsets.UTF_8)) }
      p.getOutputStream.close()
      val out = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      if (p.waitFor() != 0) sys.error(s"git ${args.mkString(" ")} failed")
      out
    }
    val paths = git(None, "ls-files", "-z", "-co", "--exclude-standard").split('\u0000').toSeq
      .filter(p => p.nonEmpty && isDecisionInput(p) && Files.isRegularFile(repo.resolve(p))).sorted
    val blobs = git(Some(paths.mkString("\n") + "\n"), "hash-object", "--stdin-paths").linesIterator.toSeq
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(blobs.zip(paths).map((b, p) => s"$b $p\n").mkString.getBytes(StandardCharsets.UTF_8))
    java.util.HexFormat.of().formatHex(digest).take(12)
  }

  // ── the environment each country's JVM is handed ─────────────────────────────────────────

  /** Every variable the capture spec reads, each the caller's own where set, else this layout's — and the country's own
   *  `MONGODB_DB`, so two captures never seed one database. The family export's prod URI is never passed on: the
   *  capture reads the export, not prod. */
  def environment(cc: String, env: Map[String, String], layout: Layout): Map[String, String] = {
    val defaults = Map(
      "KINOWO_IDENTITY_UNMATCHED_CAPTURE" -> layout.fixtures.toString,
      "KINOWO_IDENTITY_CORPUS_DIR"        -> layout.corpusDir.toString,
      "KINOWO_FIXTURE_ROOT"               -> layout.fixtureRoot.toString,
      "KINOWO_IDENTITY_AGREEMENT_CACHE"   -> layout.agreementCache.toString,
      "KINOWO_IDENTITY_FAMILY_SEED"       -> layout.families.toString,
      "KINOWO_IDENTITY_POSTER_CACHE"      -> layout.posters.toString,
      "MONGODB_URI"                       -> LocalMongo)
    defaults.map { case (name, default) => name -> env.getOrElse(name, default) } ++
      env.get("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY").map("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> _) +
      ("KINOWO_IDENTITY_FULL" -> cc) + ("MONGODB_DB" -> s"kinowo_capture_$cc")
  }

  /** A fill's variables: the fixture directory to fill, the country, and where its live answers come from and are kept. */
  def fillEnvironment(cc: String, env: Map[String, String], layout: Layout): Map[String, String] =
    Map(
      "KINOWO_IDENTITY_UNMATCHED_FILL"  -> env.getOrElse("KINOWO_IDENTITY_UNMATCHED_CAPTURE", layout.fixtures.toString),
      "KINOWO_IDENTITY_AGREEMENT_CACHE" -> env.getOrElse("KINOWO_IDENTITY_AGREEMENT_CACHE", layout.agreementCache.toString),
      "KINOWO_IDENTITY_POSTER_CACHE"    -> env.getOrElse("KINOWO_IDENTITY_POSTER_CACHE", layout.posters.toString),
      "KINOWO_IDENTITY_FULL"            -> cc) ++
      env.get("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY").map("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" -> _)

  /** Whether the corpora are this script's to fetch: not when the caller named a directory of their own. */
  def managedCorpus(env: Map[String, String]): Boolean = !env.contains("KINOWO_IDENTITY_CORPUS_DIR")
  /** Whether the enrichment trees are this script's to fetch. */
  def managedTree(env: Map[String, String]): Boolean = !env.contains("KINOWO_FIXTURE_ROOT")

  // ── artefact currency ────────────────────────────────────────────────────────────────────

  sealed trait Currency
  /** Download the recording `run`'s corpus and tree. */
  final case class Fetch(run: String, why: String) extends Currency
  /** Keep what is here, recorded by `run`. */
  final case class Keep(run: String, why: String) extends Currency
  /** Nothing here, and nothing to fetch it from. */
  final case class Missing(why: String) extends Currency

  /** What to do about a country's corpus and tree: `present` is the run they were downloaded from, `newest` the newest
   *  successful recording (`None`: not looked up — a dry run never touches the network). */
  def currency(present: Option[String], newest: Option[String]): Currency = (present, newest) match {
    case (Some(had), Some(run)) if had == run => Keep(run, s"already the newest recording's (run $run)")
    case (Some(had), Some(run))               => Fetch(run, s"run $had is older than the newest recording, $run")
    case (None, Some(run))                    => Fetch(run, "nothing downloaded yet")
    case (Some(had), None)                    => Keep(had, "the newest recording was not looked up")
    case (None, None)                         => Missing("no recording downloaded, and the newest was not looked up")
  }

  // ── the child JVM ────────────────────────────────────────────────────────────────────────

  /** sbt's own JVM options (`.jvmopts`: headless, en_US, the boxing cache the worker runs with), its heap replaced by
   *  the capture's. */
  def jvmOptions(jvmopts: Seq[String], heap: String): Seq[String] =
    s"-Xmx$heap" +: jvmopts.map(_.trim).filter(o => o.nonEmpty && !o.startsWith("#") && !o.startsWith("-Xmx"))

  /** A country's JVM did its job when its spec printed what it captured — not merely when ScalaTest ran: a suite
   *  CANCELLED for a missing variable is no failure to ScalaTest. */
  def succeeded(cc: String, mode: Mode, log: Seq[String]): Boolean = mode match {
    case Mode.Capture => log.exists(_.contains(s"[full-$cc] captured "))
    case Mode.Fill    => log.exists(_.contains(s"[$cc] filled after "))
  }

  private val Captured = """\] captured (\d+) listings""".r.unanchored
  def capturedListings(log: Seq[String]): Option[Int] = log.collectFirst { case Captured(n) => n.toInt }

  // ── the report ───────────────────────────────────────────────────────────────────────────

  /** One timed phase, and what it moved (`200.0 -> "MB"`) for its throughput. */
  final case class Phase(name: String, seconds: Double, amount: Option[(Double, String)] = None)

  def duration(seconds: Double): String = {
    val s = math.round(seconds)
    if (s >= 3600) "%dh%02dm".formatLocal(java.util.Locale.ROOT, s / 3600, s % 3600 / 60)
    else if (s >= 60) "%dm%02ds".formatLocal(java.util.Locale.ROOT, s / 60, s % 60) else s"${s}s"
  }

  def report(phases: Seq[Phase], wall: Double): String = {
    val width = (phases.map(_.name.length) :+ 5).max
    val lines = phases.map { p =>
      val rate = p.amount.filter(_ => p.seconds > 0).fold("") { case (n, unit) => "  %.1f %s/s (%.0f %s)".formatLocal(java.util.Locale.ROOT, n / p.seconds, unit, n, unit) }
      s"  ${p.name.padTo(width, ' ')}  ${duration(p.seconds).reverse.padTo(7, ' ').reverse}$rate"
    }
    (("[identity-capture] timings:" +: lines) :+ s"  ${"total".padTo(width, ' ')}  ${duration(wall).reverse.padTo(7, ' ').reverse}").mkString("\n")
  }

  // ── effects ──────────────────────────────────────────────────────────────────────────────

  /** One country's capture: its JVM's command, environment and log. */
  final case class Job(cc: String, command: Seq[String], env: Map[String, String], log: Path)

  /** Everything that reaches outside the process. */
  trait Effects {
    /** The newest successful "Record scrape fixtures" run, else `None`. */
    def newestRecording(): Option[String]
    /** Fetch recording `run`'s corpus for `cc` into the layout; the bytes fetched. */
    def fetchCorpus(run: String, cc: String, layout: Layout): Long
    /** Fetch the enrichment tree recorded with `run` for `cc`; the bytes fetched. */
    def fetchTree(run: String, cc: String, layout: Layout): Long
    /** Export prod's `identity_family_answers` of database `db` into `into/<db>.jsonl`, read-only; documents written. */
    def exportFamilies(db: String, into: Path): Long
    /** Run a country's JVM to its end; its log's lines. */
    def run(job: Job): Seq[String]
  }

  /** Prints what the real run would do; touches neither the network, prod, nor a child process. */
  final class DryRun(out: String => Unit) extends Effects {
    def newestRecording(): Option[String] = { out("would look up the newest successful \"Record scrape fixtures\" run (gh run list)"); None }
    def fetchCorpus(run: String, cc: String, layout: Layout): Long = {
      out(s"would download artifact scrape-fixtures-$cc of run $run into ${layout.corpusDir}"); 0L
    }
    def fetchTree(run: String, cc: String, layout: Layout): Long = {
      out(s"would download release asset enrichment-$cc-$run.tar.* (convergence-fixtures) into ${layout.trees}"); 0L
    }
    def exportFamilies(db: String, into: Path): Long = {
      out(s"would export $db.identity_family_answers read-only from prod into ${into.resolve(s"$db.jsonl")}"); 0L
    }
    def run(job: Job): Seq[String] = {
      val shown = job.env.map { case (k, v) => if (k.endsWith("_KEY")) s"$k=<from the vault>" else s"$k=$v" }.toSeq.sorted
      val command = job.command.zip("" +: job.command).map { case (arg, before) => if (before == "-cp") s"<classpath: ${arg.split(java.io.File.pathSeparator).length} entries>" else arg }
      out(s"would run ${job.cc}: ${command.mkString(" ")}\n      ${shown.mkString("\n      ")}\n      log ${job.log}")
      Nil
    }
  }

  /** The real run: `gh` for the recording, the Mongo driver (reads only) for the export, a JVM per country. */
  final class Live(repo: Path, env: Map[String, String]) extends Effects {
    private def gh(args: String*): String = {
      val p = new ProcessBuilder(("gh" +: args)*).directory(repo.toFile).redirectError(ProcessBuilder.Redirect.INHERIT).start()
      val out = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      if (p.waitFor() != 0) sys.error(s"gh ${args.mkString(" ")} failed")
      out.trim
    }
    private def sh(args: String*): Unit =
      if (new ProcessBuilder(args*).directory(repo.toFile).inheritIO().start().waitFor() != 0) sys.error(s"${args.mkString(" ")} failed")
    private def sizeOf(dir: Path): Long =
      if (!Files.exists(dir)) 0L else Using(Files.walk(dir))(_.iterator.asScala.filter(Files.isRegularFile(_)).map(Files.size).sum)

    def newestRecording(): Option[String] =
      Some(gh("run", "list", "--workflow", "Record scrape fixtures", "--status", "success", "--limit", "1",
        "--json", "databaseId", "--jq", ".[0].databaseId")).filter(_.nonEmpty)

    def fetchCorpus(run: String, cc: String, layout: Layout): Long = {
      val stage = layout.work.resolve(s"download-$cc")
      deleteTree(stage)
      gh("run", "download", run, "--name", s"scrape-fixtures-$cc", "--dir", stage.toString)
      sh("tar", "-xzf", stage.resolve(s"scrapes-$cc.tar.gz").toString, "-C", stage.toString)
      val corpus = stage.resolve(s"test/resources/fixtures/corpus/cinema-scrapes-$cc.json.gz")
      Files.createDirectories(layout.corpusDir)
      Files.move(corpus, layout.corpusDir.resolve(corpus.getFileName), java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      val bytes = Files.size(layout.corpusDir.resolve(corpus.getFileName))
      deleteTree(stage)
      bytes
    }

    def fetchTree(run: String, cc: String, layout: Layout): Long = {
      val stage = layout.work.resolve(s"tree-$cc")
      deleteTree(stage)
      // the tree recorded WITH this run's corpus (the pair); the working tree only where the release no longer keeps it
      val pinned = Try(gh("release", "download", "convergence-fixtures", "--pattern", s"enrichment-$cc-$run.tar.*", "--dir", stage.toString))
      if (pinned.isFailure) gh("release", "download", "convergence-fixtures", "--pattern", s"enrichment-$cc.tar.*", "--dir", stage.toString)
      val archive = Using(Files.list(stage))(_.iterator.asScala.toSeq.head)
      val bytes   = Files.size(archive)
      deleteTree(layout.fixtureRoot.resolve(s"enrichment-$cc"))
      sh(repo.resolve(".github/scripts/unpack-fixture-archive.sh").toString, archive.toString, layout.trees.toString)
      deleteTree(stage)
      bytes
    }

    def exportFamilies(db: String, into: Path): Long = {
      val uri = env.getOrElse(FamilyUri, sys.error(s"$FamilyUri is not set: scripts/identity-capture.sh opens the prod tunnel and sets it"))
      val client = org.mongodb.scala.MongoClient(uri)
      try {
        Files.createDirectories(into)
        val file = into.resolve(s"$db.jsonl")
        val tmp  = into.resolve(s"$db.jsonl.part")
        val settings = org.bson.json.JsonWriterSettings.builder().outputMode(org.bson.json.JsonMode.RELAXED).build()
        // a find, and nothing else: the export reads prod and never writes it
        val docs = scala.concurrent.Await.result(client.getDatabase(db).getCollection[org.mongodb.scala.bson.collection.immutable.Document]("identity_family_answers")
          .find().toFuture(), scala.concurrent.duration.Duration(10, "minutes"))
        Using(Files.newBufferedWriter(tmp, StandardCharsets.UTF_8))(w => docs.foreach { d => w.write(d.toJson(settings)); w.newLine() })
        Files.move(tmp, file, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        docs.size.toLong
      } finally client.close()
    }

    def run(job: Job): Seq[String] = {
      Files.createDirectories(job.log.getParent)
      val pb = new ProcessBuilder(job.command*).directory(repo.toFile).redirectErrorStream(true).redirectOutput(job.log.toFile)
      pb.environment().putAll(job.env.asJava)
      pb.start().waitFor()
      Files.readAllLines(job.log, StandardCharsets.UTF_8).asScala.toSeq
    }
  }

  private def Using[R <: AutoCloseable, A](r: R)(f: R => A): A = scala.util.Using.resource(r)(f)

  private def deleteTree(dir: Path): Unit = if (Files.exists(dir))
    Using(Files.walk(dir))(_.iterator.asScala.toSeq.reverse.foreach(Files.delete))

  // ── the run ──────────────────────────────────────────────────────────────────────────────

  /** The family-answer export's database for a country (`kinowo` is Poland's). */
  def database(cc: String): String = if (cc == "pl") "kinowo" else s"kinowo_$cc"

  /** The spec a fill runs. */
  val FillSpec = "integration.UnmatchedClustersFillIntegrationSpec"

  /** The run, over `effects`: each country's choice made and said, the recording and the family answers fetched for the
   *  captures alone (a fill reads neither), then each country's JVM. True when every country did its job. */
  def capture(options: Options, env: Map[String, String], layout: Layout, effects: Effects, classpath: String,
              jvmopts: Seq[String], code: String, out: String => Unit): Boolean = {
    val started = System.nanoTime()
    val phases  = Seq.newBuilder[Phase]
    def timed[A](name: String)(body: => A)(amount: A => Option[(Double, String)]): A = {
      val t0 = System.nanoTime(); val a = body
      phases += Phase(name, (System.nanoTime() - t0) / 1e9, amount(a)); a
    }
    val cs = options.countries
    out(s"[identity-capture] countries ${cs.mkString(" ")}; work in ${layout.work}${if (options.dryRun) "; DRY RUN" else ""}")

    // 1. which recording each country's corpus would be, and with it capture or fill
    val newest = if (managedCorpus(env) || managedTree(env)) timed("look up the newest recording")(effects.newestRecording())(_ => None) else None
    val fixtures = Path.of(env.getOrElse("KINOWO_IDENTITY_UNMATCHED_CAPTURE", layout.fixtures.toString))
    val plans = cs.map { cc =>
      val present  = Some(layout.recorded(cc)).filter(Files.exists(_)).map(Files.readString(_).trim).filter(_.nonEmpty)
      val recorded = if (managedCorpus(env)) currency(present, newest) else Keep(ownCorpus(env, cc).getOrElse(""), "the caller's own corpus")
      val run      = recorded match { case Fetch(r, _) => Some(r); case Keep(r, _) => Some(r).filter(_.nonEmpty); case Missing(_) => None }
      val stamped  = Some(fixtures.resolve(s"$cc.inputs")).filter(Files.exists(_)).flatMap(p => Inputs.parse(Files.readString(p)))
      val choice   = choose(cc, Files.exists(fixtures.resolve(s"$cc.json.gz")), stamped, run.map(Inputs(_, code)), options.forced)
      out(s"[identity-capture] $cc: ${choice.mode.toString.toUpperCase(java.util.Locale.ROOT)} — ${choice.why}")
      (choice, recorded, run)
    }
    val captures = plans.collect { case (Choice(cc, Mode.Capture, _), recorded, _) => cc -> recorded }

    // 2. the recording, for the captures: corpus and tree, fetched only when not already the newest
    captures.foreach {
      case (cc, Keep(run, why)) if run.nonEmpty => out(s"[identity-capture] $cc: keeping recording $run — $why")
      case (cc, Keep(_, why))                   => out(s"[identity-capture] $cc: $why")
      case (cc, Missing(why))                   => out(s"[identity-capture] $cc: $why")
      case (cc, Fetch(run, why)) =>
        out(s"[identity-capture] $cc: fetching recording $run — $why")
        if (managedCorpus(env)) timed(s"download corpus $cc")(effects.fetchCorpus(run, cc, layout))(b => Some(b / 1e6 -> "MB"))
        if (managedTree(env)) timed(s"download tree $cc")(effects.fetchTree(run, cc, layout))(b => Some(b / 1e6 -> "MB"))
        if (!options.dryRun) { Files.createDirectories(layout.work); Files.writeString(layout.recorded(cc), run) }
    }

    // 3. prod's family answers, read-only, for the captures, unless the caller handed a seed of their own
    if (!env.contains("KINOWO_IDENTITY_FAMILY_SEED"))
      captures.map((cc, _) => database(cc)).distinct.foreach { db =>
        timed(s"export $db family answers")(effects.exportFamilies(db, layout.families))(n => Some(n.toDouble -> "docs"))
      }

    // 4. one JVM per country
    val jvm = Seq("java") ++ jvmOptions(jvmopts, options.heap) ++ Seq("-cp", classpath, "org.scalatest.tools.Runner", "-oDW", "-s")
    val results = plans.map { case (Choice(cc, mode, _), _, run) =>
      val job = mode match {
        case Mode.Capture => Job(cc, jvm :+ CaptureSpec, environment(cc, env, layout), layout.logs.resolve(s"$cc-capture.log"))
        case Mode.Fill    => Job(cc, jvm :+ FillSpec, fillEnvironment(cc, env, layout), layout.logs.resolve(s"$cc-fill.log"))
      }
      val verb = mode.toString.toLowerCase(java.util.Locale.ROOT)
      out(s"[identity-capture] $cc: $verb (log ${job.log})")
      val log = timed(s"$verb $cc")(effects.run(job))(l => capturedListings(l).map(_.toDouble -> "listings"))
      val ok  = options.dryRun || succeeded(cc, mode, log)
      if (!ok) out(s"[identity-capture] $cc: FAILED — the spec printed no $verb; see ${job.log}")
      else {
        log.filter(l => l.contains(s"[full-$cc]") || l.contains(s"[$cc]")).foreach(l => out(s"  $l"))
        // a capture's decisions are now those of this recording under this code: what the next run compares with
        if (mode == Mode.Capture && !options.dryRun) run.foreach(r => Files.writeString(fixtures.resolve(s"$cc.inputs"), Inputs(r, code).render))
      }
      ok
    }

    if (!options.dryRun) out(report(phases.result(), (System.nanoTime() - started) / 1e9))
    results.forall(identity)
  }

  /** The caller's own corpus (`KINOWO_IDENTITY_CORPUS_DIR`), named by its content's hash for the fixture's stamp. */
  private def ownCorpus(env: Map[String, String], cc: String): Option[String] =
    env.get("KINOWO_IDENTITY_CORPUS_DIR").map(Path.of(_).resolve(s"cinema-scrapes-$cc.json.gz")).filter(Files.exists(_)).map { file =>
      "corpus " + java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(file))).take(12)
    }

  def main(args: Array[String]): Unit = parse(args.toSeq) match {
    case Left(error) =>
      System.err.println(s"[identity-capture] $error"); sys.exit(2)
    case Right(options) =>
      val repo    = Path.of("").toAbsolutePath
      val env     = sys.env
      val layout  = Layout(repo, env.get("KINOWO_IDENTITY_CAPTURE_WORK").map(Path.of(_).toAbsolutePath).getOrElse(repo.resolve("target/identity-capture")))
      val effects = if (options.dryRun) new DryRun(println) else new Live(repo, env)
      if (!options.dryRun && !env.contains("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY")) {
        System.err.println("[identity-capture] KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY is not set (scripts/identity-capture.sh reads it from the vault)")
        sys.exit(2)
      }
      val jvmopts = Try(Files.readAllLines(repo.resolve(".jvmopts")).asScala.toSeq).getOrElse(Nil)
      val ok = Try(capture(options, env, layout, effects, System.getProperty("java.class.path"), jvmopts, decisionCode(repo), println))
      ok.failed.foreach(e => System.err.println(s"[identity-capture] ${e.getMessage}"))
      sys.exit(if (ok.getOrElse(false)) 0 else 1)
  }
}
