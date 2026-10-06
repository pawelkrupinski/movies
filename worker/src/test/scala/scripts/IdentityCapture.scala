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

  final case class Options(countries: Seq[String] = Countries, dryRun: Boolean = false, heap: String = "12g")

  def parse(args: Seq[String]): Either[String, Options] = {
    @scala.annotation.tailrec def go(rest: List[String], o: Options, named: List[String]): Either[String, Options] = rest match {
      case Nil                     => Right(if (named.isEmpty) o else o.copy(countries = Countries.filter(named.contains)))
      case "--dry-run" :: tail     => go(tail, o.copy(dryRun = true), named)
      case "--heap" :: heap :: tail => go(tail, o.copy(heap = heap), named)
      case flag :: _ if flag.startsWith("-") => Left(s"unknown option $flag")
      case cc :: tail =>
        val code = cc.toLowerCase(java.util.Locale.ROOT)
        if (Countries.contains(code)) go(tail, o, code :: named) else Left(s"no recorded corpus for country '$cc' (one of ${Countries.mkString(", ")})")
    }
    go(args.toList, Options(), Nil)
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
  def succeeded(cc: String, log: Seq[String]): Boolean = log.exists(_.contains(s"[full-$cc] captured "))

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

  /** The run, over `effects`: true when every country captured. */
  def capture(options: Options, env: Map[String, String], layout: Layout, effects: Effects, classpath: String,
              jvmopts: Seq[String], out: String => Unit): Boolean = {
    val started = System.nanoTime()
    val phases  = Seq.newBuilder[Phase]
    def timed[A](name: String)(body: => A)(amount: A => Option[(Double, String)]): A = {
      val t0 = System.nanoTime(); val a = body
      phases += Phase(name, (System.nanoTime() - t0) / 1e9, amount(a)); a
    }
    val cs = options.countries
    out(s"[identity-capture] countries ${cs.mkString(" ")}; work in ${layout.work}${if (options.dryRun) "; DRY RUN" else ""}")

    // 1. the recording: corpus and tree per country, fetched only when not already the newest
    if (managedCorpus(env) || managedTree(env)) {
      val newest = timed("look up the newest recording")(effects.newestRecording())(_ => None)
      cs.foreach { cc =>
        val present = Some(layout.recorded(cc)).filter(Files.exists(_)).map(Files.readString(_).trim).filter(_.nonEmpty)
        currency(present, newest) match {
          case Keep(run, why) => out(s"[identity-capture] $cc: keeping recording $run — $why")
          case Missing(why)   => out(s"[identity-capture] $cc: $why")
          case Fetch(run, why) =>
            out(s"[identity-capture] $cc: fetching recording $run — $why")
            if (managedCorpus(env)) timed(s"download corpus $cc")(effects.fetchCorpus(run, cc, layout))(b => Some(b / 1e6 -> "MB"))
            if (managedTree(env)) timed(s"download tree $cc")(effects.fetchTree(run, cc, layout))(b => Some(b / 1e6 -> "MB"))
            if (!options.dryRun) { Files.createDirectories(layout.work); Files.writeString(layout.recorded(cc), run) }
        }
      }
    }

    // 2. prod's family answers, read-only, unless the caller handed a seed of their own
    if (!env.contains("KINOWO_IDENTITY_FAMILY_SEED"))
      cs.map(database).distinct.foreach { db =>
        timed(s"export $db family answers")(effects.exportFamilies(db, layout.families))(n => Some(n.toDouble -> "docs"))
      }

    // 3. one JVM per country
    val results = cs.map { cc =>
      val job = Job(cc, Seq("java") ++ jvmOptions(jvmopts, options.heap) ++ Seq("-cp", classpath, "org.scalatest.tools.Runner", "-oDW", "-s", CaptureSpec),
        environment(cc, env, layout), layout.logs.resolve(s"$cc.log"))
      out(s"[identity-capture] $cc: capture (log ${job.log})")
      val log = timed(s"capture $cc")(effects.run(job))(l => capturedListings(l).map(_.toDouble -> "listings"))
      val ok  = options.dryRun || succeeded(cc, log)
      if (!ok) out(s"[identity-capture] $cc: FAILED — the spec printed no capture; see ${job.log}")
      else log.filter(_.contains("[full-")).foreach(l => out(s"  $l"))
      ok
    }

    if (!options.dryRun) out(report(phases.result(), (System.nanoTime() - started) / 1e9))
    results.forall(identity)
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
      val ok = capture(options, env, layout, effects, System.getProperty("java.class.path"), jvmopts, println)
      sys.exit(if (ok) 0 else 1)
  }
}
