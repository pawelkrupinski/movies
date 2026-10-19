package scripts

import clients.TmdbClient
import models.Country
import org.bson.{BsonDocument, BsonValue}
import org.mongodb.scala.model.{Filters, Projections, Sorts}
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import services.identity.adapters.{MongoTmdbDocuments, NormalizingHttpFetch, TmdbKind, TmdbNormalizer, TmdbStore}
import tools.{CommandLine, HttpStatusException, Stopwatch}

import java.time.Clock
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Reads again, from TMDB, every stored film record (`tmdb_films`) filed before the identity store kept a record's
 * top-billed cast (f1b4f51c6, 2026-10-06), its `original_language` (f48ab8001, 2026-10-19) and its genres (2026-10-19),
 * so the cast evidence (`services.identity.CastEvidence`) and the language and genre reads see them. Each read is the one request the agreement's
 * record re-read makes (`TmdbClient.readRecordAgain`: `/movie/{id}?language=<country's>&append_to_response=credits,release_dates`)
 * through the identity store's normalizing fetch, which files the record's localized partial in today's shape.
 *
 * Which records: those whose localized partial lacks `original_language` or `genres`. Those fields are the mark of a
 * record read by code that keeps all three (TMDB states both of every film, `genres` an empty list at worst): a record
 * carrying them and no cast is one TMDB credits no one on, and is not asked again — so a
 * run is RESUMABLE (an interrupted one picks up where it stopped) and a second run reads nothing. `--current-only`
 * (the default) narrows to the records current listings and screenings read: every film a current model family's
 * questions name (`identity_model_families.films` / `reads.films`) and the TMDB film of every film row with
 * screenings (`screenings.filmId` → `movies.tmdbId`). `--all` reads every stale record.
 *
 * Pacing: a fixed pool of [[Workers]] workers at about [[PerSecond]] requests a second in all (the live workers share
 * TMDB's ~50 req/s limit with it). A 429 or 503 that outlasts the client's own retries halves the workers allowed at
 * once (at most once per [[Backoff]]), waits a [[Backoff]], and asks that record again later — up to [[MaxAttempts]].
 * Throughput is printed at the end.
 *
 * ==Running it== (the country's own database, `Country.mongoDb`; prod through `scripts/with-prod-mongo.sh`'s tunnel, the
 * secrets from the vault, plain `sbt`: a kept `scripts/sbt-warm` server reads the environment it started with, never
 * this call's — stop it first)
 * {{{
 *   scripts/sbt-warm --stop
 *   secrets run movies MONGODB_URI -- scripts/with-prod-mongo.sh \
 *     sbt "worker/Test/runMain scripts.TmdbRecordBackfill pl --dry-run"
 *   secrets run movies TMDB_API_KEY MONGODB_URI -- scripts/with-prod-mongo.sh \
 *     sbt "worker/Test/runMain scripts.TmdbRecordBackfill pl"
 * }}}
 * Run it only once the country's worker runs a build that keeps `original_language` (f48ab8001) and genres, and reads
 * cast as a tie-break only (the `cast-tiebreak` change): an older worker refiling a record drops them again, and under
 * the older cast rule the filled casts take a wrong film (UK "Straight Talk: A Tribute to Dolly Parton"). The worker
 * keeps the films it read in a cache it assumes only it writes (`CachedTmdbDocuments`): restart the country's worker
 * after a run so its model reads the refiled records.
 */
object TmdbRecordBackfill {

  val Workers: Int        = 10
  val PerSecond: Double   = 30
  val Backoff: FiniteDuration = 5.seconds
  val MaxAttempts: Int    = 5
  /** The statuses that say "too fast": TMDB's rate limit, and its overloaded edge. */
  val Refusals: Set[Int]  = Set(429, 503)

  // ── what is stored, and what is current ─────────────────────────────────────────────────────

  /** One stored record's localized partial: whether it carries a cast, an `original_language` and `genres`. */
  final case class RecordState(id: Int, hasCast: Boolean, hasLanguage: Boolean, hasGenres: Boolean) {
    /** Filed by code that keeps all three: read again, it would file nothing new. */
    def current: Boolean = hasLanguage && hasGenres
  }

  object RecordState {
    /** Off a `tmdb_films` document (`local` the localized partial `TmdbNormalizer.minimal` filed); none for a document
     *  that holds no localized partial — a film only a search named, no record of TMDB's read. */
    def of(id: String, document: BsonDocument): Option[RecordState] =
      id.toIntOption.zip(Option(document.get(TmdbStore.Partial.Local.field)).filter(_.isDocument).map(_.asDocument)).map { case (film, local) =>
        val cast = Option(local.get("credits")).filter(_.isDocument).flatMap(c => Option(c.asDocument.get("cast"))).filter(_.isArray)
        RecordState(film, hasCast = cast.exists(!_.asArray.isEmpty), hasLanguage = local.containsKey("original_language"),
          hasGenres = local.containsKey("genres"))
      }
  }

  /** Every stored record's state. */
  trait StoredRecords { def all(): Seq[RecordState] }

  /** The TMDB films current listings and screenings read. */
  trait CurrentFilms { def ids(): Set[Int] }

  /** What a run reads, and the counts it prints. */
  final case class Selection(records: Int, lackingCast: Int, lackingLanguage: Int, lackingGenres: Int, inScope: Int, toRead: Seq[Int])

  /** The records to read again: those `scope` holds (every one without it) whose partial is not [[RecordState.current]]. */
  def select(records: Seq[RecordState], scope: Option[Set[Int]]): Selection = {
    val scoped = scope.fold(records)(current => records.filter(r => current.contains(r.id)))
    Selection(records.size, records.count(!_.hasCast), records.count(!_.hasLanguage), records.count(!_.hasGenres), scoped.size,
      scoped.filterNot(_.current).map(_.id).sorted)
  }

  // ── pacing ──────────────────────────────────────────────────────────────────────────────────

  /** At most `limit` reads at once, halved — never below one — on a refusal, at most once per `quiet`. */
  final class Concurrency(initial: Int, clock: Clock, quiet: FiniteDuration) {
    private var allowed  = initial
    private var inFlight = 0
    private var halvedAt = Option.empty[Long]
    def acquire(): Unit = synchronized { while (inFlight >= allowed) wait(); inFlight += 1 }
    def release(): Unit = synchronized { inFlight -= 1; notifyAll() }
    def limit: Int = synchronized(allowed)
    /** A read was refused: halve, unless it was halved within `quiet` (a burst of refusals is one signal) or allows one already. */
    def refused(): Unit = synchronized {
      val now = clock.millis()
      if (allowed > 1 && halvedAt.forall(now - _ >= quiet.toMillis)) { allowed = math.max(1, allowed / 2); halvedAt = Some(now) }
    }
  }

  /** Spaces the reads `1 / perSecond` apart, whichever worker makes them. */
  final class Pace(perSecond: Double, clock: Clock, sleep: Long => Unit) {
    private val interval = 1000.0 / perSecond
    private var next     = Double.MinValue
    def await(): Unit = {
      val wait = synchronized {
        val now = clock.millis().toDouble
        val at  = math.max(now, next)
        next = at + interval
        at - now
      }
      if (wait >= 1) sleep(wait.toLong)
    }
  }

  /** What a run did: records read, refused reads (each asked again), records given up on, the final concurrency. */
  final case class Outcome(read: Int, refusals: Int, failed: Seq[(Int, String)], finalConcurrency: Int)

  /** Reads `ids` again through `read`, on `workers` threads, paced and backed off as above. */
  def readAll(ids: Seq[Int], read: Int => Unit, clock: Clock, sleep: Long => Unit,
              workers: Int = Workers, perSecond: Double = PerSecond, progress: Int => Unit = _ => ()): Outcome = {
    val queue       = new ConcurrentLinkedQueue[Int](ids.asJava)
    val attempts    = new java.util.concurrent.ConcurrentHashMap[Int, Int]()
    val concurrency = new Concurrency(workers, clock, Backoff)
    val pace        = new Pace(perSecond, clock, sleep)
    val done        = new AtomicInteger
    val refusals    = new AtomicInteger
    val failed      = new ConcurrentLinkedQueue[(Int, String)]()
    def work(): Unit = {
      var next = Option(queue.poll())
      while (next.isDefined) {
        val id = next.get
        concurrency.acquire()
        val refusal =
          try { pace.await(); read(id); progress(done.incrementAndGet()); false }
          catch {
            case e: HttpStatusException if Refusals(e.code) =>
              refusals.incrementAndGet(); concurrency.refused()
              if (attempts.merge(id, 1, _ + _) < MaxAttempts) queue.add(id) else failed.add(id -> e.getMessage)
              true
            case NonFatal(e) => failed.add(id -> e.toString); false
          } finally concurrency.release()
        if (refusal) sleep(Backoff.toMillis)
        next = Option(queue.poll())
      }
    }
    val threads = (1 to workers).map(i => new Thread(() => work(), s"tmdb-record-backfill-$i"))
    threads.foreach(_.start())
    threads.foreach(_.join())
    Outcome(done.get, refusals.get, failed.asScala.toSeq.sortBy(_._1), concurrency.limit)
  }

  // ── Mongo ───────────────────────────────────────────────────────────────────────────────────

  private val Timeout = 2.minutes

  final class MongoStoredRecords(db: MongoDatabase) extends StoredRecords {
    private val Page = 5000
    def all(): Seq[RecordState] = {
      val films = db.getCollection[BsonDocument](TmdbKind.Film.collection)
      val out   = Seq.newBuilder[RecordState]
      val local = TmdbStore.Partial.Local.field
      services.movies.KeysetScan.scan[BsonDocument]("tmdb_films scan", Page, maxAttempts = 3, initialBackoff = 1.second,
        keyOf = _.getString("_id").getValue,
        fetchPage = (after, size) => Await.result(films
          .find(Filters.and((Seq(Filters.exists(local)) ++ after.map(Filters.gt("_id", _)))*))
          .projection(Projections.include(s"$local.original_language", s"$local.credits.cast", s"$local.genres"))
          .sort(Sorts.ascending("_id")).limit(size).toFuture(), Timeout))(
        page => page.foreach(d => RecordState.of(d.getString("_id").getValue, d).foreach(out += _))) match {
        case tools.ScanOutcome.Complete            => out.result()
        case tools.ScanOutcome.Incomplete(failure) => throw failure
      }
    }
  }

  final class MongoCurrentFilms(db: MongoDatabase) extends CurrentFilms {
    private def ints(values: Seq[BsonValue]): Seq[Int] = values.collect {
      case v if v.isInt32 => v.asInt32.getValue
      case v if v.isInt64 => v.asInt64.getValue.toInt
    }
    def ids(): Set[Int] = {
      val families = db.getCollection[BsonDocument]("identity_model_families")
      val named    = Seq("films", "reads.films").flatMap(field => ints(Await.result(families.distinct[BsonValue](field).toFuture(), Timeout)))
      val screened = Await.result(db.getCollection[BsonDocument]("screenings").distinct[String]("filmId").toFuture(), Timeout)
      val movies   = db.getCollection[BsonDocument]("movies")
      val tmdbIds  = screened.grouped(1000).flatMap(batch => Await.result(movies.find(Filters.in("_id", batch*))
        .projection(Projections.include("tmdbId")).toFuture(), Timeout).flatMap(d => Option(d.get("tmdbId")).toSeq)).toSeq
      (named ++ ints(tmdbIds)).toSet
    }
  }

  // ── the command ─────────────────────────────────────────────────────────────────────────────

  val commandLine: CommandLine = CommandLine("TmdbRecordBackfill",
    "Reads again from TMDB the stored film records filed without cast, original_language and genres (current ones by default).",
    CommandLine.Flag("dry-run", "count and list only; ask TMDB nothing, write nothing"),
    CommandLine.Flag("current-only", "only the records current listings and screenings read (the default)"),
    CommandLine.Flag("all", "every stale record, current or not"))
    .withPositionals(CommandLine.Positional("country", "the country code (pl, uk, de, es, us)"))

  def main(args: Array[String]): Unit = {
    val line    = commandLine.parseOrExit(args)
    val country = line.country("country").getOrElse(line.fail("<country> is required"))
    if (line.flag("all") && line.flag("current-only")) line.fail("--all and --current-only exclude each other")
    val clock   = Clock.systemUTC()
    val (connection, database) = CountryDatabase.open(country)
    try run(country, database, clock, dryRun = line.flag("dry-run"), currentOnly = !line.flag("all"))
    finally connection.close()
  }

  private def run(country: Country, database: MongoDatabase, clock: Clock, dryRun: Boolean, currentOnly: Boolean): Unit = {
    val scope     = Option.when(currentOnly)(new MongoCurrentFilms(database).ids())
    val selection = select(new MongoStoredRecords(database).all(), scope)
    println(s"${country.code}: ${selection.records} records; ${selection.lackingCast} lack cast, ${selection.lackingLanguage} lack " +
      s"original_language, ${selection.lackingGenres} lack genres; ${scope.fold("every record in scope")(s => s"${s.size} current films, ${selection.inScope} of them recorded")}; " +
      s"${selection.toRead.size} to read again${if (currentOnly) " (current only)" else " (all)"}")
    if (dryRun) { println(s"first ids: ${selection.toRead.take(20).mkString(" ")}"); println("DRY RUN — nothing asked, nothing written."); return }
    val apiKey = _root_.settings.ProcessConfiguration.resolve().tmdbApiKey.getOrElse {
      println("TMDB_API_KEY is not set — run under `secrets run movies TMDB_API_KEY --`."); sys.exit(1)
    }
    val store  = new TmdbStore(new MongoTmdbDocuments(database), clock)
    val client = new TmdbClient(new NormalizingHttpFetch(new tools.RealHttpFetch(), new TmdbNormalizer(store)), apiKey = Some(apiKey),
      language = country.language)
    val total  = selection.toRead.size
    val timer  = Stopwatch.start()
    val outcome = readAll(selection.toRead, client.readRecordAgain, clock, millis => Thread.sleep(millis),
      progress = n => if (n % 500 == 0) println(f"  $n / $total read, ${n / timer.seconds}%.1f req/s"))
    outcome.failed.take(20).foreach { case (id, why) => println(s"  FAILED tmdb:$id — $why") }
    println(f"${country.code}: read ${outcome.read} of $total in ${timer.seconds}%.1fs, ~${outcome.read / math.max(timer.seconds, 0.001)}%.1f req/s; " +
      s"${outcome.refusals} refused (429/503), ${outcome.failed.size} failed, concurrency ended at ${outcome.finalConcurrency}")
  }
}
