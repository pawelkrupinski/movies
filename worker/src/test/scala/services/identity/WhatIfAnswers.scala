package services.identity

import models.Country
import org.bson.{BsonDocument, RawBsonDocument}
import services.identity.adapters.{FamilySource, TmdbDocuments, TmdbKind, TmdbStore}
import services.identity.agreement.VoterFamily
import services.movies.TitleNormalizer

import java.io.{DataInputStream, DataOutputStream, EOFException, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters._

/**
 * WHERE A WHAT-IF's ANSWERS COME FROM (`scripts/identity-what-if`), picked at [[WhatIfResolve]]'s composition root — the
 * one seam the sides differ at. Each holds the store the model and the agreement stage read and file into (`docs`, keyed
 * as production's TMDB store and `identity_family_answers` key theirs), the model's live asker (`live`, behind the
 * question memory as the worker's is: `tools.ProdLookupInputs.lookups`) and the stage's sources (`stage`,
 * [[scripts.StageAnswers]]: every question asked, filed and given up on as production's queue does it).
 *
 *  - [[offline]]: the mirror's store alone; every live ask refused, every family ask a failed attempt — a question prod
 *    never answered stays a gap (the what-if before 2026-11-05, `--offline`).
 *  - [[live]]: the mirror's store, and under it a LOCAL what-if store on disk ([[DiskTmdbDocuments]], never the mirror,
 *    never prod) holding every answer asked of the REAL services for a question the mirror lacks — TMDB, IMDb's
 *    suggestions, and the agreement's families (IMDb, Wikidata, Metacritic, Rotten Tomatoes, Filmweb where it votes),
 *    posters and catalogue pages — through the worker's own clients and parsers over production's paced wire
 *    (`HttpWiring.pacedWire`: its host policies, retries and breakers), each host at most [[HostLimit.Start]] requests at
 *    once, halved on a 429/503 ([[Throttled]]). Asked once: a repeat run reads the local store, free and deterministic.
 *    Direct requests only: no residential proxy or paid scraping service is ever asked; a host that refuses a direct
 *    request leaves its question failed, as an unreachable one.
 *
 * A recording's answers (`--recording RUN`) are `scripts.IdentityDrift.recorded` over its FakeHttp.
 */
object WhatIfAnswers {

  trait Source {
    /** The store both read and file into. */
    def docs: TmdbDocuments
    /** The model's asker for a TMDB or IMDb question the store lacks. */
    def live: IdentityLookups
    /** The agreement stage's sources. */
    def stage: scripts.StageAnswers.Sources
    /** What was asked live, by host, and at what rate; empty when nothing was. */
    def report(): String
  }

  /** The mirror's store alone, nothing asked. */
  def offline(mirror: TmdbDocuments, country: Country): Source = new Source {
    val docs: TmdbDocuments = mirror
    val live: IdentityLookups = Refused
    val stage: scripts.StageAnswers.Sources = sources(country, Map.empty, NoFetch, _ => Nil, findIn(mirror, _ => None))
    def report(): String = ""
  }

  /** The mirror's store over the local what-if store under `dir`, every gap asked of the real services through `wire`
   *  (production's paced wire over a real fetch, [[Throttled]]) with TMDB's key `tmdbKey`. */
  def live(mirror: TmdbDocuments, dir: Path, wire: tools.HttpFetch, tmdbKey: settings.TmdbApiKey, country: Country,
           clock: java.time.Clock): Source = new Source {
    private val local     = new DiskTmdbDocuments(dir)
    val docs: TmdbDocuments = new Layered(mirror, local)
    private val throttled = new Throttled(wire)
    private val bodies    = new tools.JsonBodies
    private val normalizer = new adapters.TmdbNormalizer(new TmdbStore(docs, clock), bodies,
      new adapters.KeptFailures(adapters.IdentityHistory.Discard, clock, "what-if"))
    private val counted   = new adapters.LiveAsks.Counted(new adapters.NormalizingHttpFetch(throttled, normalizer))
    private val tmdb      = new clients.TmdbClient(counted, Some(tmdbKey), country.language, bodies)
    private val imdb      = new services.enrichment.ImdbClient(counted)
    val live: IdentityLookups = new adapters.TmdbIdentityLookups(tmdb, imdb, Nil)
    private val venueClock = new models.VenueClock(clock)
    val stage: scripts.StageAnswers.Sources = sources(country, Seq(new adapters.ImdbFamily(new services.enrichment.ImdbClient(throttled)),
      new adapters.WikiFamily(new services.enrichment.WikidataClient(throttled), country.language.getLanguage),
      new adapters.FilmwebFamily(new services.enrichment.FilmwebClient(throttled),
        services.cinemas.pl.FilmwebProgrammes.resolving(throttled, () => venueClock.todayInPoland).of),
      new adapters.RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(throttled)),
      new adapters.MetacriticFamily(new services.enrichment.MetacriticClient(throttled))).map(s => s.family -> s).toMap,
      throttled, id => scala.util.Try(tmdb.posters(id, also = Seq("en"))).getOrElse(Nil),
      findIn(docs, imdbId => scala.util.Try(tmdb.findByImdbId(imdbId)).toOption.map(_ => ())))
    def report(): String = throttled.report()
  }

  /** The country's agreement families ([[tools.UnmatchedClusters.familiesOf]], as every harness's stage asks them), each
   *  asked of its site in `sites`. */
  private def sources(of: Country, sites: Map[VoterFamily, FamilySource], over: tools.HttpFetch, images: Int => Seq[clients.TmdbClient.PosterImage],
                      found: String => Answer[Option[Int]]): scripts.StageAnswers.Sources = new scripts.StageAnswers.Sources {
    val country: Country                            = of
    val normalizer: TitleNormalizer                 = TitleNormalizer.forCountry(of)
    val families: Seq[VoterFamily]                  = tools.UnmatchedClusters.familiesOf(of)
    val sources: Map[VoterFamily, FamilySource]     = sites.filter(site => families.contains(site._1))
    val fetch: tools.HttpFetch                      = over
    def posterImages(tmdbId: Int): Seq[clients.TmdbClient.PosterImage] = images(tmdbId)
    def find(imdb: String): Answer[Option[Int]]     = found(imdb)
  }

  /** An IMDb id's TMDB film as `docs` holds its find; one it does not, `ask`ed (filed into `docs` by the asking fetch)
   *  and read again. */
  private def findIn(docs: TmdbDocuments, ask: String => Option[Unit]): String => Answer[Option[Int]] = imdb => {
    val store = new TmdbStore(docs, java.time.Clock.systemUTC())
    store.foundFirst(imdb).orElse(ask(imdb).flatMap(_ => store.foundFirst(imdb)))
      .fold[Answer[Option[Int]]](Answer.Unknown)(Answer.Known(_))
  }

  /** Every live ask refused: a gap stays the gap the mirror's store holds, nothing is fetched. */
  object Refused extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]       = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }

  /** No request leaves: a family, poster or catalogue ask fails, as an unreachable site's. */
  object NoFetch extends tools.HttpFetch {
    private def refused(url: String) = throw new IllegalStateException(s"offline: no request is made ($url)")
    def get(url: String): String                                     = refused(url)
    def post(url: String, body: String, contentType: String): String = refused(url)
  }

  /** `over`'s documents first; a document it lacks read from `under`; every write to `under` alone — the mirror is
   *  read, never written. */
  final class Layered(over: TmdbDocuments, under: TmdbDocuments) extends TmdbDocuments {
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      val held = over.get(kind, ids)
      held ++ under.get(kind, ids.filterNot(held.contains))
    }
    override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      val held = over.answers(kind, ids)
      held ++ under.answers(kind, ids.filterNot(held.contains))
    }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = under.put(kind, docs)
  }

  /** `body`, asked again (up to `times`, a pause between) when a read of the mirror timed out: a mirror's `mongod` on a
   *  loaded laptop answers a batch past its 30 s now and then, and a run waited for its JVM slot for longer than that
   *  read (the parked-items review's UK run, 2026-11-05). Any other failure is the caller's. */
  def patient[A](body: => A, times: Int = 4): A =
    try body
    catch {
      case e: java.util.concurrent.TimeoutException if times > 1 =>
        System.err.println(s"[what-if] a mirror read timed out ($e): asked again")
        Thread.sleep(2000)
        patient(body, times - 1)
    }

  /** `inner`'s documents, each read once and kept, a missing one too: a stage settled over a whole corpus reads its family
   *  answers one cluster at a time, and through the mirror's replay each read was a round trip (ES: 254 s of one apply).
   *  [[warm]] reads a kind's ids in batches up front; a write goes through to `inner` and is kept. */
  final class Memoised(inner: TmdbDocuments) extends TmdbDocuments {
    private val held = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, Option[BsonDocument]]()).toMap
    private def through(kind: TmdbKind, ids: Seq[String])(read: Seq[String] => Map[String, BsonDocument]): Map[String, BsonDocument] = {
      val missing = ids.distinct.filterNot(held(kind).containsKey)
      if (missing.nonEmpty) {
        val found = missing.grouped(1000).flatMap(batch => patient(read(batch))).toMap
        missing.foreach(id => held(kind).putIfAbsent(id, found.get(id)))
      }
      ids.flatMap(id => Option(held(kind).get(id)).flatten.map(id -> _.clone())).toMap
    }
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = through(kind, ids)(inner.get(kind, _))
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = {
      inner.put(kind, docs)
      docs.foreach { case (id, d) => held(kind).put(id, Some(d.clone())) }
    }
    /** `ids` of `kind` read now, in batches: what a whole stage will read one by one. */
    def warm(kind: TmdbKind, ids: Seq[String]): Unit = { get(kind, ids); () }
  }

  /**
   * The local what-if store: each document's BSON bytes in memory, every write appended to `<dir>/<collection>.log` (the kind's
   * production collection) and read back on the next start — each record its id and its BSON, a later one for an id
   * replacing an earlier. A torn last record (a killed run) is dropped.
   */
  final class DiskTmdbDocuments(dir: Path) extends TmdbDocuments {
    // each document kept as its BSON bytes, read back afresh: the driver's codec alone, so a ref older than any store
    // helper of the checkout's compiles this file too
    private val codec  = new org.bson.codecs.BsonDocumentCodec
    private val memory = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, Array[Byte]]()).toMap
    private def bytesOf(d: BsonDocument): Array[Byte] = {
      val buffer = new RawBsonDocument(d, codec).getByteBuffer
      val bytes  = new Array[Byte](buffer.remaining())
      buffer.get(bytes)
      bytes
    }
    Files.createDirectories(dir)
    private def fileOf(kind: TmdbKind): Path = dir.resolve(s"${kind.collection}.log")
    TmdbKind.values.foreach { kind =>
      val file = fileOf(kind)
      if (Files.exists(file)) {
        val in = new DataInputStream(new java.io.BufferedInputStream(new FileInputStream(file.toFile)))
        try {
          var more = true
          while (more) {
            try {
              val idLength = in.readInt()
              val id       = new String(in.readNBytes(idLength), StandardCharsets.UTF_8)
              val length   = in.readInt()
              val bytes    = in.readNBytes(length)
              if (id.getBytes(StandardCharsets.UTF_8).length != idLength || bytes.length != length) more = false
              else memory(kind).put(id, bytes)
            } catch { case _: EOFException => more = false }
          }
        } finally in.close()
      }
    }
    private val logs = new ConcurrentHashMap[TmdbKind, java.nio.channels.FileChannel]()
    private def logOf(kind: TmdbKind) = logs.computeIfAbsent(kind, k => java.nio.channels.FileChannel.open(fileOf(k),
      java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.APPEND))
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
      ids.flatMap(id => Option(memory(kind).get(id)).map(bytes => id -> new RawBsonDocument(bytes).decode(codec))).toMap
    /** Kept, and appended to the kind's log as ONE write: another what-if appending to the same store at once (two
     *  agents, one country) interleaves whole batches, never parts of a record. */
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = if (docs.nonEmpty) {
      val batch = new java.io.ByteArrayOutputStream()
      val out   = new DataOutputStream(batch)
      docs.foreach { case (id, d) =>
        val bytes   = bytesOf(d)
        val idBytes = id.getBytes(StandardCharsets.UTF_8)
        memory(kind).put(id, bytes)
        out.writeInt(idBytes.length); out.write(idBytes); out.writeInt(bytes.length); out.write(bytes)
      }
      out.flush()
      val channel = logOf(kind)
      channel.synchronized {
        val bytes = java.nio.ByteBuffer.wrap(batch.toByteArray)
        while (bytes.hasRemaining) channel.write(bytes)
      }
    }
    def size(kind: TmdbKind): Int = memory(kind).size
  }

  /** How many requests one host is asked at once: [[Start]], halved on every 429 or 503 it answers, never below 1. */
  final class HostLimit(start: Int) {
    private var limit    = start
    private var inFlight = 0
    def acquire(): Unit = synchronized { while (inFlight >= limit) wait(); inFlight += 1 }
    def release(): Unit = synchronized { inFlight -= 1; notifyAll() }
    def halve(): Int    = synchronized { limit = math.max(1, limit / 2); notifyAll(); limit }
    def current: Int    = synchronized(limit)
  }
  object HostLimit { val Start = 8 }

  /** `inner`, each host behind its own [[HostLimit]], and every request counted by host for the throughput line. */
  final class Throttled(inner: tools.HttpFetch, start: Int = HostLimit.Start) extends tools.ForwardingHttpFetch {
    private val limits   = new ConcurrentHashMap[String, HostLimit]()
    private val asked    = new ConcurrentHashMap[String, AtomicLong]()
    private val failed   = new ConcurrentHashMap[String, AtomicLong]()
    private val halvings = new AtomicLong
    private val started  = System.nanoTime()
    private def hostOf(url: String) = scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).getOrElse("?")
    protected def around[T](url: String)(call: tools.HttpFetch => T): T = {
      val host  = hostOf(url)
      val limit = limits.computeIfAbsent(host, _ => new HostLimit(start))
      limit.acquire()
      try {
        asked.computeIfAbsent(host, _ => new AtomicLong).incrementAndGet()
        call(inner)
      } catch {
        case e: tools.HttpStatusException if e.code == 429 || e.code == 503 =>
          val now = limit.halve(); halvings.incrementAndGet()
          System.err.println(s"[what-if] $host answered ${e.code}: at most $now request(s) at once from now on")
          failed.computeIfAbsent(host, _ => new AtomicLong).incrementAndGet(); throw e
        case scala.util.control.NonFatal(e) =>
          failed.computeIfAbsent(host, _ => new AtomicLong).incrementAndGet(); throw e
      } finally limit.release()
    }
    def requests: Long = asked.values.asScala.map(_.get).sum
    /** `N requests (host n, ...; f failed) in Ts, ~r req/s[, k halvings]`, or empty when nothing was asked. */
    def report(): String = if (requests == 0) "" else {
      val seconds = math.max((System.nanoTime() - started) / 1e9, 0.001)
      val byHost  = asked.asScala.toSeq.sortBy(-_._2.get).map { case (h, n) => s"$h ${n.get}" }.mkString(", ")
      val fails   = failed.values.asScala.map(_.get).sum
      f"$requests live request(s) ($byHost; $fails failed) in $seconds%.0fs, ~${requests / seconds}%.1f req/s" +
        (if (halvings.get > 0) s", concurrency halved ${halvings.get} time(s)" else "")
    }
  }
}
