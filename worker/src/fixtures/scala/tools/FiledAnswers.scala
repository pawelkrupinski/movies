package tools

import org.bson.BsonDocument
import services.identity._

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * The answers a recording does not hold that prod and the capture read: the family answers PROD filed
 * (`identity_family_answers`), the IMDb-id finds, and the TMDB answers the unmatched-cluster capture asked LIVE where its
 * recording had a gap (`KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`) — all as an unmatched-cluster capture holds them. A hermetic
 * leg replays none of them, and its agreement takes nothing a family must agree on. An identity slice reads them only
 * where the recording does not answer, notes which it read ([[Noting]], [[Lookups]]), keeps exactly those, and names
 * them as filed, never as the recording's.
 */
final case class FiledAnswers(capture: UnmatchedClusters.Capture, source: String,
                              /** The capture's live HTTP answers ([[FiledAnswers.liveOf]]): held beside `capture`, never in it, so
                               *  this file compiles dropped into a ref whose `Capture` predates them (identity-slice's tool files). */
                              live: Map[String, String] = Map.empty) {
  def families: Map[String, BsonDocument] = capture.families
  def finds: Map[String, Option[Int]] = capture.finds
  def isEmpty: Boolean = capture.families.isEmpty && capture.finds.isEmpty && capture.queries.isEmpty && capture.films.isEmpty &&
    capture.details.isEmpty && capture.casts.isEmpty && live.isEmpty
  /** How many answers of each kind it holds. */
  def render: String = s"${capture.families.size} family answer(s), ${capture.finds.size} IMDb find(s), ${capture.queries.size} TMDB search(es), " +
    s"${capture.films.size} TMDB record(s), ${capture.details.size} venue page(s), ${live.size} live HTTP answer(s)"
}

object FiledAnswers {
  def of(country: models.Country): FiledAnswers = FiledAnswers(UnmatchedClusters.Capture(country, Nil, Nil, Map.empty, Map.empty, Map.empty, Set.empty,
    Map.empty, Map.empty), "")

  /** An unmatched-cluster capture's answers. */
  def ofCapture(path: Path): FiledAnswers = FiledAnswers(UnmatchedClusters.read(path), path.toString, liveOf(path))

  /** A capture file's live HTTP answers (`UnmatchedClusters.Capture.live`, its `live` array), read off the file itself —
   *  none in a capture from before them. */
  def liveOf(path: Path): Map[String, String] = {
    val in = new java.util.zip.GZIPInputStream(Files.newInputStream(path))
    val d  = try BsonDocument.parse(new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)) finally in.close()
    if (!d.containsKey("live")) Map.empty
    else d.getArray("live").getValues.asScala.map(_.asDocument).map(l => l.getString("request").getValue -> l.getString("body").getValue).toMap
  }

  /** The family documents `filed` seeds a store with, every one of them read noted in `read`. */
  final class Noting(filed: FiledAnswers) extends TmdbDocuments {
    private val inner = new InMemoryTmdbDocuments
    inner.put(TmdbKind.Family, filed.families.toSeq)
    val read = new ConcurrentHashMap[String, BsonDocument]()
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      val found = inner.get(kind, ids)
      if (kind == TmdbKind.Family) found.keys.foreach(id => filed.families.get(id).foreach(d => if (d == found(id)) read.put(id, d)))
      found
    }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = inner.put(kind, docs)
  }

  /** `recorded`'s answers, and `filed`'s where the recording has none (`Unknown`): each filed one read noted. */
  final class Lookups(recorded: IdentityLookups, filed: FiledAnswers) extends IdentityLookups {
    val queries = new ConcurrentHashMap[CandidateQuery, Seq[Hit]]()
    val films   = new ConcurrentHashMap[Int, Option[IdentityMeasures.Film]]()
    val details = new ConcurrentHashMap[String, Option[DetailFacts]]()
    val casts   = new ConcurrentHashMap[Int, Option[Seq[String]]]()
    private def orFiled[A](answer: Answer[A], held: Option[A], note: A => Any): Answer[A] = answer match {
      case Answer.Unknown => held.fold[Answer[A]](Answer.Unknown) { a => note(a); Answer.Known(a) }
      case known          => known
    }
    private val key = (l: Listing) => services.movies.ListingKey.serialised(l.key)
    override def hasDetail(listing: Listing): Boolean = recorded.hasDetail(listing) || filed.capture.withDetail(key(listing))
    override def detail(listing: Listing): Answer[Option[DetailFacts]] =
      orFiled(recorded.detail(listing), filed.capture.details.get(key(listing)), (d: Option[DetailFacts]) => details.put(key(listing), d))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
      orFiled(recorded.candidates(query), filed.capture.queries.get(query), (h: Seq[Hit]) => queries.put(query, h))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
      orFiled(recorded.film(tmdbId), filed.capture.films.get(tmdbId), (f: Option[IdentityMeasures.Film]) => films.put(tmdbId, f))
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] =
      orFiled(recorded.cast(tmdbId), filed.capture.casts.get(tmdbId), (c: Option[Seq[String]]) => casts.put(tmdbId, c))
    override def prefetch(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[Listing]): Unit = recorded.prefetch(qs, ids, pages)
  }

  /** What a slice read of `filed`: its family answers and finds, and the lookups' filed answers. */
  def read(filed: FiledAnswers, families: Map[String, BsonDocument], finds: Map[String, Option[Int]], lookups: Option[Lookups]): FiledAnswers =
    FiledAnswers(filed.capture.copy(listings = Nil, decisions = Nil, families = families, finds = finds,
      queries = lookups.fold(Map.empty[CandidateQuery, Seq[Hit]])(_.queries.asScala.toMap),
      films = lookups.fold(Map.empty[Int, Option[IdentityMeasures.Film]])(_.films.asScala.toMap),
      details = lookups.fold(Map.empty[String, Option[DetailFacts]])(_.details.asScala.toMap),
      withDetail = lookups.fold(Set.empty[String])(_.details.keySet.asScala.toSet),
      casts = lookups.fold(Map.empty[Int, Option[Seq[String]]])(_.casts.asScala.toMap), unreachable = Set.empty), filed.source) // the live HTTP answers a slice read are in its own HTTP notes

  /** `filed` with `families` — prod's own, read from the local prod mirror ([[ProdMirror]]) — in preference to its own. */
  def preferring(filed: FiledAnswers, families: Map[String, BsonDocument], source: String): FiledAnswers =
    FiledAnswers(filed.capture.copy(families = filed.families ++ families), if (filed.source.isEmpty) source else s"$source (over ${filed.source})")

  /** Two reads' answers together. */
  def merge(a: FiledAnswers, b: FiledAnswers): FiledAnswers =
    FiledAnswers(b.capture.copy(families = a.families ++ b.families, finds = a.finds ++ b.finds, queries = a.capture.queries ++ b.capture.queries,
      films = a.capture.films ++ b.capture.films, details = a.capture.details ++ b.capture.details, withDetail = a.capture.withDetail ++ b.capture.withDetail,
      casts = a.capture.casts ++ b.capture.casts), Seq(a.source, b.source).filter(_.nonEmpty).distinct.mkString(", "), a.live ++ b.live)

  /** The capture's live HTTP answers ([[FiledAnswers.live]]) answered before `gap` — the recording's leaf,
   *  asked only for what the recording does not hold — so a captured model decision that read an IMDb suggestion live
   *  reads it again. `served` counts the answers it gave. */
  final class LiveLeaf(live: Map[String, String], gap: HttpFetch) extends HttpFetch {
    private val count = new java.util.concurrent.atomic.AtomicInteger
    def served: Int = count.get
    private def answer(request: String)(orGap: => String): String =
      live.get(request).fold(orGap) { body => count.incrementAndGet(); body }
    override def get(url: String): String = answer(s"GET $url")(gap.get(url))
    override def get(url: String, headers: Map[String, String]): String = answer(s"GET $url")(gap.get(url, headers))
    override def getBytes(url: String): Array[Byte] = gap.getBytes(url)
    override def post(url: String, body: String, contentType: String): String = answer(s"POST $url $body")(gap.post(url, body, contentType))
  }

  /** Kept as an unmatched-cluster capture of no clusters, its source beside it. */
  def write(dir: Path, answers: FiledAnswers): Unit = {
    UnmatchedClusters.write(dir.resolve("filed.json.gz"), answers.capture)
    Files.writeString(dir.resolve("filed.source"), answers.source)
  }

  def readFrom(dir: Path, country: models.Country): FiledAnswers =
    if (!Files.exists(dir.resolve("filed.json.gz"))) of(country)
    else FiledAnswers(UnmatchedClusters.read(dir.resolve("filed.json.gz")),
      Option(dir.resolve("filed.source")).filter(Files.exists(_)).map(Files.readString(_)).getOrElse(""))
}
