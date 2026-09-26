package tools

import models.ResolvedMovie
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.json.{JsonMode, JsonWriterSettings}
import org.bson.{BsonArray, BsonDocument, BsonDocumentReader, BsonDocumentWriter, BsonValue}
import services.movies.{MovieCodecs, StoredMovieDto, StoredMovieRecord, TitleNormalizer}
import services.readmodel.{Derivation, DerivationScope, DerivationVersion, ReadModelProjection}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import scala.jdk.CollectionConverters.*

/**
 * THE DERIVATION CORPUS: what decides whether a change moved what the projection DERIVES from a row,
 * as opposed to moving the rows.
 *
 * Two checked-in files beside the read-model snapshot. `read-model-derivation-rows.jsonl` holds
 * every ready row of the fixture pipeline, one per line, written through the PROD codec
 * (`StoredMovieDto` + `MovieCodecs`) — so reading it back under newer code is exactly what a
 * worker does with documents an older one wrote, new fields defaulted. `read-model-derivation-
 * hashes.tsv` holds what those rows projected to when they were captured: per row, one hash per
 * card part and one for its screenings, under a header naming the derivation that made them.
 *
 * Projecting the checked-in rows again under the current code and comparing with the checked-in
 * hashes answers the question with no judgment: the same input, the old code's output against the
 * new code's. A scraper change moves the rows and never the answer (2026-09-26: the four
 * derivation versions that shipped that night each moved 0 of ~720 rows this way, where the old
 * snapshot fingerprint had re-projected every country's corpus for them); the 2026-09-24 poster
 * change moves 58 rows, their poster alone, and so owes a cards-only pass.
 *
 * Row ids follow the scrape's arrival order, so each row is written under a stand-in derived from
 * its own content: the file is a pure function of the corpus, which CI's unchanged-tree check needs.
 */
object ReadModelDerivationCorpus {

  val RowsPath: Path   = Paths.get("test", "resources", "fixtures", "08-06-2026", "read-model-derivation-rows.jsonl")
  val HashesPath: Path = Paths.get("test", "resources", "fixtures", "08-06-2026", "read-model-derivation-hashes.tsv")

  private val HeaderPrefix = "# derivation "
  private val Screenings   = "screenings"
  private val CardSet      = "cards"

  /** The parts whose move a cards-only pass repairs: everything a card shows. `whole` catches a
   *  card field none of the named parts covers yet. */
  private val CardParts: Seq[(String, ResolvedMovie => Any)] = Seq(
    "title"          -> (m => (m.title, m.originalTitle)),
    "poster"         -> (m => (m.posterUrl, m.fallbackPosterUrls)),
    "facts"          -> (m => (m.runtimeMinutes, m.releaseYear, m.genres, m.countries, m.directors, m.cast)),
    "synopsis"       -> (m => m.synopsis),
    "synopsisByCity" -> (m => m.synopsisByCity),
    "ratings"        -> (m => (m.ratings, m.weightedRating)),
    "trailers"       -> (m => m.trailerUrls),
    "ageRating"      -> (m => m.ageRating),
    "whole"          -> (m => m))

  /** One row's projection, by part. */
  final case class RowHashes(id: String, parts: Map[String, String], title: String)

  /** A row whose projection under the current code differs from the one recorded, and in what. */
  final case class Moved(id: String, title: String, parts: Seq[String])

  /** The checked-in hashes: the derivation named in the header, and each row's parts. */
  final case class Recorded(derivation: Derivation, rows: Map[String, RowHashes])

  /** What a regeneration writes, and the derivation it names — `bumped` when it is a new one. */
  final case class Regeneration(rowsText: String, hashesText: String, derivation: Derivation, bumped: Boolean, moved: Seq[Moved])

  private lazy val codec = MovieCodecs.registry.get(classOf[StoredMovieDto])
  private val jsonSettings = JsonWriterSettings.builder().outputMode(JsonMode.RELAXED).build()

  private def encode(row: StoredMovieRecord, id: String): String = {
    val dto = row.storedKey.fold(StoredMovieDto.fromDomain(id, row.record, Instant.EPOCH))(key =>
      StoredMovieDto.fromDomain(id, key, row.record, Instant.EPOCH))
    val document = new BsonDocument()
    codec.encode(new BsonDocumentWriter(document), dto, EncoderContext.builder().build())
    sortedKeys(document).asDocument().toJson(jsonSettings)
  }

  /** The document with every key in order, however deep: `sourceData` is a map keyed by cinema,
   *  and a map's iteration order is not the corpus's. The codec reads fields by name. */
  private def sortedKeys(value: BsonValue): BsonValue = value match {
    case document: BsonDocument =>
      val sorted = new BsonDocument()
      document.keySet().asScala.toSeq.sorted.foreach(key => sorted.put(key, sortedKeys(document.get(key))))
      sorted
    case array: BsonArray => new BsonArray(array.getValues.asScala.map(sortedKeys).asJava)
    case other            => other
  }

  def decode(line: String, normalizer: TitleNormalizer): StoredMovieRecord =
    StoredMovieDto.toDomain(codec.decode(new BsonDocumentReader(BsonDocument.parse(line)), DecoderContext.builder().build()), normalizer)

  /** Every ready row, each under the stand-in its content names, sorted. */
  def renderRows(rows: Seq[StoredMovieRecord]): String = {
    val standIns = rows.filter(_.record.readyToProject).map(row => row -> ("d" + sha(encode(row, "-"))))
    val unique = standIns.groupBy(_._2).toSeq.flatMap { case (standIn, same) =>
      same.map(_._1).sortBy(encode(_, "-")).zipWithIndex.map { case (row, i) => (if (i == 0) standIn else s"$standIn-$i") -> row }
    }
    unique.sortBy(_._1).map { case (id, row) => encode(row, id) }.mkString("", "\n", "\n")
  }

  def parseRows(text: String, normalizer: TitleNormalizer): Seq[StoredMovieRecord] =
    text.linesIterator.filter(_.nonEmpty).map(decode(_, normalizer)).toSeq

  def hashes(row: StoredMovieRecord, normalizer: TitleNormalizer): RowHashes = {
    val projected = ReadModelProjection.partition(row, normalizer).projectAll
    val cards     = projected.map(_._1)
    def part(f: ResolvedMovie => Any): String = sha(cards.map(card => card._id + ":" + canon(f(card))).mkString("|"))
    val parts = (CardSet -> sha(cards.map(_._id).mkString("|"))) +:
      CardParts.map { case (name, f) => name -> part(f) } :+
      (Screenings -> sha(projected.flatMap(_._2).map(canon).sorted.mkString("|")))
    RowHashes(row.id.value, parts.toMap, row.title)
  }

  def renderHashes(derivation: Derivation, rows: Seq[RowHashes]): String =
    (s"$HeaderPrefix${derivation.version.value} ${derivation.scope.label}" +:
      rows.sortBy(_.id).map(r => (r.id +: r.parts.toSeq.sorted.map { case (k, v) => s"$k=$v" } :+ r.title).mkString("\t")))
      .mkString("", "\n", "\n")

  def parseHashes(text: String): Recorded = {
    val lines  = text.linesIterator.filter(_.nonEmpty).toSeq
    val header = lines.headOption.filter(_.startsWith(HeaderPrefix))
      .getOrElse(throw new IllegalArgumentException(s"$HashesPath has no '$HeaderPrefix<version> <scope>' header"))
    val Array(version, scope) = header.stripPrefix(HeaderPrefix).split(' ')
    val rows = lines.tail.map { line =>
      val fields = line.split('\t')
      RowHashes(fields.head, fields.slice(1, fields.length - 1).map(_.split("=", 2)).map(kv => kv(0) -> kv(1)).toMap, fields.last)
    }
    Recorded(Derivation(DerivationVersion(version), DerivationScope.parse(scope)), rows.map(r => r.id -> r).toMap)
  }

  /** The rows whose projection under the current code is not the one recorded. */
  def moved(recorded: Recorded, rows: Seq[StoredMovieRecord], normalizer: TitleNormalizer): Seq[Moved] =
    rows.flatMap { row =>
      val now = hashes(row, normalizer)
      recorded.rows.get(now.id) match {
        case None       => Some(Moved(now.id, now.title, Seq("row not recorded")))
        case Some(recordedRow) =>
          val parts = now.parts.keySet.union(recordedRow.parts.keySet).toSeq.sorted.filter(p => now.parts.get(p) != recordedRow.parts.get(p))
          Option.when(parts.nonEmpty)(Moved(now.id, now.title, parts))
      }
    }.sortBy(_.id)

  /** Cards alone when every move was to a part a card shows. */
  def scopeOf(moved: Seq[Moved]): DerivationScope =
    if (moved.forall(_.parts.forall(part => CardParts.exists(_._1 == part)))) DerivationScope.Cards else DerivationScope.Full

  /** The corpus to check in for `fresh` (the pipeline's rows now), given what is checked in: a new
   *  derivation exactly when the checked-in rows project differently under the current code, named
   *  by the previous one and what moved, so it is the same on every machine. With nothing checked
   *  in, nothing can be compared: a new derivation owing everything. */
  def regenerate(fresh: Seq[StoredMovieRecord], normalizer: TitleNormalizer, checkedIn: Option[(String, String)]): Regeneration = {
    val rowsText = renderRows(fresh)
    val rows     = parseRows(rowsText, normalizer)
    val (derivation, bumped, moves) = checkedIn match {
      case None =>
        (Derivation(DerivationVersion(sha("no recorded derivation|" + rowsText)), DerivationScope.Full), true, Nil)
      case Some((oldRows, oldHashes)) =>
        val recorded = parseHashes(oldHashes)
        val moves    = moved(recorded, parseRows(oldRows, normalizer), normalizer)
        if (moves.isEmpty) (recorded.derivation, false, Nil)
        else {
          val next = sha(recorded.derivation.version.value + "|" + moves.map(m => m.id + ":" + m.parts.mkString(",")).mkString(";"))
          (Derivation(DerivationVersion(next), scopeOf(moves)), true, moves)
        }
    }
    Regeneration(rowsText, renderHashes(derivation, rows.map(hashes(_, normalizer))), derivation, bumped, moves)
  }

  def readCheckedIn(): Option[(String, String)] =
    Option.when(Files.exists(RowsPath) && Files.exists(HashesPath))((Files.readString(RowsPath, UTF_8), Files.readString(HashesPath, UTF_8)))

  def write(regeneration: Regeneration): Unit = {
    Files.createDirectories(RowsPath.getParent)
    Files.writeString(RowsPath, regeneration.rowsText, UTF_8)
    Files.writeString(HashesPath, regeneration.hashesText, UTF_8)
  }

  /** The line `ReadModelDerivation.History` owes a new derivation. */
  def historyEntry(derivation: Derivation): String =
    s"""Derivation(DerivationVersion("${derivation.version.value}"), DerivationScope.${derivation.scope})"""

  private val ObjectIdentity = "@[0-9a-f]{4,}$".r

  /** A rendering independent of map and set iteration order. Throws on a value whose `toString`
   *  is the default identity form, which would differ from one run to the next. */
  private[tools] def canon(value: Any): String = value match {
    case null                        => "null"
    case s: String                   => "\"" + s + "\""
    case o: Option[?]                => o.fold("None")(v => "Some(" + canon(v) + ")")
    case m: scala.collection.Map[?, ?] =>
      m.iterator.map { case (k, v) => canon(k) + "->" + canon(v) }.toSeq.sorted.mkString("Map(", ",", ")")
    case s: scala.collection.Set[?]  => s.iterator.map(canon).toSeq.sorted.mkString("Set(", ",", ")")
    case s: Iterable[?]              => s.iterator.map(canon).mkString("Seq(", ",", ")")
    case a: Array[?]                 => a.iterator.map(canon).mkString("Array(", ",", ")")
    case p: Product if p.productArity > 0 =>
      p.productPrefix + (0 until p.productArity).map(i => p.productElementName(i) + "=" + canon(p.productElement(i))).mkString("(", ",", ")")
    case other =>
      val rendered = other.toString
      if (ObjectIdentity.findFirstIn(rendered).isDefined)
        throw new IllegalStateException(s"no stable rendering of $rendered (${other.getClass.getName})")
      rendered
  }

  private def sha(text: String): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)).take(8).map(b => f"${b & 0xff}%02x").mkString
}
