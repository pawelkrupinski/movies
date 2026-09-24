package tools.contracts

import play.api.libs.json.{JsObject, Json}

import java.nio.file.{Files, Path, Paths}

/**
 * The checked-in retry/error classification table (`test/resources/retry-classification.json`)
 * that the Scala, Swift and Kotlin specs each hold their production classifiers to, row by row.
 *
 * Why one table: the same line — is this failure final, worth retrying, or the egress route's
 * own fault — was drawn in a dozen places and got redrawn wrong eight times (a Zyte 429 opening
 * the origin's breaker, a require failure retried to exhaustion, a Cloudflare 403 dropping a
 * user's language pick on three platforms). Each platform's spec reads this file, so a verdict
 * changed in one place and not the others fails a test instead of shipping.
 */
object RetryClassificationTable {

  enum Verdict { case Permanent, Transient, Provider }

  object Verdict {
    def parse(s: String): Verdict = s match {
      case "permanent" => Permanent
      case "transient" => Transient
      case "provider"  => Provider
      case other       => throw new IllegalArgumentException(s"unknown verdict '$other'")
    }
  }

  final case class Row(source: String, error: String, verdict: Verdict) {
    override def toString: String = s"$source/$error → ${verdict.toString.toLowerCase}"

    /** The status of an `http:NNN` / `tunnel:NNN` error, when it names one. */
    def status: Option[Int] = error.split(':') match {
      case Array(_, code) => code.toIntOption
      case _              => None
    }
  }

  final case class Table(sources: Map[String, Seq[String]], rows: Seq[Row]) {
    def rowsFor(source: String): Seq[Row] = {
      require(sources.contains(source), s"no source '$source' in the table")
      rows.filter(_.source == source)
    }

    /** Every source a platform's code enforces — its spec must cover each of them. */
    def sourcesConsumedBy(platform: String): Set[String] =
      sources.collect { case (source, consumers) if consumers.contains(platform) => source }.toSet
  }

  val FileName = "retry-classification.json"

  /** The table file, found by walking up from the working directory to the repo root — sbt
   *  runs the web module's page tests from the root and forks others from their module dir. */
  def path: Path =
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("test").resolve("resources").resolve(FileName))
      .find(Files.isRegularFile(_))
      .getOrElse(throw new IllegalStateException(s"test/resources/$FileName not found above ${Paths.get("").toAbsolutePath}"))

  lazy val load: Table = parse(Files.readString(path))

  def parse(json: String): Table = {
    val root    = Json.parse(json).as[JsObject]
    val sources = (root \ "sources").as[JsObject].fields.map { case (name, body) =>
      name -> (body \ "consumers").as[Seq[String]]
    }.toMap
    val rows = (root \ "rows").as[Seq[JsObject]].map { row =>
      Row((row \ "source").as[String], (row \ "error").as[String], Verdict.parse((row \ "verdict").as[String]))
    }
    rows.filterNot(r => sources.contains(r.source)).foreach(r => throw new IllegalArgumentException(s"row $r names an undeclared source"))
    rows.groupBy(r => (r.source, r.error)).collect { case (key, dups) if dups.size > 1 => key }
      .foreach(key => throw new IllegalArgumentException(s"duplicate row $key"))
    Table(sources, rows)
  }
}
