package services.review

import play.api.libs.json.{JsValue, Json}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** One suspect grouping as the review page lists it: a cluster [[services.identity.GroupingCoherence]] found listings of
 *  more than one film in, as `scripts/ci/grouping_suspects.py` ranked it in `grouping-suspects-<cc>.json`. */
final case class SuspectGrouping(country: String, rank: Int, score: Double, flagged: Boolean, film: String, title: String, size: Int,
                                 signals: Seq[String], findings: Seq[String], involved: Seq[String], judgedRight: Int, judgedWrong: Int) {
  /** The film's TMDB page, for a TMDB take. */
  def filmUrl: Option[String] = film.stripPrefix("tmdb:").toIntOption.filter(_ => film.startsWith("tmdb:")).map(id => s"https://www.themoviedb.org/movie/$id")
}

/**
 * The SUSPECT GROUPINGS the review page lists, sorted by suspicion: read from the identity takes check's ranked reports
 * (`grouping-suspects-<cc>.json`, the `identity-takes-<cc>` artifact of a convergence run, or a local resolve dump ranked
 * by `scripts/ci/grouping_suspects.py`) in one directory — `KINOWO_GROUPING_SUSPECTS_DIR`, else `target/grouping-suspects`.
 * The model's stored decisions keep no trace of a match (`DecisionTrace.kept`), so the page cannot work them out from the
 * mirror: it shows what the last whole resolve ranked. None, and a notice, without the directory.
 */
object GroupingSuspects {
  val Env = "KINOWO_GROUPING_SUSPECTS_DIR"
  val DefaultDir: Path = Paths.get("target", "grouping-suspects")

  /** Where the reports are, when there is a directory of them. */
  def locate(env: Map[String, String] = sys.env): Option[Path] =
    Some(env.get(Env).filter(_.nonEmpty).fold(DefaultDir)(Paths.get(_))).filter(Files.isDirectory(_))

  /** Every country's ranked suspects in `dir` (`grouping-suspects-<cc>.json`), a report that cannot be read left out. */
  def read(dir: Path): Map[String, Seq[SuspectGrouping]] =
    Try(Files.list(dir)).toOption.toSeq.flatMap(_.iterator.asScala.toSeq).flatMap { file =>
      val name = file.getFileName.toString
      Option.when(name.startsWith("grouping-suspects-") && name.endsWith(".json"))(name.stripPrefix("grouping-suspects-").stripSuffix(".json"))
        .flatMap(cc => Try(of(cc, Json.parse(Files.readString(file)))).toOption.map(cc -> _))
    }.toMap

  private[review] def of(country: String, report: JsValue): Seq[SuspectGrouping] =
    (report \ "suspects").asOpt[Seq[JsValue]].getOrElse(Nil).map { s =>
      SuspectGrouping(country, (s \ "rank").asOpt[Int].getOrElse(0), (s \ "score").asOpt[Double].getOrElse(0.0),
        (s \ "flagged").asOpt[Boolean].getOrElse(false), (s \ "film").asOpt[String].getOrElse(""), (s \ "title").asOpt[String].getOrElse(""),
        (s \ "size").asOpt[Int].getOrElse(0), (s \ "signals").asOpt[Seq[String]].getOrElse(Nil),
        (s \ "findings").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(f => (f \ "why").asOpt[String].map(why => s"${(f \ "kind").asOpt[String].getOrElse("")}: $why")),
        (s \ "members").asOpt[Seq[JsValue]].getOrElse(Nil).filter(m => (m \ "involved").asOpt[Boolean].contains(true))
          .map(m => s"${(m \ "venue").asOpt[String].getOrElse("")} '${(m \ "rawTitle").asOpt[String].getOrElse("")}'"),
        (s \ "judged_right").asOpt[Int].getOrElse(0), (s \ "judged_wrong").asOpt[Int].getOrElse(0))
    }

  /** The flagged suspects of `countries` (every country read, when none is named), most suspect first, at most `limit`. */
  def ranked(read: Map[String, Seq[SuspectGrouping]], countries: Option[Set[String]], limit: Int): Seq[SuspectGrouping] =
    read.filter { case (cc, _) => countries.forall(_(cc)) }.values.flatten.filter(_.flagged).toSeq
      .sortBy(s => (-s.score, -s.involved.size, -s.size, s.country, s.rank)).take(limit)
}
