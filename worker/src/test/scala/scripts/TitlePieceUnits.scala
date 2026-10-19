package scripts

import play.api.libs.json.Json
import services.identity.{IdentityCalibration, IdentityMeasures}
import services.identity.IdentityMeasures.{Category, Measure, Missing, Number}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * EXPERIMENT (experiment/title-head): the refit's labelled units ([[IdentityRefit.labelledUnits]]) over the five
 * captures, one TSV row each — country, node, film, fold group, same film or not, the `title` and `title.piece`
 * measures, and the calibration's log-odds and probability — for an offline read of whether `title.piece` separates
 * right from wrong.
 *
 *   sbt "worker/Test/runMain scripts.TitlePieceUnits <out.tsv>"
 */
object TitlePieceUnits {
  private def show(m: Option[Measure]): String = m match {
    case Some(Category(v)) => v
    case Some(Number(x))   => x.toString
    case Some(Missing(s))  => s"missing:$s"
    case _                 => ""
  }

  def main(args: Array[String]): Unit = {
    val out         = Path.of(args.headOption.getOrElse("target/title-piece-units.tsv"))
    val labels      = tools.IdentityJudgements.labels()
    val calibration = Json.parse(Files.readString(IdentityRefit.Artefact)).as[IdentityCalibration]
    val scope       = IdentityMeasures.ListingFilm
    val rows = CaptureReplay.all().flatMap(r => IdentityRefit.labelledUnits(r.capture, labels, calibration)).map { u =>
      Seq(u.country, u.node.replace('\u0000', '|').replace('\t', ' '), u.tmdbId.toString, u.group, u.same.toString,
        show(u.measures.get("title")), show(u.measures.get("title.piece")),
        f"${calibration.logOdds(scope, u.measures)}%.4f", f"${calibration.probability(scope, u.measures)}%.4f").mkString("\t")
    }
    Files.createDirectories(out.toAbsolutePath.getParent)
    Files.writeString(out, ("country\tnode\ttmdb\tgroup\tsame\ttitle\ttitlePiece\tlogOdds\tprobability" +: rows).mkString("", "\n", "\n"),
      StandardCharsets.UTF_8)
    println(s"wrote ${rows.size} units to $out")
  }
}
