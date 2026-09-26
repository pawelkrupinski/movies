package scripts

import models.SourceData
import play.api.libs.json.{JsObject, JsValue, Json}
import services.identity.{IdentityCalibration, IdentityMeasures, StoredIdentityConfidence}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream
import scala.jdk.CollectionConverters.*

/**
 * What switching the rating gate on (`KINOWO_IDENTITY_RATING_GATE`) would hide, per country,
 * BEFORE it is switched: every rated film a venue lists that the gate
 * (`RatingGate.fromEvidence`, through [[StoredIdentityConfidence]]) would withhold, judged against
 * the calibration's labelled set — a hide of a film the labels corroborate is a FALSE hide, of one
 * they contradict a TRUE hide.
 *
 * Input: a read-only JSONL export of production rows, one file per country (`rows-<cc>.jsonl`:
 * `{country, id, tmdbId, rated, screening, tmdb: <slot>, venues: [{venue, <slot>}]}`, written
 * by a mongosh `find` over `movies` + `movie_slots` + `web_screenings`).
 *
 *   sbt "worker/Test/runMain scripts.IdentityGateImpact rows-pl.jsonl rows-uk.jsonl …"
 */
object IdentityGateImpact {

  /** Listings the labels hold for the pipeline's own enrichment slots, not a venue's listing. */
  private val PseudoVenues = Set("TMDB", "IMDB", "EM", "IL KINO")

  final case class Label(tmdbId: Option[Int], status: String)

  private def slot(js: JsValue): SourceData = SourceData(
    title          = (js \ "title").asOpt[String],
    rawTitle       = (js \ "rawTitle").asOpt[String],
    originalTitle  = (js \ "originalTitle").asOpt[String],
    englishTitle   = (js \ "englishTitle").asOpt[String],
    releaseYear    = (js \ "releaseYear").asOpt[Int],
    runtimeMinutes = (js \ "runtimeMinutes").asOpt[Int],
    director       = (js \ "director").asOpt[Seq[String]].getOrElse(Nil),
    countries      = (js \ "countries").asOpt[Seq[String]].getOrElse(Nil))

  private def labels(path: Path): Map[(String, String, String), Seq[Label]] = {
    val in = new GZIPInputStream(Files.newInputStream(path))
    val js = try Json.parse(in) finally in.close()
    (js \ "listings").as[Seq[JsValue]].iterator
      .filterNot(l => PseudoVenues((l \ "venue").as[String]))
      .map(l => ((l \ "country").as[String], (l \ "venue").as[String], IdentityMeasures.key((l \ "title").as[String])) ->
        Label((l \ "tmdbId").asOpt[Int], (l \ "status").asOpt[String].getOrElse("")))
      .toSeq.groupMap(_._1)(_._2)
  }

  def main(args: Array[String]): Unit = {
    val calibration = IdentityCalibration.default
    val labelled    = labels(Paths.get("test/resources/fixtures/identity/identity-labels.json.gz"))
    println(s"calibration ${calibration.version}; showRatings threshold " +
      calibration.scopes(IdentityMeasures.ListingFilm).thresholds("showRatings").probability)
    println("| country | listed films | rated | hidden | hidden % of rated | labelled corroborated (false hides) | labelled contradicted (true hides) | unlabelled | unscored (tmdbId, no TMDB slot) |")
    val examples = Seq.newBuilder[String]
    args.foreach { file =>
      val rows = Files.readAllLines(Paths.get(file), StandardCharsets.UTF_8).asScala.filter(_.trim.nonEmpty).map(Json.parse).toSeq
      val country = rows.headOption.fold("?")(r => (r \ "country").as[String])
      val listed  = rows.filter(r => (r \ "venues").as[Seq[JsValue]].nonEmpty)
      val rated   = listed.filter(r => (r \ "rated").as[Boolean])
      val hidden  = rated.flatMap { r =>
        val venues = (r \ "venues").as[Seq[JsValue]].map(v => (v \ "venue").as[String] -> StoredIdentityConfidence.listing(slot(v)))
        for {
          tmdbId <- (r \ "tmdbId").asOpt[Int]
          tmdb   <- (r \ "tmdb").asOpt[JsObject]
          p      <- StoredIdentityConfidence.of(StoredIdentityConfidence.film(slot(tmdb)), venues, calibration)
          if !calibration.showsRatings(p)
        } yield {
          val ls = venues.flatMap { case (venue, l) => labelled.getOrElse((country, venue, IdentityMeasures.key(l.title)), Nil) }
            .filter(_.tmdbId.forall(_ == tmdbId))
          val verdict =
            if (ls.exists(_.status == "contradicted")) "contradicted"
            else if (ls.exists(_.status == "corroborated")) "corroborated"
            else "unlabelled"
          (r, venues, tmdbId, p, verdict)
        }
      }
      def count(v: String) = hidden.count(_._5 == v)
      val unscored = rated.count(r => (r \ "tmdbId").asOpt[Int].isDefined && (r \ "tmdb").asOpt[JsObject].isEmpty)
      println(f"| $country | ${listed.size} | ${rated.size} | ${hidden.size} | ${100.0 * hidden.size / math.max(1, rated.size)}%.1f%% | " +
        s"${count("corroborated")} | ${count("contradicted")} | ${count("unlabelled")} | $unscored |")
      examples += s"\n### $country — hidden examples (title · tmdbId · p · labels)"
      hidden.sortBy(h => (h._5 != "corroborated", h._4)).take(20).foreach { case (_, venues, tmdbId, p, verdict) =>
        val (venue, l) = venues.head
        examples += f"- ${l.title} ($venue${if (venues.size > 1) s" +${venues.size - 1}" else ""}) · $tmdbId · $p%.2f · $verdict"
      }
    }
    examples.result().foreach(println)
  }
}
