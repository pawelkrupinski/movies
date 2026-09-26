package scripts

import models.SourceData
import play.api.libs.json.{JsObject, JsValue, Json}
import models.TitleSearch
import services.identity.{Hit, IdentityCalibration, IdentityMeasures, StoredIdentityConfidence}

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
 * With `--trees <dir of enrichment-<cc>/>`, a row whose TMDB slot stores no title searches is
 * measured from the recorded answers instead (`IdentityMeasures.titleSearch`, the pipeline's own
 * measure) — what a backfill would store — and `--plan <dir>` writes those measurements as
 * `title-searches-<cc>.jsonl` for `scripts/identity-gate/backfill-title-searches.js`.
 *
 *   sbt "worker/Test/runMain scripts.IdentityGateImpact [--trees DIR] [--plan DIR] rows-pl.jsonl rows-uk.jsonl …"
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

  private def searches(js: JsValue): Seq[TitleSearch] = (js \ "titleSearches").asOpt[Seq[JsValue]].getOrElse(Nil).map(t =>
    TitleSearch((t \ "titleKey").as[String], (t \ "rank").asOpt[Int], (t \ "rivals").as[Int]))

  def main(argv: Array[String]): Unit = {
    def option(name: String) = argv.sliding(2).collectFirst { case Array(`name`, v) => Paths.get(v) }
    val trees = option("--trees"); val plan = option("--plan")
    val args  = argv.zipWithIndex.filterNot { case (a, i) => a.startsWith("--") || (i > 0 && argv(i - 1).startsWith("--")) }.map(_._1)
    val calibration = IdentityCalibration.default
    val labelled    = labels(Paths.get("test/resources/fixtures/identity/identity-labels.json.gz"))
    println(s"calibration ${calibration.version}; showRatings threshold " +
      calibration.scopes(IdentityMeasures.ListingFilm).thresholds("showRatings").probability)
    println("| country | listed films | rated | with a title search | hidden | hidden % of rated | labelled corroborated (false hides) | labelled contradicted (true hides) | unlabelled | unscored (tmdbId, no TMDB slot) |")
    val examples = Seq.newBuilder[String]
    args.foreach { file =>
      val rows = Files.readAllLines(Paths.get(file), StandardCharsets.UTF_8).asScala.filter(_.trim.nonEmpty).map(Json.parse).toSeq
      val country = rows.headOption.fold("?")(r => (r \ "country").as[String])
      val answers = trees.map(t => new IdentityCalibrationData.TmdbAnswers(Seq(t.resolve(s"enrichment-$country")).filter(Files.isDirectory(_)),
        Map.empty, IdentityCalibrationData.languageOf(models.Country.byCode(country).get)))
      val listed  = rows.filter(r => (r \ "venues").as[Seq[JsValue]].nonEmpty)
      val rated   = listed.filter(r => (r \ "rated").as[Boolean])
      // What each row's TMDB slot stores, else what the recorded answers measure for it.
      val measured: Map[String, Seq[TitleSearch]] = listed.flatMap { r =>
        for {
          tmdbId <- (r \ "tmdbId").asOpt[Int]
          tmdb   <- (r \ "tmdb").asOpt[JsObject]
        } yield (r \ "id").as[String] -> {
          val stored = searches(tmdb)
          if (stored.nonEmpty) stored
          else answers.fold(Seq.empty[TitleSearch]) { a =>
            val search = (q: String) => a.search(q).map(_.map(h => Hit(h.id, h.title, h.originalTitle, h.year, h.popularity)))
            (r \ "venues").as[Seq[JsValue]].map(v => StoredIdentityConfidence.listing(slot(v))).filter(_.title.trim.nonEmpty)
              .sortBy(l => (l.title, l.rawTitle.getOrElse(""))).distinctBy(l => IdentityMeasures.key(l.title))
              .flatMap(IdentityMeasures.titleSearch(_, tmdbId, search)).sortBy(_.titleKey)
          }
        }
      }.toMap
      plan.foreach { dir =>
        Files.createDirectories(dir)
        val lines = listed.flatMap(r => measured.get((r \ "id").as[String]).filter(_.nonEmpty).filter(_ => searches((r \ "tmdb").get).isEmpty)
          .map(ts => Json.stringify(Json.obj("filmId" -> (r \ "id").as[String], "tmdbId" -> (r \ "tmdbId").as[Int],
            "titleSearches" -> ts.map(t => Json.obj("titleKey" -> t.titleKey, "rivals" -> t.rivals) ++
              t.rank.fold(Json.obj())(k => Json.obj("rank" -> k)))))))
        Files.write(dir.resolve(s"title-searches-$country.jsonl"), lines.asJava, StandardCharsets.UTF_8)
      }
      val hidden  = rated.flatMap { r =>
        val venues = (r \ "venues").as[Seq[JsValue]].map(v => (v \ "venue").as[String] -> StoredIdentityConfidence.listing(slot(v)))
        val found  = measured.getOrElse((r \ "id").as[String], Nil)
        for {
          tmdbId <- (r \ "tmdbId").asOpt[Int]
          tmdb   <- (r \ "tmdb").asOpt[JsObject]
          p      <- StoredIdentityConfidence.of(StoredIdentityConfidence.film(slot(tmdb)), venues, found, calibration)
          if !calibration.showsRatings(p)
        } yield {
          val ls = venues.flatMap { case (venue, l) => labelled.getOrElse((country, venue, IdentityMeasures.key(l.title)), Nil) }
            .filter(_.tmdbId.forall(_ == tmdbId))
          val verdict =
            if (ls.exists(_.status == "contradicted")) "contradicted"
            else if (ls.exists(_.status == "corroborated")) "corroborated"
            else "unlabelled"
          (r, venues, tmdbId, p, verdict, found)
        }
      }
      def count(v: String) = hidden.count(_._5 == v)
      val unscored = rated.count(r => (r \ "tmdbId").asOpt[Int].isDefined && (r \ "tmdb").asOpt[JsObject].isEmpty)
      val searched = rated.count(r => measured.get((r \ "id").as[String]).exists(_.nonEmpty))
      println(f"| $country | ${listed.size} | ${rated.size} | $searched | ${hidden.size} | ${100.0 * hidden.size / math.max(1, rated.size)}%.1f%% | " +
        s"${count("corroborated")} | ${count("contradicted")} | ${count("unlabelled")} | $unscored |")
      examples += s"\n### $country — hidden: the labelled ones, then 30 unlabelled (title [venue +n] · year/runtime/director any venue states · TMDB film · search rank/rivals · p · labels)"
      val (judged, unjudged) = hidden.partition(_._5 != "unlabelled")
      (judged ++ scala.util.Random(7).shuffle(unjudged).take(30)).foreach { case (r, venues, tmdbId, p, verdict, found) =>
        val (venue, l) = venues.head
        val t = slot((r \ "tmdb").get)
        val facts = Seq(venues.flatMap(_._2.year).distinct.mkString("/"), venues.flatMap(_._2.runtime).distinct.mkString("/"),
          venues.flatMap(_._2.directors).distinct.take(2).mkString("/")).mkString(" · ")
        val search = found.find(_.titleKey == IdentityMeasures.key(l.title)).fold("no search")(s => s"rank ${s.rank.getOrElse("-")} rivals ${s.rivals}")
        examples += f"- ${l.title} [$venue${if (venues.size > 1) s" +${venues.size - 1}" else ""}] · $facts · $tmdbId ${t.title.getOrElse("")} (${t.originalTitle.getOrElse("")}) ${t.releaseYear.getOrElse("")} ${t.runtimeMinutes.getOrElse("")}m ${t.director.take(1).mkString} · $search · $p%.2f · $verdict"
      }
    }
    examples.result().foreach(println)
  }
}
