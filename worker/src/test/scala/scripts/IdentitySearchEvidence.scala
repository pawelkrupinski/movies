package scripts

import models.Country
import play.api.libs.json.{JsValue, Json}
import scripts.IdentityCalibrationData.TmdbAnswers
import services.identity.IdentityMeasures.{Film, Listing, ListingFilm}
import services.identity.{IdentityCalibration, IdentityMeasures}

import java.nio.file.{Files, Paths}
import java.util.zip.GZIPInputStream

/**
 * Does a listing's own TMDB title search tell a right bare-title match from a wrong one? Measured
 * on the calibration's labelled listings (never the pseudo-venues), each replayed through the
 * recorded title searches of its night (`enrichment-<cc>/`): where the labelled film ranked, how
 * many other films the listing's title names as closely (`rivals`), and what the calibrated score
 * says with the listing STRIPPED to its title — with and without those search measures.
 * Counted per unit (country, title key, film), as the calibration counts.
 *
 *   sbt "worker/Test/runMain scripts.IdentitySearchEvidence <fixtures dir holding enrichment-<cc>/>"
 */
object IdentitySearchEvidence {

  private val PseudoVenues = Set("TMDB", "IMDB", "EM", "IL KINO")

  final case class Measured(country: String, status: String, k3: Boolean, usesVenues: Boolean, test: Boolean, rank: Option[Int],
                        rivals: Int, relation: String, pBare: Double, pBarePriors: Double, pBareNoPopularity: Double,
                        pFull: Double, pFullSearch: Double,
                        bareTopIsFilm: Boolean, bareTopShown: Boolean, title: String, film: Int)

  def main(args: Array[String]): Unit = {
    val fixtures    = Paths.get(args(0))
    val calibration = IdentityCalibration.default
    val in = new GZIPInputStream(Files.newInputStream(Paths.get("test/resources/fixtures/identity/identity-labels.json.gz")))
    val labels = try Json.parse(in) finally in.close()
    val listings = (labels \ "listings").as[Seq[JsValue]].filter { l =>
      !PseudoVenues((l \ "venue").as[String]) && (l \ "tmdbId").asOpt[Int].isDefined &&
        Set("corroborated", "contradicted")((l \ "status").asOpt[String].getOrElse(""))
    }
    val units = listings.groupBy(l => ((l \ "country").as[String], (l \ "tmdbId").as[Int])).toSeq.flatMap { case ((cc, _), ls) =>
      ls.groupBy(l => IdentityMeasures.key((l \ "title").as[String])).values.map(_.head).map(cc -> _)
    }.groupMap(_._1)(_._2)

    val all = units.toSeq.sortBy(_._1).flatMap { case (cc, ls) =>
      val country = Country.byCode(cc).get
      val answers = new TmdbAnswers(Seq(fixtures.resolve(s"enrichment-$cc")).filter(Files.isDirectory(_)), Map.empty,
        IdentityCalibrationData.languageOf(country))
      ls.flatMap { l =>
        val film = (l \ "tmdbId").as[Int]
        val full = Listing((l \ "title").as[String], (l \ "rawTitle").asOpt[String], (l \ "originalTitle").asOpt[String],
          (l \ "year").asOpt[Int], (l \ "runtime").asOpt[Int], (l \ "directors").asOpt[Seq[String]].getOrElse(Nil))
        val bare = Listing(full.title, full.rawTitle)
        val hits = IdentityMeasures.searchQueries(full).flatMap(q => answers.search(q).toSeq.flatMap(_.zipWithIndex))
        if (hits.isEmpty) None
        else {
          val ranked = hits.groupMapReduce(_._1.id)(h => h)((a, b) => if (a._2 <= b._2) a else b)
          def filmOf(id: Int): Option[Film] = answers.details(id).map(_.film).orElse(ranked.get(id).map { case (h, _) =>
            Film(h.title, h.originalTitle, Nil, h.year, None, None, None, Some(h.popularity)) })
          val pool = (ranked.keySet + film).toSeq.flatMap(id => filmOf(id).map(id -> _)).toMap
          if (!pool.contains(film)) None
          else {
            val close = pool.map { case (id, f) => id -> IdentityMeasures.titleRelation(bare, f).value }
            val Rivalling = Set("exact", "original", "alternative")
            val rivalsOf = (id: Int) => close.count { case (o, r) => o != id && Rivalling(r) }
            def p(listing: Listing, id: Int, priors: Boolean, popularity: Boolean = true): Double = {
              val m = IdentityMeasures.listingFilm(listing, pool(id), ranked.get(id).map(_._2 + 1), rivalsOf(id), 0)
              calibration.probability(ListingFilm,
                if (!priors) m -- IdentityMeasures.RankingPriors else if (!popularity) m - "popularity.log2" else m)
            }
            val bareScores = pool.keys.toSeq.map(id => id -> p(bare, id, priors = true)).sortBy(s => (-s._2, s._1))
            val agree = (l \ "agree").asOpt[Seq[String]].getOrElse(Nil)
            Some(Measured(cc, (l \ "status").as[String], (l \ "corroborated3").asOpt[Boolean].getOrElse(false),
              agree.contains("venues"), (l \ "split").asOpt[String].contains("test"), ranked.get(film).map(_._2 + 1), rivalsOf(film),
              close(film), p(bare, film, priors = false), p(bare, film, priors = true), p(bare, film, priors = true, popularity = false),
              p(full, film, priors = false), p(full, film, priors = true, popularity = false),
              bareScores.head._1 == film, calibration.showsRatings(bareScores.head._2), full.title, film))
          }
        }
      }
    }

    def pct(n: Int, d: Int) = if (d == 0) "–" else f"${100.0 * n / d}%.1f%%"
    def shown(us: Seq[Measured], f: Measured => Double) = us.count(u => calibration.showsRatings(f(u)))
    println("\n## Bare title (year/director/runtime/original title stripped): share of units whose labelled film would be SHOWN")
    println("| country | status | units | no search measures | rank+rivals | rank+rivals+popularity | unique hit (rank 1, 0 rivals) |")
    for (cc <- all.map(_.country).distinct.sorted :+ "all"; st <- Seq("corroborated", "contradicted")) {
      val us = all.filter(u => (cc == "all" || u.country == cc) && u.status == st)
      println(s"| $cc | $st | ${us.size} | ${pct(shown(us, _.pBare), us.size)} | ${pct(shown(us, _.pBareNoPopularity), us.size)} | ${pct(shown(us, _.pBarePriors), us.size)} | " +
        s"${pct(us.count(u => u.rank.contains(1) && u.rivals == 0), us.size)} |")
    }
    println("\n## Full listing evidence (as published): share SHOWN — own facts only, with rank+rivals, and search lending but never withdrawing")
    println("| status | split | units | own facts | + rank+rivals | max of the two |")
    for (st <- Seq("corroborated", "contradicted"); held <- Seq(false, true)) {
      val us = all.filter(u => u.status == st && (!held || u.test))
      println(s"| $st | ${if (held) "held-out" else "all"} | ${us.size} | ${pct(shown(us, _.pFull), us.size)} | " +
        s"${pct(shown(us, _.pFullSearch), us.size)} | ${pct(shown(us, u => u.pFull max u.pFullSearch), us.size)} |")
    }
    println("\n## Label-selection sensitivity (corroborated units, bare title, with search measures)")
    println("| subset | corroborated units | shown (rank+rivals) | shown (+popularity) | unique hit | contradicted units | contradicted shown (rank+rivals) |")
    Seq[(String, Measured => Boolean)]("k = 2 (all)" -> (_ => true), "k = 3" -> (_.k3),
        "labels not using other venues" -> (u => !u.usesVenues), "held-out split only" -> (_.test), "held-out, k = 3" -> (u => u.test && u.k3))
      .foreach { case (name, keep) =>
        val us = all.filter(u => u.status == "corroborated" && keep(u))
        val cs = all.filter(u => u.status == "contradicted" && (name.startsWith("k =") || name.startsWith("labels") || u.test))
        println(s"| $name | ${us.size} | ${pct(shown(us, _.pBareNoPopularity), us.size)} | ${pct(shown(us, _.pBarePriors), us.size)} | " +
          s"${pct(us.count(u => u.rank.contains(1) && u.rivals == 0), us.size)} | ${cs.size} | ${pct(shown(cs, _.pBareNoPopularity), cs.size)} |")
      }
    println("\n## Uniqueness cells: corroborated vs contradicted units")
    println("| rank | rivals | title | corroborated | contradicted |")
    all.groupBy(u => (u.rank.fold("absent")(r => if (r == 1) "1" else if (r <= 3) "2-3" else "4+"),
        if (u.rivals == 0) "0" else if (u.rivals <= 2) "1-2" else "3+", u.relation)).toSeq.sortBy(-_._2.size).take(25)
      .foreach { case ((r, v, t), us) => println(s"| $r | $v | $t | ${us.count(_.status == "corroborated")} | ${us.count(_.status == "contradicted")} |") }
    println("\n## Resolution on a bare title (with search measures): top candidate, and whether it is shown")
    println("| status | units | top is the labelled film | top shown and is the film | top shown and is ANOTHER film |")
    Seq("corroborated", "contradicted").foreach { st =>
      val us = all.filter(_.status == st)
      println(s"| $st | ${us.size} | ${us.count(_.bareTopIsFilm)} | ${us.count(u => u.bareTopIsFilm && u.bareTopShown)} | ${us.count(u => !u.bareTopIsFilm && u.bareTopShown)} |")
    }
    println("\n## Contradicted units shown on a bare title with search measures")
    all.filter(u => u.status == "contradicted" && calibration.showsRatings(u.pBarePriors))
      .foreach(u => println(f"- ${u.country} ${u.title} → ${u.film} rank=${u.rank} rivals=${u.rivals} ${u.relation} p=${u.pBarePriors}%.2f"))
  }
}
