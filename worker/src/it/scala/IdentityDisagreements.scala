package integration

import play.api.libs.json.{JsNull, JsObject, JsValue, Json}
import services.identity.{Evidence, Listing, ResolverDecision, Resolution}
import services.movies.ListingKey

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

/**
 * Every DISAGREEMENT between the resolver and today's pipeline on one corpus, cell by cell — a
 * cell is the listings one pipeline film and one resolver cluster share — with every per-listing
 * fact a case-by-case analysis needs; and the STRICT per-listing comparison on the held-out labels
 * (`IdentityShadowIntegrationSpec`). Test code.
 */
object IdentityDisagreements {

  import IdentityShadow.{FilmAnswer, PipelineFilm}

  /** One labelled listing's outcome, old against new. */
  enum Cell {
    case BothRight, LossCoverage, LossWrong, Win, BothWrong
  }

  def cellOf(label: Int, old: Option[Int], neu: Option[Int]): Cell =
    (old.contains(label), neu.contains(label), neu.isDefined) match {
      case (true, true, _)      => Cell.BothRight
      case (true, false, false) => Cell.LossCoverage
      case (true, false, true)  => Cell.LossWrong
      case (false, true, _)     => Cell.Win
      case _                    => Cell.BothWrong
    }

  /** The kind of a disagreement cell. */
  def kindOf(pipelineTmdb: Option[Int], resolverTmdb: Option[Int], pipelineSplit: Boolean, resolverMerges: Boolean): Option[String] =
    (pipelineTmdb, resolverTmdb) match {
      case (Some(a), Some(b)) if a != b => Some("moved")
      case (Some(_), None)              => Some("unmatched")
      case (None, Some(_))              => Some("newly-matched")
      case _ if resolverMerges          => Some("merge")
      case _ if pipelineSplit           => Some("split")
      case _                            => None
    }

  /** Evidence adjudication of one cell: which film do the listings' own facts back? */
  final case class Adjudication(verdict: String, pipelineAgree: Int, pipelineDeny: Int, resolverAgree: Int, resolverDeny: Int)

  def adjudicate(evidences: Seq[Evidence], pipeline: Option[FilmAnswer], resolver: Option[FilmAnswer]): Adjudication = {
    def tally(f: Option[FilmAnswer]): (Int, Int, Boolean) = f.fold((0, 0, false)) { a =>
      val cs = evidences.map(e => IdentityShadow.agreement(e, a.film))
      (cs.count(_._1 > 0), cs.count(_._2 >= 2), cs.nonEmpty && cs.forall(_._2 >= 2))
    }
    val (pa, pd, pAllDenied) = tally(pipeline)
    val (ra, rd, rAllDenied) = tally(resolver)
    val verdict =
      if (pipeline.map(_.tmdbId) == resolver.map(_.tmdbId)) "same-film"
      else if (pAllDenied && rAllDenied) "both-wrong"
      else if (ra - rd > pa - pd) "resolver-right"
      else if (pa - pd > ra - rd) "pipeline-right"
      else "undecidable"
    Adjudication(verdict, pa, pd, ra, rd)
  }

  private def film(f: Option[FilmAnswer]): JsValue =
    f.fold[JsValue](JsNull)(a => Json.obj("tmdbId" -> a.tmdbId, "title" -> a.film.title, "year" -> a.film.year))

  /** One cell as the JSONL line the analysis reads. */
  def cellJson(country: String, corpus: String, pipelineIndex: Option[Int], films: Seq[PipelineFilm], clusterIndex: Int,
               decision: ResolverDecision, resolution: Resolution, listings: Seq[Listing], evidenceOf: ListingKey => Evidence,
               showtimes: ListingKey => Int, labels: Map[String, IdentityShadow.Label], kind: String,
               pipeline: Option[FilmAnswer], resolver: Option[FilmAnswer], adjudication: Adjudication): JsObject = {
    val statuses = listings.map(l => labels.get(l.key.toString))
    val label = statuses.flatten.headOption
    Json.obj(
      "country"      -> country,
      "corpus"       -> corpus,
      "kind"         -> kind,
      "pipelineFilm" -> pipelineIndex.fold[JsValue](JsNull)(i => Json.obj("id" -> films(i).key, "tmdbId" -> films(i).tmdbId,
        "title" -> films(i).film.map(_.title), "year" -> films(i).film.flatMap(_.year))),
      "resolverDecision" -> Json.obj(
        "clusterId"       -> clusterIndex,
        "tmdbId"          -> decision.film,
        "title"           -> decision.film.flatMap(resolution.films.get).map(_.title),
        "year"            -> decision.film.flatMap(resolution.films.get).flatMap(_.year),
        "confidence"      -> decision.confidence,
        "basis"           -> decision.basis.toString,
        "unmatchedReason" -> Option.when(!decision.basis.matched)(decision.basis.toString),
        "explanation"     -> decision.explanation,
        "contradictions"  -> decision.contradictions),
      "listings" -> listings.map { l =>
        val e = evidenceOf(l.key)
        Json.obj("venue" -> l.venue, "title" -> l.title, "rawTitle" -> l.rawTitle, "originalTitle" -> e.originalTitle,
          "year" -> e.year, "statedYear" -> e.statedYear, "directors" -> e.directors,
          "runtime" -> e.runtime, "countries" -> e.countries, "filmUrl" -> l.page, "listingKey" -> l.key.toString,
          "showtimeCount" -> showtimes(l.key),
          "label" -> labels.get(l.key.toString).map(x => Json.obj("tmdbId" -> x.tmdbId,
            "status" -> (if (x.corroborated) "corroborated" else "contradicted"))))
      },
      "labelStatus" -> Json.obj(
        "status"   -> label.fold("unlabelled")(x => if (x.corroborated) "corroborated" else "contradicted"),
        "tmdbId"   -> label.map(_.tmdbId),
        "labelled" -> statuses.count(_.isDefined)),
      "adjudication" -> Json.obj(
        "verdict"  -> adjudication.verdict,
        "evidence" -> Json.obj(
          "pipeline" -> Json.obj("film" -> film(pipeline), "listingsAgreeing" -> adjudication.pipelineAgree, "listingsContradicted" -> adjudication.pipelineDeny),
          "resolver" -> Json.obj("film" -> film(resolver), "listingsAgreeing" -> adjudication.resolverAgree, "listingsContradicted" -> adjudication.resolverDeny),
          "rule" -> ("per listing: agreeing = one or more of year within 1, same director, original title matching; " +
            "contradicted = two or more denials; the side with more agreeing minus contradicted listings wins"))))
  }

  private def filmFacts(id: Int, f: services.identity.IdentityMeasures.Film): JsObject =
    Json.obj("tmdbId" -> id, "title" -> f.title, "originalTitle" -> f.originalTitle, "year" -> f.year, "runtime" -> f.runtime,
      "directors" -> f.directors)

  /** EVERY listing's outcome on both sides, one line each, so two runs (a baseline and a candidate
   *  resolver) can be diffed listing by listing. */
  def listingJson(country: String, corpus: String, l: Listing, e: Evidence, pipeline: Option[FilmAnswer], cluster: Int,
                  decision: ResolverDecision, resolution: Resolution, label: Option[IdentityShadow.Label],
                  pipelineBasis: Option[String] = None, titleRules: Seq[String] = Nil): JsObject =
    Json.obj("country" -> country, "corpus" -> corpus, "key" -> l.key.toString, "venue" -> l.venue, "rawTitle" -> l.rawTitle,
      "originalTitle" -> e.originalTitle, "year" -> e.year, "statedYear" -> e.statedYear, "directors" -> e.directors,
      "runtime" -> e.runtime,
      "pipeline" -> pipeline.fold[JsValue](JsNull)(a => filmFacts(a.tmdbId, a.film) + ("basis" -> Json.toJson(pipelineBasis))),
      "resolver" -> decision.film.flatMap(id => resolution.films.get(id).map(filmFacts(id, _))).getOrElse[JsValue](JsNull),
      "cluster" -> cluster, "basis" -> decision.basis.toString, "confidence" -> decision.confidence,
      "explanation" -> decision.explanation.take(4),
      // The rules that decided it, as rule ids (`DecisionTrace`, the title rules its title took): what `rules.py` reads.
      "rules" -> (decision.trace.rulesOf(l.key) ++ titleRules), "vetoedBy" -> decision.trace.vetoed.flatMap(_.by),
      "label" -> label.fold[JsValue](JsNull)(x => Json.obj("tmdbId" -> x.tmdbId, "corroborated" -> x.corroborated)),
      // The absolute referee, each side alone (`IdentityReferee`).
      "pipelineVerdict" -> pipeline.fold[JsValue](JsNull)(a => verdict(IdentityReferee.judge(e, a.film))),
      "resolverVerdict" -> decision.film.flatMap(resolution.films.get).fold[JsValue](JsNull)(f => verdict(IdentityReferee.judge(e, f))))

  private def verdict(v: (IdentityReferee.Verdict, Seq[String])): JsValue =
    Json.obj("verdict" -> v._1.toString.toLowerCase, "denials" -> v._2)

  def write(path: Path, lines: Seq[JsObject]): Unit = {
    Files.createDirectories(path.getParent)
    Files.writeString(path, lines.map(Json.stringify).mkString("", "\n", if (lines.isEmpty) "" else "\n"), StandardCharsets.UTF_8,
      StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    ()
  }
}
