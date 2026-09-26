package scripts

import models.Country
import play.api.libs.json.{JsValue, Json}
import scripts.IdentityCalibrationData.TmdbAnswers
import services.identity.IdentityCalibration.{Condition, EvidenceClass}
import services.identity.IdentityMeasures.{Film, Listing, ListingFilm}
import services.identity.{IdentityCalibration, IdentityMeasures}

import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

/**
 * The calibration's EVIDENCE CLASSES (docs/design/identity-resolver.md §14, `evidenceClasses` in
 * `identity-weights.json`): evidence the naive-Bayes sum undersells, measured AS A CLASS on the
 * labels instead of signal by signal, and credited with what it measured.
 *
 * One class shape is measured: a listing's EXACT TOP HIT ([[IdentityMeasures.exactTopHits]]) — the
 * one film its whole title names exactly that its own title search returned first, in TMDB's order
 * — split by its same-titled `rivals` along the artefact's own `rivals` bins. Each labelled unit
 * (country, title key, film; pseudo-venues excluded) is replayed STRIPPED to its title through the
 * recorded searches of its night: a unit whose title has one exact top hit counts as right when
 * that film is its corroborated film, and as wrong when it is another film (also when the labelled
 * film was not in the search at all — the "Samson" shape) or production's CONTRADICTED filing.
 *
 * A class ships only when its wrong share on the fitting splits (train + calibration) has a
 * Bonferroni-corrected (over the bins tried) one-sided 95% upper bound within the wrong rate the
 * ratings are shown at (`showRatings.measured.targetWrongRate`); its probability is one minus that
 * bound. Nothing is chosen by hand: the bins, the target and the bound are the calibration's.
 *
 *   sbt "worker/Test/runMain scripts.IdentityEvidenceClasses --fixtures <dir of enrichment-<cc>/> \
 *        [--weights common/src/main/resources/identity-weights.json] [--labels test/resources/fixtures/identity/identity-labels.json.gz] \
 *        [--source '<what the trees are, for the provenance>']"
 *
 * rewrites only the artefact's `evidenceClasses`. `IdentityCalibrate` calls [[derive]] after it
 * writes the artefact, so a regeneration keeps them.
 */
object IdentityEvidenceClasses {

  private val PseudoVenues = Set("TMDB", "IMDB", "EM", "IL KINO")

  /** One labelled unit whose bare title has an exact top hit. */
  final case class Outcome(country: String, split: String, title: String, film: Int, top: Int, rivals: Int, right: Boolean)

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    derive(
      fixtures = Paths.get(opts.getOrElse("fixtures", sys.error("--fixtures <dir of enrichment-<cc>/ trees>"))),
      labels   = Paths.get(opts.getOrElse("labels", "test/resources/fixtures/identity/identity-labels.json.gz")),
      weights  = opts.get("weights").map(Paths.get(_)).getOrElse(IdentityCalibrate.ResolverArtefact),
      source   = opts.getOrElse("source", "the trees under --fixtures"))
  }

  /** Measures the classes from `labels` and the recorded searches under `fixtures`, and writes them
   *  into the artefact at `weights`. Prints what it measured. */
  def derive(fixtures: Path, labels: Path, weights: Path, source: String): Unit = {
    val calibration = Json.parse(Files.readAllBytes(weights)).as[IdentityCalibration]
    val outcomes = measure(fixtures, labels)
    val derived = classes(outcomes, calibration)
    val provenance = calibration.provenance + ("evidenceClasses" ->
      s"scripts.IdentityEvidenceClasses: the labels above, replayed stripped to their titles through the recorded title searches of $source")
    Files.writeString(weights, Json.prettyPrint(Json.toJson(calibration.copy(evidenceClasses = derived, provenance = provenance))) + "\n")
    println(report(outcomes, derived, calibration))
  }

  /** Every labelled unit's bare title replayed through its recorded searches. */
  def measure(fixtures: Path, labelsPath: Path): Seq[Outcome] = {
    val in = new GZIPInputStream(Files.newInputStream(labelsPath))
    val labels = try Json.parse(in) finally in.close()
    val listings = (labels \ "listings").as[Seq[JsValue]].filter { l =>
      !PseudoVenues((l \ "venue").as[String]) && (l \ "tmdbId").asOpt[Int].isDefined &&
        Set("corroborated", "contradicted")((l \ "status").asOpt[String].getOrElse(""))
    }
    // A unit: one country, one title key, one film — its first listing in the file's order.
    val units = listings.groupBy(l => ((l \ "country").as[String], IdentityMeasures.key((l \ "title").as[String]), (l \ "tmdbId").as[Int]))
      .toSeq.sortBy(_._1).map(_._2.head)
    units.groupBy(l => (l \ "country").as[String]).toSeq.sortBy(_._1).flatMap { case (cc, ls) =>
      val answers = new TmdbAnswers(Seq(fixtures.resolve(s"enrichment-$cc")).filter(Files.isDirectory(_)), Map.empty,
        IdentityCalibrationData.languageOf(Country.byCode(cc).get))
      ls.flatMap { l =>
        val film = (l \ "tmdbId").as[Int]
        val corroborated = (l \ "status").as[String] == "corroborated"
        val bare = Listing((l \ "title").as[String], (l \ "rawTitle").asOpt[String])
        val hits = IdentityMeasures.searchQueries(bare).flatMap(q => answers.search(q).toSeq.flatMap(_.zipWithIndex))
        val ranked = hits.groupMapReduce(_._1.id)(h => h)((a, b) => if (a._2 <= b._2) a else b)
        def filmOf(id: Int): Film = answers.details(id).map(_.film).getOrElse {
          val (h, _) = ranked(id); Film(h.title, h.originalTitle, Nil, h.year, popularity = Some(h.popularity))
        }
        val pool = ranked.keys.toSeq.sorted.map(id => (id, filmOf(id), Some(ranked(id)._2 + 1)))
        IdentityMeasures.exactTopHits(bare, pool) match {
          case Seq(top) if corroborated || top == film =>
            val rivals = IdentityMeasures.rivals(bare, pool.map(p => p._1 -> p._2).toMap, top)
            Some(Outcome(cc, (l \ "split").as[String], bare.title, film, top, rivals, right = corroborated && top == film))
          case _ => None // no single exact top hit, or a contradicted filing the top hit is not: nothing known
        }
      }
    }
  }

  /** The classes that qualify: one per `rivals` bin of the artefact. */
  def classes(outcomes: Seq[Outcome], calibration: IdentityCalibration): Seq[EvidenceClass] = {
    val scope  = calibration.scopes(ListingFilm)
    val target = scope.thresholds("showRatings").measured("targetWrongRate")
    val bins   = scope.signals("rivals").bins
    val z      = IdentityCalibrate.normalQuantile(1 - 0.05 / bins.size)
    bins.flatMap { bin =>
      val in      = outcomes.filter(o => bin.contains(o.rivals.toDouble))
      val fitting = in.filter(_.split != "test")
      val heldOut = in.filter(_.split == "test")
      val (w, n)  = (fitting.count(!_.right), fitting.size)
      val upper   = IdentityCalibrate.upperBound(w, n, z)
      Option.when(n > 0 && upper <= target)(EvidenceClass(
        name = s"title=exact AND search.rank<=1 AND rivals in [${bin.atLeast.fold("-inf")(_.toString)}, ${bin.atMost.fold("inf")(_.toString)}]",
        scope = ListingFilm,
        all = Seq(Condition("title", in = Seq("exact")), Condition("search.rank", atMost = Some(1)),
          Condition("rivals", atLeast = bin.atLeast, atMost = bin.atMost)),
        probability = 1 - upper,
        measured = Map("fittingUnits" -> n.toDouble, "fittingWrong" -> w.toDouble, "fittingWrongUpper" -> upper,
          "heldOutUnits" -> heldOut.size.toDouble, "heldOutWrong" -> heldOut.count(!_.right).toDouble, "targetWrongRate" -> target),
        basis = "a listing's exact top hit (its whole title names the film exactly and a title search returned it first) as a class, " +
          "measured on bare titles replayed through the recorded searches: its wrong share on the fitting splits has a " +
          "Bonferroni-corrected one-sided 95% upper bound within the wrong rate ratings are shown at; the probability is one minus it"))
    }
  }

  def report(outcomes: Seq[Outcome], derived: Seq[EvidenceClass], calibration: IdentityCalibration): String = {
    val bins = calibration.scopes(ListingFilm).signals("rivals").bins
    val sb = new StringBuilder("\n## Exact top hit on a bare title, by rivals bin (units)\n")
    sb ++= "| rivals | fitting right | fitting wrong | held-out right | held-out wrong | ships |\n"
    bins.foreach { bin =>
      val in = outcomes.filter(o => bin.contains(o.rivals.toDouble))
      val (f, t) = in.partition(_.split != "test")
      val name = s"[${bin.atLeast.fold("-inf")(_.toString)}, ${bin.atMost.fold("inf")(_.toString)}]"
      val ships = derived.find(_.all.exists(c => c.signal == "rivals" && c.atLeast == bin.atLeast && c.atMost == bin.atMost))
      sb ++= s"| $name | ${f.count(_.right)} | ${f.count(!_.right)} | ${t.count(_.right)} | ${t.count(!_.right)} | " +
        s"${ships.fold("no")(c => f"p = ${c.probability}%.4f")} |\n"
    }
    sb ++= "\n## Wrong exact top hits (every one)\n"
    outcomes.filterNot(_.right).sortBy(o => (o.rivals, o.country, o.title)).foreach(o =>
      sb ++= s"- ${o.country} [${o.split}] '${o.title}' rivals=${o.rivals}: top hit ${o.top}, labelled ${o.film}\n")
    sb.toString
  }
}
