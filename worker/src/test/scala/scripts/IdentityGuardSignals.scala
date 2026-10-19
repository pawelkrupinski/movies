package scripts

import play.api.libs.json.Json
import services.identity._
import services.identity.IdentityCalibration.SignalWeights
import services.identity.IdentityMeasures.{Category, Measure, Missing}
import services.identity.adapters.{FamilyAnswerStore, InMemoryTmdbDocuments, PosterAnswerStore, TmdbKind}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * Experiment `weighted-guards` (docs/identity-experiments.md): the signals that today only veto or pin a take — the release
 * countries ([[ReleaseVeto]]), the certificate ([[Certificates.below]]), the venue poster's pHash ([[PosterEvidence]]) and
 * the synopsis' proper names ([[agreement.SynopsisEvidence]]) — read as weighted LEVELS on every labelled unit of the
 * checked-in captures ([[IdentityRefit.labelledUnits]]).
 *
 * Writes `<out>/units.tsv` (one row per labelled listing-film unit: the shipped model's log-odds without the new signals,
 * each signal's level) for the confidence study (`scripts/ml-identity/weighted_guards.py`), and, for each of the two
 * signals the per-candidate model measures (`release.country`, `certificate`), the shipped artefact with that signal's table
 * fitted by the calibration's own fit ([[IdentityRefit.fit]]) from the units' counts: `<out>/weights-<variant>.json`.
 * The poster and synopsis levels are read from the capture's filed answers only: the model never holds them.
 *
 *   sbt "worker/Test/runMain scripts.IdentityGuardSignals [<out dir>]"
 */
object IdentityGuardSignals {

  /** Bits within which a venue poster is the film's ([[PosterEvidence.VetoMatchBits]]) and beyond which it is another's. */
  val PosterMatch = PosterEvidence.VetoMatchBits
  val PosterFar   = 20

  def posterLevel(distance: Option[Int], anotherMatches: Boolean): String = distance match {
    case None                     => "missing"
    case Some(d) if d <= PosterMatch => "match"
    case Some(d) if d < PosterFar => if (anotherMatches) "contradicts" else "near"
    case Some(_)                  => if (anotherMatches) "contradicts" else "far"
  }

  def synopsisLevel(shared: Option[Int], anotherNames: Boolean): String = shared match {
    case None             => "missing"
    case Some(n) if n >= SynopsisWords => "match"
    case Some(n) if n >= 1 => "near"
    case Some(_)          => if (anotherNames) "contradicts" else "none"
  }
  val SynopsisWords = agreement.SynopsisEvidence.Words

  /** How many of the words the node's synopses capitalise each candidate's TMDB overview holds — every title's words left
   *  out, as [[agreement.SynopsisEvidence]] reads them; `None` where the listings state no synopsis or the film no overview. */
  def synopsisShares(listings: Seq[Listing], films: Seq[(Int, IdentityMeasures.Film)], overview: Int => Option[ProseWords]): Map[Int, Option[Int]] = {
    val words  = listings.foldLeft(ProseWords.None)(_ ++ _.words)
    val titles = ProseWords.ofWords(listings.flatMap(l => Seq(l.rawTitle, l.title, l.cleanTitle)) ++ films.flatMap((_, f) => f.ownTitles ++ f.alternativeTitles))
    films.map { (id, _) => id -> (if (words.isEmpty) None else overview(id).filterNot(_.isEmpty).map(o => ((words -- titles) & (o -- titles)).size)) }.toMap
  }

  /** Each candidate's synopsis level ([[synopsisLevel]]) from its [[synopsisShares]]. */
  def synopsisLevels(shares: Map[Int, Option[Int]]): Map[Int, String] = shares.map { (id, n) =>
    id -> synopsisLevel(n, shares.exists((other, m) => other != id && m.exists(_ >= SynopsisWords))) }

  private def level(m: Option[Measure]): String = m match {
    case Some(Category(v)) => v
    case Some(Missing(s))  => s"missing:$s"
    case _                 => "-"
  }

  def main(args: Array[String]): Unit = {
    val out      = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("target/weighted-guards"))
    Files.createDirectories(out)
    val labels   = tools.IdentityJudgements.labels()
    val artefact = IdentityRefit.Artefact
    val start    = Json.parse(Files.readString(artefact)).as[IdentityCalibration]
    val rows = Vector.newBuilder[String]
    rows += Seq("country", "node", "group", "tmdbId", "same", "heldOut", "logit", "probability", "release", "certificate",
      "poster", "posterDistance", "synopsis", "synopsisShared").mkString("\t")
    var units = Vector.empty[IdentityRefit.LabelledUnit]
    models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read).foreach { capture =>
      val found = IdentityRefit.labelledUnits(capture, labels, start)
      units ++= found
      val docs = new InMemoryTmdbDocuments
      docs.put(TmdbKind.Family, capture.families.toSeq)
      val posters  = new PosterAnswerStore(new FamilyAnswerStore(docs, tools.SpecClock.Pinned), tools.SpecClock.Pinned)
      val byKey    = capture.listings.map(l => l.key -> l).toMap
      found.groupBy(_.node).foreach { case (_, ofNode) =>
        val listings = ofNode.head.keys.flatMap(byKey.get)
        val venueHashes = PosterEvidence.urls(listings).flatMap(url => posters.venue(url).toOption.flatten)
        def distance(u: IdentityRefit.LabelledUnit) =
          if (venueHashes.isEmpty) None else posters.film(u.tmdbId).toOption.filter(_.nonEmpty).flatMap(PosterEvidence.nearest(venueHashes, _))
        val distances = ofNode.map(u => u.tmdbId -> distance(u)).toMap
        val shares    = synopsisShares(listings, ofNode.flatMap(u => u.film.map(u.tmdbId -> _)), id => capture.overviews.get(id).flatten)
        ofNode.foreach { u =>
          val logit = start.logOdds(IdentityMeasures.ListingFilm, u.measures)
          val posterOther   = distances.exists { case (id, d) => id != u.tmdbId && d.exists(_ <= PosterMatch) }
          rows += Seq(u.country, u.node.replace('\t', ' ').replace('\u0000', '|'), u.group, u.tmdbId.toString, u.same.toString,
            IdentityRefit.heldOut(u.group).toString, f"$logit%.5f", f"${start.probability(IdentityMeasures.ListingFilm, u.measures)}%.6f",
            level(u.measures.get("release.country")), level(u.measures.get("certificate")),
            posterLevel(distances(u.tmdbId), posterOther), distances(u.tmdbId).fold("")(_.toString),
            synopsisLevels(shares)(u.tmdbId), shares(u.tmdbId).fold("")(_.toString)).mkString("\t")
        }
      }
      println(s"${capture.country.code}: ${found.size} labelled units")
    }
    Files.write(out.resolve("units.tsv"), rows.result().mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    val scope = start.scopes(IdentityMeasures.ListingFilm)
    def table(signal: String): SignalWeights = {
      val neutral = Map("unrated" -> "the venue's country rates no film (Poland): no scale to read a certificate on",
        "listing" -> "the venue's country is unknown")
      val empty = SignalWeights("categorical", neutral = if (signal == "certificate") neutral else Map("listing" -> neutral("listing")))
      IdentityRefit.fit(signal, IdentityRefit.added(empty, units.map(u => u.measures.get(signal) -> u.same)))
    }
    val variants = Seq("release" -> Seq("release.country"), "certificate" -> Seq("certificate"), "both" -> Seq("release.country", "certificate"))
    variants.foreach { case (name, signals) =>
      val tables = signals.map(s => s -> table(s)).toMap
      tables.foreach { case (s, w) => println(s"$s: ${w.categories.toSeq.sortBy(_._1).map((k, v) => f"$k=$v%.2f").mkString(" ")} " +
        s"missing ${w.missing.toSeq.sortBy(_._1).map((k, v) => f"$k=$v%.2f").mkString(" ")} counts ${w.counts.toSeq.sortBy(_._1).map((k, c) => s"$k=${c.mkString("/")}").mkString(" ")}") }
      val now = start.copy(version = s"${start.version}+weighted-guards-$name",
        scopes = start.scopes.updated(IdentityMeasures.ListingFilm, scope.copy(signals = scope.signals ++ tables)))
      val file: Path = out.resolve(s"weights-$name.json")
      Files.writeString(file, Json.prettyPrint(Json.toJson(now)) + "\n", StandardCharsets.UTF_8)
      println(s"wrote $file")
    }
    println(s"units: ${units.size} (${units.count(_.same)} same film)")
  }
}
