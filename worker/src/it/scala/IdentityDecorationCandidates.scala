package integration

import play.api.libs.json.Json
import scripts.IdentityCalibrationData.{TmdbAnswers, languageOf}
import services.identity._
import services.movies.{ListingKey, TitleContainment}
import tools.{ConvergenceStorage, IntegrationMongoTarget, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Try

/**
 * The PROGRAMME- AND FORMAT-DECORATION DETECTOR: proposes the decorations [[TitleDecorations.learn]] cannot see yet
 * ([[TitleDecorations.candidates]] — "Edukacja Młode Horyzonty", "2D PL LOLO", "Weekend Seniora z Kulturą" around films
 * no other listing bills), MEASURES them on the recorded full corpora, and keeps only those that take more listings
 * right and none wrong. Re-runnable as venues add programmes:
 *
 *   KINOWO_IDENTITY_FULL=pl,uk,de,es,us KINOWO_IDENTITY_CORPUS_DIR=<dir> KINOWO_FIXTURE_ROOT=<dir>
 *   [KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=<key>] MONGODB_URI=<throwaway> MONGODB_DB=<unique>
 *   sbt "worker/IntegrationTest/runMain integration.IdentityDecorationCandidates --out <dir> [--write <artefact.json>]"
 *
 * The measure: every country's corpus resolved with the learned decorations (the baseline), then with every surviving
 * candidate beside them, in ROUNDS. Each listing whose film moved is put down to the candidates its own title (else a
 * cluster-mate's) carries, and judged: a listing the baseline left unmatched now taking a film is RIGHT or WRONG by
 * `labels.tsv` (the hand labels of the unmatched-cluster ratchet) or UNJUDGED; a listing the baseline matched that
 * moves at all is a MOVE — never accepted on this evidence. A candidate with a wrong take or a move is dropped and the
 * rest resolved again, until a round drops nothing (at most `--rounds`). What survives with a right take is KEPT: written
 * to `<out>/kept.json` as [[TitleDecorations.Learned]] entries and, with `--write`, merged into the resolver's artefact
 * (`TitleDecorations.accumulate` keeps them across a relearn). Unjudged takes are listed for a hand label, never counted.
 *
 * Only the model is measured — TMDB's searches (the recording, its gaps asked live with the live-gaps key) — not the
 * agreement stage's families: `UnmatchedClustersRatchetSpec`, re-captured, is the check that a kept decoration moves no
 * family's take wrong.
 */
object IdentityDecorationCandidates {

  /** One listing's film under a decoration set: `tmdb:<id>`, `imdb:<tt>` for a fallback, `""` for none. */
  private final case class Taken(film: String, cluster: Int, title: String)

  final case class Judged(country: String, venue: String, rawTitle: String, before: String, after: String, verdict: String, by: Seq[String])

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val out  = Paths.get(opts.getOrElse("out", sys.error("--out <dir>")))
    val rounds = opts.get("rounds").map(_.toInt).getOrElse(4)
    val minRemainders = opts.get("min-remainders").map(_.toInt).getOrElse(TitleDecorations.MinCandidateRemainders)
    val configuration = settings.ProcessConfiguration.resolve()
    val target = IntegrationMongoTarget.from(configuration).getOrElse(sys.error("MONGODB_URI names no throwaway Mongo"))
    target.requireThrowaway()
    val corpusDir = configuration.identityCorpusDirectory.getOrElse(sys.error("KINOWO_IDENTITY_CORPUS_DIR")).value
    val corpora   = IdentityShadow.full(configuration.identityFullCorpora.value, corpusDir, configuration.fixtureRoot, configuration.identityLiveGaps)
    val labels    = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    Files.createDirectories(out)
    try {
      val base = TitleDecorations.resolver
      final case class Loaded(c: IdentityShadow.Corpus, listings: Seq[Listing], lookups: IdentityShadow.Memo)
      val loaded = corpora.map { c =>
        val w = IdentityShadow.wiring(target, c, storages, configuration.fixtureRoot, configuration.env)
        val lookups = new IdentityShadow.Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(IdentityShadow.StubTmdbKey)),
          language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps))
        Loaded(c, IdentityShadow.listingsOf(w, c.normalizer), lookups)
      }
      val records = loaded.flatMap { l =>
        new TmdbAnswers(Seq(Paths.get(configuration.fixtureRoot.of(s"enrichment-${l.c.country.code}"))).filter(Files.isDirectory(_)), Map.empty,
          languageOf(l.c.country)).films.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles)
      }
      val titled = loaded.flatMap(l => l.listings.flatMap(x => Seq(x.title, x.rawTitle).distinct.map(x.venue -> _)))

      def resolveAll(decorations: TitleDecorations): Map[(String, ListingKey), Taken] = loaded.flatMap { l =>
        val started = System.nanoTime()
        val r = IdentityResolver.resolve(l.listings, l.lookups, l.c.normalizer, IdentityCalibration.resolver, decorations = decorations)
        println(f"[${l.c.label}] resolved ${l.listings.size} listings in ${(System.nanoTime() - started) / 1e9}%.1fs; unanswered ${r.unknownQueries}")
        val clusterOf = r.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
        l.listings.map { x =>
          val d = r.decisionOf(x.key)
          val film = d.film.map(id => s"tmdb:$id").orElse(d.fallback.filter(_.source == "imdb").map(f => s"imdb:${f.id}")).getOrElse("")
          (l.c.country.code, x.key) -> Taken(film, clusterOf.getOrElse(x.key, -1), d.film.flatMap(r.films.get).fold("")(f => s"${f.title} (${f.year.getOrElse("?")})"))
        }
      }.toMap

      val byKey = loaded.flatMap(l => l.listings.map(x => (l.c.country.code, x.key) -> x)).toMap
      val baseline = resolveAll(base)
      // two signals: runs recurring around many inner titles, and runs one venue adds to a film another bills plain
      val recurring = TitleDecorations.candidates(titled, records, base, minRemainders)
      val clusters  = baseline.toSeq.filter(_._2.film.nonEmpty).groupMap { case ((cc, _), t) => (cc, t.cluster) }(_._1).values
        .map(_.flatMap(byKey.get).flatMap(x => Seq(x.title, x.rawTitle).distinct.map(x.venue -> _)))
      val aligned   = TitleDecorations.aligned(clusters, base)
      val signalOf  = (recurring.map(d => (d.side, d.decoration) -> "recurring") ++ aligned.map(d => (d.side, d.decoration) -> "aligned"))
        .groupMap(_._1)(_._2).view.mapValues(_.distinct.sorted.mkString("+")).toMap
      val proposed  = (recurring ++ aligned).groupBy(d => (d.side, d.decoration)).values.map(_.maxBy(_.films))
        .map(d => d.copy(measured = signalOf.get((d.side, d.decoration)))).toSeq.sortBy(d => (-d.films, d.side, d.decoration))
      println(s"proposed ${proposed.size} candidate decoration(s) (${recurring.size} recurring, ${aligned.size} aligned) from ${titled.size} listing titles, " +
        s"${records.size} record titles")
      tsv(out.resolve("candidates.tsv"), "side\tdecoration\tsignal\tfilms\tvenues\ttitles\texamples",
        proposed.map(d => Seq(d.side, d.decoration, d.measured.getOrElse(""), d.films, d.venues, d.titles, d.examples.mkString(" | ")).mkString("\t")))
      // what the corpus says of each candidate ([[DecorationScore.Features]]), and the fitted score, if one ships
      val keyOf     = (d: TitleDecorations.Learned) => s"${d.side}:${d.decoration}"
      val wanted    = proposed.map(d => (d.side, d.decoration.split(" ").toSeq)).toSet
      val carriers  = byKey.toSeq.flatMap { case (k, x) =>
        Seq(x.title, x.rawTitle).distinct.map(TitleContainment.tokens).flatMap { ts =>
          (1 until ts.size).flatMap(n => Seq(("prefix", ts.take(n)) -> (k, ts.drop(n)), ("suffix", ts.takeRight(n)) -> (k, ts.dropRight(n)))).filter(e => wanted(e._1))
        }
      }.groupMap(_._1)(_._2)
      val matchedTitles = baseline.toSeq.filter(_._2.film.nonEmpty).flatMap { case (k, _) => Seq(byKey(k).title, byKey(k).rawTitle) }.map(TitleContainment.tokens).toSet
      val alignedFilms  = aligned.map(d => keyOf(d) -> d.films).toMap
      val recurringFilms = recurring.map(d => keyOf(d) -> d.films).toMap
      val formatWords   = services.movies.FormatTags.FormatToken.keySet
      def featuresOf(d: TitleDecorations.Learned): DecorationScore.Features = {
        val run    = d.decoration.split(" ").toSeq
        val carry  = carriers.getOrElse((d.side, run), Nil)
        val inners = carry.map(_._2).distinct
        val keys   = carry.map(_._1).distinct
        DecorationScore.Features(alignedFilms.getOrElse(keyOf(d), 0), recurringFilms.getOrElse(keyOf(d), 0), d.venues, d.side == "prefix", run.size,
          run.count(formatWords).toDouble / run.size, if (inners.isEmpty) 0.0 else inners.count(matchedTitles).toDouble / inners.size,
          if (keys.isEmpty) 0.0 else keys.count(k => baseline.get(k).exists(_.film.nonEmpty)).toDouble / keys.size)
      }
      val features = proposed.map(d => keyOf(d) -> featuresOf(d)).toMap
      val score    = DecorationScore.fromResource()
      val badEver  = mutable.Set.empty[String]
      var alive    = score.fold(proposed)(s => proposed.sortBy(d => -s.probability(features(keyOf(d)))))
      var judged   = Seq.empty[Judged]
      var round    = 0
      var settled  = false
      while (!settled && round < rounds && alive.nonEmpty) {
        round += 1
        val runs = alive.map(d => d.side -> d.decoration.split(" ").toSeq)
        val now  = resolveAll(base.copy(prefixes = base.prefixes ++ runs.collect { case ("prefix", r) => r }, suffixes = base.suffixes ++ runs.collect { case ("suffix", r) => r }))
        val membersOf = (taken: Map[(String, ListingKey), Taken]) => taken.toSeq.groupMap { case ((cc, _), t) => (cc, t.cluster) }(_._1)
        val (before, after) = (membersOf(baseline), membersOf(now))
        judged = now.toSeq.filter { case (k, t) => baseline.get(k).forall(_.film != t.film) }.map { case (k @ (cc, key), t) =>
          val was     = baseline.get(k).fold("")(_.film)
          val listing = byKey(k)
          def carried(x: Listing) = alive.filter(d => (Seq(x.title, x.rawTitle)).exists(title => TitleContainment.tokens(title).containsSlice(d.decoration.split(" ").toSeq)))
          val own = carried(listing)
          val by  = if (own.nonEmpty) own else (before.getOrElse((cc, baseline.get(k).fold(-1)(_.cluster)), Nil) ++ after.getOrElse((cc, t.cluster), Nil))
            .flatMap(byKey.get).flatMap(carried).distinct
          val verdict =
            if (was.nonEmpty) "move"
            // a double programme matches neither of its works: a stripped banner must not make it a single film's
            else if (services.identity.agreement.Agreement.billsSeveral(listing) || IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing(listing.title, Some(listing.rawTitle), decorations = base))) "wrong"
            else UnmatchedClusters.verdict(UnmatchedClusters.Take(cc, key.venue, key.rawTitle, t.film.stripPrefix("tmdb:").toIntOption.filter(_ => t.film.startsWith("tmdb:")),
              Option.when(t.film.startsWith("imdb:"))(t.film.stripPrefix("imdb:")), "", t.title), labels) match {
              case Some(true)  => "right"
              case Some(false) => "wrong"
              case None        => "unjudged"
            }
          Judged(cc, key.venue, key.rawTitle, was, s"${t.film} ${t.title}".trim, verdict, by.map(d => s"${d.side}:${d.decoration}"))
        }.sortBy(j => (j.country, j.venue, j.rawTitle))
        tsv(out.resolve(s"round-$round.tsv"), "country\tvenue\trawTitle\tbefore\tafter\tverdict\tcandidates",
          judged.map(j => Seq(j.country, j.venue, j.rawTitle, j.before, j.after, j.verdict, j.by.mkString(", ")).mkString("\t")))
        val bad = judged.filter(j => j.verdict == "wrong" || j.verdict == "move").flatMap(_.by).toSet
        val unattributed = judged.count(j => (j.verdict == "wrong" || j.verdict == "move") && j.by.isEmpty)
        println(s"round $round: ${alive.size} candidate(s); ${judged.count(_.verdict == "right")} right, ${judged.count(_.verdict == "wrong")} wrong, " +
          s"${judged.count(_.verdict == "move")} moved, ${judged.count(_.verdict == "unjudged")} unjudged; dropping ${bad.size}; $unattributed bad unattributed")
        settled = bad.isEmpty
        badEver ++= bad
        alive = alive.filterNot(d => bad(keyOf(d)))
      }
      val gains = judged.flatMap(j => j.by.map(_ -> j)).groupMap(_._1)(_._2)
      def rightOf(d: TitleDecorations.Learned) = gains.getOrElse(keyOf(d), Nil).count(_.verdict == "right")
      def unjudgedOf(d: TitleDecorations.Learned) = gains.getOrElse(keyOf(d), Nil).count(_.verdict == "unjudged")
      // every candidate's outcome — the fit's training rows: good (a right take, nothing wrong or moved), bad, or neither
      val rows = proposed.map(d => DecorationScore.Row(keyOf(d), features(keyOf(d)), good = settled && !badEver(keyOf(d)) && rightOf(d) > 0, bad = badEver(keyOf(d))))
      Files.writeString(out.resolve("training.tsv"), (scripts.IdentityDecorationScoreFit.Header +: rows.sortBy(_.key).map(scripts.IdentityDecorationScoreFit.line))
        .mkString("", "\n", "\n"), StandardCharsets.UTF_8)
      val kept = proposed.filter(d => rows.exists(r => r.key == keyOf(d) && r.good) && score.forall(_.accepts(features(keyOf(d))))).map { d =>
        d.copy(measured = Some(s"${d.measured.getOrElse("")}: ${rightOf(d)} right, 0 wrong, 0 moved, ${unjudgedOf(d)} unjudged" +
          score.fold("")(s => f", score ${s.probability(features(keyOf(d)))}%.3f") + s" (${opts.getOrElse("version", "unversioned")})"))
      }
      tsv(out.resolve("measured.tsv"), "side\tdecoration\toutcome\tright\tunjudged\tscore\tfilms\tvenues\texamples",
        proposed.map { d =>
          val outcome = if (badEver(keyOf(d))) "bad" else if (rightOf(d) > 0) "good" else "none"
          Seq(d.side, d.decoration, outcome, rightOf(d), unjudgedOf(d), score.fold("")(s => f"${s.probability(features(keyOf(d)))}%.3f"),
            d.films, d.venues, d.examples.mkString(" | ")).mkString("\t")
        })
      Files.writeString(out.resolve("kept.json"), Json.prettyPrint(Json.toJson(kept)) + "\n")
      println(s"${if (settled) "settled" else "NOT settled"} after $round round(s): kept ${kept.size} decoration(s) → ${out.resolve("kept.json")}")
      opts.get("write").map(Paths.get(_)).filter(_ => kept.nonEmpty).foreach { artefact =>
        val earlier = Json.parse(Files.readString(artefact)).as[TitleDecorations.Artefact]
        val merged  = earlier.copy(decorations = TitleDecorations.accumulate(earlier.decorations, kept, records))
        Files.writeString(artefact, Json.prettyPrint(Json.toJson(merged)) + "\n")
        println(s"merged into $artefact")
      }
    } finally storages.foreach(s => Try(s.close()))
  }

  private def tsv(path: Path, header: String, lines: Seq[String]): Unit =
    Files.writeString(path, (header +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
}
