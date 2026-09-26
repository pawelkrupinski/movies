package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.movies.ListingKey
import tools._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.collection.mutable
import scala.util.{Random, Try}

/**
 * The identity resolver in SHADOW over real corpora, head to head with today's pipeline
 * (docs/design/identity-resolver.md §phase 2). Per corpus it boots the pipeline the way the
 * convergence legs do, resolves the same raw listings from the same recorded answers, and
 * measures both:
 *
 *  - the phase-2 gate: cannot-linked pairs inside one resolver cluster (must be 0), order
 *    independence over permutations and split arrivals, and the historical checks for both;
 *  - identity accuracy against CORROBORATED labels (a node's one candidate its own evidence backs
 *    with two independent signals and nothing against it), on the held-out families only;
 *  - label-free measures: the share of matched films a listing's own evidence contradicts,
 *    cross-country agreement, recovery under perturbation, behaviour under a TMDB outage;
 *  - every disagreement, adjudicated on evidence alone (resolver right / pipeline right / both
 *    wrong / undecidable);
 *  - cost: seconds and lookups.
 *
 * The five hard-cluster corpora always run (the itAll layer). The five full corpora run when
 * `KINOWO_IDENTITY_FULL=pl,uk,de,es,us`, `KINOWO_IDENTITY_CORPUS_DIR` (the recorder's
 * `cinema-scrapes-<cc>.json.gz`) and `KINOWO_FIXTURE_ROOT` (real `enrichment-<cc>` directories)
 * are set; `KINOWO_HARD_CLUSTERS_COUNTRIES` narrows the hard clusters. Reports and the calibration
 * dataset go to `KINOWO_IDENTITY_OUT` (default `target/identity-shadow`).
 */
class IdentityShadowIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val fixtureRoot = configuration.fixtureRoot

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]
  override def afterAll(): Unit = {
    storages.synchronized(storages.foreach(s => Try(s.close())))
    super.afterAll()
  }

  private val calibration = IdentityCalibration.default
  private val out     = configuration.identityShadowOutput.value
  private val corpora: Seq[Corpus] =
    hardClusters(configuration.hardClusterCountries.map(_.value.map(_.code))) ++
      configuration.identityCorpusDirectory.toSeq.flatMap(d => IdentityShadow.full(configuration.identityFullCorpora.value, d.value, fixtureRoot))
  private val permutations = configuration.identityShadowPermutations.value

  /** One corpus's measurements, for the summary table. */
  private final case class Row(label: String, listings: Int, nodes: Int, families: Int,
                               pipelineFilms: Int, pipelineResolved: Int, resolverClusters: Int, resolverResolved: Int,
                               identicalClusters: Int, violations: Int, orderVariants: Int, presentations: Int,
                               heldOutLabelled: Int, pipelineAccuracy: Double, resolverAccuracy: Double,
                               pipelineRecall: Double, resolverRecall: Double, contradictedLabels: (Int, Int),
                               unmatched: Map[String, Int],
                               pipelinePairwise: Pairwise, resolverPairwise: Pairwise,
                               pipelineContradicted: (Int, Int), resolverContradicted: (Int, Int),
                               verdicts: Map[String, Int], checks: Map[String, (Int, Int, Int)],
                               outage: (Int, Int, Int), perturbation: (Int, Int), unknown: (Int, Int, Int), lookups: (Int, Int, Int),
                               pipelineSeconds: Double, pipelineRequests: Long, resolverSeconds: Double, resolverRequests: Long)
  private val rows = mutable.ListBuffer.empty[Row]
  /** (country, title key, directors) → the films each system gave those listings, per corpus. */
  private val crossCountry = mutable.ListBuffer.empty[(String, String, Option[Int], Option[Int])]

  corpora.foreach { c =>
    "The shadow resolver" should s"hold the phase-2 gate and be measured against the pipeline on ${c.label}" in {
      val report = new Report(out.resolve(s"${c.label}-report.txt"))
      val w = wiring(mongoTarget, c, storages, fixtureRoot, configuration.env)
      val missesBeforeBoot = c.misses()
      val (films, bootSeconds) = timed(bootPipeline(w))
      val pipelineRequests = c.fetch.requests.get()
      val listings = listingsOf(w, c.normalizer)
      val pipelineOf = pipelineFilmOf(listings, films, c.normalizer)
      report.line(f"[${c.label}] ${listings.size} listings at ${listings.map(_.venue).distinct.size} venues; pipeline: ${films.size} films " +
        f"(${films.count(_.tmdbId.isDefined)} with a tmdbId) in $bootSeconds%.0fs, $pipelineRequests HTTP requests, " +
        s"${c.misses() - missesBeforeBoot} unanswerable; ${listings.count(l => !pipelineOf.contains(l.key))} listings on no pipeline film")

      // ── the resolver ─────────────────────────────────────────────────────────────────
      val source = new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language,
        retrySleep = (_: Long) => ()), w.detailEnrichers, c.misses)
      val lookups = new Memo(source)
      val requestsBefore = c.fetch.requests.get()
      val (resolution, resolveSeconds) = timed(IdentityResolver.resolve(listings, lookups, c.normalizer, calibration))
      val resolverRequests = c.fetch.requests.get() - requestsBefore
      val decisionOf = resolution.decisionOf
      val evidenceOf: Map[ListingKey, Evidence] = listings.map(l =>
        l.key -> Evidence.of(l, if (lookups.hasDetail(l)) lookups.detail(l).toOption.flatten else None)).toMap
      val clusterIndex: Map[ListingKey, Int] = resolution.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
      def resolverFilm(k: ListingKey): Option[FilmAnswer] =
        decisionOf.get(k).flatMap(_.film).flatMap(id => resolution.films.get(id).map(FilmAnswer(id, _)))
      def pipelineFilm(k: ListingKey): Option[FilmAnswer] =
        pipelineOf.get(k).flatMap(i => films(i).tmdbId.zip(films(i).film).map { case (id, f) => FilmAnswer(id, f) })
      report.line(f"[${c.label}] resolver: ${resolution.nodes} nodes in ${resolution.families} families → ${resolution.decisions.size} clusters " +
        f"(${resolution.decisions.count(_.film.isDefined)} matched) in $resolveSeconds%.1fs; lookups ${lookups.sizes} (details, queries, films), " +
        s"unanswerable ${lookups.unknown}; $resolverRequests HTTP requests; cannot-linked pairs inside a cluster: ${resolution.violations}")

      // Why the resolver left listings unmatched: (a) nothing answerable, (b) below the cut, (c) vetoed.
      val unmatched = listings.flatMap(l => decisionOf.get(l.key)).filterNot(_.basis.matched)
        .groupMapReduce(_.basis.toString)(_ => 1)(_ + _)
      report.line(s"[${c.label}] resolver's unmatched listings by why: " +
        unmatched.toSeq.sortBy(-_._2).map { case (b, n) => s"$b $n" }.mkString(", "))

      // ── labels: the calibration's held-out (split == test) corroborated films ─────────
      val heldOutLabels = IdentityShadow.labels(c.country)
      val labelOf: Map[ListingKey, Int] = listings.flatMap(l => heldOutLabels.get(l.key.toString).filter(_.corroborated).map(l.key -> _.tmdbId)).toMap
      val wrongOf: Map[ListingKey, Int] = listings.flatMap(l => heldOutLabels.get(l.key.toString).filterNot(_.corroborated).map(l.key -> _.tmdbId)).toMap

      // ── accuracy on held-out labelled listings ────────────────────────────────────────
      val labelled = listings.map(_.key).filter(labelOf.contains)
      /** Of the labelled listings a system MATCHED, the share it matched to the labelled film. */
      def accuracy(film: ListingKey => Option[FilmAnswer]) = {
        val matched = labelled.filter(k => film(k).isDefined)
        if (matched.isEmpty) Double.NaN else matched.count(k => film(k).exists(_.tmdbId == labelOf(k))).toDouble / matched.size
      }
      /** Of every labelled listing, the share matched to the labelled film. */
      def recall(film: ListingKey => Option[FilmAnswer]) =
        if (labelled.isEmpty) Double.NaN else labelled.count(k => film(k).exists(_.tmdbId == labelOf(k))).toDouble / labelled.size
      def onContradicted(film: ListingKey => Option[FilmAnswer]) = wrongOf.count { case (k, id) => film(k).exists(_.tmdbId == id) }
      // Every labelled listing the resolver matched to ANOTHER film, with the decision's reasons.
      val mismatched = labelled.filter(k => resolverFilm(k).exists(_.tmdbId != labelOf(k)))
      report.line(s"[${c.label}] resolver matched ${mismatched.size} labelled listing(s) to another film:" +
        mismatched.groupBy(k => (labelOf(k), decisionOf(k).film)).toSeq.sortBy(-_._2.size).take(25).map { case ((label, film), ks) =>
          s"\n    ×${ks.size} '${ks.head.rawTitle}' label $label → ${film.getOrElse("—")}: ${decisionOf(ks.head).explanation.take(3).mkString(" | ")}"
        }.mkString)
      val truth = labelled.map(k => k -> labelOf(k)).toMap
      val pipePairwise = pairwise(truth, labelled.flatMap(k => pipelineOf.get(k).map(k -> _)).toMap)
      val resPairwise  = pairwise(truth, labelled.map(k => k -> clusterIndex(k)).toMap)

      // Coverage against accuracy as the confidence gate rises (the new side's curve), beside the
      // pipeline's single point: covered = matched at or above the gate, over all listings; accuracy
      // over the held-out labelled ones it covers.
      val curve = Seq(0.0, 0.5, 0.7, 0.9, 0.95).map { gate =>
        val covered = (k: ListingKey) => decisionOf(k).film.isDefined && decisionOf(k).confidence >= gate
        val lab = labelled.filter(covered)
        f"≥$gate%.2f: coverage ${pct(listings.count(l => covered(l.key)).toLong, listings.size.toLong)}, " +
          s"accuracy ${pct(lab.count(k => resolverFilm(k).exists(_.tmdbId == labelOf(k))).toLong, lab.size.toLong)} of ${lab.size}"
      }
      val pipelinePoint = s"coverage ${pct(listings.count(l => pipelineFilm(l.key).isDefined).toLong, listings.size.toLong)}, " +
        s"accuracy ${pct(labelled.count(k => pipelineFilm(k).exists(_.tmdbId == labelOf(k))).toLong, labelled.count(k => pipelineFilm(k).isDefined).toLong)}"
      report.line(s"[${c.label}] coverage/accuracy — pipeline: $pipelinePoint; resolver by confidence gate: ${curve.mkString("; ")}")

      // ── label-free: matched films a listing's own evidence contradicts ────────────────
      def contradicted(groups: Seq[(Option[IdentityMeasures.Film], Seq[ListingKey])]): (Int, Int) = {
        val matched = groups.collect { case (Some(f), ks) => (f, ks) }
        (matched.count { case (f, ks) => ks.exists(k => contradicts(evidenceOf(k), f)) }, matched.size)
      }
      val pipeGroups = pipelineOf.toSeq.groupMap(_._2)(_._1).toSeq.map { case (i, ks) => films(i).film.filter(_ => films(i).tmdbId.isDefined) -> ks }
      val resGroups  = resolution.decisions.map(d => d.film.flatMap(resolution.films.get) -> d.members)
      val pipeContra = contradicted(pipeGroups)
      val resContra  = contradicted(resGroups)

      // ── identical clusters, and every disagreement adjudicated on evidence ────────────
      val pipeSets = pipelineOf.toSeq.groupMap(_._2)(_._1).values.map(_.toSet).toSet
      val identical = resolution.decisions.count(d => pipeSets.contains(d.listings.toSet))
      val cells = listings.map(_.key).groupBy(k => (pipelineOf.get(k), clusterIndex(k)))
      // Components of the bipartite pipeline-film ↔ resolver-cluster graph that are not a clean 1:1 with one film.
      val byPipe = cells.keys.groupMap(_._1)(_._2)
      val byRes  = cells.keys.groupMap(_._2)(_._1)
      val seen   = mutable.HashSet.empty[(Option[Int], Int)]
      val verdicts = mutable.ArrayBuffer.empty[(String, String)]
      cells.keys.toSeq.sortBy(k => (k._1.getOrElse(-1), k._2)).foreach { start =>
        if (!seen(start)) {
          val component = mutable.LinkedHashSet(start)
          var frontier = Seq(start)
          while (frontier.nonEmpty) {
            frontier = frontier.flatMap { case (p, r) => byPipe(p).map(p -> _) ++ byRes(r).map(_ -> r) }.filterNot(component)
            component ++= frontier
          }
          seen ++= component
          val clean = component.size == 1 && {
            val (p, r) = start
            val pf = p.flatMap(films(_).tmdbId); val rf = resolution.decisions(r).film
            pf == rf && (p.isDefined || cells(start).size == resolution.decisions(r).members.size)
          }
          if (!clean) verdicts += adjudicate(component.toSeq, cells, pipelineFilm, resolverFilm, evidenceOf, films, resolution)
        }
      }
      verdicts.groupBy(_._1).toSeq.sortBy(-_._2.size).foreach { case (v, xs) =>
        report.line(s"[${c.label}] disagreement verdict '$v' ×${xs.size}\n    ${xs.map(_._2).sorted.take(12).mkString("\n    ")}")
      }

      // ── historical checks, for both ───────────────────────────────────────────────────
      val withEvidence = listings.map(l => l -> evidenceOf(l.key))
      val resolverAnswer = IdentityHistoricalChecks.Answer(k => clusterIndex.get(k), k => resolverFilm(k).map(f => f.tmdbId -> f.film.year))
      val pipelineAnswer = IdentityHistoricalChecks.Answer(k => pipelineOf.get(k), k => pipelineFilm(k).map(f => f.tmdbId -> f.film.year))
      val checkTally = mutable.HashMap.empty[String, (Int, Int, Int)]
      IdentityHistoricalChecks.All.filter(_.country == c.country.code).foreach { check =>
        val (r, p) = (IdentityHistoricalChecks.judge(check, withEvidence, resolverAnswer), IdentityHistoricalChecks.judge(check, withEvidence, pipelineAnswer))
        report.line(s"[${c.label}] check '${check.name}': resolver $r, pipeline $p")
        if (r == IdentityHistoricalChecks.Verdict.Fail)
          withEvidence.filter { case (l, e) => check.selectors.exists(_(l, e)) }
            .flatMap { case (l, _) => decisionOf.get(l.key) }.distinct.take(6)
            .foreach(d => report.line(s"    ${d.members.head.rawTitle}: ${d.render}"))
        def tally(system: String, v: IdentityHistoricalChecks.Verdict) = {
          val (pass, fail, na) = checkTally.getOrElse(system, (0, 0, 0))
          checkTally(system) = v match {
            case IdentityHistoricalChecks.Verdict.Pass => (pass + 1, fail, na)
            case IdentityHistoricalChecks.Verdict.Fail => (pass, fail + 1, na)
            case _                                     => (pass, fail, na + 1)
          }
        }
        tally("resolver", r); tally("pipeline", p)
        if (r == IdentityHistoricalChecks.Verdict.Fail && p == IdentityHistoricalChecks.Verdict.Pass) tally("regression", r)
      }

      // ── determinism: permutations and split arrivals ──────────────────────────────────
      def signature(r: Resolution) = r.decisions.map(d => (d.listings, d.film, math.round(d.confidence * 1e9))).toSet
      val reference = signature(resolution)
      val n = if (c.isHardCluster) 21 else permutations
      val variants = (1 to n).count { s =>
        val rnd = new Random(s.toLong)
        val shuffled = rnd.shuffle(listings)
        if (s % 3 == 2) IdentityResolver.resolve(shuffled.take(shuffled.size / 2), lookups, c.normalizer, calibration)
        signature(IdentityResolver.resolve(shuffled, lookups, c.normalizer, calibration)) != reference
      }
      report.line(s"[${c.label}] determinism: $variants of $n presentations (permutations and split arrivals) differ from the sorted one")

      // ── robustness: a 30% TMDB outage ─────────────────────────────────────────────────
      val outaged = IdentityResolver.resolve(listings, new Outage(lookups, 0.3), c.normalizer, calibration)
      val (lost, moved) = listings.map(_.key).foldLeft((0, 0)) { case ((l, m), k) =>
        (decisionOf(k).film, outaged.decisionOf(k).film) match {
          case (Some(a), Some(b)) if a != b => (l, m + 1)
          case (Some(_), None)              => (l + 1, m)
          case _                            => (l, m)
        }
      }
      report.line(s"[${c.label}] 30% outage: $lost listing(s) lost their film, $moved moved to another film, " +
        s"${outaged.violations} cannot-linked pairs inside a cluster")

      // ── perturbation recovery (resolver): decorate a confidently matched listing ───────
      val sample = new Random(7).shuffle(resolution.decisions.filter(d => d.film.isDefined && calibration.showsRatings(d.confidence))).take(40)
      val transforms: Seq[String => String] = Seq(t => s"Pokaz specjalny: $t", t => s"$t (2026)",
        _.toUpperCase(java.util.Locale.ROOT), t => tools.TextNormalization.deburr(t))
      val familyListings = listings.groupBy(l => resolution.familyOf(l.key))
      var (tried, recovered) = (0, 0)
      sample.foreach { d =>
        val original = listings.find(_.key == d.members.head).get
        val family   = familyListings(resolution.familyOf(original.key))
        if (family.size <= 400) transforms.foreach { t =>
          val title = t(original.rawTitle)
          val moved = original.copy(key = ListingKey.Published(original.venue + " (perturbed)", title, original.year, original.directors),
            rawTitle = title, title = t(original.title), cleanTitle = t(original.cleanTitle), page = None)
          val r = IdentityResolver.resolve(family :+ moved, lookups, c.normalizer, calibration)
          tried += 1
          if (r.decisionOf(moved.key).film == d.film) recovered += 1
        }
      }
      report.line(s"[${c.label}] perturbation: $recovered of $tried decorated/re-dated/re-cased copies of a matched listing recovered its film")

      // ── cross-country keys ────────────────────────────────────────────────────────────
      listings.filter(_.directors.nonEmpty).foreach { l =>
        val key = c.normalizer.sanitize(l.originalTitle.getOrElse(l.cleanTitle)) + "|" + l.directors.map(c.normalizer.sanitize).sorted.mkString(",")
        crossCountry.synchronized(crossCountry += ((c.label, key, pipelineFilm(l.key).map(_.tmdbId), resolverFilm(l.key).map(_.tmdbId))))
      }

      Files.writeString(out.resolve(s"${c.label}-decisions.txt"),
        resolution.decisions.map(d => s"${d.members.map(_.toString).take(3).mkString(" | ")}${if (d.members.size > 3) s" … (${d.members.size})" else ""}\n  ${d.render}")
          .mkString("\n"), StandardCharsets.UTF_8)
      rows.synchronized(rows += Row(c.label, listings.size, resolution.nodes, resolution.families,
        films.size, films.count(_.tmdbId.isDefined), resolution.decisions.size, resolution.decisions.count(_.film.isDefined),
        identical, resolution.violations, variants, n, labelled.size, accuracy(pipelineFilm), accuracy(resolverFilm),
        recall(pipelineFilm), recall(resolverFilm), (onContradicted(pipelineFilm), onContradicted(resolverFilm)), unmatched,
        pipePairwise, resPairwise, pipeContra, resContra, verdicts.groupBy(_._1).view.mapValues(_.size).toMap, checkTally.toMap,
        (lost, moved, outaged.violations), (recovered, tried), lookups.unknown, lookups.sizes,
        bootSeconds, pipelineRequests, resolveSeconds, resolverRequests))

      withClue("the phase-2 gate: no cannot-linked pair inside a cluster") { resolution.violations shouldBe 0 }
      withClue("order independence") { variants shouldBe 0 }
      outaged.violations shouldBe 0
    }
  }

  /** One disagreement component, adjudicated on the listings' own evidence alone. Each cell (the
   *  listings one pipeline film and one resolver cluster share) asks: which of the two films does
   *  the evidence back? The component's verdict is the listing-weighted majority of its cells. */
  private def adjudicate(component: Seq[(Option[Int], Int)], cells: Map[(Option[Int], Int), Seq[ListingKey]],
                         pipelineFilm: ListingKey => Option[FilmAnswer], resolverFilm: ListingKey => Option[FilmAnswer],
                         evidenceOf: Map[ListingKey, Evidence], films: Seq[PipelineFilm], resolution: Resolution): (String, String) = {
    def net(ks: Seq[ListingKey], f: Option[FilmAnswer]): (Int, Boolean) = f.fold((0, false)) { answer =>
      val cs = ks.map(k => agreement(evidenceOf(k), answer.film))
      (cs.count(_._1 > 0) - cs.count(_._2 >= 2), cs.nonEmpty && cs.forall(_._2 >= 2))
    }
    val judged = component.map { cell =>
      val ks = cells(cell)
      val (p, r) = (pipelineFilm(ks.head), resolverFilm(ks.head))
      val verdict =
        if (p.map(_.tmdbId) == r.map(_.tmdbId)) "same-film"
        else {
          val ((np, cp), (nr, cr)) = (net(ks, p), net(ks, r))
          if (cp && cr) "both wrong" else if (nr > np) "resolver right" else if (np > nr) "pipeline right" else "undecidable"
        }
      (verdict, ks.size)
    }
    val decided = judged.filterNot(_._1 == "same-film")
    val verdict =
      if (decided.isEmpty) {
        // Only the GROUPING differs, not the films: a split both sides call the same film, or a
        // merge of films the pipeline left unmatched. The published years decide it when they can.
        val years = component.flatMap(cells).flatMap(k => evidenceOf(k).year).distinct
        val pipeFilms = component.flatMap(_._1).distinct
        if (pipeFilms.size > 1 && pipeFilms.flatMap(films(_).tmdbId).distinct.size == 1) "resolver right"
        else if (years.nonEmpty && years.max - years.min > services.resolution.YearWindow.ProductionToRelease) "undecidable (years apart)"
        else "undecidable"
      } else decided.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (v, n) => (-n, v) }.head._1
    val pipeDesc = component.flatMap(_._1).distinct.map(i => s"${films(i).key}(tmdb=${films(i).tmdbId.getOrElse("—")})")
    val resDesc  = component.map(_._2).distinct.map(i => s"#$i(tmdb=${resolution.decisions(i).film.getOrElse("—")})")
    val titles   = component.flatMap(cells).map(_.rawTitle).distinct.take(4)
    (verdict, s"${titles.mkString(" / ")} :: pipeline ${pipeDesc.mkString(" + ")} vs resolver ${resDesc.mkString(" + ")} [${judged.map(_._1).distinct.mkString(",")}]")
  }

  "The shadow report" should "summarise every corpus, head to head" in {
    val report = new Report(out.resolve("summary.md"))
    def f2(x: Double) = if (x.isNaN) "—" else f"${x * 100}%.1f%%"
    report.line(s"calibration: ${calibration.version}\n")
    report.line("| corpus | listings | pipeline films (tmdb) | resolver clusters (tmdb) | identical | P3 violations | order variants | held-out labelled | tmdb accuracy of matched old / new | labelled recall old / new | on a contradicted filing old / new | resolver unmatched by why | pairwise F1 old / new | wrong merges old / new | wrong splits old / new | contradicted matches old / new | verdicts (resolver/pipeline/both/undecidable) | checks pass-fail old / new (regressions) | outage lost/moved | perturbation recovered | unanswerable (details/queries/films) | seconds old / new | HTTP old / new |")
    report.line("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    rows.foreach { r =>
      def v(k: String) = r.verdicts.filter(_._1.startsWith(k)).values.sum
      def ck(s: String) = r.checks.get(s).fold("—")(t => s"${t._1}-${t._2}")
      report.line(s"| ${r.label} | ${r.listings} | ${r.pipelineFilms} (${r.pipelineResolved}) | ${r.resolverClusters} (${r.resolverResolved}) | " +
        s"${r.identicalClusters} | ${r.violations} | ${r.orderVariants}/${r.presentations} | ${r.heldOutLabelled} | " +
        s"${f2(r.pipelineAccuracy)} / ${f2(r.resolverAccuracy)} | ${f2(r.pipelineRecall)} / ${f2(r.resolverRecall)} | " +
        s"${r.contradictedLabels._1} / ${r.contradictedLabels._2} | ${r.unmatched.toSeq.sortBy(-_._2).map { case (b, n) => s"$b $n" }.mkString(", ")} | " +
        s"${f2(r.pipelinePairwise.f1)} / ${f2(r.resolverPairwise.f1)} | " +
        s"${r.pipelinePairwise.wrongMerges} / ${r.resolverPairwise.wrongMerges} | ${r.pipelinePairwise.wrongSplits} / ${r.resolverPairwise.wrongSplits} | " +
        s"${r.pipelineContradicted._1}/${r.pipelineContradicted._2} / ${r.resolverContradicted._1}/${r.resolverContradicted._2} | " +
        s"${v("resolver right")}/${v("pipeline right")}/${v("both wrong")}/${v("undecidable")} | " +
        s"${ck("pipeline")} / ${ck("resolver")} (${r.checks.get("regression").fold(0)(_._2)}) | ${r.outage._1}/${r.outage._2} | " +
        s"${r.perturbation._1}/${r.perturbation._2} | ${r.unknown} of ${r.lookups} | ${f"${r.pipelineSeconds}%.0f"} / ${f"${r.resolverSeconds}%.0f"} | " +
        s"${r.pipelineRequests} / ${r.resolverRequests} |")
    }
    // Cross-country agreement: one director-credited title across corpora, one film everywhere?
    val byKey = crossCountry.toSeq.groupBy(_._2).filter(_._2.map(_._1).distinct.size > 1)
    def disagreements(pick: ((String, String, Option[Int], Option[Int])) => Option[Int]) =
      byKey.count(_._2.flatMap(pick).distinct.size > 1)
    report.line(s"\ncross-country: ${byKey.size} director-credited titles listed in 2+ corpora; films disagreeing across corpora — " +
      s"pipeline ${disagreements(_._3)}, resolver ${disagreements(_._4)}")
    succeed
  }
}
