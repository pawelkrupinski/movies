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

  private val calibration = IdentityCalibration.resolver
  private val out     = configuration.identityShadowOutput.value
  /** Every disagreement cell, one JSONL per country (`<out>/identity-disagreements/<cc>.jsonl`). */
  private val disagreementsDir = {
    val d = out.resolve("identity-disagreements")
    if (Files.isDirectory(d)) Files.list(d).forEach(Files.delete(_))
    d
  }
  private val corpora: Seq[Corpus] =
    hardClusters(configuration.hardClusterCountries.map(_.value.map(_.code))) ++
      configuration.identityCorpusDirectory.toSeq.flatMap(d => IdentityShadow.full(configuration.identityFullCorpora.value, d.value, fixtureRoot))
  private val permutations = configuration.identityShadowPermutations.value
  private val pipelineCache = configuration.identityPipelineCache
  /** Off for a resolver-variant run: the measures below that resolve the corpus again. */
  private val robustness = configuration.identityShadowRobustness.value
  private val focus      = configuration.identityFocus

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
      val listings = listingsOf(w, c.normalizer)
      // Today's pipeline, booted once per corpus: read from the cache when a run already booted it.
      val cached = pipelineCache.map(_.value.resolve(s"${c.label}.json.gz"))
      val earlier = cached.filter(Files.isRegularFile(_))
      val booted = earlier.fold {
        val missesBeforeBoot = c.misses()
        val (films, seconds) = timed(bootPipeline(w))
        val b = BootedPipeline(films, pipelineFilmOf(listings, films, c.normalizer), seconds, c.fetch.requests.get(),
          (c.misses() - missesBeforeBoot).toLong)
        cached.foreach(BootedPipeline.write(_, b))
        b
      }(BootedPipeline.read(_, listings))
      val (films, pipelineOf, bootSeconds, pipelineRequests) = (booted.films, booted.filmOf, booted.seconds, booted.requests)
      report.line(f"[${c.label}] ${listings.size} listings at ${listings.map(_.venue).distinct.size} venues; pipeline: ${films.size} films " +
        f"(${films.count(_.tmdbId.isDefined)} with a tmdbId) in $bootSeconds%.0fs, $pipelineRequests HTTP requests, " +
        s"${booted.unanswerable} unanswerable; ${listings.count(l => !pipelineOf.contains(l.key))} listings on no pipeline film" +
        earlier.fold("")(p => s" (booted earlier: $p)"))

      // ── the resolver ─────────────────────────────────────────────────────────────────
      val source = new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language,
        retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, new TmdbIdentityLookups.CountedGaps(c.misses))
      val lookups = new Memo(source)
      // Focus mode: resolve only the families of the named titles, print their decisions, stop.
      focus.foreach { f =>
        val tokens = (l: Listing) => services.movies.TitleContainment.tokens(l.rawTitle).toSet ++
          services.movies.TitleContainment.tokens(l.cleanTitle).toSet
        val focused = listings.filter(l => f.covers(tokens(l)))
        val (r, secs) = timed(IdentityResolver.resolve(if (f.alone) focused else listings, lookups, c.normalizer, calibration))
        report.line(f"[${c.label}] FOCUS ${f.phrases.map(_.mkString(" ")).mkString(",")}: ${focused.size} listing(s), " +
          f"resolved ${if (f.alone) "alone" else s"with the whole corpus (${listings.size})"} in $secs%.1fs")
        focused.map(l => r.decisionOf(l.key)).distinct.foreach(d => report.line(d.render))
        // Why each focused node resolved as it did and sits in its family (`IdentityResolver.explain`).
        report.line(s"[${c.label}] FOCUS explanations:")
        IdentityResolver.explain(if (f.alone) focused else listings, lookups, c.normalizer, calibration)(focused.map(_.key).toSet)
          .foreach(told => told.render.foreach(line => report.line(s"  $line")))
        // Each focused node's candidates as its family scored them: why it took what it took.
        val focusedKeys = focused.map(_.key).toSet
        report.line(s"[${c.label}] FOCUS candidates (tmdb, probability, search rank, flags, record — measures):")
        IdentityResolver.candidatesOf(if (f.alone) focused else listings, lookups, c.normalizer, calibration)(listing => focusedKeys(listing.key))
          .foreach { node =>
            report.line(s"  ${node.label}")
            node.banners.foreach(banner => report.line(s"    $banner"))
            node.candidates.take(8).foreach(candidate => report.line(s"    ${candidate.render}"))
            if (node.candidates.sizeIs > 8) report.line(s"    … ${node.candidates.size - 8} more")
          }
        // Old against new for the focused listings, each side refereed alone: which film each
        // gives them, how each groups them, and whether the listings' own facts back it.
        def side(id: Option[Int], film: Option[IdentityMeasures.Film], e: Evidence): String =
          id.fold("unmatched")(i => s"$i ${film.fold("?")(f => s"${f.title} (${f.year.getOrElse("?")})")}" +
            film.fold("")(f => { val (v, d) = IdentityReferee.judge(e, f); s" [${v.toString.toLowerCase}${if (d.nonEmpty) d.mkString(": ", ",", "") else ""}]" }))
        report.line(s"[${c.label}] FOCUS old vs new (listings | old film [referee] | new film [referee] | spellings):")
        focused.groupBy { l =>
          val e = Evidence.of(l, if (lookups.hasDetail(l)) lookups.detail(l).toOption.flatten else None)
          val old = pipelineOf.get(l.key).flatMap(i => films(i).tmdbId.map(_ -> films(i).film))
          val neu = r.decisionOf(l.key).film.map(id => id -> r.films.get(id))
          (side(old.map(_._1), old.flatMap(_._2), e), side(neu.map(_._1), neu.flatMap(_._2), e))
        }.toSeq.sortBy(-_._2.size).foreach { case ((o, n), ls) =>
          report.line(f"    ${ls.size}%4d | old $o | new $n | ${ls.map(_.rawTitle).distinct.sorted.take(4).mkString(" / ")}")
        }
        cancel(s"focus mode: ${focused.size} listing(s) resolved; the corpus-wide measures need every listing")
      }
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
      // The largest families taken apart: which block keys glue one far past a film (PL held ~75% of
      // its listings in one family, so the incremental model re-resolved most of PL on any event).
      IdentityResolver.familyAnatomy(listings, lookups, c.normalizer, calibration)(2)
        .foreach(anatomy => anatomy.render.foreach(line => report.line(s"[${c.label}] $line")))

      // The same corpus kept INCREMENTALLY: taken whole, one title-key component at a time (a new
      // country, or a rebuild after the rules change), it must decide exactly as the whole resolve;
      // then the steady state — a venue re-scraped unchanged, a venue leaving and coming back, and
      // ten venues leaving and coming back in one batch.
      {
        def liveMb(): Long = { System.gc(); val runtime = Runtime.getRuntime; (runtime.totalMemory - runtime.freeMemory) >> 20 }
        def signature(decisions: Seq[ResolverDecision]) = decisions.map(d => (d.listings, d.film, math.round(d.confidence * 1e9), d.basis)).toSet
        val byVenue = listings.groupBy(_.venue).toSeq.sortBy(_._1).map(_._2)
        val before  = liveMb()
        val model   = new IncrementalResolver(lookups, c.normalizer, calibration)
        val (_, seedSeconds) = timed(model.seed(listings))
        val held    = liveMb() - before
        val seeded  = model.familiesResolved
        val same    = signature(model.decisions) == signature(resolution.decisions)
        def cost(body: => Unit): (Int, Double) = { val start = model.familiesResolved; val (_, seconds) = timed(body); (model.familiesResolved - start, seconds) }
        val venue   = byVenue.maxBy(_.size)
        val ten     = byVenue.sortBy(-_.size).slice(1, 11).flatten
        val (unchanged, unchangedSeconds) = cost(model.listingsSeen(venue))
        val (cycle, cycleSeconds)         = cost { model.listingsGone(venue.map(_.key)); model.listingsSeen(venue) }
        val (tenCycle, tenSeconds)        = cost { model.batch(Nil, ten.map(_.key), AnswersChanged.Empty); model.batch(ten, Nil, AnswersChanged.Empty) }
        val sameAfter = signature(model.decisions) == signature(resolution.decisions)
        report.line(f"[${c.label}] incremental: seeded whole in $seedSeconds%.1fs ($seeded family resolves, ${resolution.families} families), " +
          f"model holds ~${held}MB live; equals the whole resolve: $same; the largest venue (${venue.size} listings) re-scraped unchanged: " +
          f"$unchanged resolves in ${unchangedSeconds * 1000}%.0f ms; gone and back: $cycle resolves in ${cycleSeconds * 1000}%.0f ms; " +
          f"the next ten venues (${ten.size} listings) gone and back, one batch each: $tenCycle resolves in ${tenSeconds * 1000}%.0f ms; still equal: $sameAfter; " +
          s"time in ${model.timings.render}")
        withClue(s"${c.label}: the incremental model decides as the whole resolve") { same shouldBe true; sameAfter shouldBe true }
      }

      // The normalized TMDB store answers every question the resolve asked as the recorded responses did.
      {
        val counted = c.fetch.requests.get()
        val equivalence = StoredLookupsEquivalence.check(c.fetch, c.misses, c.missedKeys, c.country.language, lookups.asked)
        c.fetch.requests.set(counted)
        report.line(s"[${c.label}] stored lookups: ${equivalence.questions} questions and ${equivalence.films} film records answered " +
          s"from the normalized store; ${equivalence.mismatches.size} differ from the recorded answers" +
          equivalence.mismatches.take(5).map("\n    " + _).mkString)
        withClue(s"${c.label}: the normalized store answers as the recorded responses") { equivalence.mismatches shouldBe empty }
      }

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

      // ── STRICT per-listing comparison on the held-out labels ────────────────────────────
      import IdentityDisagreements.Cell
      val listingByKey = listings.map(l => l.key -> l).toMap
      def describe(k: ListingKey): String = {
        val d = decisionOf(k)
        s"'${k.rawTitle}' at ${k.venue}: old ${pipelineFilm(k).fold("no film")(f => s"${f.tmdbId} ${f.film.title} (${f.film.year.getOrElse("?")})")}" +
          s" → new ${d.film.fold(s"no film [${d.basis}]")(id => s"$id ${resolution.films.get(id).fold("")(_.title)} [${d.basis}]")}" +
          s" — ${d.explanation.headOption.getOrElse("")}"
      }
      val cellOfListing = labelled.map(k => k -> IdentityDisagreements.cellOf(labelOf(k), pipelineFilm(k).map(_.tmdbId), resolverFilm(k).map(_.tmdbId))).toMap
      val cellCounts = Cell.values.map(v => v -> cellOfListing.count(_._2 == v)).toMap
      report.line(s"[${c.label}] labelled cells: both right ${cellCounts(Cell.BothRight)} | LOSS-coverage ${cellCounts(Cell.LossCoverage)} | " +
        s"LOSS-wrong ${cellCounts(Cell.LossWrong)} | WIN ${cellCounts(Cell.Win)} | both wrong/unmatched ${cellCounts(Cell.BothWrong)}; " +
        s"old wrong or missed ${labelled.count(k => !pipelineFilm(k).exists(_.tmdbId == labelOf(k)))} of ${labelled.size} (the ceiling for a WIN)")
      Seq(Cell.LossWrong, Cell.LossCoverage, Cell.Win).foreach { v =>
        val ks = cellOfListing.collect { case (k, x) if x == v => k }.toSeq.sortBy(_.toString)
        if (ks.nonEmpty) report.line(s"[${c.label}] $v examples (${ks.size}):\n    " + ks.take(20).map(describe).mkString("\n    "))
      }

      // ── every disagreement cell, adjudicated, to JSONL ──────────────────────────────────
      val showtimeCount: Map[ListingKey, Int] = w.archivedListings.toSeq.flatMap { case (cinema, cms) =>
        cms.map(cm => ListingKey.of(cinema, cm) -> cm.showtimes.size) }.groupMapReduce(_._1)(_._2)(_ + _)
      val clustersOfPipe = cells.keys.groupMap(_._1)(_._2)
      val pipesOfCluster = cells.keys.groupMap(_._2)(_._1)
      val cellLines = cells.toSeq.sortBy { case ((p, r), _) => (p.getOrElse(-1), r) }.flatMap { case ((p, r), ks) =>
        val decision = resolution.decisions(r)
        val (pf, rf) = (pipelineFilm(ks.head), resolverFilm(ks.head))
        IdentityDisagreements.kindOf(pf.map(_.tmdbId), rf.map(_.tmdbId),
          pipelineSplit = p.exists(i => clustersOfPipe(Some(i)).size > 1), resolverMerges = pipesOfCluster(r).size > 1).map { kind =>
          val adj = IdentityDisagreements.adjudicate(ks.map(evidenceOf), pf, rf)
          (kind, adj, ks, IdentityDisagreements.cellJson(c.country.code, if (c.isHardCluster) "hc" else "full", p, films, r, decision,
            resolution, ks.map(listingByKey), evidenceOf, k => showtimeCount.getOrElse(k, 0), heldOutLabels, kind, pf, rf, adj))
        }
      }
      IdentityDisagreements.write(disagreementsDir.resolve(s"${c.country.code}.jsonl"), cellLines.map(_._4))
      IdentityDisagreements.write(disagreementsDir.resolve(s"listings-${c.label}.jsonl"), listings.map(l =>
        IdentityDisagreements.listingJson(c.country.code, if (c.isHardCluster) "hc" else "full", l, evidenceOf(l.key), pipelineFilm(l.key),
          clusterIndex(l.key), decisionOf(l.key), resolution, heldOutLabels.get(l.key.toString),
          pipelineOf.get(l.key).flatMap(i => films(i).basis))))
      // The absolute referee on every listing, old and new alone: an old match judged wrong counts as
      // unresolved (unresolved beats wrong); a new one is the resolver's error, which must not grow.
      val judged = listings.map { l =>
        val e = evidenceOf(l.key)
        (l, pipelineFilm(l.key).map(a => IdentityReferee.judge(e, a.film)), resolverFilm(l.key).map(a => IdentityReferee.judge(e, a.film)))
      }
      // `denied`: matches any fact of the listing's denies — the count no change may raise.
      def tally(side: Seq[Option[(IdentityReferee.Verdict, Seq[String])]]) =
        IdentityReferee.Verdict.values.map(v => s"${v.toString.toLowerCase} ${side.count(_.exists(_._1 == v))}").mkString(" / ") +
          s" [denied ${side.count(_.exists(_._2.nonEmpty))}]"
      report.line(s"[${c.label}] referee (each side alone): old ${tally(judged.map(_._2))} | new ${tally(judged.map(_._3))}")
      val newWrong = judged.filter(_._3.exists(_._1 == IdentityReferee.Verdict.Wrong))
      if (newWrong.nonEmpty) report.line(s"[${c.label}] referee: the RESOLVER's wrong matches (${newWrong.size}):\n    " +
        newWrong.groupBy(j => (j._1.rawTitle, resolverFilm(j._1.key).map(_.tmdbId))).toSeq.sortBy(-_._2.size).take(20)
          .map { case ((t, id), js) => s"'$t' → ${id.getOrElse("—")} ×${js.size} (denied by ${js.head._3.get._2.mkString(",")})" }.mkString("\n    "))
      val unlabelledCells = cellLines.filter { case (_, _, ks, _) => ks.forall(k => !heldOutLabels.contains(k.toString)) }
      val adjudicated = unlabelledCells.groupMapReduce(_._2.verdict)(_ => 1)(_ + _)
      val pipelineRightMoved = unlabelledCells.filter { case (kind, adj, _, _) => kind == "moved" && adj.verdict == "pipeline-right" }
      report.line(s"[${c.label}] disagreement cells ${cellLines.size} (${cellLines.groupMapReduce(_._1)(_ => 1)(_ + _).toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}); " +
        s"unlabelled cells by adjudication: ${adjudicated.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}")
      if (pipelineRightMoved.nonEmpty) report.line(s"[${c.label}] unlabelled pipeline-right cells where the resolver picks a DIFFERENT film (${pipelineRightMoved.size}):\n    " +
        pipelineRightMoved.map(x => describe(x._3.head) + s" ×${x._3.size}").mkString("\n    "))
      val strict = cellCounts(Cell.LossWrong) == 0 && cellCounts(Cell.LossCoverage) == 0 && adjudicated.getOrElse("pipeline-right", 0) == 0
      val weak   = cellCounts(Cell.LossWrong) == 0 && pipelineRightMoved.isEmpty && cellCounts(Cell.LossCoverage) <= cellCounts(Cell.Win)
      report.line(s"[${c.label}] VERDICT: STRICT no-worse ${if (strict) "YES" else "NO"}; WEAK no-worse ${if (weak) "YES" else "NO"}")

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
            .flatMap { case (l, _) => decisionOf.get(l.key) }.distinct
            .sortBy(d => -withEvidence.count { case (l, e) => d.listings(l.key) && check.selectors.count(_(l, e)) > 0 }).take(6)
            .foreach(d => report.line(s"    ${d.members.head.rawTitle}: ${d.render}\n      members: " +
              withEvidence.collect { case (l, e) if d.listings(l.key) && check.selectors.exists(_(l, e)) => l.rawTitle }.distinct.take(8).mkString(" | ")))
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
      val n = if (!robustness) 0 else if (c.isHardCluster) 21 else permutations
      val variants = (1 to n).count { s =>
        val rnd = new Random(s.toLong)
        val shuffled = rnd.shuffle(listings)
        if (s % 3 == 2) IdentityResolver.resolve(shuffled.take(shuffled.size / 2), lookups, c.normalizer, calibration)
        signature(IdentityResolver.resolve(shuffled, lookups, c.normalizer, calibration)) != reference
      }
      report.line(s"[${c.label}] determinism: $variants of $n presentations (permutations and split arrivals) differ from the sorted one")

      // ── robustness: a 30% TMDB outage ─────────────────────────────────────────────────
      val outaged = if (robustness) IdentityResolver.resolve(listings, new Outage(lookups, 0.3), c.normalizer, calibration) else resolution
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
      val sample = new Random(7).shuffle(resolution.decisions.filter(d => d.film.isDefined && calibration.showsRatings(d.confidence)))
        .take(if (robustness) 40 else 0)
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
