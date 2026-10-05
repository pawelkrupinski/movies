package integration

import services.identity._
import services.identity.agreement.{Agreement, AgreementStage, FamilyAnswers, VoterFamily}
import services.movies.ListingKey
import tools.{ConvergenceStorage, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPOutputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * The UNIFIED evidence model's training rows (docs/design/identity-resolver.md §20): for every cluster of the recorded
 * full corpora, every film its evidence reaches ([[UnifiedEvidence.contenders]]) with every signal of the model and the
 * agreement stage as a feature, labelled — re-runnable as the corpora, the labels and the answers move:
 *
 *   KINOWO_IDENTITY_FULL=pl,uk,de,es,us KINOWO_IDENTITY_CORPUS_DIR=<dir> KINOWO_FIXTURE_ROOT=<dir>
 *   [KINOWO_IDENTITY_FAMILY_SEED=<dir>] [KINOWO_IDENTITY_POSTER_CACHE=<dir>] [KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=<key>]
 *   MONGODB_URI=<throwaway> MONGODB_DB=<unique>
 *   sbt "worker/IntegrationTest/runMain integration.IdentityUnifiedDataset --out <dir> [--threads 6]"
 *
 * The clusters the model leaves unmatched are the unmatched-cluster fixture's (`UnmatchedClusters`: their decisions,
 * the families' answers and the posters' hashes as captured, the agreement stage's takes on them by `replay`); the
 * rest are the whole corpus's resolve. Every family's verdict is the resolver over its answers as the stage asks it —
 * the fixture's, beside prod's filed answers (`KINOWO_IDENTITY_FAMILY_SEED`) — and the posters are hashed from
 * `KINOWO_IDENTITY_POSTER_CACHE` alone: nothing is asked of a family or a poster host here, a question no answer holds is
 * a missing signal. Labels, per contender:
 *
 *  - HAND (`labels.tsv`, the unmatched ratchet's labels, judged per listing as the ratchet judges a take): right, wrong,
 *    and every other contender of a cluster whose right film a label names is wrong;
 *  - WEAK: a cluster the model matched and no label judges — its film right, its other contenders wrong — unless a venue
 *    poster names another candidate against it, or a family took another film its title names: those are left out.
 *
 * Writes `<out>/training.tsv.gz` (one row per contender, the signals in [[UnifiedEvidence.Names]]'s order), the rows
 * `scripts.IdentityUnifiedFit` fits and measures.
 */
object IdentityUnifiedDataset {

  val Header: Seq[String] = Seq("country", "cluster", "origin", "venue", "listings", "rawTitle", "film", "filmTitle", "today", "label", "source", "right",
    "wrong") ++ UnifiedEvidence.Names

  /** One contender's row: its cluster, where the cluster comes from (`fixture`: an unmatched cluster as the agreement
   *  stage took it; `corpus`: the model's decision), the venue its hold-out fold goes by, how many listings it holds, the
   *  film, whether today's stack takes it, its label (`1`, `0`, `""` unlabelled) and where that came from, how many of
   *  its listings a hand label judges it right and wrong for, and its signals. */
  final case class Row(country: String, cluster: String, origin: String, venue: String, listings: Int, rawTitle: String, film: String,
                       filmTitle: String, today: String, label: String, source: String, right: Int, wrong: Int, signals: Map[String, Double]) {
    def line: String = (Seq(country, cluster, origin, venue, listings.toString, clean(rawTitle), film, clean(filmTitle), today, label, source, right.toString,
      wrong.toString) ++ UnifiedEvidence.Names.map(name => signals.get(name).fold("0")(number))).mkString("\t")
  }
  private def clean(text: String) = text.replace('\t', ' ').replace('\n', ' ')
  private def number(x: Double) = if (x == math.rint(x)) x.toLong.toString else String.format(java.util.Locale.ROOT, "%.2f", Double.box(x))

  def main(args: Array[String]): Unit = {
    val opts    = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val out     = Paths.get(opts.getOrElse("out", sys.error("--out <dir>")))
    val threads = opts.get("threads").map(_.toInt).getOrElse(6)
    val configuration = settings.ProcessConfiguration.resolve()
    val labels   = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val storages = mutable.ListBuffer.empty[ConvergenceStorage]
    Files.createDirectories(out)
    try {
      val corpora = new DecorationCorpora(configuration, storages)
      val rows = corpora.loaded.flatMap(l => country(l, configuration, labels, threads))
      val written = out.resolve("training.tsv.gz")
      val gz = new GZIPOutputStream(Files.newOutputStream(written))
      try gz.write((Header.mkString("\t") +: rows.sortBy(r => (r.country, r.cluster, r.film)).map(_.line)).mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      finally gz.close()
      println(s"wrote $written: ${rows.size} rows")
    } finally storages.foreach(s => scala.util.Try(s.close()))
  }

  private def country(l: DecorationCorpora.Loaded, configuration: settings.ProcessConfiguration, labels: Seq[UnmatchedClusters.Label],
                      threads: Int): Seq[Row] = {
    val c        = l.c
    val code     = c.country.code
    val started  = System.nanoTime()
    val resolution = IdentityResolver.resolve(l.listings, l.lookups, c.normalizer, IdentityCalibration.resolver)
    val nodeOf   = IdentityResolver.evidenceOf(l.listings, l.lookups, c.normalizer)(_ => true).flatMap(n => n.keys.map(_ -> n)).toMap
    val byKey    = l.listings.map(x => x.key -> x).toMap
    val capture  = Some(UnmatchedClusters.fixturePath(c.country)).filter(Files.exists(_)).map(UnmatchedClusters.read)
    val outcome  = capture.map(UnmatchedClusters.replay)

    // the families' answers: prod's filed ones, the fixture's beside them
    val docs = new InMemoryTmdbDocuments
    configuration.identityFamilySeed.map(_.value.resolve(s"${if (code == "pl") "kinowo" else s"kinowo_$code"}.jsonl")).filter(Files.exists(_)).foreach { file =>
      docs.put(TmdbKind.Family, Files.readAllLines(file).asScala.toSeq.filter(_.nonEmpty).map { line =>
        val d = org.bson.BsonDocument.parse(line)
        d.remove("_id").asString.getValue -> d
      })
    }
    capture.foreach(cap => docs.put(TmdbKind.Family, cap.families.toSeq))
    val store   = new FamilyAnswerStore(docs, tools.SpecClock.Pinned)
    val answers: Seq[(VoterFamily, FamilyAnswers)] = UnmatchedClusters.familiesOf(c.country).map(f => f -> store.answers(f))
    val posterStore = new PosterAnswerStore(store, tools.SpecClock.Pinned)
    val posters = configuration.identityPosterCache.map(cache => new CachedPosters(cache.value, live = None, c.country.language.getLanguage, offline = true))
    val finds   = capture.fold(Map.empty[String, Option[Int]])(_.finds)
    val thisYear = java.time.LocalDate.ofInstant(tools.SpecClock.Pinned.instant(), java.time.ZoneOffset.UTC).getYear

    // the fixture's clusters, as the agreement stage took them; the rest of the corpus as the model decided
    val fixture: Seq[(Seq[Listing], ResolverDecision, ResolverDecision, IdentityLookups)] = (for {
      cap <- capture.toSeq
      out <- outcome.toSeq
      replay = new UnmatchedClusters.Replay(cap)
      held   = cap.listings.map(x => x.key -> x).toMap
      (model, agreed) <- out.model.decisions.zip(out.agreed.decisions)
    } yield (model.members.flatMap(held.get), model, agreed, replay: IdentityLookups))
    val covered = fixture.flatMap(_._2.members).toSet
    val corpus  = resolution.decisions.filter(_.basis != ResolverDecision.Basis.Pinned).flatMap { d =>
      val kept = d.members.filterNot(covered)
      Option.when(kept.nonEmpty)(d.copy(members = kept)(d.trace)).map(decision => (decision.members.flatMap(byKey.get), decision, decision, l.lookups: IdentityLookups))
    }
    val clusters = fixture.map(_ -> true) ++ corpus.map(_ -> false)

    val stats = new java.util.concurrent.ConcurrentHashMap[String, java.util.concurrent.atomic.AtomicInteger]()
    def count(what: String): Unit = stats.computeIfAbsent(what, _ => new java.util.concurrent.atomic.AtomicInteger()).incrementAndGet()

    def clusterRows(code: String, listings: Seq[Listing], model: ResolverDecision, today: ResolverDecision, venues: IdentityLookups,
                    fromFixture: Boolean): Seq[Row] = {
      if (listings.isEmpty) Nil else {
        val nodes    = model.members.flatMap(nodeOf.get).distinctBy(_.keys)
        if (nodes.isEmpty) count("no-nodes")
        val verdicts = answers.flatMap { case (family, familyAnswers) =>
          Agreement.verdict(listings, familyAnswers, venues, c.normalizer, IdentityCalibration.resolver.withPriorSpread(family.priorSpread)).toOption
        }
        if (verdicts.nonEmpty) count("with-families")
        val distances = posterDistances(listings, nodes, verdicts)
        if (distances.nonEmpty) count("with-posters")
        val contenders = UnifiedEvidence.contenders(UnifiedEvidence.ClusterEvidence(listings, model, nodes, verdicts, distances,
          imdb => finds.get(imdb).flatten, thisYear))
        val taken = today.film.map(id => s"tmdb:$id").orElse(today.fallback.filter(_.source == "imdb").map(f => s"imdb:${f.id}")).getOrElse("")
        val judged = contenders.map { k =>
          val verdict = listings.map(x => UnmatchedClusters.verdict(UnmatchedClusters.Take(code, x.venue, x.rawTitle, k.tmdb, k.imdb, "", k.title, k.familyIds), labels))
          k -> (verdict.count(_.contains(true)), verdict.count(_.contains(false)))
        }
        val handRight = judged.exists { case (_, (right, wrong)) => right > 0 && wrong == 0 }
        val todays    = judged.find(_._1.film == taken)
        // a model take a venue poster or a family names another film against is no weak label, nor one a hand label denies
        val weakOk = !fromFixture && taken.nonEmpty && todays.exists { case (k, (_, wrong)) =>
          wrong == 0 && !k.signals.contains("poster.otherMatches") && !k.signals.contains("family.dissent") }
        if (!fromFixture && taken.nonEmpty && !weakOk) count("weak-excluded")
        val rawTitle = listings.map(_.rawTitle).groupBy(identity).toSeq.sortBy(t => (-t._2.size, t._1)).head._1
        val cluster  = model.members.map(ListingKey.serialised).min
        val venue    = listings.map(_.venue).min
        val origin   = if (fromFixture) "fixture" else "corpus"
        judged.map { case (k, (right, wrong)) =>
          val (label, source) =
            if (right > 0 && wrong == 0) ("1", "hand")
            else if (wrong > 0 && right == 0) ("0", "hand")
            else if (right > 0) ("", "conflict")
            else if (handRight) ("0", "hand")
            else if (weakOk) (if (k.film == taken) "1" else "0", "weak")
            else ("", "")
          Row(code, cluster, origin, venue, listings.size, rawTitle, k.film, k.title, if (k.film == taken && taken.nonEmpty) "1" else "0", label, source,
            right, wrong, k.signals)
        } ++ Option.when(taken.nonEmpty && !contenders.exists(_.film == taken)) {
          count("today-not-a-contender")
          Row(code, cluster, origin, venue, listings.size, rawTitle, taken, "", "1", "", "", 0, 0, Map.empty)
        }
      }
    }

    /** Each venue poster's nearest distance to each TMDB film the cluster's evidence reaches, as the agreement stage reads
     *  them — from the answers filed, else the poster cache; none where no poster is held. */
    def posterDistances(listings: Seq[Listing], nodes: Seq[IdentityResolver.NodeEvidence], verdicts: Seq[services.identity.agreement.FamilyVerdict]):
        Seq[Map[Int, Option[Int]]] = {
      val urls = PosterEvidence.urls(listings)
      if (urls.isEmpty) Nil
      else {
        def answered[A](ask: => Answer[A], question: AgreementStage.PosterQuestion): Option[A] = ask.toOption.orElse {
          posters.foreach(_.file(posterStore, Seq(question)))
          ask.toOption
        }
        val shown = urls.flatMap(url => answered(posterStore.venue(url), AgreementStage.PosterQuestion.Venue(url)).flatten)
        if (shown.isEmpty) Nil
        else {
          val showing = listings.filter(x => PosterEvidence.shows(x) && x.poster.isDefined)
          val scored  = nodes.flatMap(_.candidates).filterNot(k => k.denied || FallbackIds.isFallback(k.tmdbId) ||
            showing.exists(PosterEvidence.editionsApart(_, k.film))).map(_.tmdbId)
          val named   = verdicts.flatMap(v => v.pick.map(_.record) ++ v.leaning).flatMap(r => r.crossIds.get("tmdb").flatMap(_.toIntOption)
            .orElse(r.crossIds.get("imdb").flatMap(finds.get).flatten))
          val films   = (scored ++ named).distinct.sorted
          val hashes  = films.map(film => film -> answered(posterStore.film(film), AgreementStage.PosterQuestion.Film(film)).getOrElse(Nil))
          shown.map(poster => hashes.map { case (film, held) => film -> PosterEvidence.nearest(Seq(poster), held) }.toMap)
        }
      }
    }

    val pool = java.util.concurrent.Executors.newFixedThreadPool(threads)
    val rows = try clusters.map { case ((listings, model, today, venues), fromFixture) =>
      pool.submit(new java.util.concurrent.Callable[Seq[Row]] {
        def call(): Seq[Row] = scala.util.Try(clusterRows(code, listings, model, today, venues, fromFixture)).recover { case e =>
          count("failed"); System.err.println(s"[$code] ${model.members.headOption.map(ListingKey.serialised).getOrElse("")}: $e " +
            e.getStackTrace.take(6).mkString(" < ")); Nil
        }.get
      })
    }.flatMap(_.get()) finally pool.shutdown()

    val labelled = rows.groupBy(_.source).view.mapValues(_.size).toMap
    println(f"[${c.label}] ${clusters.size} clusters (${fixture.size} from the unmatched fixture) → ${rows.size} contender rows " +
      s"$labelled; ${stats.asScala.toSeq.sortBy(_._1).map { case (k, v) => s"$k ${v.get}" }.mkString(", ")} " +
      f"in ${(System.nanoTime() - started) / 1e9}%.0fs")
    rows
  }
}
