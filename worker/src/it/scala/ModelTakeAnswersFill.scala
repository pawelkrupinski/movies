package integration

import services.identity._
import services.identity.agreement.{Agreement, AgreementStage, FamilyAnswers, FamilyVerdict, SourceHit, SourceRecord, VoterFamily}
import tools.{ConvergenceStorage, UnmatchedClusters}

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Answers what the families and the posters would say of the clusters the MODEL matched — the evidence that can
 * contradict a model take, which production never asks for (the agreement stage questions unmatched clusters only), so
 * `integration.IdentityUnifiedDataset` can sign every model take's contenders with it:
 *
 *   KINOWO_IDENTITY_FULL=pl,uk,de,es,us KINOWO_IDENTITY_CORPUS_DIR=<dir> KINOWO_FIXTURE_ROOT=<dir>
 *   KINOWO_IDENTITY_FAMILY_SEED=<prod answers dir> KINOWO_IDENTITY_POSTER_CACHE=<dir> KINOWO_IDENTITY_AGREEMENT_CACHE=<dir>
 *   KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=<key> MONGODB_URI=<throwaway> MONGODB_DB=<unique>
 *   sbt "worker/IntegrationTest/runMain integration.ModelTakeAnswersFill --out <seed dir> [--rounds 8] [--threads 6]"
 *
 * Every family's question is asked as the fill asks it (`ExperimentCacheFetch`: the experiment's cache, else live, four
 * at a time per host); every venue poster and every candidate's TMDB posters as `CachedPosters` keeps them. Writes
 * `<out>/<kinowo|kinowo_cc>.jsonl` (prod's answers with the new ones, the seed format) and `<out>/finds-<cc>.tsv` (the
 * IMDb ids a family took, found on TMDB) — `<out>` is then the dataset's `KINOWO_IDENTITY_FAMILY_SEED`. A re-run reads
 * what an earlier one filed and asks only what is still open.
 */
object ModelTakeAnswersFill {

  def main(args: Array[String]): Unit = {
    val opts    = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val out     = Paths.get(opts.getOrElse("out", sys.error("--out <dir>")))
    val rounds  = opts.get("rounds").map(_.toInt).getOrElse(8)
    val threads = opts.get("threads").map(_.toInt).getOrElse(6)
    val askPosters  = opts.get("posters").forall(_ != "false")
    val askFamilies = opts.get("families").forall(_ != "false")
    // the clusters to ask families of (each by its least serialised member key, as the dataset names it), else all
    val only = opts.get("only").map(f => Files.readAllLines(Paths.get(f)).asScala.filter(_.nonEmpty).toSet)
    val sample = opts.get("sample").map(_.toInt)
    val configuration = settings.ProcessConfiguration.resolve()
    val cache    = configuration.identityAgreementCache.getOrElse(sys.error("KINOWO_IDENTITY_AGREEMENT_CACHE")).value
    val storages = mutable.ListBuffer.empty[ConvergenceStorage]
    Files.createDirectories(out)
    try new DecorationCorpora(configuration, storages).loaded.foreach(l => country(l, configuration, cache, out, rounds, threads, askPosters, askFamilies, only, sample))
    finally storages.foreach(s => Try(s.close()))
  }

  private def seedName(code: String) = if (code == "pl") "kinowo.jsonl" else s"kinowo_$code.jsonl"

  private def readSeed(file: Path): Seq[(String, org.bson.BsonDocument)] =
    if (!Files.exists(file)) Nil
    else Files.readAllLines(file).asScala.toSeq.filter(_.nonEmpty).map { line =>
      val d = org.bson.BsonDocument.parse(line)
      d.remove("_id").asString.getValue -> d
    }

  /** A family's answers that note every question still open — what a round asks. */
  private final class Recording(inner: FamilyAnswers, open: java.util.Set[(VoterFamily, String)]) extends FamilyAnswers {
    def family: VoterFamily = inner.family
    private def note[A](question: String, a: Answer[A]): Answer[A] = { if (a == Answer.Unknown) open.add(family -> question); a }
    def titled(text: String): Answer[Seq[SourceHit]]     = note(s"title|$text", inner.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = note(s"director|$name", inner.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = note(s"record|$id", inner.record(id))
    override def fresh(question: String): Boolean = inner.fresh(question)
  }

  private def country(l: DecorationCorpora.Loaded, configuration: settings.ProcessConfiguration, cache: Path, out: Path, rounds: Int,
                      threads: Int, askPosters: Boolean, askFamilies: Boolean, only: Option[Set[String]], sample: Option[Int]): Unit = {
    val c       = l.c
    val code    = c.country.code
    val started = System.nanoTime()
    val resolution = IdentityResolver.resolve(l.listings, l.lookups, c.normalizer, IdentityCalibration.resolver)
    val byKey   = l.listings.map(x => x.key -> x).toMap
    val clusters = resolution.decisions.filter(d => d.film.isDefined && d.basis != ResolverDecision.Basis.Pinned)
      .map(_.members.flatMap(byKey.get)).filter(_.nonEmpty)
    def name(listings: Seq[Listing]) = listings.map(x => services.movies.ListingKey.serialised(x.key)).min
    // the clusters families are asked of: those named, and a fixed random sample of the rest
    val sampled = sample.fold(Set.empty[String])(n => new scala.util.Random(code.hashCode).shuffle(clusters.map(name)).take(n).toSet)
    val asking  = if (!askFamilies) Nil else if (only.isEmpty && sample.isEmpty) clusters
      else clusters.filter(ls => only.exists(_(code + "\t" + name(ls))) || sampled(name(ls)))
    Files.writeString(out.resolve(s"asked-$code.tsv"), asking.map(ls => s"$code\t${name(ls)}").sorted.mkString("", "\n", "\n"))

    val docs = new InMemoryTmdbDocuments
    configuration.identityFamilySeed.map(_.value.resolve(seedName(code))).foreach(f => docs.put(TmdbKind.Family, readSeed(f)))
    Some(UnmatchedClusters.fixturePath(c.country)).filter(Files.exists(_)).map(UnmatchedClusters.read).foreach(cap => docs.put(TmdbKind.Family, cap.families.toSeq))
    docs.put(TmdbKind.Family, readSeed(out.resolve(seedName(code))))
    val store    = new FamilyAnswerStore(docs, tools.SpecClock.Pinned)
    val families = UnmatchedClusters.familiesOf(c.country)
    val fetch    = new ExperimentCacheFetch(cache)
    val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(fetch)),
      new WikiFamily(new services.enrichment.WikidataClient(fetch), c.country.language.getLanguage),
      new FilmwebFamily(new services.enrichment.FilmwebClient(fetch)), new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(fetch)),
      new MetacriticFamily(new services.enrichment.MetacriticClient(fetch))).filter(s => families.contains(s.family)).map(s => s.family -> s).toMap

    def save(): Unit = {
      val filed = docs.get(TmdbKind.Family, docs.fetchedBefore(TmdbKind.Family, Long.MaxValue).map(_._1)).toSeq.sortBy(_._1)
      Files.writeString(out.resolve(seedName(code)), filed.map { case (id, d) =>
        val copy = d.clone(); copy.put("_id", new org.bson.BsonString(id)); copy.toJson }.mkString("", "\n", "\n"))
    }

    val pool = java.util.concurrent.Executors.newFixedThreadPool(threads)
    def verdictsOf(open: java.util.Set[(VoterFamily, String)]): Seq[Seq[FamilyVerdict]] = asking.map { listings =>
      pool.submit(new java.util.concurrent.Callable[Seq[FamilyVerdict]] {
        def call(): Seq[FamilyVerdict] = families.flatMap { family =>
          Try(Agreement.verdict(listings, new Recording(store.answers(family), open), l.lookups, c.normalizer,
            IdentityCalibration.resolver.withPriorSpread(family.priorSpread)).toOption).toOption.flatten
        }
      })
    }.map(_.get())

    var round    = 0
    var asked    = 0
    var open     = java.util.concurrent.ConcurrentHashMap.newKeySet[(VoterFamily, String)]()
    var verdicts = try verdictsOf(open) catch { case e: Throwable => pool.shutdown(); throw e }
    try {
      while (!open.isEmpty && round < rounds) {
        round += 1
        val roundStart = System.nanoTime()
        val questions = open.asScala.toSeq
        println(s"[$code] round $round: ${questions.size} open question(s) ${questions.groupBy(_._1.label).view.mapValues(_.size).toMap}")
        questions.groupBy(_._1).toSeq.map { case (family, asks) =>
          java.util.concurrent.CompletableFuture.runAsync { () =>
            asks.map(_._2).grouped(4).foreach(_.map(question => java.util.concurrent.CompletableFuture.runAsync(() =>
              { Try(AgreementQuestions.file(store, family, sources(family), question)); () })).foreach(_.join()))
          }
        }.foreach(_.join())
        asked += questions.size
        val seconds = (System.nanoTime() - roundStart) / 1e9
        println(f"[$code] round $round filed ${questions.size} in $seconds%.0fs (${questions.size / math.max(seconds, 0.001)}%.1f/s)")
        save()
        open = java.util.concurrent.ConcurrentHashMap.newKeySet[(VoterFamily, String)]()
        verdicts = verdictsOf(open)
      }
    } finally pool.shutdown()
    save()
    println(s"[$code] families: ${asking.size} of ${clusters.size} model takes, $asked question(s) asked in $round round(s), ${open.size} still open")

    // the IMDb ids a family took, on TMDB — what the dataset maps a pick by
    val tmdb = configuration.identityLiveGaps.map(key => new clients.TmdbClient(new tools.RealHttpFetch(), apiKey = Some(settings.TmdbApiKey(key.tmdbKey)),
      language = c.country.language, retrySleep = (_: Long) => ()))
    val findsFile = out.resolve(s"finds-$code.tsv")
    val finds = new java.util.concurrent.ConcurrentHashMap[String, String]()
    if (Files.exists(findsFile)) Files.readAllLines(findsFile).asScala.map(_.split('\t')).foreach(a => finds.put(a(0), a.lift(1).getOrElse("")))
    val imdbs = verdicts.flatten.flatMap(v => v.pick.map(_.record) ++ v.leaning).flatMap(_.crossIds.get("imdb")).distinct.filterNot(finds.containsKey)
    imdbs.grouped(8).foreach(_.map(imdb => java.util.concurrent.CompletableFuture.runAsync(() =>
      tmdb.flatMap(client => Try(client.findByImdbId(imdb).map(_.id)).toOption).foreach(found => finds.put(imdb, found.fold("")(_.toString)))))
      .foreach(_.join()))
    Files.writeString(findsFile, finds.asScala.toSeq.sorted.map { case (k, v) => s"$k\t$v" }.mkString("", "\n", "\n"))

    // every venue poster and every film a poster is compared with, as the dataset compares them
    if (askPosters) {
      val nodeOf = IdentityResolver.evidenceOf(l.listings, l.lookups, c.normalizer)(_ => true).flatMap(n => n.keys.map(_ -> n)).toMap
      val verdictOf = asking.zip(verdicts).toMap
      val questions = clusters.map(ls => ls -> verdictOf.getOrElse(ls, Nil)).flatMap { case (listings, vs) =>
        val urls = PosterEvidence.urls(listings)
        if (urls.isEmpty) Nil
        else {
          val showing = listings.filter(x => PosterEvidence.shows(x) && x.poster.isDefined)
          val scored  = listings.flatMap(x => nodeOf.get(x.key)).distinctBy(_.keys).flatMap(_.candidates)
            .filterNot(k => k.denied || FallbackIds.isFallback(k.tmdbId) || showing.exists(PosterEvidence.editionsApart(_, k.film))).map(_.tmdbId)
          val named   = vs.flatMap(v => v.pick.map(_.record) ++ v.leaning).flatMap(r => r.crossIds.get("tmdb").flatMap(_.toIntOption)
            .orElse(r.crossIds.get("imdb").flatMap(i => Option(finds.get(i))).flatMap(_.toIntOption)))
          // a poster names another film against the take only where the evidence reaches two films or more
          val films = (scored ++ named).distinct
          if (films.size < 2) Nil
          else urls.map(AgreementStage.PosterQuestion.Venue(_)) ++ films.map(AgreementStage.PosterQuestion.Film(_))
        }
      }.distinct
      val posterStart = System.nanoTime()
      CachedPosters.of(configuration, c.country).file(new PosterAnswerStore(store, tools.SpecClock.Pinned), questions)
      val seconds = (System.nanoTime() - posterStart) / 1e9
      save()
      println(f"[$code] posters: ${questions.size} question(s) in $seconds%.0fs (${questions.size / math.max(seconds, 0.001)}%.1f/s)")
    }
    println(f"[$code] done in ${(System.nanoTime() - started) / 1e9}%.0fs")
  }
}
