package integration

import services.identity._
import services.identity.agreement.{AgreementStage, InMemoryAgreementVerdicts}
import tools.{ConvergenceStorage, SpecClock, UnmatchedClusters}

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * MEASUREMENT: what reading the model's takes against the evidence that can correct them (`agreement.Correction`) costs a
 * projection's agreement phase, over the whole recorded corpora — the stage applied to the model's resolution with its
 * takes read (as the worker runs it) and with them passed by (every model take handed over as a pin: the stage as it
 * was): CPU, bytes allocated and wall time of a cold apply (a fresh stage, as at boot) and of a warm one (the same stage,
 * one answer filed elsewhere), medians of 5 after a warm-up, and the heap the stages keep after an apply (used heap after
 * a full GC). The answers are those a whole-corpus dump filed (`answers-<cc>.jsonl`, `IdentityResolveDumpIntegrationSpec`).
 *
 *   KINOWO_IDENTITY_FULL=pl,uk,de,es,us KINOWO_IDENTITY_CORPUS_DIR=<dir> KINOWO_FIXTURE_ROOT=<dir> KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=<key>
 *   MONGODB_URI=<throwaway> MONGODB_DB=<unique>
 *   sbt "worker/IntegrationTest/runMain integration.ModelTakeCorrectionCost --answers <dump dir>"
 */
object ModelTakeCorrectionCost {

  def main(args: Array[String]): Unit = {
    val opts     = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val answers  = Paths.get(opts.getOrElse("answers", sys.error("--answers <dump dir>")))
    val configuration = settings.ProcessConfiguration.resolve()
    val storages = mutable.ListBuffer.empty[ConvergenceStorage]
    try {
      val loaded = new DecorationCorpora(configuration, storages).loaded
      val inputs = loaded.map { l =>
        val code = l.c.country.code
        val docs = new InMemoryTmdbDocuments
        val file = answers.resolve(s"answers-$code.jsonl")
        if (Files.exists(file)) docs.put(TmdbKind.Family, Files.readAllLines(file).asScala.toSeq.filter(_.nonEmpty).map { line =>
          val d = org.bson.BsonDocument.parse(line); d.remove("_id").asString.getValue -> d })
        val store = new FamilyAnswerStore(docs, SpecClock.Pinned)
        val resolution = IdentityResolver.resolve(l.listings, l.lookups, l.c.normalizer, IdentityCalibration.resolver)
        // the stage as it was: every model take passed by, as a pin is
        val pinned = resolution.copy(decisions = resolution.decisions.map(d =>
          if (d.film.isDefined && (d.basis == ResolverDecision.Basis.OwnMatch || d.basis == ResolverDecision.Basis.PooledMatch))
            d.copy(basis = ResolverDecision.Basis.Pinned)(d.trace) else d))
        val finds = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]]()
        val tmdb  = configuration.identityLiveGaps.map(key => new clients.TmdbClient(new tools.RealHttpFetch(), apiKey = Some(settings.TmdbApiKey(key.tmdbKey)),
          language = l.c.country.language, retrySleep = (_: Long) => ()))
        val tmdbOf: String => Answer[Option[Int]] = imdb => Answer.Known(finds.computeIfAbsent(imdb, id =>
          tmdb.flatMap(client => scala.util.Try(client.findByImdbId(id).map(_.id)).toOption.flatten)))
        val byKey = l.listings.map(x => x.key -> x).toMap
        println(s"[$code] ${resolution.decisions.count(_.film.isDefined)} takes, ${docs.size(TmdbKind.Family)} answers")
        (l, store, resolution, pinned, tmdbOf, byKey)
      }
      def stageOf(i: Int) = {
        val (l, store, _, _, tmdbOf, _) = inputs(i)
        new AgreementStage(UnmatchedClusters.familiesOf(l.c.country).map(f => f -> store.answers(f)).toMap, l.lookups, l.c.normalizer,
          IdentityCalibration.resolver, tmdbOf, new InMemoryAgreementVerdicts, clock = SpecClock.Pinned, changes = store,
          posters = new PosterAnswerStore(store, SpecClock.Pinned), tmdb = Some(l.lookups),
          identities = modules.wiring.IdentityCutoverWiring.identities(l.c.country.code),
          catalogue = new CatalogueAnswerStore(store, SpecClock.Pinned, UnmatchedClusters.CataloguePages),
          listedOn = modules.wiring.IdentityCutoverWiring.listedOn(l.c.country.code))
      }
      def resolutionOf(i: Int, correcting: Boolean) = if (correcting) inputs(i)._3 else inputs(i)._4
      /** Fresh stages, one apply each at the store's version. */
      def cold(correcting: Boolean): Seq[AgreementStage] = inputs.indices.map { i =>
        val stage = stageOf(i); stage.apply(resolutionOf(i, correcting), inputs(i)._6.get, inputs(i)._2.version); stage }
      val os  = java.lang.management.ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
      val rt  = Runtime.getRuntime
      def used(): Long = { (1 to 3).foreach(_ => System.gc()); rt.totalMemory - rt.freeMemory }
      def measured(run: => Unit): (Long, Long, Long) = {
        System.gc()
        var allocated = 0L
        val (c0, w0) = (os.getProcessCpuTime, System.nanoTime())
        tools.ThreadAllocation.measure(allocated = _)(run)   // the stages apply on this thread
        (os.getProcessCpuTime - c0, allocated, System.nanoTime() - w0)
      }
      def median(xs: Seq[Long]) = xs.sorted.apply(xs.size / 2)
      (1 to 2).foreach(_ => { cold(true); cold(false) })   // warm up both, the finds asked
      Seq(false, true).foreach { correcting =>
        val colds = (1 to 5).map(_ => measured(cold(correcting)))
        val stages = cold(correcting)
        // a warm apply: the same decisions, one answer filed elsewhere since
        val warms = (1 to 5).map { n => measured(inputs.indices.foreach { i =>
          inputs(i)._2.noteFiled(s"cost|$n"); stages(i).apply(resolutionOf(i, correcting), inputs(i)._6.get, inputs(i)._2.version) }) }
        val before = used()
        val held   = cold(correcting)
        val after  = used()
        java.lang.ref.Reference.reachabilityFence(held)
        java.lang.ref.Reference.reachabilityFence(stages)
        val taken = held.zipWithIndex.map { case (stage, i) => stage.apply(resolutionOf(i, correcting), inputs(i)._6.get, inputs(i)._2.version) }
        def of(basis: ResolverDecision.Basis) = taken.map(_.decisions.count(_.basis == basis)).sum
        println(f"cost\t${if (correcting) "correcting" else "as it was"}\tcold cpu ${median(colds.map(_._1)) / 1e6}%.0f ms alloc ${median(colds.map(_._2)) / 1e6}%.1f MB" +
          f" wall ${median(colds.map(_._3)) / 1e6}%.0f ms\twarm cpu ${median(warms.map(_._1)) / 1e6}%.0f ms alloc ${median(warms.map(_._2)) / 1e6}%.1f MB" +
          f" wall ${median(warms.map(_._3)) / 1e6}%.0f ms\theld by the stages ${(after - before) / 1e6}%.2f MB" +
          s"\twithdrawn ${of(ResolverDecision.Basis.Withdrawn)} corrected ${of(ResolverDecision.Basis.Corrected)}")
      }
    } finally storages.foreach(s => scala.util.Try(s.close()))
  }
}
