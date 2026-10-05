package scripts

import models.Country
import services.identity._
import services.identity.agreement.{AgreementStage, InMemoryAgreementVerdicts}
import services.movies.TitleNormalizer
import tools.{SpecClock, UnmatchedClusters}

import java.nio.file.Files

/**
 * MEASUREMENT: what the catalogue take costs a projection — the agreement stage applied over the unmatched-cluster
 * fixture (every cluster the model leaves unmatched on the five recorded full corpora), with the catalogue answers and
 * without them (`CatalogueAnswers.Silent`, the stage as it was): CPU, bytes allocated and wall time per apply (median of
 * 9 after a warm-up), and the heap the stage keeps after an apply (used heap after a full GC, the answers' store held
 * alike in both). The catalogue answers' share of the stored answers is printed beside.
 *
 *   sbt "worker/Test/runMain scripts.CatalogueTakeCost"
 */
object CatalogueTakeCost {

  private val captures = Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)
  private val stores   = captures.map { capture =>
    val docs = new InMemoryTmdbDocuments
    docs.put(TmdbKind.Family, capture.families.toSeq)
    capture -> new FamilyAnswerStore(docs, SpecClock.Pinned)
  }

  /** One fresh stage per country, applied once — a projection's agreement phase from a cold verdict store — kept. */
  private def applyAll(catalogue: Boolean): Seq[AnyRef] = stores.map { case (capture, store) =>
    val lookups = new UnmatchedClusters.Replay(capture)
    val stage = new AgreementStage(UnmatchedClusters.familiesOf(capture.country).map(f => f -> store.answers(f)).toMap, lookups,
      TitleNormalizer.forCountry(capture.country), IdentityCalibration.resolver,
      imdb => capture.finds.get(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(Answer.Known(_)), new InMemoryAgreementVerdicts, clock = SpecClock.Pinned,
      posters = new PosterAnswerStore(store, SpecClock.Pinned), tmdb = Some(lookups),
      identities = modules.wiring.IdentityCutoverWiring.identities(capture.country.code),
      catalogue = if (catalogue) new CatalogueAnswerStore(store, SpecClock.Pinned, UnmatchedClusters.CataloguePages) else CatalogueAnswers.Silent)
    stage.apply(UnmatchedClusters.resolutionOf(capture.decisions), capture.listings.map(l => l.key -> l).toMap.get, 1)
    stage
  }

  def main(args: Array[String]): Unit = {
    val catalogueDocs = captures.flatMap(_.families.toSeq).filter(_._1.startsWith("catalogue"))
    println(s"catalogue answers stored: ${catalogueDocs.size} documents, ${catalogueDocs.map(_._2.toJson.length.toLong).sum} bytes of JSON " +
      s"(of ${captures.map(_.families.size).sum} answers)")
    val os  = java.lang.management.ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
    val thr = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val rt  = Runtime.getRuntime
    def used(): Long = { (1 to 3).foreach(_ => System.gc()); rt.totalMemory - rt.freeMemory }
    (1 to 4).foreach(i => applyAll(i % 2 == 0))   // warm up both
    Seq(false, true, false, true).foreach { catalogue =>
      val samples = (1 to 9).map { _ =>
        System.gc()
        val (c0, a0, w0) = (os.getProcessCpuTime, thr.getTotalThreadAllocatedBytes, System.nanoTime())
        applyAll(catalogue)
        (os.getProcessCpuTime - c0, thr.getTotalThreadAllocatedBytes - a0, System.nanoTime() - w0)
      }
      val before = used()
      val held   = applyAll(catalogue)
      val after  = used()
      java.lang.ref.Reference.reachabilityFence(held)
      def median(xs: Seq[Long]) = xs.sorted.apply(xs.size / 2)
      println(f"cost\t${if (catalogue) "catalogue" else "without "}\tcpu ${median(samples.map(_._1)) / 1e6}%.0f ms\talloc ${median(samples.map(_._2)) / 1e6}%.1f MB" +
        f"\twall ${median(samples.map(_._3)) / 1e6}%.0f ms\theld by the stages ${(after - before) / 1e6}%.2f MB")
    }
  }
}
