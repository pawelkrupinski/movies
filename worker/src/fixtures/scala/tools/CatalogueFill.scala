package tools

import services.identity._

import java.time.Clock
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._
import scala.util.Try

/** The agreement's catalogue questions answered as the worker answers them — the ids mapped by Wikidata (a Letterboxd
 *  slug on no item by its own page), the venue pages' links read — through `fetch` (the experiment's cache, else live,
 *  kept: [[ExperimentCacheFetch]]), and filed among `families`' answers, where the unmatched-cluster fixtures record them.
 *  For the measures and the fixture tools only. */
@nowarn("msg=unused import")
final class CatalogueFill(families: CatalogueFill.Families, clock: Clock, fetch: HttpFetch) {
  // The identity adapters, in either package a ref keeps them (OlderRefsPackage).
  import services.identity.adapters.*

  val answers: CatalogueAnswerStore = new CatalogueAnswerStore(families, clock, UnmatchedClusters.CataloguePages)
  private val mapping = new WikidataCatalogueMapping(new services.enrichment.WikidataClient(fetch), new services.enrichment.LetterboxdClient(fetch))
  private val pages   = new CatalogueLinkReader(UnmatchedClusters.CataloguePages.map(_ -> fetch))

  /** Asks and files `questions`: ids a batch of one source at a time, pages four at a time. A question whose ask fails
   *  stays a gap, said so. */
  def file(questions: Iterable[CatalogueQuestion]): Unit = { val _ = failedOf(questions) }

  /** [[file]], returning the questions whose ask failed: each a failed ask, as the worker's memory records one
   *  (`QuestionMemory.failed`), for a replay that tells a failed question from one never asked ([[scripts.StageAnswers]]). */
  def failedOf(questions: Iterable[CatalogueQuestion]): Seq[CatalogueQuestion] = {
    val (ids, read) = questions.toSeq.partition(_.isInstanceOf[CatalogueQuestion.Id])
    val failed      = java.util.concurrent.ConcurrentLinkedQueue[CatalogueQuestion]()
    def fileBatch(batch: Seq[CatalogueQuestion], kind: String): Unit =
      Try(AgreementCatalogueQuestions.file(answers, mapping, pages, batch)).failed.foreach { e =>
        println(s"catalogue $kind: $e"); batch.foreach(failed.add)
      }
    ids.groupBy { case CatalogueQuestion.Id(id) => id.source; case _ => "" }.values.flatMap(_.grouped(AgreementCatalogueQuestions.Batch))
      .foreach(fileBatch(_, "ids"))
    tools.ConcurrentAsks.each(read, atOnce = 4)(page => fileBatch(Seq(page), "page"))
    failed.asScala.toSeq
  }
}

@nowarn("msg=unused import")
object CatalogueFill {
  // The identity adapters, in either package a ref keeps them (OlderRefsPackage).
  import services.identity.adapters.*

  type Families = FamilyAnswerStore
}
