package integration

import services.identity._
import tools.{HttpFetch, UnmatchedClusters}

import java.time.Clock
import scala.util.Try

/** The agreement's catalogue questions answered as the worker answers them — the ids mapped by Wikidata (a Letterboxd
 *  slug on no item by its own page), the venue pages' links read — through `fetch` (the experiment's cache, else live,
 *  kept: [[ExperimentCacheFetch]]), and filed among `families`' answers, where the unmatched-cluster fixtures record them.
 *  For the measures and the fixture tools only. */
final class CatalogueFill(families: FamilyAnswerStore, clock: Clock, fetch: HttpFetch) {
  val answers: CatalogueAnswerStore = new CatalogueAnswerStore(families, clock, UnmatchedClusters.CataloguePages)
  private val mapping = new WikidataCatalogueMapping(new services.enrichment.WikidataClient(fetch), new services.enrichment.LetterboxdClient(fetch))
  private val pages   = new CatalogueLinkReader(UnmatchedClusters.CataloguePages.map(_ -> fetch))

  /** Asks and files `questions`: ids a batch of one source at a time, pages four at a time. A question whose ask fails
   *  stays a gap, said so. */
  def file(questions: Iterable[CatalogueQuestion]): Unit = {
    val (ids, read) = questions.toSeq.partition(_.isInstanceOf[CatalogueQuestion.Id])
    ids.groupBy { case CatalogueQuestion.Id(id) => id.source; case _ => "" }.values.flatMap(_.grouped(AgreementCatalogueQuestions.Batch)).foreach(batch =>
      Try(AgreementCatalogueQuestions.file(answers, mapping, pages, batch)).failed.foreach(e => println(s"catalogue ids: $e")))
    read.grouped(4).foreach(_.map(page => java.util.concurrent.CompletableFuture.runAsync(() =>
      Try(AgreementCatalogueQuestions.file(answers, mapping, pages, Seq(page))).failed.foreach(e => println(s"catalogue page: $e")))).foreach(_.join()))
  }
}
