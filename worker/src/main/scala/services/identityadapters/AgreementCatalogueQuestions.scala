package services.identityadapters

import services.identity.*
import services.tasks.Task

object AgreementCatalogueQuestions {
  private val Source = "source"
  private val Ids    = "ids"
  private val Page   = "page"
  /** How many ids of one source one task maps: one SPARQL query ([[AgreementQuestions.enqueue]]). */
  val Batch = 50

  /** The questions an [[TaskType.AgreementCatalogue]] task asks. Old queued tasks only; remove after 2026-11-07. */
  def of(task: Task): Seq[CatalogueQuestion] =
    task.payload.get(Page).map(url => Seq(CatalogueQuestion.Page(url))).getOrElse(
      (for { source <- task.payload.get(Source).toSeq; ids <- task.payload.get(Ids).toSeq; id <- ids.split(",").toSeq if id.nonEmpty }
        yield CatalogueQuestion.Id(CatalogueId(source, id))))

  /** Asks and files `questions` — those still wanted — mapping the ids in one batch and reading each page. */
  def file(store: CatalogueAnswerStore, mapping: CatalogueMapping, pages: CatalogueLinkReader, questions: Seq[CatalogueQuestion]): Int = {
    val open = questions.filter(store.wanted)
    val ids  = open.collect { case CatalogueQuestion.Id(id) => id }
    if (ids.nonEmpty) { val found = mapping.map(ids); ids.foreach(id => store.fileMapped(id, found.getOrElse(id, Nil))) }
    open.collect { case CatalogueQuestion.Page(url) => url }.foreach(url => store.filePage(url, pages.links(url)))
    open.size
  }
}
