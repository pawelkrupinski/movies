package services.identityadapters

import services.identity.*

object AgreementCatalogueQuestions {
  /** How many ids of one source one task maps: one SPARQL query ([[AgreementQuestions.enqueue]]). */
  val Batch = 50

  /** Asks and files `questions` — those still wanted — mapping the ids in one batch and reading each page. */
  def file(store: CatalogueAnswerStore, mapping: CatalogueMapping, pages: CatalogueLinkReader, questions: Seq[CatalogueQuestion]): Int = {
    val open = questions.filter(store.wanted)
    val ids  = open.collect { case CatalogueQuestion.Id(id) => id }
    if (ids.nonEmpty) { val found = mapping.map(ids); ids.foreach(id => store.fileMapped(id, found.getOrElse(id, Nil))) }
    open.collect { case CatalogueQuestion.Page(url) => url }.foreach(url => store.filePage(url, pages.links(url)))
    open.size
  }
}
