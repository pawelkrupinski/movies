package services.identity

import org.bson.{BsonDocument, BsonInt32, BsonString}
import org.mongodb.scala.bson.BsonArray
import services.enrichment.{LetterboxdClient, WikidataClient}
import services.tasks.HandlerOutcome.{Done, Skipped}
import services.tasks.{EnqueueResult, HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}
import tools.{HttpFetch, HttpRead, HttpStatusException}

import java.time.Clock
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The catalogue mappings the agreement's catalogue take reads ([[agreement.Catalogue]]): what each catalogue id maps to,
 * and the catalogue ids a venue's film page links — filed once asked and read from here; one not filed yet is a gap
 * ([[Answer.Unknown]]) the queue asks, never "no film". Filed among the families' answers (`answers`:
 * `identity_family_answers`, kept long and counted in its `version`, recorded in the unmatched-cluster fixtures with
 * them), under `catalogue|<source>|<id>` and `catalogue-page|<url>`. A page is read only where a venue client declares
 * its pages link a catalogue (`pages`); any other page links none.
 */
final class CatalogueAnswerStore(answers: FamilyAnswerStore, clock: Clock, pages: Seq[CatalogueLinkPages]) extends CatalogueAnswers {
  import CatalogueAnswerStore._

  def linked(page: String): Answer[Seq[CatalogueId]] =
    if (CatalogueLinks.pagesOf(page, pages).isEmpty) Answer.Known(Nil)
    else answers.document(pageId(page)).fold[Answer[Seq[CatalogueId]]](Answer.Unknown)(d => Answer.Known(linksOf(d)))

  def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] =
    answers.document(idOf(id)).fold[Answer[Seq[CatalogueHit]]](Answer.Unknown)(d => Answer.Known(hitsOf(d)))

  def fileMapped(id: CatalogueId, hits: Seq[CatalogueHit]): Unit = answers.put(idOf(id), new BsonDocument("hits", BsonArray.fromIterable(hits.map { hit =>
    val d = new BsonDocument("via", BsonString(hit.via))
    hit.item.foreach(item => d.append("item", BsonString(item)))
    hit.tmdb.foreach(tmdb => d.append("tmdb", BsonInt32(tmdb)))
    hit.imdb.foreach(imdb => d.append("imdb", BsonString(imdb)))
    d
  })))

  def filePage(page: String, links: Seq[CatalogueId]): Unit = answers.put(pageId(page), new BsonDocument("links", BsonArray.fromIterable(links.map(id =>
    new BsonDocument("source", BsonString(id.source)).append("id", BsonString(id.id))))))

  /** Is the question's answer missing, or older than its kind keeps it: a mapping found for [[MappedAge]], one Wikidata
   *  states on no item yet for [[UnmappedAge]] (it gains items), a page's links for [[PageAge]]? */
  def wanted(question: CatalogueQuestion): Boolean = {
    val (id, age) = question match {
      case CatalogueQuestion.Page(url) => (pageId(url), PageAge)
      case CatalogueQuestion.Id(of)    => (idOf(of), if (answers.document(idOf(of)).exists(d => hitsOf(d).nonEmpty)) MappedAge else UnmappedAge)
    }
    answers.document(id).forall { d =>
      Option(d.get(TmdbStore.FetchedAt)).filter(_.isInt64).forall(at => clock.millis() - at.asInt64.getValue > age.toMillis)
    }
  }
}

object CatalogueAnswerStore {
  val MappedAge: FiniteDuration   = 365.days
  val UnmappedAge: FiniteDuration = 30.days
  val PageAge: FiniteDuration     = 90.days

  def idOf(id: CatalogueId): String = s"catalogue|${id.source}|${id.id}"
  def pageId(page: String): String  = s"catalogue-page|$page"

  private def hitsOf(d: BsonDocument): Seq[CatalogueHit] =
    Option(d.get("hits")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asDocument)).map { h =>
      CatalogueHit(Option(h.get("item")).map(_.asString.getValue), Option(h.get("tmdb")).map(_.asInt32.getValue),
        Option(h.get("imdb")).map(_.asString.getValue), h.getString("via").getValue)
    }
  private def linksOf(d: BsonDocument): Seq[CatalogueId] =
    Option(d.get("links")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asDocument))
      .map(l => CatalogueId(l.getString("source").getValue, l.getString("id").getValue))
}

/** Maps catalogue ids to the films they name — batched: one call for many ids of one source. An id absent from the
 *  result maps to nothing; a failed read throws, to be asked again. */
trait CatalogueMapping {
  def map(ids: Seq[CatalogueId]): Map[CatalogueId, Seq[CatalogueHit]]
}

/** Each catalogue Wikidata states ([[CatalogueSources.ByWikidata]]) mapped by one SPARQL query a batch, the items' TMDB
 *  and IMDb ids with them; a Letterboxd slug no item states, by its own film page's TMDB and IMDb ids. */
final class WikidataCatalogueMapping(wikidata: WikidataClient, letterboxd: LetterboxdClient) extends CatalogueMapping {
  def map(ids: Seq[CatalogueId]): Map[CatalogueId, Seq[CatalogueHit]] =
    ids.distinct.groupBy(_.source).toSeq.sortBy(_._1).flatMap { case (source, of) =>
      CatalogueSources.ByWikidata.get(source).toSeq.flatMap { mapped =>
        val stated = wikidata.itemsStating(mapped.properties, of.map(id => mapped.spelled(id.id)))
        of.flatMap { id =>
          val items = stated.getOrElse(mapped.spelled(id.id), Nil)
          if (items.nonEmpty) Some(id -> items.map(i => CatalogueHit(Some(i.item), i.tmdbId, i.imdbId, s"Wikidata ${i.property}")))
          else if (source == CatalogueSources.Letterboxd.source)
            letterboxd.bySlug(id.id).filter(ids => ids.tmdbId.isDefined || ids.imdbId.isDefined)
              .map(found => id -> Seq(CatalogueHit(None, found.tmdbId, found.imdbId, "Letterboxd")))
          else None
        }
      }
    }.toMap
}

/** Reads a venue film page's catalogue links through the fetch its client scrapes with — `readers`, one per
 *  declaration; a page gone for good (404/410) links none, any other failure throws. */
final class CatalogueLinkReader(readers: Seq[(CatalogueLinkPages, HttpFetch)]) {
  val pages: Seq[CatalogueLinkPages] = readers.map(_._1)
  def links(page: String): Seq[CatalogueId] =
    CatalogueLinks.pagesOf(page, pages).flatMap(declared => readers.find(_._1 == declared)).fold(Seq.empty[CatalogueId]) { case (declared, fetch) =>
      try CatalogueLinks.of(HttpRead.page(fetch, declared.readAt(page)), declared.sources)
      catch { case e: HttpStatusException if HttpStatusException.isDurable(e.code) => Nil }
    }
}

object AgreementCatalogueQuestions {
  private val Source = "source"
  private val Ids    = "ids"
  private val Page   = "page"
  /** How many ids of one source one task maps: one SPARQL query. */
  val Batch = 50

  /** The catalogue questions as queue tasks — ids a batch per source, a page each — claimed after every other task
   *  ([[AgreementQuestions.Behind]]). */
  def enqueue(queue: TaskQueue, questions: Set[CatalogueQuestion], clock: Clock, metrics: AgreementQuestionMetrics): Unit = {
    val ids   = questions.toSeq.collect { case CatalogueQuestion.Id(id) => id }
    val pages = questions.toSeq.collect { case CatalogueQuestion.Page(url) => url }.sorted
    ids.groupBy(_.source).toSeq.sortBy(_._1).foreach { case (source, of) =>
      of.map(_.id).sorted.grouped(Batch).foreach { batch =>
        metrics.enqueued(AgreementQuestionMetrics.Catalogue, queue.enqueue(TaskType.AgreementCatalogue, s"agreement-catalogue|$source|${batch.mkString(",")}",
          Map(Source -> source, Ids -> batch.mkString(",")), submittedAt = clock.instant(), claimAhead = -AgreementQuestions.Behind) == EnqueueResult.Added)
      }
    }
    pages.foreach(url => metrics.enqueued(AgreementQuestionMetrics.Catalogue, queue.enqueue(TaskType.AgreementCatalogue,
      s"agreement-catalogue-page|$url", Map(Page -> url), submittedAt = clock.instant(), claimAhead = -AgreementQuestions.Behind) == EnqueueResult.Added))
  }

  /** The questions a task asks. */
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

/** One catalogue task: its ids mapped or its page read, and filed — skipped when the store holds fresh answers — and,
 *  once filed, a projection asked for (`filed`), which reads them. */
final class AgreementCatalogueHandler(store: CatalogueAnswerStore, mapping: CatalogueMapping, pages: CatalogueLinkReader, filed: () => Unit, clock: Clock,
                                      metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler {
  val taskType: TaskType = TaskType.AgreementCatalogue

  def handle(task: Task): HandlerOutcome = {
    val questions = AgreementCatalogueQuestions.of(task)
    if (questions.isEmpty) Skipped
    else if (!questions.exists(store.wanted)) { metrics.asked(AgreementQuestionMetrics.Catalogue, AgreementQuestionMetrics.Fresh); Skipped }
    else {
      val outcome = try { AgreementCatalogueQuestions.file(store, mapping, pages, questions); filed(); Done }
      catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"catalogue ${task.payload.values.mkString(" ")}", clock) }
      metrics.asked(AgreementQuestionMetrics.Catalogue, outcome match {
        case Done                       => AgreementQuestionMetrics.Answered
        case _: HandlerOutcome.Deferred => AgreementQuestionMetrics.Deferred
        case _                          => AgreementQuestionMetrics.Failed
      })
      outcome
    }
  }
}
