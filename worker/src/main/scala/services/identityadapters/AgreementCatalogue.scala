package services.identityadapters

import services.tmdb.{FamilyAnswerStore, QuestionPolicies, TmdbStore}
import java.time.Clock

import org.bson.{BsonDocument, BsonInt32, BsonString}
import org.mongodb.scala.bson.BsonArray
import services.enrichment.{LetterboxdClient, WikidataClient}
import services.identity.*
import services.identity.vocabulary.toAnswer
import services.identity.vocabulary.{Answer, BsonFields, CatalogueAnswers, CatalogueHit, CatalogueId, CatalogueLinkPages, CatalogueLinks, CatalogueQuestion, CatalogueSources}

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
    else answers.document(pageId(page)).map(linksOf).toAnswer

  def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] =
    answers.document(idOf(id)).map(hitsOf).toAnswer

  def fileMapped(id: CatalogueId, hits: Seq[CatalogueHit]): Unit = answers.put(idOf(id), new BsonDocument("hits", BsonArray.fromIterable(hits.map { hit =>
    val d = new BsonDocument("via", BsonString(hit.via))
    hit.item.foreach(item => d.append("item", BsonString(item)))
    hit.tmdb.foreach(tmdb => d.append("tmdb", BsonInt32(tmdb)))
    hit.imdb.foreach(imdb => d.append("imdb", BsonString(imdb)))
    d
  })))

  def filePage(page: String, links: Seq[CatalogueId]): Unit = answers.put(pageId(page), new BsonDocument("links", BsonArray.fromIterable(links.map(id =>
    new BsonDocument("source", BsonString(id.source)).append("id", BsonString(id.id))))))

  /** Is the question's answer missing, or older than its kind keeps it ([[QuestionPolicies]]): a mapping found for
   *  `CatalogueMappedAge`, one Wikidata states on no item yet for `CatalogueUnmappedAge` (it gains items), a page's links
   *  for `CataloguePageAge`? */
  def wanted(question: CatalogueQuestion): Boolean = {
    val (id, age) = question match {
      case CatalogueQuestion.Page(url) => (pageId(url), QuestionPolicies.CataloguePageAge)
      case CatalogueQuestion.Id(of) =>
        (idOf(of), if (answers.document(idOf(of)).exists(d => hitsOf(d).nonEmpty)) QuestionPolicies.CatalogueMappedAge else QuestionPolicies.CatalogueUnmappedAge)
    }
    answers.document(id).forall(TmdbStore.olderThan(_, age, clock))
  }
}

object CatalogueAnswerStore {
  def idOf(id: CatalogueId): String = s"catalogue|${id.source}|${id.id}"
  def pageId(page: String): String  = s"catalogue-page|$page"

  private def hitsOf(d: BsonDocument): Seq[CatalogueHit] =
    BsonFields.documentsOf(d, "hits").map { h =>
      CatalogueHit(Option(h.get("item")).map(_.asString.getValue), Option(h.get("tmdb")).map(_.asInt32.getValue),
        Option(h.get("imdb")).map(_.asString.getValue), h.getString("via").getValue)
    }
  private def linksOf(d: BsonDocument): Seq[CatalogueId] =
    BsonFields.documentsOf(d, "links")
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
