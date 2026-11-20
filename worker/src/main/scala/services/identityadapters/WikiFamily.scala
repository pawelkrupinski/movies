package services.identityadapters

import services.enrichment.WikidataClient
import services.identity.agreement.{SourceHit, SourceRecord}
import services.identity.VoterFamily

/** Wikidata's film items: its entity search in the country's language and English, the items crediting a director, and
 *  an item's record, linked to other families by the ids it states. */
final class WikiFamily(wikidata: WikidataClient, language: String) extends FamilySource {
  val family: VoterFamily = VoterFamily.Wiki
  def titled(text: String): Seq[SourceHit] =
    wikidata.identitySearch(text, language, FamilySources.SearchedFilms).map { case (id, label) => SourceHit(id, label, None, None) }
  def directedBy(name: String): Seq[SourceHit] = wikidata.identityDirectedBy(name, language).map(id => SourceHit(id, "", None, None))
  def record(id: String): Option[SourceRecord] = wikidata.identityRecord(id, language).map { case (film, crossIds) => SourceRecord(film, crossIds) }
}
