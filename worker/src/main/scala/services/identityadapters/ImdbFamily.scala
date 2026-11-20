package services.identityadapters

import services.enrichment.{ImdbClient, ImdbSearch}
import services.identity.agreement.{SourceHit, SourceRecord}
import services.identity.vocabulary.VoterFamily

/** IMDb's own title search (any language IMDb lists a title under), its directors' credits, and its record. */
final class ImdbFamily(imdb: ImdbClient, search: ImdbSearch) extends FamilySource {
  val family: VoterFamily = VoterFamily.Imdb
  def titled(text: String): Seq[SourceHit] =
    search.searchTitles(text).take(FamilySources.SearchedFilms).map(t => SourceHit(t.id, t.title, None, t.year))
  def directedBy(name: String): Seq[SourceHit] = search.directedBy(name).map(id => SourceHit(id, "", None, None))
  def record(id: String): Option[SourceRecord] = imdb.identityRecord(id).map(film => SourceRecord(film, Map("imdb" -> id)))
}
