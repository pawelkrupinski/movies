package services.identity

import services.enrichment.{FilmwebClient, ImdbClient, MetacriticClient, RottenTomatoesClient, WikidataClient}
import services.identity.agreement.{Showing, SourceHit, SourceRecord, VoterFamily}

/**
 * Another film database FAMILY asked live ([[VoterFamily]]): its title search's films, a credited person's films where
 * it has a person search, and its record of one film — what [[FamilyAnswerStore]] files and the agreement reads. A
 * failed read throws (the fill asks again); an answer naming nothing is empty.
 */
trait FamilySource {
  def family: VoterFamily
  def titled(text: String): Seq[SourceHit]
  def directedBy(name: String): Seq[SourceHit]
  def record(id: String): Option[SourceRecord]
  /** The films the family's site lists a venue screening, and on which days: none where it lists no programmes. */
  def showing(venue: String): Seq[Showing] = Nil
}

object FamilySources {
  /** How many of a search's films are read: the experiment read the first eight. */
  val SearchedFilms = 8
}

/** IMDb's own title search (any language IMDb lists a title under), its directors' credits, and its record. */
final class ImdbFamily(imdb: ImdbClient) extends FamilySource {
  val family: VoterFamily = VoterFamily.Imdb
  def titled(text: String): Seq[SourceHit] =
    imdb.searchTitles(text).take(FamilySources.SearchedFilms).map(t => SourceHit(t.id, t.title, None, t.year))
  def directedBy(name: String): Seq[SourceHit] = imdb.directedBy(name).map(id => SourceHit(id, "", None, None))
  def record(id: String): Option[SourceRecord] = imdb.identityRecord(id).map(film => SourceRecord(film, Map("imdb" -> id)))
}

/** Filmweb's search (its own and other languages' titles) and its film records — films only, never a series — and, in
 *  Poland, each venue's programme there (`programmes`: [[services.cinemas.pl.FilmwebProgrammes]]). */
final class FilmwebFamily(filmweb: FilmwebClient, programmes: String => Seq[Showing] = _ => Nil) extends FamilySource {
  val family: VoterFamily = VoterFamily.Filmweb
  override def showing(venue: String): Seq[Showing] = programmes(venue)
  def titled(text: String): Seq[SourceHit] =
    filmweb.search(text).filter(_.kind == "film").take(FamilySources.SearchedFilms).map(hit => SourceHit(hit.id.toString, "", None, None))
  def directedBy(name: String): Seq[SourceHit] = Nil
  def record(id: String): Option[SourceRecord] = id.toIntOption.flatMap { film =>
    filmweb.info(film).map { info =>
      val preview = filmweb.preview(film)
      SourceRecord(IdentityMeasures.Film(
        title             = info.title,
        originalTitle     = info.originalTitle.orElse(preview.flatMap(_.originalTitle)).filterNot(_ == info.title),
        year              = info.year.orElse(preview.flatMap(_.year)),
        runtime           = preview.flatMap(_.runtime),
        directors         = preview.map(_.directors.toSeq.sorted),
        countries         = preview.map(_.countries).filter(_.nonEmpty)), Map("filmweb" -> id))
    }
  }
}

/** Rotten Tomatoes' search and its film pages: name, year, running time and directors. */
final class RottenTomatoesFamily(rt: RottenTomatoesClient) extends FamilySource {
  val family: VoterFamily = VoterFamily.RottenTomatoes
  def titled(text: String): Seq[SourceHit] =
    rt.search(text).take(FamilySources.SearchedFilms).map(hit => SourceHit(hit.slug, hit.title, None, hit.year))
  def directedBy(name: String): Seq[SourceHit] = Nil
  def record(id: String): Option[SourceRecord] = rt.pageFor(RottenTomatoesClient.movieUrl(id)).map(page => SourceRecord(IdentityMeasures.Film(
    title = page.title.map(RottenTomatoesClient.ownName).getOrElse(id), year = page.year, runtime = page.runtime,
    directors = Some(page.directors.toSeq.sorted)), Map("rt" -> id)))
}

/** Metacritic's search and its film pages: name, year, running time and directors. */
final class MetacriticFamily(mc: MetacriticClient) extends FamilySource {
  val family: VoterFamily = VoterFamily.Metacritic
  def titled(text: String): Seq[SourceHit] =
    mc.search(text).take(FamilySources.SearchedFilms).map(hit => SourceHit(hit.slug, hit.title, None, hit.year))
  def directedBy(name: String): Seq[SourceHit] = Nil
  def record(id: String): Option[SourceRecord] = mc.pageFor(MetacriticClient.movieUrl(id)).map(page => SourceRecord(IdentityMeasures.Film(
    title = page.title.getOrElse(id), year = page.year, runtime = page.runtime, directors = Some(page.directors.toSeq.sorted)), Map("metacritic" -> id)))
}

/** Wikidata's film items: its entity search in the country's language and English, the items crediting a director, and
 *  an item's record, linked to other families by the ids it states. */
final class WikiFamily(wikidata: WikidataClient, language: String) extends FamilySource {
  val family: VoterFamily = VoterFamily.Wiki
  def titled(text: String): Seq[SourceHit] =
    wikidata.identitySearch(text, language, FamilySources.SearchedFilms).map { case (id, label) => SourceHit(id, label, None, None) }
  def directedBy(name: String): Seq[SourceHit] = wikidata.identityDirectedBy(name, language).map(id => SourceHit(id, "", None, None))
  def record(id: String): Option[SourceRecord] = wikidata.identityRecord(id, language).map { case (film, crossIds) => SourceRecord(film, crossIds) }
}
