package services.identityadapters

import services.identity.IdentityMeasures
import services.enrichment.{MetacriticClient, RottenTomatoesClient}
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}

/** A review site's search and its film pages, each film by its slug — its own id, linked as `idKey`: name, year,
 *  running time and directors; no person search. */
sealed abstract class SlugPageFamily(val family: VoterFamily, idKey: String) extends FamilySource {
  /** The site's search's films, in its order. */
  protected def found(text: String): Seq[SourceHit]
  /** The film page of `slug`, read: its name, year, running time and directors. */
  protected def page(slug: String): Option[SlugPageFamily.Page]
  final def titled(text: String): Seq[SourceHit] = found(text).take(FamilySources.SearchedFilms)
  final def directedBy(name: String): Seq[SourceHit] = Nil
  final def record(id: String): Option[SourceRecord] = page(id).map(page => SourceRecord(IdentityMeasures.Film(
    title = page.title.getOrElse(id), year = page.year, runtime = page.runtime, directors = Some(page.directors.toSeq.sorted)), Map(idKey -> id)))
}

object SlugPageFamily {
  final case class Page(title: Option[String], year: Option[Int], runtime: Option[Int], directors: Set[String])
}

/** Rotten Tomatoes' search and its film pages. */
final class RottenTomatoesFamily(rt: RottenTomatoesClient) extends SlugPageFamily(VoterFamily.RottenTomatoes, "rt") {
  protected def found(text: String): Seq[SourceHit] = rt.search(text).map(hit => SourceHit(hit.slug, hit.title, None, hit.year))
  protected def page(slug: String): Option[SlugPageFamily.Page] = rt.pageFor(RottenTomatoesClient.movieUrl(slug)).map(page =>
    SlugPageFamily.Page(page.title.map(RottenTomatoesClient.ownName), page.year, page.runtime, page.directors))
}

/** Metacritic's search and its film pages. */
final class MetacriticFamily(mc: MetacriticClient) extends SlugPageFamily(VoterFamily.Metacritic, "metacritic") {
  protected def found(text: String): Seq[SourceHit] = mc.search(text).map(hit => SourceHit(hit.slug, hit.title, None, hit.year))
  protected def page(slug: String): Option[SlugPageFamily.Page] = mc.pageFor(MetacriticClient.movieUrl(slug)).map(page =>
    SlugPageFamily.Page(page.title, page.year, page.runtime, page.directors))
}
