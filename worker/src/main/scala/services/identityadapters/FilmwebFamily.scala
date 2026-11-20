package services.identityadapters

import services.identity.measures.Film
import services.identity.vocabulary.VoterFamily
import services.enrichment.FilmwebClient
import services.identity.agreement.{Showing, SourceHit, SourceRecord}

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
      SourceRecord(Film(
        title             = info.title,
        originalTitle     = info.originalTitle.orElse(preview.flatMap(_.originalTitle)).filterNot(_ == info.title),
        year              = info.year.orElse(preview.flatMap(_.year)),
        runtime           = preview.flatMap(_.runtime),
        directors         = preview.map(_.directors.toSeq.sorted),
        countries         = preview.map(_.countries).filter(_.nonEmpty)), Map("filmweb" -> id))
    }
  }
}
