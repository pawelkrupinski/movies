package services.sharecards

import services.readmodel.ReadModelReader

/**
 * SHARE CARDS RESOLVABLE: the card file a `web_movies.shareCard` names is on disk, is the film's
 * own, and carries the version the URL names.
 *
 * `og:image` is that URL verbatim, and a preview scraper caches what it fetches for about a month,
 * so a document pointing at a file that is not there (a pruned card, a lost directory) or at a
 * file whose version moved on without the document following (a render whose re-projection never
 * came) shows a broken or wrong picture wherever the film is shared — and nothing else looks: the
 * coverage gauge asks whether each film on screen HAS a current card on disk, never whether the
 * document points at it.
 */
object ShareCardAudit {

  /** What is wrong with `shareCard` as film `filmId`'s pointer into `store`, or Nil. */
  def problems(filmId: String, shareCard: String, store: ShareCardStore): Seq[String] = {
    val file  = ShareCardFile.fileOf(shareCard)
    val named = shareCard.split("\\?v=", 2).lift(1)
    val owner = Option.when(file != ShareCardFile.name(filmId))(s"$shareCard names another film's file")
    val disk  = store.version(store.root.resolve(file)) match {
      case None                                     => Some(s"$shareCard: no such file")
      case Some(version) if named.contains(version) => None
      case Some(version)                            => Some(s"$shareCard: the file on disk is version $version")
    }
    owner.toSeq ++ disk
  }

  /** The audit's check of one film: None when its document could not be read, is gone, or names
   *  no card (a film with no card yet is the coverage gauge's business, not this invariant's). */
  def check(filmId: String, reader: ReadModelReader, store: ShareCardStore): Option[Seq[String]] =
    reader.findCard(filmId).flatMap(_.movie).flatMap(_.shareCard).map(problems(filmId, _, store))

  /** The films whose document names a card, or None when `web_movies` could not be read whole. */
  def ids(reader: ReadModelReader): Option[Seq[String]] = {
    val (refs, complete) = reader.findAllShareCardRefsChecked()
    Option.when(complete)(refs.filter(_.shareCard.nonEmpty).map(_.filmId))
  }
}
