package services.sharecards

import services.readmodel.{ReadModelReader, ShareCardRef}

/** Which films' share cards are live, as the read model says: what [[ShareCardJanitor]] never deletes. */
final class LiveShareCards(reader: ReadModelReader) {

  /** The live films. Which films are on screen takes a scan of every screenings id — a country's largest
   *  collection — so only the daily prune (`daily`) pays for it. The budget pass protects every film with a
   *  card instead. */
  def read(daily: Boolean): LiveShareCards.Films = {
    val refsRead = reader.findAllShareCardRefsChecked().answered
    val refs     = refsRead.getOrElse(Seq.empty)
    val screenedRead =
      if (daily) reader.findAllScreeningRefsChecked().answered.map(_.iterator.map(_.filmId).toSet)
      else Some(refs.iterator.filter(_.shareCard.nonEmpty).map(_.filmId).toSet)
    val screened = screenedRead.getOrElse(Set.empty[String])
    LiveShareCards.Films(refs, screened, refsRead.isDefined && screenedRead.isDefined,
      refs.iterator.filter(ref => screened(ref.filmId)).map(ref => ShareCardFile.token(ref.filmId)).toSet)
  }
}

object LiveShareCards {
  /** The read model's card refs, the films on screen, whether both read complete, and the live films' card tokens. */
  final case class Films(refs: Seq[ShareCardRef], screened: Set[String], complete: Boolean, tokens: Set[String])
}
