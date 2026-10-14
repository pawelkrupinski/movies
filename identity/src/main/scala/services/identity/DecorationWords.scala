package services.identity

import services.movies.{TitleContainment, TitleWords}

/** The words a venue bills AROUND a work rather than as part of it: an event's ([[EventWords]]) and an edition's
 *  ([[EditionWords]]). The listing shape, the measures, the suspect-take signals and the broadcast join read them; the
 *  offline segment learner (`DecorationSegments`, worker tests) reads them as features too. */
object DecorationWords {

  /** Words naming an event around a screening, not a work ([[MultiFilmBill.billsTwoWholeWorks]] and the broadcast join read them too):
   *  a premiere, a talk, the screening as an event, a festival, a relay, a concert, a marathon — the word table's
   *  ([[TitleWords]]), in every language a country bills in. */
  private[identity] val EventWords: Set[String] = {
    import TitleWords.Concept.*
    TitleWords.tokens(Premiere, Talk, Screening, Festival, Relay, Performance, Marathon)
  }
  /** Words naming an EDITION of a film — a cut, a restoration, a re-release, an anniversary ([[TitleWords]]). */
  private[identity] val EditionWords: Set[String] = TitleWords.tokens(TitleWords.Concept.Edition, TitleWords.Concept.Anniversary)

  /** Does a listing billed under `titles` name an EDITION of the film titled `filmTitles` — a word of [[EditionWords]]
   *  ("Extended Edition", "Director's Cut", "Redux") its titles carry and the film's do not? TMDB keeps one record for a
   *  film and its cuts, so an edition can run longer than every runtime the record states (`IdentityMeasures`' runtime
   *  delta reads it). */
  def billsAnEdition(titles: Seq[String], filmTitles: Seq[String]): Boolean = {
    val filmWords = filmTitles.iterator.flatMap(TitleContainment.tokens).toSet
    titles.iterator.flatMap(TitleContainment.tokens).exists(word => EditionWords(word) && !filmWords(word))
  }
}
