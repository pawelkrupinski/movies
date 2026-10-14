package services.identity

import services.movies.TitleContainment

/** The words a venue bills AROUND a work rather than as part of it: an event's ([[EventWords]]) and an edition's
 *  ([[EditionWords]]). The listing shape, the measures, the suspect-take signals and the broadcast join read them; the
 *  offline segment learner (`DecorationSegments`, worker tests) reads them as features too. */
object DecorationWords {

  /** Words naming an event around a screening, not a work ([[IdentityMeasures.billsTwoWholeWorks]] and the broadcast join read them too). */
  private[identity] val EventWords = Set("spotkanie", "prelekcja", "pokaz", "specjalny", "q", "a", "qa", "premiera", "przedpremiera", "konkurs",
    "dyskusja", "rozmowa", "seans", "screening", "event", "special", "preview", "live", "talk", "intro", "introduced", "gesprach", "vorpremiere",
    "festiwal", "festival", "retransmisja", "koncert", "karnet", "maraton", "marathon")
  private[identity] val EditionWords = Set("cut", "remaster", "remastered", "restored", "restoration", "4k", "rerelease", "re", "release", "anniversary",
    "aniversario", "rocznica", "edition", "extended", "director", "directors", "wersja", "rozszerzona", "jubileusz", "uncut", "redux", "fassung", "restauriert", "restaurierte")

  /** Does a listing billed under `titles` name an EDITION of the film titled `filmTitles` — a word of [[EditionWords]]
   *  ("Extended Edition", "Director's Cut", "Redux") its titles carry and the film's do not? TMDB keeps one record for a
   *  film and its cuts, so an edition can run longer than every runtime the record states (`IdentityMeasures`' runtime
   *  delta reads it). */
  def billsAnEdition(titles: Seq[String], filmTitles: Seq[String]): Boolean = {
    val filmWords = filmTitles.iterator.flatMap(TitleContainment.tokens).toSet
    titles.iterator.flatMap(TitleContainment.tokens).exists(word => EditionWords(word) && !filmWords(word))
  }
}
