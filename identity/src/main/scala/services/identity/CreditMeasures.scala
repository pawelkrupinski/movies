package services.identity

import IdentityMeasures.*

/** How a listing's director credits relate to a film's: the director relation, and a credit read by search or by title.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object CreditMeasures {
  /** How two credit lists relate: one person in common — the same words or letters in any order,
   *  case or splitting (`same_person`) — a shared name word only (a surname, a transliteration), or
   *  nobody in common. Credits in different scripts (a venue's "Bi Gan", TMDB's "毕赣") are compared in Latin letters
   *  ([[CreditRelation.latinized]]); nobody in common THERE is `different_script`, its own category, since a
   *  transliteration can miss a person a Latin spelling names (a Japanese reading of Kanji is not
   *  its pinyin). `incomparable` only when a side has no name left to compare in Latin letters. */
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = CreditRelation.of(new CreditRelation.Credits(a), new CreditRelation.Credits(b))

  /** `measures`, the director read as the listing's own when its credits and the record's are in different
   *  scripts and no transliteration joins them, but the person TMDB finds by the listing's OWN spelling directed
   *  (or wrote) the film: "Wong Kar Wai" is TMDB's 王家衛, whose pinyin reads "Wang Jiawei" (film 843). */
  def creditedBySearch(measures: Map[String, Measure], directedBySpelling: Boolean): Map[String, Measure] =
    if (directedBySpelling && measures.get("director").exists(m => m == Category("different_script") || m == Category("incomparable")))
      measures + ("director" -> SamePersonMeasure)
    else measures

  /** `measures`, the director read as credited when the listing credits nobody but its title bills the film under a
   *  banner naming one of its directors ([[bannerNamesItsDirector]]). */
  def creditedByTitle(l: Listing, f: Film, measures: Map[String, Measure]): Map[String, Measure] =
    if (!measures.get("director").contains(MissingListing) || !bannerNamesItsDirector(l, f)) measures
    else measures + ("director" -> SamePersonMeasure)

  /** Does the listing's title bill one of the film's titles whole under a BANNER naming one of its directors — the
   *  surname ([[Film.directorSurnames]]) among the words before it? "Konwicki Salto", "Konwicki: Salto" (PL Kino Fenix's
   *  retrospective of Tadeusz Konwicki), and a series' banner naming two: DE Babylon (Mitte)'s "Hitchcock & Chabrol:
   *  Spellbound" is Hitchcock's 1945 film. A banner the resolver learned and peels off ([[TitleDecorations]]) still says
   *  so: the peeled title is searched, the banner stays evidence. So does a credit CLOSING the title — one of its
   *  directors named whole, two words at least, after one of its titles whole: DE Caligari's "ES (ULRICH SCHAMONI)" is
   *  Schamoni's 1966 "Es", not Muschietti's 2017 "It" its German title shares. Only for a listing crediting nobody. */
  private[identity] def bannerNamesItsDirector(l: Listing, f: Film): Boolean =
    l.directors.isEmpty && (l.bannerCuts.exists { case (titled, banner) => f.titleKeys.contains(titled) && banner.exists(f.directorSurnames) } ||
      l.closingCuts.exists { case (titled, credit) => f.directorNameKeys.contains(credit) && f.titleKeys.contains(titled) })
}
