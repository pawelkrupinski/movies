package services.identity.agreement

import services.identity.ProseWords

/**
 * The venue's own SYNOPSIS against the TMDB descriptions of the films its cluster reaches: a film the families agree on
 * is not taken when the synopsis names, among the words it capitalises ([[ProseWords]]), [[SynopsisEvidence.Words]] or
 * more that a namesake's description holds and the agreed film's does not, and none that the agreed film's alone holds.
 * PL Janosik's "edukacja MH: Binti" synopsis — a girl in "Belgii" who meets "Eliasa" — is Migom's 2019 film, whose
 * overview has her stay in Belgium with Elias, not Shamte's 2021 one of four women in Dar-es-Salaam, which IMDb, RT and
 * Wikidata agree on. Proper names survive translation and inflection where nothing else of a description does, so one
 * language's synopsis is read against another's overview: the stems, not the words, are compared.
 *
 * The agreed film's own words are its description's and its people's — its top-billed cast and its directors — so a
 * synopsis naming the cast it credits never refuses it for a namesake that names the novel both adapt (PL Kino CK
 * Lublin's "Lalka": Dorociński, Urzędowska and Kondrat star in Kawalski's 2026 film; Bolesław Prus, whom the synopsis
 * also names, only the 2010 musical's overview). Every title's words are left out: a namesake shares them by being one.
 * A film with no description known is never refused: no overview, nothing to compare.
 */
object SynopsisEvidence {

  /** The words of a namesake's description a venue's synopsis must name, the agreed film's naming none. */
  val Words = 2

  /** The namesake (with the words it shares) the venue's synopsis `venue` names over the agreed film — whose description
   *  holds `description` and whose people `people` — among `namesakes`' descriptions, each film's titles' words in
   *  `titles` left out. None where the agreed film has no description, the synopsis shares a word with it no namesake
   *  shares, or no namesake shares [[Words]] the agreed film does not. */
  def veto(venue: ProseWords, description: ProseWords, people: ProseWords, namesakes: Seq[(Int, ProseWords)],
           titles: ProseWords): Option[(Int, Int)] =
    if (venue.isEmpty || description.isEmpty || namesakes.isEmpty) None
    else {
      val said   = venue -- titles
      val own    = (description ++ people) -- titles
      val theirs = namesakes.map { case (film, words) => film -> (words -- titles) }
      val anyOfTheirs = theirs.foldLeft(ProseWords.None)(_ ++ _._2)
      if ((said & (own -- anyOfTheirs)).nonEmpty) None
      else theirs.map { case (film, words) => film -> (said & (words -- own)).size }.filter(_._2 >= Words).maxByOption(_._2)
    }
}
