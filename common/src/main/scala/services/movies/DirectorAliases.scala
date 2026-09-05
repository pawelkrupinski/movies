package services.movies

/**
 * Directors whose two names share no letters, so no comparison of the strings can
 * connect them: a pseudonym TMDB credits where the venue names the person, or one
 * name romanised from two dialects.
 *
 * Facts, not rules — each entry is one director, added because a real row was
 * flagged over it. Deliberately NOT a general fuzzy-matching layer: everything
 * that CAN be reached by comparing the strings is handled in
 * [[CinemaCorroboration]], and this exists only for what cannot.
 *
 * It is also the smallest of the classes it sits among. A venue naming a film's
 * OTHER director — Ethan for a film TMDB credits to Joel Coen, David Leitch for
 * "John Wick" — outnumbers the pseudonyms here, and no list of names fixes that;
 * only asking TMDB who the film's crew actually were does.
 */
object DirectorAliases {

  /** Each set is one person. Compared after the same folding the name comparison
   *  uses, so accents and punctuation need not be repeated here. */
  private val Groups: Seq[Set[String]] = Seq(
    Set("loriot", "viccovonbulow"),                      // Ödipussi, Pappa ante Portas
    Set("anthonymdawson", "antoniomargheriti"),          // the anglicised credit on his genre films
    Set("laukarleung", "liuchialiang"),                  // Cantonese / Mandarin of one name
    Set("kukla", "katarinaresek"),                       // the director's artist name
    Set("dkwelchman", "dorotakobiela"),                  // credited both ways on "Loving Vincent"
    Set("brucele", "huangkinlung")                       // stage name / given name
  )

  private val byName: Map[String, Int] =
    Groups.zipWithIndex.flatMap { case (names, group) => names.map(_ -> group) }.toMap

  /** True when both folded names are listed as the same person. */
  def sameDirector(a: String, b: String): Boolean =
    (byName.get(a), byName.get(b)) match {
      case (Some(x), Some(y)) => x == y
      case _                  => false
    }
}
