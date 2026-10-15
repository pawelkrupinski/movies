package services.identity

/**
 * The director a title CLOSES on, where nobody is credited: a bracket holding nothing but a person's name after the
 * title's own words, which a search for the very title suggests as a director (IMDb's suggestions name people beside
 * films). DE Caligari FilmBühne's "ES (ULRICH SCHAMONI)" is Ulrich Schamoni's 1966 "Es": IMDb suggests him, "Director,
 * Next Year, Same Time (1967)", ahead of any film, while TMDB's search of "Es" ranks Muschietti's 2017 "It" first and
 * his film nowhere. Asked as [[CandidateQuery.TitleCredit]], answered with that director's films.
 *
 * Only a CLOSING bracket of two to four words of letters: a name heading a title is its subject as often as its maker
 * ("Werner Herzog – Radical Dreamer" is a portrait by another), and a bracket of one word, or with a number, is a format,
 * a language or a year ("(OmU)", "(1966)"). The suggested director must be the bracket's name whole.
 */
object TitleCredit {
  private val ClosingName = """^.*\p{L}.*?\s*\(\s*(\p{L}[\p{L}.'’-]*(?:\s+\p{L}[\p{L}.'’-]*){1,3})\s*\)\s*$""".r

  /** The name `title`'s closing bracket holds, if it holds nothing but what may be one. */
  def bracketedName(title: String): Option[String] = title.trim match {
    case ClosingName(name) => Some(name)
    case _                 => None
  }

  /** Of the directors a search for `title` suggests, the one its closing bracket names whole, if any. */
  def named(title: String, suggestedDirectors: Seq[String]): Option[String] =
    bracketedName(title).map(IdentityMeasures.key).flatMap(key => suggestedDirectors.find(IdentityMeasures.key(_) == key))
}
