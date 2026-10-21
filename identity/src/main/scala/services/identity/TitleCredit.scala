package services.identity

import services.billing.Credits

/**
 * The director a title CLOSES on, where nobody is credited: a bracket holding nothing but a person's name after the
 * title's own words, which a search for the very title suggests as a director (IMDb's suggestions name people beside
 * films). DE Caligari FilmBühne's "ES (ULRICH SCHAMONI)" is Ulrich Schamoni's 1966 "Es": IMDb suggests him, "Director,
 * Next Year, Same Time (1967)", ahead of any film, while TMDB's search of "Es" ranks Muschietti's 2017 "It" first and
 * his film nowhere. Asked as [[CandidateQuery.TitleCredit]], answered with that director's films.
 *
 * Only a CLOSING bracket of two to four words of letters: a name heading a title is its subject as often as its maker
 * ("Werner Herzog – Radical Dreamer" is a portrait by another), and a bracket of one word, or with a number, is a format,
 * a language or a year ("(OmU)", "(1966)"). Nor a bracket of note words: a cut, a score, an event or a version
 * (`DecorationWords`: "(Director's Cut)", "(Live Score)"), subtitles ("(mit Untertiteln)"), a word starting lower-case
 * that is no name's particle. The suggested director must be the bracket's name whole.
 */
object TitleCredit {
  /** Of the directors a search for `title` suggests, the one its closing bracket names whole, if any. */
  def named(title: String, suggestedDirectors: Seq[String]): Option[String] =
    Credits.bracketedName(title).map(IdentityMeasures.key).flatMap(key => suggestedDirectors.find(IdentityMeasures.key(_) == key))
}
