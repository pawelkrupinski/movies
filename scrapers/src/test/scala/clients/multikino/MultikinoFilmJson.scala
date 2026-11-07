package clients.multikino

/** Multikino's films API answer holding one film: every member its films carry, empty, then `members` (raw JSON,
 *  `"genres": []`) — the one shape the parser specs vary a field of. */
object MultikinoFilmJson {
  def apply(members: String*): String = s"""
    {"result":[{
      "filmTitle": "Test Film",
      "filmId": "TST",
      "filmUrl": "/filmy/test",
      "posterImageSrc": "/poster.jpg",
      "synopsisShort": "",
      "cast": "",
      "director": "",
      "originalTitle": "",
      "movieXchangeCode": "",
      "showingGroups": [],
      "trailers": []${members.map(m => s",\n      $m").mkString}
    }]}
  """
}
