package models

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** Filmweb's film pages, as their URLs name them: `https://www.filmweb.pl/film/Title+Words-2024-12345` — the title, its
 *  year and Filmweb's own id; the site answers any page ending in a year and the id with a redirect to the canonical one. */
object FilmwebPages {

  /** The canonical page URL the way Filmweb encodes it: spaces as `+`, everything else percent-encoded. `kind` picks the
   *  segment: `film` → `/film/`, `serial` → `/serial/`. */
  def url(id: Int, kind: String, title: String, year: Option[Int]): String = {
    val slug = URLEncoder.encode(title, StandardCharsets.UTF_8).replace("%20", "+")
    s"https://www.filmweb.pl/$kind/$slug-${year.fold("")(_.toString)}-$id"
  }

  // Canonical Filmweb URLs end in `-{id}` (optionally a trailing slash):
  //   https://www.filmweb.pl/film/Title+Words-2024-12345
  //   https://www.filmweb.pl/film/Title-12345/
  private val IdFromUrl = "-(\\d+)/?$".r

  /** The film id a page URL ends in. */
  def idOf(url: String): Option[Int] = IdFromUrl.findFirstMatchIn(url).flatMap(_.group(1).toIntOption)
}
