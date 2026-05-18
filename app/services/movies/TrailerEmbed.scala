package services.movies

import java.net.URI
import scala.util.Try

/**
 * Pure URL → embed-URL translation for trailer links.
 *
 * Cinemas store whatever the upstream surfaces (`youtube.com/watch?v=…`,
 * `youtu.be/…`, an `youtube.com/embed/…` URL, a `youtube.com/shorts/…`, a
 * `vimeo.com/…`). Before rendering in an `<iframe>` we have to normalise to
 * the provider's embed shape: `youtube.com/watch` won't load in a frame
 * because of YouTube's `X-Frame-Options: SAMEORIGIN` policy, and Vimeo
 * watch URLs likewise need `player.vimeo.com/video/…`.
 *
 * Stays a pure function (no I/O, no driver): the view layer calls it per
 * render. Returns None for shapes we can't embed (raw mp4 hosted elsewhere,
 * unfamiliar providers, garbage strings) so callers can fall back to a
 * "watch externally" link or render nothing.
 */
object TrailerEmbed {

  /** Best-effort: return an embeddable URL for `url` when the provider is
   *  one of the shapes we recognise; None otherwise. */
  def embedUrlFor(url: String): Option[String] =
    youTubeId(url).map(id => s"https://www.youtube.com/embed/$id")
      .orElse(vimeoId(url).map(id => s"https://player.vimeo.com/video/$id"))

  /** Extract the YouTube video id from any of the common URL shapes:
   *   - https://www.youtube.com/watch?v=ID
   *   - https://m.youtube.com/watch?v=ID
   *   - https://youtu.be/ID
   *   - https://www.youtube.com/embed/ID
   *   - https://www.youtube.com/shorts/ID
   *  Returns None for non-YouTube URLs and YouTube URLs we can't parse
   *  (channel pages, search results, malformed strings). */
  def youTubeId(url: String): Option[String] = Try {
    val cleaned = url.trim
    val parsed  = URI.create(cleaned)
    val host    = Option(parsed.getHost).getOrElse("").toLowerCase.stripPrefix("www.").stripPrefix("m.")
    val path    = Option(parsed.getPath).getOrElse("")
    host match {
      case "youtu.be" =>
        Some(path.stripPrefix("/")).map(_.takeWhile(_ != '/')).filter(isYouTubeId)
      case "youtube.com" | "youtube-nocookie.com" =>
        if (path == "/watch")
          queryParam(parsed.getRawQuery, "v").filter(isYouTubeId)
        else if (path.startsWith("/embed/") || path.startsWith("/shorts/") || path.startsWith("/v/"))
          Some(path.split("/").lift(2).getOrElse("")).map(_.takeWhile(_ != '?')).filter(isYouTubeId)
        else None
      case _ => None
    }
  }.toOption.flatten

  /** Extract the Vimeo numeric id. Accepts `vimeo.com/123456` and
   *  `player.vimeo.com/video/123456` plus an optional `/<hash>` review-link
   *  suffix that Vimeo's private-link share UI appends. */
  def vimeoId(url: String): Option[String] = Try {
    val parsed = URI.create(url.trim)
    val host   = Option(parsed.getHost).getOrElse("").toLowerCase.stripPrefix("www.")
    val parts  = Option(parsed.getPath).getOrElse("").split("/").filter(_.nonEmpty).toList
    val candidate = (host, parts) match {
      case ("vimeo.com",        id :: _)               => Some(id)
      case ("player.vimeo.com", "video" :: id :: _)    => Some(id)
      case _                                            => None
    }
    candidate.filter(_.forall(_.isDigit))
  }.toOption.flatten

  /** YouTube IDs are 11 chars from `[A-Za-z0-9_-]`. Used to reject channel
   *  pages, search results, and other paths that share `/watch` but don't
   *  carry a video id we can embed. */
  private def isYouTubeId(s: String): Boolean =
    s.length == 11 && s.forall(c => c.isLetterOrDigit || c == '_' || c == '-')

  private def queryParam(rawQuery: String, key: String): Option[String] =
    Option(rawQuery).toSeq.flatMap(_.split("&"))
      .map(_.split("=", 2))
      .collectFirst { case Array(k, v) if k == key => v }
}
