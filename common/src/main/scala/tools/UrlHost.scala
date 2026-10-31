package tools

import java.net.URI
import java.util.Locale
import scala.util.Try

/** A URL's host, lower-cased: the name the host breakers, the pacing, the throttle and the question memory key a host by. */
object UrlHost {

  /** `url`'s host, lower-cased; none for a URL that does not parse or names no host. */
  def of(url: String): Option[String] =
    Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase(Locale.ROOT))
}
