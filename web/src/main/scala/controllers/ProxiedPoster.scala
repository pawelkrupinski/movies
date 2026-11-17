package controllers

/** A vetted poster through the image proxy ([[tools.PosterProxy.proxy]]): the weserv address is https, so the answer
 *  stays a [[WebHref]]. Beside the templates that render posters, so the proxy stays out of what WebHref's readers reach. */
object ProxiedPoster {
  def of(poster: WebHref): WebHref = WebHref.of(tools.PosterProxy.proxy(poster.url)).getOrElse(poster)
}
