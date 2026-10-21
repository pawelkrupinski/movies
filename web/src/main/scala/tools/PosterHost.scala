package tools

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.util.Try

/** The name a poster's origin is counted under on /metrics (`poster_host`): its REGISTRABLE domain.
 *
 *  Low-cardinality on purpose. A CDN's shard hosts (`de.web.img2.acsta.net`, `…img3…`) and a chain's
 *  per-venue subdomains (`arthousecrouchend.savoysystems.co.uk`) fail together and are one series;
 *  the full host is the uptime page's job (`img:` rows), not Prometheus's. A weserv URL counts under
 *  the origin it fronts, the way the browser beacon unwraps it (shared.js `originHost`).
 *
 *  The same rule is applied by the synthetic poster probes' discovery
 *  (infra/nix/files/synthetic-probe-targets.sh, `poster_family`), so a probe's `poster_host` and
 *  this gauge's line up in one Grafana row. */
object PosterHost {

  /** The second-level labels a country suffix is registered under (`co.uk`, `com.pl`, `org.uk`):
   *  for these the registrable domain is the last THREE labels. Not the Public Suffix List — the
   *  poster origins are a few dozen cinema and CDN hosts, and these cover every one served. */
  private val SecondLevel = Set("co", "com", "org", "net", "ac", "gov", "edu")

  def family(url: String): Option[String] =
    host(unwrapProxy(url)).map(registrable)

  private def unwrapProxy(url: String): String =
    if (!url.contains(PosterProxy.ProxyHost)) url
    else
      Try(new URI(url)).toOption
        .filter(u => Option(u.getHost).exists(_.equalsIgnoreCase(PosterProxy.ProxyHost)))
        .flatMap(u => Option(u.getRawQuery))
        .flatMap(_.split('&').collectFirst { case p if p.startsWith("url=") => p.drop(4) })
        .map(target => URLDecoder.decode(target, StandardCharsets.UTF_8))
        .map(target => if (target.startsWith("http://") || target.startsWith("https://")) target else "https://" + target)
        .getOrElse(url)

  private def host(url: String): Option[String] =
    if (!(url.startsWith("http://") || url.startsWith("https://"))) None
    else
      Try(new URI(url)).toOption.flatMap(u => Option(u.getHost))
        .map(_.toLowerCase(Locale.ROOT))
        .filter(_.contains('.'))

  private def registrable(host: String): String = {
    val labels = host.split('.')
    val keep =
      if (labels.length >= 3 && labels.last.length == 2 && SecondLevel.contains(labels(labels.length - 2))) 3
      else 2
    labels.takeRight(keep).mkString(".")
  }
}
