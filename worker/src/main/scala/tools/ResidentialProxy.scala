package tools

import java.util.Properties

/**
 * Assembles the residential-proxy egress config from two sources, by design:
 *
 *   - NON-SECRET host + ports from a committed classpath properties file
 *     (`/residential-proxy.properties`) — `host`, `ports` (comma-separated).
 *   - SECRET user + pass from [[Env]] (env var -> -D sysprop -> .env.local), so
 *     credentials never land in the repo.
 *
 * Returns a [[RealHttpFetch.ProxyConfig]] only when ALL four are present, so the
 * proxy stays disabled — and the worker falls back to Zyte/direct — wherever the
 * credentials aren't set: local dev without `.env.local`, CI, fixture replay.
 * See the `reference_decodo_isp_proxy` memory.
 */
object ResidentialProxy {
  private val PropertiesResource = "/residential-proxy.properties"

  /** The configured proxy, or None when host/ports (file) or user/pass (env) are
   *  absent — in which case the caller keeps the Zyte/direct egress. */
  def fromEnv(): Option[RealHttpFetch.ProxyConfig] =
    fromConfig(loadProperties(PropertiesResource), Env.get("KINOWO_PROXY_USER"), Env.get("KINOWO_PROXY_PASS"))

  private[tools] def fromConfig(
    props:    Properties,
    user:     Option[String],
    password: Option[String]
  ): Option[RealHttpFetch.ProxyConfig] =
    for {
      host  <- Option(props.getProperty("host")).map(_.trim).filter(_.nonEmpty)
      ports <- Option(props.getProperty("ports")).map(parsePorts).filter(_.nonEmpty)
      u     <- user.map(_.trim).filter(_.nonEmpty)
      p     <- password.filter(_.nonEmpty)
    } yield RealHttpFetch.ProxyConfig(host, ports, u, p)

  /** Comma-separated port list → ints, silently dropping blanks/garbage. */
  private[tools] def parsePorts(raw: String): Seq[Int] =
    raw.split(",").map(_.trim).filter(_.nonEmpty).flatMap(s => scala.util.Try(s.toInt).toOption).toSeq

  private[tools] def loadProperties(resource: String): Properties = {
    val props = new Properties()
    Option(getClass.getResourceAsStream(resource)).foreach { in =>
      try props.load(in) finally in.close()
    }
    props
  }
}
