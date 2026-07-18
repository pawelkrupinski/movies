package scripts

import tools.{RealHttpFetch, ResidentialProxy}
import services.cinemas.pl.MultikinoClient

/**
 * Prod-faithful PRE-DEPLOY check for the Decodo residential-proxy egress.
 * Exercises the REAL `java.net.http` proxy path — proxy routing + Basic
 * CONNECT-tunnel auth + the cookie/TLS/gzip stack of [[RealHttpFetch]] — through
 * the Decodo ISP proxy against the LIVE Multikino API and a biletyna venue. This
 * is what curl can't prove: that the JVM client (with the
 * `jdk.http.auth.tunneling.disabledSchemes` fix) actually authenticates and
 * clears the block. Run it before wiring the proxy into the worker's deploy:
 *
 *   KINOWO_PROXY_USER=… KINOWO_PROXY_PASS=… sbt 'worker/Test/runMain scripts.ProxyProbe'
 *
 * Host + ports come from residential-proxy.properties; KINOWO_PROXY_USER /
 * KINOWO_PROXY_PASS come from the environment / .env.local (same path the worker
 * uses, via [[ResidentialProxy]]). Pass/fail is printed per target; a `407` or
 * `403` shows as FAIL with the status in the message.
 */
object ProxyProbe {
  private val BiletynaVenue = "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe"

  def main(args: Array[String]): Unit = ResidentialProxy.fromEnv() match {
    case None =>
      println("No proxy config — set KINOWO_PROXY_USER / KINOWO_PROXY_PASS (host+ports come from residential-proxy.properties).")
    case Some(config) =>
      val fetch = new RealHttpFetch(Some(config))
      println(s"proxy: ${config.host} ports=${config.ports.mkString(",")} (rotating), via java.net.http RealHttpFetch")
      probe("egress-ip", fetch, "https://api.ipify.org", b => s"ip=${b.trim}")
      probe("multikino",  fetch, MultikinoClient.ApiUrl, b => s"films=${b.split("\"filmTitle\"").length - 1}")
      probe("biletyna",   fetch, BiletynaVenue, b => s"jsonld=${b.contains("application/ld+json")}")
  }

  private def probe(label: String, fetch: RealHttpFetch, url: String, summary: String => String): Unit =
    try {
      val body = fetch.get(url)
      println(f"$label%-11s OK   bytes=${body.length}%-8d ${summary(body)}")
    } catch {
      case e: Throwable => println(f"$label%-11s FAIL ${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")}")
    }
}
