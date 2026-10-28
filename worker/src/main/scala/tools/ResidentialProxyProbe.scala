package tools

import play.api.Logging

import java.io.IOException
import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.Base64
import scala.concurrent.duration._

/** One cheap yes/no health check of an egress dependency — the check a scrape is parked on and un-parked by
 *  (`services.tasks.ProxyParking`). */
trait ProxyProbe {

  /** Does the dependency work right now? Never throws: a probe that cannot tell says no. */
  def passes(): Boolean
}

/**
 * The residential proxy's health, asked the way its outages show: a `CONNECT` through each pool port, which is
 * exactly what answered `503` for every port during the account-wide outages (2026-09-10, 2026-10-27). The tunnel is
 * closed as soon as the proxy's status line is read — no TLS, no request to `target` — so a probe moves a couple of
 * hundred bytes and touches no cinema site.
 *
 * Passes when ANY port tunnels: the venues are spread across the ports, so one working port is the proxy back, and a
 * venue sticky to a port still dead fails again on its own and backs off as its own failure.
 */
final class ResidentialProxyProbe(
  config:  RealHttpFetch.ProxyConfig,
  // What each CONNECT asks for. A neutral host, never a scraped one: the probe must not count as a visit to a site.
  target:  String         = ResidentialProxyProbe.Target,
  timeout: FiniteDuration = 5.seconds
) extends ProxyProbe with Logging {

  private val authorization =
    "Basic " + Base64.getEncoder.encodeToString(s"${config.user.value}:${config.password.value}".getBytes(ISO_8859_1))

  def passes(): Boolean = config.ports.exists(port => statusThrough(port).contains(200))

  /** The proxy's status code for a CONNECT through `port`, or None when it could not be read. */
  private[tools] def statusThrough(port: Int): Option[Int] =
    try {
      val socket = new Socket()
      try {
        socket.connect(new InetSocketAddress(config.host, port), timeout.toMillis.toInt)
        socket.setSoTimeout(timeout.toMillis.toInt)
        val out = socket.getOutputStream
        out.write((s"CONNECT $target HTTP/1.1\r\nHost: $target\r\nProxy-Authorization: $authorization\r\n\r\n")
          .getBytes(ISO_8859_1))
        out.flush()
        ResidentialProxyProbe.statusOf(ResidentialProxyProbe.firstLine(socket.getInputStream))
      } finally socket.close()
    } catch {
      case e: IOException =>
        logger.debug(s"residential proxy probe: port $port did not answer: $e")
        None
    }
}

object ResidentialProxyProbe {

  /** The CONNECT target: a neutral, always-up host. */
  val Target = "www.cloudflare.com:443"

  // A status line is short; anything longer is not one.
  private val MaxLine = 256

  private[tools] def firstLine(in: java.io.InputStream): String = {
    val line = new StringBuilder
    var b    = in.read()
    while (b != -1 && b != '\n' && line.length < MaxLine) { if (b != '\r') line += b.toChar; b = in.read() }
    line.result()
  }

  /** `HTTP/1.1 200 Connection established` → 200. */
  private[tools] def statusOf(line: String): Option[Int] =
    line.split(' ').lift(1).flatMap(_.toIntOption).filter(_ => line.startsWith("HTTP/"))
}
