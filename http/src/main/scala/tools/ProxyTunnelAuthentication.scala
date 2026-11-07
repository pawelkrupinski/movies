package tools

/**
 * Whether java.net.http may answer an HTTP proxy's Basic challenge on an HTTPS `CONNECT`
 * tunnel — `jdk.http.auth.tunneling.disabledSchemes`, whose JDK default ("Basic") makes
 * every HTTPS fetch through the authenticated Decodo egress 407.
 *
 * A JVM-WIDE setting the JDK reads ONCE, the first time anything in the process touches
 * java.net.http, and never again. So it is not something a fetch can set for itself: it
 * used to be cleared by every `RealHttpFetch` and every `ProxyConfig` as they were built,
 * which mutated the whole JVM from deep inside the object graph — and a proxy built after
 * a direct fetch had initialised java.net.http was already too late (the roster audit's
 * ~170 direct reads before its proxy shards; run 35912986387). It is a property of the
 * PROCESS, applied once by each `main` that will tunnel through the proxy, before it
 * builds anything.
 */
enum ProxyTunnelAuthentication(val disabledSchemes: String) {

  /** The JDK default: Basic refused on tunnels. */
  case BasicRefused extends ProxyTunnelAuthentication("Basic")

  /** Basic permitted — what the residential egress needs. Credentials only ever go to a
   *  proxy the process configured; nothing is sent where no proxy is used. */
  case BasicAllowed extends ProxyTunnelAuthentication("")

  /** Apply to this JVM. Call from a process's composition root, FIRST — before anything in
   *  the process has touched java.net.http, or the JDK has already read the old value. */
  def applyToJvm(): Unit = { System.setProperty(ProxyTunnelAuthentication.Property, disabledSchemes); () }

  /** The same policy as a JVM option — for starting a child JVM with it. */
  def jvmOption: String = s"-D${ProxyTunnelAuthentication.Property}=$disabledSchemes"
}

object ProxyTunnelAuthentication {
  val Property = "jdk.http.auth.tunneling.disabledSchemes"
}
