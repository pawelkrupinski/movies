package tools

/**
 * Whether the JDK's PKIX path builder may FETCH a missing intermediate certificate from a
 * leaf's Authority-Information-Access `caIssuers` URL — `com.sun.security.enableAIAcaIssuers`,
 * off by default. Some cinema hosts serve a chain that omits the leaf's issuer (see
 * [[TlsTrust]], case 2a); with fetching off, every handshake to them fails PKIX.
 *
 * A JVM-WIDE setting the JDK reads ONCE, when its path builder first loads, so it is not
 * something a TLS context can set for itself: it used to be set as a side effect of the
 * first touch of `TlsTrust`, from deep inside whatever built a fetch. It is a property of
 * the PROCESS, applied once by each `main` before anything handshakes.
 */
enum IssuerCertificateFetching(val enabled: Boolean) {

  /** The JDK default: a chain missing its intermediate fails. */
  case Disabled extends IssuerCertificateFetching(false)

  /** Fetch a missing intermediate from the leaf's AIA URL — what the scrapers need. */
  case Enabled extends IssuerCertificateFetching(true)

  /** Apply to this JVM. Call from a process's composition root, FIRST — before any TLS
   *  handshake, or the JDK has already read the old value. */
  def applyToJvm(): Unit = { System.setProperty(IssuerCertificateFetching.Property, enabled.toString); () }

  /** The same policy as a JVM option — for starting a child JVM with it. */
  def jvmOption: String = s"-D${IssuerCertificateFetching.Property}=$enabled"
}

object IssuerCertificateFetching {
  val Property = "com.sun.security.enableAIAcaIssuers"
}
