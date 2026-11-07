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
 *
 * Since JDK 25.0.4 the switch alone fetches nothing: `com.sun.security.allowedAIALocations`
 * (a security property, overridable by the system property of the same name) filters which
 * `caIssuers` URLs may be followed, and it ships as deny-all. `Enabled` sets it to `any`,
 * restoring what the switch meant before — any host's leaf may name its issuer's location.
 */
enum IssuerCertificateFetching(val enabled: Boolean) {

  /** The JDK default: a chain missing its intermediate fails. */
  case Disabled extends IssuerCertificateFetching(false)

  /** Fetch a missing intermediate from the leaf's AIA URL — what the scrapers need. */
  case Enabled extends IssuerCertificateFetching(true)

  /** Apply to this JVM. Call from a process's composition root, FIRST — before any TLS
   *  handshake, or the JDK has already read the old value. */
  def applyToJvm(): Unit = properties.foreach(System.setProperty(_, _))

  /** The same policy as JVM options — for starting a child JVM with it. */
  def jvmOptions: Seq[String] = properties.map((name, value) => s"-D$name=$value")

  private def properties: Seq[(String, String)] =
    (IssuerCertificateFetching.Property -> enabled.toString) +:
      Option.when(enabled)(IssuerCertificateFetching.AllowedLocationsProperty -> "any").toSeq
}

object IssuerCertificateFetching {
  val Property                 = "com.sun.security.enableAIAcaIssuers"
  val AllowedLocationsProperty = "com.sun.security.allowedAIALocations"
}
