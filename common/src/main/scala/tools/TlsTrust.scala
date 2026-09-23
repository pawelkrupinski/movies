package tools

import play.api.Logging

import java.security.KeyStore
import java.security.cert.{CertificateException, CertificateFactory, X509Certificate}
import javax.net.ssl.{SSLContext, TrustManager, TrustManagerFactory, X509TrustManager}
import scala.util.{Try, Using}

/**
 * Builds an [[javax.net.ssl.SSLContext]] that trusts the JDK's default CAs PLUS
 * extra roots bundled as classpath PEM resources.
 *
 * Why this exists: a handful of Polish cinema / CA sites — artmuseum.pl
 * (Kinomuzeum), kinomuranow.pl (Kino Muranów), sdk.waw.pl, … — serve
 * certificates that chain to Certum's "Certum Trusted Root CA". Browsers and
 * OpenSSL trust that root, but OpenJDK's curated `cacerts` omits it, so every
 * scrape of those hosts died with `PKIX path building failed: unable to find
 * valid certification path to requested target` (Kinomuzeum was 100% red on
 * /uptime). Two distinct breakages, both fixed here:
 *
 *   1. Hosts that send a complete chain to the Certum root (kinomuranow.pl,
 *      sdk.waw.pl) just need that root to be a trust anchor — we bundle it.
 *   2. artmuseum.pl serves a *broken* chain: the leaf (`*.artmuseum.pl`, issued
 *      by `home pl DV TLS G2 R35 CA`) ships with an unrelated, orphaned set of
 *      intermediates that don't include its real issuer. The leaf's actual
 *      issuer is reachable two ways, and we bundle BOTH so neither is a single
 *      point of failure:
 *        a. `enableAIAcaIssuers` (set below) lets the JVM fetch the missing
 *           intermediate from the leaf's Authority-Information-Access URL —
 *           durable across the upstream rotating that intermediate, but it is a
 *           SYNCHRONOUS network fetch to certum.pl on every cold handshake. When
 *           that repository hiccups, the whole scrape dies with the PKIX error
 *           (a recurring transient red on Kinomuzeum's /uptime).
 *        b. We also bundle the `home pl DV TLS G2 R35 CA` intermediate itself as
 *           a trust anchor, so the common case needs no network at all. If the
 *           upstream rotates it, (a) still recovers. Belt and braces.
 *   3. A cluster of cinema-ticketing hosts on nazwa.pl (`bilety.ck105.koszalin.pl`
 *      / Kryterium, `bilety.dkkozienice.pl`, `bilety.switzwolen.pl`,
 *      `bilety.mok.com.pl` / Chemik+Twierdza) serve leaf+intermediate chaining to
 *      `Certum EC-384 CA` — a DIFFERENT Certum root (the ECC one) than the RSA
 *      `Certum Trusted Root CA` of case (1). The JDK omits this root too, so all
 *      of them died with the same PKIX error. The chain is complete (the servers
 *      send the `nazwaSSL DV TLS G2 E29 CA` sub-CA), so we only need the root as a
 *      trust anchor — one cert recovers every nazwa.pl venue at once.
 *   4. `ec1lodz.pl` (Kino NCKF EC1) renewed in Aug 2026 onto a leaf issued by
 *      `Certum OV TLS G2 R39 CA` and serves that leaf ALONE — no intermediate at
 *      all. Its root IS the `Certum Trusted Root CA` of case (1), which we already
 *      bundle, so the anchor was never the problem: the path builder simply had
 *      nothing to bridge leaf -> root with, and the AIA fetch of (2a) evidently
 *      does not survive the worker's egress (the venue went red and stayed red).
 *      Same remedy as (2b) — bundle the intermediate so the path closes offline.
 *   5. `kinoroma.zabrze.pl` (Kino Roma) is a DIFFERENT class of breakage: the leaf
 *      itself expired 2026-09-14 (a Let's Encrypt cert whose renewal automation
 *      stopped running) — not a missing anchor, so no amount of root/intermediate
 *      bundling helps (see [[reference_expired_cert_probe_plain_http]]). The usual
 *      escape hatch — reroute to plain `http://` — doesn't apply either: the host
 *      redirects http -> https unconditionally (even by IP + Host header) and sends
 *      `Strict-Transport-Security`. So this one leaf is pinned by exact DER bytes
 *      in [[PinnedExpiredLeafResources]] and trusted ONLY on a byte-for-byte match
 *      — not the host, not any other cert that host might ever present, not a
 *      blanket "ignore expiry" flag. Once the venue renews, the new leaf's bytes
 *      won't match this pin and normal PKIX validation takes back over.
 *
 * The extra roots are ADDED to the defaults, never a replacement: well-behaved
 * APIs (TMDB, IMDb, Filmweb, …) keep validating against the standard store. The
 * composite trust manager tries the default store first and only falls back to
 * the bundled roots on a `CertificateException`, so trust is strictly widened.
 * The pinned-expired-leaf check (case 5) runs only after both of those have
 * rejected the chain, and it can only ever match the handful of exact leaves in
 * [[PinnedExpiredLeafResources]], so it doesn't widen trust for anything else.
 */
object TlsTrust extends Logging {

  // Read by the JDK's PKIX path builder at validation time: when a server omits
  // an intermediate, fetch it from the cert's AIA caIssuers URL. Off by default.
  // Set before the first TLS handshake (this object is touched when RealHttpFetch
  // is constructed, at wiring time, ahead of any scrape).
  System.setProperty("com.sun.security.enableAIAcaIssuers", "true")

  /** Classpath-absolute paths of PEM certs to add as trust anchors on top of the
   *  default store: the two Certum roots the JDK omits (RSA `Certum Trusted Root
   *  CA` for case 1, ECC `Certum EC-384 CA` for case 3), plus the two issuing
   *  intermediates whose servers omit them — artmuseum.pl's (point 2b) and
   *  ec1lodz.pl's (point 4) — so those chains validate without a per-handshake
   *  AIA network fetch. */
  val BundledRootResources: Seq[String] = Seq(
    "/certs/certum-trusted-root-ca.pem",
    "/certs/home-pl-dv-tls-g2-r35-ca.pem",
    "/certs/certum-ec-384-ca.pem",
    "/certs/certum-ov-tls-g2-r39-ca.pem"
  )

  /** The bundled extra roots, parsed. Empty if a resource is missing (logged). */
  private[tools] def bundledCerts: Seq[X509Certificate] = loadCerts(BundledRootResources)

  /** Classpath-absolute paths of individual leaf certs we trust despite being
   *  technically expired, pinned by exact bytes — see case 5 in the class doc.
   *  Each entry is a single known-good keypair whose CA-issued validity window
   *  has lapsed, not a host or a CA, so this can never grow into a general
   *  "skip expiry" switch. */
  val PinnedExpiredLeafResources: Seq[String] = Seq(
    "/certs/expired-leaf-kinoroma-zabrze-pl.pem"
  )

  /** The pinned expired leaves, parsed. Empty if a resource is missing (logged). */
  private[tools] def pinnedExpiredLeafs: Seq[X509Certificate] = loadCerts(PinnedExpiredLeafResources)

  /** SSLContext trusting default CAs + [[BundledRootResources]] +
   *  [[PinnedExpiredLeafResources]]. Falls back to the platform default if
   *  anything goes wrong, so a packaging slip can never take TLS down — it just
   *  reverts to the unaugmented store. */
  lazy val augmentedContext: SSLContext =
    build().recover { case exception =>
      logger.warn(s"TlsTrust: falling back to default SSLContext (${exception.getMessage})")
      SSLContext.getDefault
    }.get

  /** The X509TrustManager backing [[augmentedContext]] — exposed for tests. */
  def augmentedTrustManager: X509TrustManager = compositeTrustManager(bundledCerts, pinnedExpiredLeafs)

  private def build(): Try[SSLContext] = Try {
    val context = SSLContext.getInstance("TLS")
    context.init(null, Array[TrustManager](compositeTrustManager(bundledCerts, pinnedExpiredLeafs)), null)
    context
  }

  private def loadCerts(resources: Seq[String]): Seq[X509Certificate] = {
    val cf = CertificateFactory.getInstance("X.509")
    resources.flatMap { path =>
      Option(getClass.getResourceAsStream(path)) match {
        case Some(stream) =>
          Using.resource(stream)(in => Some(cf.generateCertificate(in).asInstanceOf[X509Certificate]))
        case None =>
          logger.warn(s"TlsTrust: bundled cert resource not found: $path")
          None
      }
    }
  }

  private def trustManagerFor(ks: KeyStore): X509TrustManager = {
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks) // null keystore → the JDK default cacerts
    tmf.getTrustManagers.collectFirst { case x: X509TrustManager => x }
      .getOrElse(throw new IllegalStateException("no X509TrustManager from default factory"))
  }

  private def extraTrustManager(certs: Seq[X509Certificate]): X509TrustManager = {
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(null, null)
    certs.zipWithIndex.foreach { case (c, i) => ks.setCertificateEntry(s"extra-$i", c) }
    trustManagerFor(ks)
  }

  /** True when the chain's leaf is byte-identical to one of the pinned expired
   *  certs — the only condition under which case 5 trust (see the class doc)
   *  kicks in. Pure and side-effect-free so it's directly unit-testable without
   *  a full TLS handshake. */
  private[tools] def matchesPinnedLeaf(chain: Array[X509Certificate], pinned: Seq[X509Certificate]): Boolean =
    chain.headOption.exists(leaf => pinned.exists(p => p.getEncoded.sameElements(leaf.getEncoded)))

  /** Try the default store first; fall back to the bundled roots on a
   *  `CertificateException`; if that also fails, accept a byte-exact match
   *  against a pinned expired leaf (case 5 in the class doc — see
   *  [[matchesPinnedLeaf]]). Accepted-issuers is the union of the default store
   *  and the bundled roots — pinned leaves are deliberately NOT added as
   *  issuers, since they're leaves being matched exactly, not anchors. */
  private def compositeTrustManager(extra: Seq[X509Certificate], pinnedExpired: Seq[X509Certificate]): X509TrustManager = {
    val primary = trustManagerFor(null)
    if (extra.isEmpty && pinnedExpired.isEmpty) primary
    else {
      val secondary = if (extra.nonEmpty) Some(extraTrustManager(extra)) else None
      new X509TrustManager {
        override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit =
          try primary.checkServerTrusted(chain, authType)
          catch {
            case primaryFailure: CertificateException =>
              val secondaryResult = secondary match {
                case Some(s) => Try(s.checkServerTrusted(chain, authType))
                case None    => scala.util.Failure(primaryFailure)
              }
              secondaryResult.recover {
                case _: CertificateException if matchesPinnedLeaf(chain, pinnedExpired) => ()
              }.get
          }
        override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit =
          primary.checkClientTrusted(chain, authType)
        override def getAcceptedIssuers: Array[X509Certificate] =
          primary.getAcceptedIssuers ++ secondary.map(_.getAcceptedIssuers).getOrElse(Array.empty[X509Certificate])
      }
    }
  }
}
