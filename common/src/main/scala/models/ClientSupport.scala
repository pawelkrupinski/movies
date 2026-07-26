package models

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Properties

import scala.util.Using

/**
 * Which mobile client versions this server still serves, and where to send the
 * ones it doesn't — the payload behind `GET /api/client-support`.
 *
 * Shaped like [[Catalog]] on purpose: a static, deterministic body plus a content
 * [[etag]], so the apps can revalidate cheaply and an unchanged answer costs a
 * `304` with no body. Both are country-AGNOSTIC, so every deployment serves the
 * same bytes.
 *
 * The server publishes MINIMUMS rather than a per-request verdict. That keeps the
 * response cacheable and identical for everyone (a verdict would vary by query and
 * could not be revalidated), and the comparison a client has to do is one tuple
 * compare — see [[ClientVersion.isSupported]], which is the reference the Kotlin
 * and Swift implementations mirror.
 *
 * Values come from the committed `client-support.properties`; dropping support is
 * a deploy, deliberately, so the decision sits in git history beside the release
 * that forced it.
 */
object ClientSupport {

  /**
   * One platform's rule. GATED only when both halves are present: a minimum with
   * nowhere to send people would strand them in a dead app, and a store URL with
   * no minimum gates nobody. Either missing means "everything is supported", which
   * is why an unpublished iOS app simply never nags.
   */
  final case class Platform(minimumVersion: Option[String], storeUrl: Option[String]) {
    val isGated: Boolean = minimumVersion.isDefined && storeUrl.isDefined
  }

  private val props: Properties = {
    val p = new Properties()
    Using.resource(getClass.getResourceAsStream("/client-support.properties")) { in =>
      require(in != null, "client-support.properties is missing from the classpath")
      p.load(in)
    }
    p
  }

  /** A trimmed property, or None when absent or blank — blank is how the file says
   *  "not set yet" (see ios.storeUrl). */
  private def opt(key: String): Option[String] =
    Option(props.getProperty(key)).map(_.trim).filter(_.nonEmpty)

  private def platform(prefix: String): Platform = {
    val min = opt(s"$prefix.minimumVersion")
    // Fail at startup, not at the first client request: a typo'd version here
    // would otherwise gate every user out of the app on a Friday evening.
    min.foreach(v =>
      require(ClientVersion.parse(v).isDefined, s"$prefix.minimumVersion is not a version: '$v'"))
    val url = opt(s"$prefix.storeUrl")
    url.foreach(u => require(u.startsWith("https://"), s"$prefix.storeUrl must be https: '$u'"))
    Platform(min, url)
  }

  val android: Platform = platform("android")
  val ios: Platform     = platform("ios")

  /**
   * Canonical, deterministic JSON: `{"android":{…},"ios":{…}}`. An ungated
   * platform still appears, with nulls, so a client can tell "no rule" from "I
   * failed to parse the response" — the two must not look alike when the answer
   * decides whether the app is usable. Hand-built, like [[Catalog.json]]: `common`
   * has no play-json, and none of these values need escaping (the URLs are
   * validated as https above).
   */
  val json: String = {
    def obj(p: Platform): String = {
      val min = p.minimumVersion.map(v => s""""$v"""").getOrElse("null")
      val url = p.storeUrl.map(u => s""""$u"""").getOrElse("null")
      s"""{"minimumVersion":$min,"storeUrl":$url}"""
    }
    s"""{"android":${obj(android)},"ios":${obj(ios)}}"""
  }

  /** Content hash — the payload has no meaningful timestamp, so its identity IS
   *  its bytes, exactly as for [[Catalog.etag]]. */
  val etag: String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(json.getBytes(StandardCharsets.UTF_8))
    "\"" + digest.take(8).map("%02x".format(_)).mkString + "\""
  }
}

/**
 * Dotted numeric version comparison, and the one rule that decides whether a
 * client is still allowed in. Lives here so the contract has exactly one
 * definition the Kotlin and Swift gates are written against — three
 * implementations of "is 1.10 newer than 1.9" is three chances to get it wrong.
 */
object ClientVersion {

  /** `"1.2.3"` → `Some(Seq(1,2,3))`; None for anything non-numeric. Trailing parts
   *  are optional, so `"1.0"` and `"1.0.0"` both parse and compare equal. */
  def parse(version: String): Option[Seq[Int]] = {
    val parts = version.trim.split('.').toSeq
    Option.when(parts.nonEmpty && parts.forall(p => p.nonEmpty && p.forall(_.isDigit)))(
      parts.map(_.toInt))
  }

  /** True when `version` is at least `minimum`. Compared component-wise with
   *  missing components read as 0, so 1.10 > 1.9 (which a string compare gets
   *  backwards) and 1.0 == 1.0.0. */
  def isAtLeast(version: String, minimum: String): Option[Boolean] =
    for { v <- parse(version); m <- parse(minimum) } yield {
      val width = math.max(v.length, m.length)
      def pad(s: Seq[Int]) = s.padTo(width, 0)
      pad(v).zip(pad(m)).collectFirst { case (a, b) if a != b => a > b }.getOrElse(true)
    }

  /**
   * Whether a client on `version` may keep using an ungraded/gated platform.
   *
   * Unsupported ONLY when the platform is gated AND the version parses AND it is
   * below the minimum. An unparseable client version is treated as SUPPORTED: a
   * malformed version string is our bug or a bad build, and locking someone out of
   * a working app over it is the worse failure of the two.
   */
  def isSupported(version: String, platform: ClientSupport.Platform): Boolean =
    platform.minimumVersion match {
      case Some(min) if platform.isGated => isAtLeast(version, min).getOrElse(true)
      case _                             => true
    }
}
