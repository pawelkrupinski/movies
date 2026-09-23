package modules

import com.typesafe.config.{ConfigFactory, ConfigResolveOptions}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

/** The session cookie's `Secure` flag follows `KINOWO_SESSION_SECURE`: off by
 *  default (localhost, the page-test servers and Playwright's WebKit all run
 *  over plain http, where a `Secure` cookie would never come back), on wherever
 *  a deployment sets it — production is HTTPS-only behind Caddy's HSTS. */
class SessionCookieConfigSpec extends AnyFlatSpec with Matchers {

  // application.conf alone, its `${?VAR}` substitutions resolved against `env`
  // (placed at the root, where HOCON looks them up) instead of the real
  // environment, so the spec can't pick up whatever the shell exported.
  private def sessionSecure(env: Map[String, String]): Boolean =
    ConfigFactory.parseMap(env.asJava)
      .withFallback(ConfigFactory.parseResources("application.conf"))
      .resolve(ConfigResolveOptions.noSystem())
      .getBoolean("play.http.session.secure")

  "the session cookie" should "not be Secure by default" in {
    sessionSecure(Map.empty) shouldBe false
  }

  it should "be Secure when KINOWO_SESSION_SECURE=true" in {
    sessionSecure(Map("KINOWO_SESSION_SECURE" -> "true")) shouldBe true
  }

  // Every write route is `nocsrf`, so SameSite=Lax on the session cookie is what
  // keeps a cross-site form POST from carrying it (`CrossSiteWriteFilter` is the
  // layer that does not depend on it). It comes from Play's own reference.conf;
  // this pins that nothing of ours loosens it.
  it should "be SameSite=Lax" in {
    ConfigFactory.parseResources("application.conf")
      .withFallback(ConfigFactory.defaultReference())
      .resolve(ConfigResolveOptions.noSystem().setAllowUnresolved(true))
      .getString("play.http.session.sameSite") shouldBe "lax"
  }
}
