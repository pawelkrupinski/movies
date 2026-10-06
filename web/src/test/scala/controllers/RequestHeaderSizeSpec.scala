package controllers

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.util.ContextInitializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import play.api.{Configuration, Environment, Mode}
import play.api.mvc.Results
import play.api.routing.sird.*
import play.core.server.{PekkoHttpServer, ServerConfig}

/** The request-header ceiling of the server we actually ship, on the config we ship.
 *
 *  Every cookie set on `.kinowo.net` rides along on every Polish page request, and the
 *  fleet's Google sign-in (oauth2-proxy, for grafana./logs./headlamp.) sets its sealed
 *  session plus a CSRF cookie per sign-in there. Together they pushed a signed-in
 *  browser's `Cookie` header past Play's 8k default, and every Polish page answered
 *  431 "HTTP header value exceeds the configured limit of 8192 characters".
 */
class RequestHeaderSizeSpec extends AnyFlatSpec with Matchers {

  "the web server" should "serve a request whose Cookie header is far past the 8k default" in {
    val server = PekkoHttpServer.fromRouterWithComponents(
      ServerConfig(port = Some(0), address = "127.0.0.1", mode = Mode.Test)
        .copy(configuration = Configuration.load(Environment.simple(mode = Mode.Test)))
    )(components => { case GET(p"/") => components.defaultActionBuilder(Results.Ok("ok")) })
    try {
      val cookie = (1 to 6).map(i => s"_oauth2_proxy_$i=" + "a" * 3000).mkString("; ")
      val response = HttpClient.newHttpClient().send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:${server.httpPort.get}/"))
          .header("Cookie", cookie)
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      response.statusCode shouldBe 200
    } finally {
      server.stop()
      // `Server.stop` shuts logback down for the whole test JVM; put logback.xml back
      // so the specs that run after this one (LogbackConfigSpec) see the real config.
      val logback = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      logback.reset()
      new ContextInitializer(logback).autoConfig()
    }
  }
}
