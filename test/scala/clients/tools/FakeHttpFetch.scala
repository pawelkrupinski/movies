package clients.tools

import tools.HttpFetch

import java.net.URI
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture

class FakeHttpFetch(fixtureDir: String) extends HttpFetch {
  val fixtureRoot = "test/resources/fixtures/" + fixtureDir

  override def get(url: String): String = {
    val uri  = new URI(url)
    val path = uri.getPath.stripPrefix("/")
    val base = s"$fixtureRoot/${uri.getHost}/$path"
    Seq(base, s"$base.html", s"$base.json")
      .map(Paths.get(_))
      .find(Files.exists(_))
      .map(path => new String(Files.readAllBytes(path), "UTF-8"))
      .getOrElse(throw new java.io.FileNotFoundException(
        s"No fixture file for $url — tried $base[.html|.json]"
      ))
  }

  override def getAsync(url: String): CompletableFuture[String] =
    try CompletableFuture.completedFuture(get(url))
    catch { case e: Exception =>
      val f = new CompletableFuture[String]()
      f.completeExceptionally(e)
      f
    }
}
