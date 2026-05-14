package tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.CompletableFuture

class RealHttpFetch extends HttpFetch {
  private val underlying = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  override def get(url: String): String = {
    val resp = underlying.send(buildRequest(url), HttpResponse.BodyHandlers.ofString())
    if (resp.statusCode() != 200)
      throw new RuntimeException(s"HTTP ${resp.statusCode()} for $url")
    resp.body()
  }

  override def getAsync(url: String): CompletableFuture[String] =
    underlying.sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofString())
      .thenApply { response =>
        if (response.statusCode() == 200) response.body()
        else throw new RuntimeException(s"HTTP ${response.statusCode()} for $url")
      }

  override def post(url: String, body: String, contentType: String = "application/json"): String = {
    val req = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("Content-Type", contentType)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    val resp = underlying.send(req, HttpResponse.BodyHandlers.ofString())
    if (resp.statusCode() != 200)
      throw new RuntimeException(s"HTTP ${resp.statusCode()} for POST $url")
    resp.body()
  }

  private def buildRequest(url: String): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .GET()

    decorateBuilder(builder, url).build()
  }

  protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder = builder
}
