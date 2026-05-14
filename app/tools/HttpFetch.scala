package tools

import java.util.concurrent.CompletableFuture

trait HttpFetch {
  def get(url: String): String
  def getAsync(url: String): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => get(url))

  /** POST with a string body. Default impl throws — only `RealHttpFetch`
   *  needs it, and test stubs override when the unit under test uses POST. */
  def post(url: String, body: String, contentType: String = "application/json"): String =
    throw new UnsupportedOperationException(s"HttpFetch.post not implemented (url=$url)")
}




