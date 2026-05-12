package tools

import java.util.concurrent.CompletableFuture

trait HttpFetch {
  def get(url: String): String
  def getAsync(url: String): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => get(url))
}




