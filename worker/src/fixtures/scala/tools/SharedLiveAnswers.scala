package tools

import java.util.concurrent.{CompletableFuture, CompletionException, ConcurrentHashMap}

/**
 * One live answer per request for every pass of a GAP-FILL convergence leg (`KINOWO_CONVERGENCE_FILL_ONLY`).
 *
 * Such a leg replays the recorded tree and asks the live web only what the tree lacks. Its order
 * passes run side by side in one JVM, so they miss the same request at the same moment, and each
 * fetched it for itself: three fetches of one Rotten Tomatoes page came back two with the Tomatometer
 * and one without, and the leg reported an arrival-order dependence the pipeline does not have (run
 * 36801876786). Wrapped here, the first pass to ask fetches; every other pass — at once or later —
 * gets that answer, or that failure.
 *
 * Kept for the leg's life, so only for what the tree lacks: a recording leg, which fetches everything
 * live, does not use it.
 */
final class SharedLiveAnswers {
  private val answers = new ConcurrentHashMap[String, CompletableFuture[Any]]()

  def over(live: HttpFetch): HttpFetch = new HttpFetch {
    def get(url: String): String = once(s"GET $url")(live.get(url))
    override def get(url: String, headers: Map[String, String]): String =
      once(s"GET $url ${headers.toSeq.sorted.mkString(",")}")(live.get(url, headers))
    override def getBytes(url: String): Array[Byte] = once(s"BYTES $url")(live.getBytes(url)).clone()
    def post(url: String, body: String, contentType: String): String =
      once(s"POST $url $contentType $body")(live.post(url, body, contentType))
  }

  private def once[A](request: String)(fetch: => A): A = {
    val mine  = new CompletableFuture[Any]()
    val first = answers.putIfAbsent(request, mine)
    if (first == null) {
      try mine.complete(fetch) catch { case e: Throwable => mine.completeExceptionally(e) }
      ()
    }
    try Option(first).getOrElse(mine).join().asInstanceOf[A]
    catch { case e: CompletionException if e.getCause != null => throw e.getCause }
  }
}
