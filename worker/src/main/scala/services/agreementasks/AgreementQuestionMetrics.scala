package services.agreementasks

/** Where the agreement's questions report: each one enqueued (`added`, or already queued), and each one asked, by how it
 *  came out — [[Answered]], [[Nothing]] there (a refused query, a missing page: filed as no film), [[Fresh]] (an answer
 *  already filed: skipped), [[Waiting]] (inside the wait a failed ask left it: skipped), [[Deferred]] (the host's breaker
 *  open or its pace turned it away: remembered as a failure) or [[Failed]] (remembered, asked again once its wait ends). A family is its label (`imdb`, `rt` …), TMDB's find
 *  of an agreed IMDb id [[TmdbFind]], a TMDB record read again for its release day [[TmdbRecord]]. */
trait AgreementQuestionMetrics {
  def enqueued(family: String, added: Boolean): Unit
  def asked(family: String, outcome: String): Unit
  /** One inline ask ([[InlineFamilyAsks]]) of `family`'s, in two parts: how long it waited for the inline pool, then how
   *  long asking it took (the host's pace, the wire and its retries, the filing). */
  def inlineAsk(family: String, queuedSeconds: Double, askingSeconds: Double): Unit = ()
}

object AgreementQuestionMetrics {
  val Answered = "answered"; val Nothing = "nothing"; val Fresh = "fresh"; val Waiting = "waiting"; val Deferred = "deferred"
  val Failed = "failed"
  val Outcomes: Seq[String] = Seq(Answered, Nothing, Fresh, Waiting, Deferred, Failed)
  val TmdbFind = "tmdb-find"
  val TmdbRecord = "tmdb-record"
  /** A poster hashed for the agreement's poster evidence. */
  val Poster = "poster"
  /** A catalogue id batch mapped, or a venue page's catalogue links read, for the agreement's catalogue take. */
  val Catalogue = "catalogue"
  val Silent: AgreementQuestionMetrics = new AgreementQuestionMetrics {
    def enqueued(family: String, added: Boolean): Unit = ()
    def asked(family: String, outcome: String): Unit = ()
  }
}
