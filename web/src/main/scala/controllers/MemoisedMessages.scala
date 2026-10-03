package controllers

import play.api.i18n.{Lang, Messages}

/**
 * `underlying`, with each message asked for WITHOUT arguments looked up once and kept.
 *
 * WHY. Play's `DefaultMessagesApi.translate` builds a fresh `java.text.MessageFormat`
 * for every lookup, arguments or not, and a listing card asks for a handful of fixed
 * labels: ~1.1 MB of a Warsaw listing render whose cards were not cached. An
 * argument-less message is a pure function of its key for a fixed language, so the
 * kept string is the one a lookup would format. Lookups with arguments, `translate`
 * and `isDefinedAt` go straight through.
 *
 * Bounded: the keys are the templates' own, but a key built from data must not grow it
 * without limit — past [[MemoisedMessages.MaxKeys]] a new key is formatted every time.
 */
final class MemoisedMessages(underlying: Messages) extends Messages {
  private val plain = new java.util.concurrent.ConcurrentHashMap[String, String]()

  def lang: Lang = underlying.lang

  def apply(key: String, args: Any*): String =
    if (args.nonEmpty) underlying(key, args*)
    else {
      val kept = plain.get(key)
      if (kept != null) kept
      else {
        val formatted = underlying(key)
        if (plain.size < MemoisedMessages.MaxKeys) plain.putIfAbsent(key, formatted)
        formatted
      }
    }

  def apply(keys: Seq[String], args: Any*): String = underlying(keys, args*)
  def translate(key: String, args: Seq[Any]): Option[String] = underlying.translate(key, args)
  def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
  def asJava: play.i18n.Messages = underlying.asJava
}

object MemoisedMessages {
  /** Every message key the templates use, several times over. */
  val MaxKeys: Int = 4096
}
