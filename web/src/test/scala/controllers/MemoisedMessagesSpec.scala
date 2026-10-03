package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.i18n.{Lang, Messages}

class MemoisedMessagesSpec extends AnyFlatSpec with Matchers {

  private val api = testsupport.TestMessages.messagesApi

  "MemoisedMessages" should "answer every key exactly as the messages it wraps, with and without arguments" in {
    for (lang <- Seq("pl", "en", "de")) {
      val underlying = api.preferred(Seq(Lang(lang)))
      val memoised   = new MemoisedMessages(underlying)
      val keys       = api.messages.getOrElse(lang, Map.empty).keys.toSeq :+ "no.such.key"
      keys.foreach { key =>
        memoised(key) shouldBe underlying(key)
        memoised(key) shouldBe underlying(key)               // the kept copy
        memoised(key, 7, "x") shouldBe underlying(key, 7, "x")
        memoised.isDefinedAt(key) shouldBe underlying.isDefinedAt(key)
      }
      memoised.lang shouldBe underlying.lang
    }
  }

  it should "format an argument-less key once" in {
    var lookups = 0
    val underlying = api.preferred(Seq(Lang("pl")))
    val counting = new Messages {
      def lang = underlying.lang
      def apply(key: String, args: Any*) = { lookups += 1; underlying(key, args*) }
      def apply(keys: Seq[String], args: Any*) = underlying(keys, args*)
      def translate(key: String, args: Seq[Any]) = underlying.translate(key, args)
      def isDefinedAt(key: String) = underlying.isDefinedAt(key)
      def asJava = underlying.asJava
    }
    val memoised = new MemoisedMessages(counting)
    (1 to 5).foreach(_ => memoised("poster.missing"))
    lookups shouldBe 1
  }
}
