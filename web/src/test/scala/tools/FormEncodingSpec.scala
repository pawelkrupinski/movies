package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

class FormEncodingSpec extends AnyFlatSpec with Matchers {

  private def encoded(text: String, from: Int = 0): String =
    FormEncoding.append(text, from, new java.lang.StringBuilder).toString

  "FormEncoding.append" should "write exactly what URLEncoder does, for the URLs posters carry" in {
    Seq(
      "image.bilety24.pl/sf_api_thumb_400/dealer-default/235/poster.jpg?rev=abc&v=1",
      "kinomuza.pl/wp-content/uploads/Milcząca-przyjaciółka_plakat.jpg",
      "example.com/a b~c*d_e.f-g%20h+i",
      "example.com/📽️ ß Straße/ünïcödé",
      "",
    ).foreach(url => encoded(url) shouldBe URLEncoder.encode(url, UTF_8))
  }

  it should "encode only from the offset it is given" in {
    encoded("https://example.com/x?y=1", from = 8) shouldBe URLEncoder.encode("example.com/x?y=1", UTF_8)
  }

  it should "spell a lone surrogate half as the encoder does" in {
    Seq("a\uD83Db/", "a\uDE00b", "\uD83D", "x😀\uD83D/").foreach(text => encoded(text) shouldBe URLEncoder.encode(text, UTF_8))
  }

  it should "agree with URLEncoder on arbitrary text" in {
    val random = new scala.util.Random(20261003)
    val alphabet = "aZ09-_.*~ /?&=%+:#ąęłśżźćńó€ß日本😀🐀\uDE00"
    (1 to 5000).foreach { _ =>
      val text = Seq.fill(random.nextInt(24))(alphabet.charAt(random.nextInt(alphabet.length))).mkString
      encoded(text) shouldBe URLEncoder.encode(text, UTF_8)
    }
  }
}
