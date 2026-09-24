package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The worker draws the share card's director label from [[ShareCardText]], having no Play
 *  `Messages`; the web's own word for it is `detail.director`. The two must not drift apart. */
class ShareCardTextSpec extends AnyFlatSpec with Matchers {

  private def message(file: String, key: String): Option[String] =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(s"/$file"), "UTF-8").getLines().toSeq
      .collectFirst { case line if line.startsWith(s"$key=") => line.stripPrefix(s"$key=").trim }

  "The share card's director label" should "be the web's detail.director in every language the site ships" in {
    Seq("pl" -> "messages", "en" -> "messages.en", "de" -> "messages.de", "es" -> "messages.es").foreach { case (lang, file) =>
      withClue(s"$lang ($file): ") { Some(ShareCardText.directorLabel(lang)) shouldBe message(file, "detail.director") }
    }
  }

  it should "cover every language a country renders its pages in" in {
    models.Country.all.map(ShareCardText.language).toSet shouldBe Set("pl", "en", "de", "es")
  }
}
