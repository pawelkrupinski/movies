package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BbfcCertificateSpec extends AnyFlatSpec with Matchers {

  "the title rules' CERT placeholder" should "be the BBFC list's public certificates and the pending one, nothing more" in {
    // it spelled "U|PG|12A?|15|18|TBC" itself, beside four UK clients' lists of their own
    val cert = services.titlerules.TitleRulePlaceholders.all("CERT").r
    (BbfcCertificate.Public :+ BbfcCertificate.Pending).foreach(value => withClue(value)(cert.matches(value) shouldBe true))
    Seq("R18", "12B", "16", "PG-13").foreach(value => withClue(value)(cert.matches(value) shouldBe false))
    BbfcCertificate.alternation(Seq("12A", "12")).r.findFirstIn("12A") shouldBe Some("12A")
  }
}
