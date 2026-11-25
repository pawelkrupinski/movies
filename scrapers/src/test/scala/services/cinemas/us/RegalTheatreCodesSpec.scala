package services.cinemas.us

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.MessageDigest

class RegalTheatreCodesSpec extends AnyFlatSpec with Matchers {

  "RegalTheatreCodes.bySlug" should "hold exactly the 401 pairs the generated TSV held before it was restored as a literal" in {
    val tsv = RegalTheatreCodes.bySlug.toSeq.sorted.map { case (slug, code) => s"$slug\t$code\n" }.mkString
    val digest = MessageDigest.getInstance("SHA-256").digest(tsv.getBytes("UTF-8")).map("%02x".format(_)).mkString

    RegalTheatreCodes.bySlug should have size 401
    RegalTheatreCodes.bySlug("regal-aiken-mall") shouldBe "0252"
    RegalTheatreCodes.bySlug("regal-albany") shouldBe "0833"
    digest shouldBe "a9bf4674afe0a706f2f3a1721e96fd6ee8e73ec876afb2cc8763c8d35b130d4f"
  }
}
