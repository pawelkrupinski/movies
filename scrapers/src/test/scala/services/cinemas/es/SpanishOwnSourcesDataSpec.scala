package services.cinemas.es

import models.GeneratedTable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpanishOwnSourcesDataSpec extends AnyFlatSpec with Matchers {

  "the Spanish own sources" should "be the generated tables, row for row" in {
    SpanishOwnSourcesData.taquilla.size shouldBe GeneratedTable.rows("generated/es-own-sources/taquilla.tsv").size
    SpanishOwnSourcesData.artesiete.size shouldBe GeneratedTable.rows("generated/es-own-sources/artesiete.tsv").size
  }

  they should "name a venue's Taquilla id and path, and its Artesiete page" in {
    SpanishOwnSourcesData.taquilla.size shouldBe 60
    SpanishOwnSourcesData.artesiete.size shouldBe 10
    SpanishOwnSourcesData.taquilla("E0547") shouldBe TaquillaVenue("33199", "/murcia/neocine-thader-murcia")
    SpanishOwnSourcesData.ownSourced("E0547") shouldBe true
    SpanishOwnSourcesData.ownSourced(SpanishOwnSourcesData.artesiete.keys.head) shouldBe true
    SpanishOwnSourcesData.ownSourced("E0364") shouldBe false
  }
}
