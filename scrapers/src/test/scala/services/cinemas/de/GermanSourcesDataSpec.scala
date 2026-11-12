package services.cinemas.de

import models.GeneratedTable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GermanSourcesDataSpec extends AnyFlatSpec with Matchers {

  "the generated German sources" should "read generated/de-sources/feeds.tsv and match the digest of the Scala literal it replaced" in {
    val dump = GermanSourcesData.byVenueKey.toSeq.sortBy(_._1).map { case (key, feeds) => s"$key -> ${feeds.mkString(", ")}" }.mkString("\n")
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(dump.getBytes("UTF-8")).map("%02x".format(_)).mkString
    digest shouldBe "0ef619b913c1e7ffc654c8208d8fca2c75ecf238d030077808c45d0f82a23631"
    GeneratedTable.rows("generated/de-sources/feeds.tsv") should not be empty
  }
}
