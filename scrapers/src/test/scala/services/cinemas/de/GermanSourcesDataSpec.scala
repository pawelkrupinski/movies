package services.cinemas.de

import models.GeneratedTable
import services.cinemas.common.KinoheldVenue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GermanSourcesDataSpec extends AnyFlatSpec with Matchers {

  "the generated German sources" should "read generated/de-sources/feeds.tsv and match the digest of the Scala literal it replaced" in {
    val dump = GermanSourcesData.byVenueKey.toSeq.sortBy(_._1).map { case (key, feeds) => s"$key -> ${feeds.mkString(", ")}" }.mkString("\n")
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(dump.getBytes("UTF-8")).map("%02x".format(_)).mkString
    digest shouldBe "6b809e15b28c603caf5aee0b6e77ffd070c8acc60674bd613ca02c7d3c1b7d6f"
    GeneratedTable.rows("generated/de-sources/feeds.tsv") should not be empty
  }

  "Rio Palast Nürnberg" should "fall back to its kinoheld cinema 1100 when Filmstarts lists nothing" in {
    // kinoheld's /kino/nuernberg/rio-palast page carries cinema 1100 (Fürther Straße 61), read
    // in the browser 2026-11-18; it lists no shows yet, so the harvest cannot match it by its programme.
    GermanSources.of("A1604", Some("/kino/nuernberg-mittelfranken/rio-palast-32905")) shouldBe Seq(
      GermanFeed.Filmstarts("A1604"),
      GermanFeed.Kinoheld(KinoheldVenue("1100", "/kino/nuernberg/rio-palast")),
      GermanFeed.Kinoprogramm("/kino/nuernberg-mittelfranken/rio-palast-32905"))
  }
}
