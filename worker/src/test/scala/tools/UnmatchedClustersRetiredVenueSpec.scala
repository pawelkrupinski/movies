package tools

import models.{Country, RegalStowmarket}
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Listing
import services.movies.ListingKey

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/** A capture recorded before a venue left the roster holds that venue's listings, which this code's roster cannot place.
 *  Identity model convergence #1041: e7fb123c0f retired Germany's "Kino in Dorfen" (sKino listed twice), and every
 *  Germany drift row and the takes ratchet died reading the pinned recording's capture ("no venue named Kino in
 *  Dorfen") -- while the convergence row, replaying the same recording's scrapes, simply had no such venue. A retired
 *  venue's listings are no input to this code's model, as they are none to production's once it stops scraping it. */
class UnmatchedClustersRetiredVenueSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private val uk = Country.all.find(_.code == "uk").get

  private def listing(title: String): Listing = {
    val page = s"https://regalstowmarket.co.uk/RegalStowmarket.dll/WhatsOn?f=${title.hashCode.abs}"
    Listing(RegalStowmarket, ListingKey.Native("Regal Stowmarket", page, title), title, title, title, None, Nil, Some(150), Some(page), None)
  }

  /** The capture of `titles` written to a file, then its first listing's venue (and key) renamed to `venue`, `key` the
   *  serialised key the first listing is given. */
  private def captureWith(titles: Seq[String], venue: String, key: Option[String] = None): Path = {
    val file = tempDirectory("capture-retired").resolve("uk.json.gz")
    UnmatchedClusters.write(file, UnmatchedClusters.Capture(uk, titles.map(listing), Nil, Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, Map.empty))
    val in = new GZIPInputStream(Files.newInputStream(file))
    val d  = try BsonDocument.parse(new String(in.readAllBytes(), StandardCharsets.UTF_8)) finally in.close()
    val first = d.getArray("listings").get(0).asDocument
    first.put("cinema", new BsonString(venue))
    key.foreach(k => first.put("key", new BsonString(k)))
    val out = new GZIPOutputStream(Files.newOutputStream(file))
    try out.write(d.toJson.getBytes(StandardCharsets.UTF_8)) finally out.close()
    file
  }

  "A capture holding a retired venue's listing" should "read back without it, and with every other listing" in {
    val file = captureWith(Seq("Hirten", "The Wizard of Oz"), venue = "Kino in Dorfen")
    UnmatchedClusters.read(file).listings.map(_.rawTitle) shouldBe Seq("The Wizard of Oz")
  }

  it should "still refuse a listing it cannot read for any other reason" in {
    val file = captureWith(Seq("Hirten", "The Wizard of Oz"), venue = "Regal Stowmarket", key = Some("not a key"))
    an[RuntimeException] should be thrownBy UnmatchedClusters.read(file)
  }
}
