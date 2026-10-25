package tools

import models.Country
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{StoredCorrection, StoredFill, StoredVerdict}
import services.movies.ListingKey

/** Prod's state as the local mirror holds it ([[ProdMirror]]): its filed family answers preferred over a capture's, and
 *  one listing's prod side — the model's stored decision, the agreement's stored verdict — read as the take prod serves. */
class ProdMirrorSpec extends AnyFlatSpec with Matchers {

  private val uk  = Country.all.find(_.code == "uk").get
  private val panto = ListingKey.Native("Cineworld Leigh", "https://cineworld.co.uk/films/1", "CBeebies Panto")
  private def doc(answer: String) = new BsonDocument("answer", new BsonString(answer))

  "The mirror's family answers" should "win over the capture's, and keep the capture's the mirror lacks" in {
    val capture = FiledAnswers(FiledAnswers.of(uk).capture.copy(
      families = Map("imdb|title|Panto" -> doc("captured"), "rt|title|Panto" -> doc("captured only"))), "uk.json.gz")
    val both = FiledAnswers.preferring(capture, Map("imdb|title|Panto" -> doc("prod now")), "prod mirror kinowo_uk_prod_mirror")
    both.families shouldBe Map("imdb|title|Panto" -> doc("prod now"), "rt|title|Panto" -> doc("captured only"))
    both.source shouldBe "prod mirror kinowo_uk_prod_mirror (over uk.json.gz)"
  }

  private val unmatched = ProdSide.Model(None, "NoCandidate", Seq(panto))
  private def verdict(fill: Option[StoredFill] = None, correction: Option[StoredCorrection] = None) =
    StoredVerdict("id", 1L, Map.empty, None, filled = fill, correction = correction)

  "Prod's take" should "be the model's film when the agreement stored nothing" in {
    ProdSide(panto, Some(unmatched.copy(film = Some(550), basis = "OwnMatch")), None, None, None, Map.empty).take shouldBe ("tmdb:550", "OwnMatch")
  }

  it should "be the fill's film where the model took none" in {
    ProdSide(panto, Some(unmatched), None, Some(verdict(fill = Some(StoredFill("venues.current", Some(1710881), None, "filled")))), None, Map.empty)
      .take shouldBe ("tmdb:1710881", "Filled venues.current")
  }

  it should "be the correction's film over the model's" in {
    val corrected = verdict(correction = Some(StoredCorrection(Some(42), Some("imdb, wiki take tmdb 42"), waiting = false)))
    ProdSide(panto, Some(unmatched.copy(film = Some(550), basis = "OwnMatch")), None, None, Some(corrected), Map.empty).take shouldBe ("tmdb:42", "Corrected")
  }

  it should "stay the model's when a correction corrects nothing" in {
    val none = verdict(correction = Some(StoredCorrection(None, None, waiting = false)))
    ProdSide(panto, Some(unmatched.copy(film = Some(550), basis = "OwnMatch")), None, None, Some(none), Map.empty).take shouldBe ("tmdb:550", "OwnMatch")
  }

  it should "be none when prod holds no record of the listing" in {
    ProdSide(panto, None, None, None, None, Map.empty).take shouldBe ("missing", "-")
  }

  "The verdict's id" should "be the cluster's smallest listing, as the agreement stage files it" in {
    val other = ListingKey.Native("Abbeygate", "https://flicks.co.uk/movie/panto/", "CBeebies Panto")
    ProdSide.clusterId(Seq(panto, other)) shouldBe ListingKey.serialised(other)
  }
}
