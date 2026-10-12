package tools

import models.{Country, KinoApollo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{DetailFacts, Listing}
import services.movies.ListingKey

/** A capture whose venue detail a host refused wholesale — the UK capture of 2026-10-12 (run 38308006014) over recording
 *  38103789067, whose every Cineworld box-office API read had been 403'd: 384 listings marked as having detail, none of
 *  it stored, and the agreement stage lost the 77 "RBO Cinema Season 2026-27: Macbeth" takes it reads off that detail. */
class UnmatchedClustersDetailSpec extends AnyFlatSpec with Matchers {

  private def listing(page: String): Listing = {
    val title = page.split('/').last
    Listing(KinoApollo, ListingKey.Native("Kino Apollo", page, title), title, title, title, None, Nil, None, Some(page), None)
  }
  private def key(l: Listing) = ListingKey.serialised(l.key)
  private val facts = Some(DetailFacts(Some(2026), Seq("Louisa Proske"), Some(210), None))

  private val cineworld = (1 to UnmatchedClusters.RefusedHostListings).map(n => listing(s"https://www.cineworld.co.uk/films/$n-macbeth"))
  private val omniplex  = Seq("macbeth", "tosca").map(s => listing(s"https://www.omniplex.ie/whatson/$s"))
  // one page of a small house left unread (PL, run 38315368375: Kino Marzenie's) is no refusal of the recording's reads
  private val marzenie  = Seq(listing("https://www.kinomarzenie.pl/repertuar/macbeth"))

  private def capture(details: Map[String, Option[DetailFacts]]) =
    UnmatchedClusters.Capture(Country.all.head, cineworld ++ omniplex ++ marzenie, Nil, Map.empty, Map.empty, details,
      (cineworld ++ omniplex ++ marzenie).map(key).toSet, Map.empty, Map.empty)

  "A capture's detail check" should "name the host it marks as having detail for every listing of which it stores none" in {
    val partlyRead = (omniplex.take(1).map(l => key(l) -> facts) ++ omniplex.drop(1).map(l => key(l) -> None)).toMap
    UnmatchedClusters.hostsMissingDetail(capture(partlyRead)) shouldBe Map("www.cineworld.co.uk" -> UnmatchedClusters.RefusedHostListings)
  }

  it should "pass a capture holding some detail of every host it marks" in {
    val read = (cineworld.take(1) ++ omniplex.take(1)).map(l => key(l) -> facts).toMap
    UnmatchedClusters.hostsMissingDetail(capture(read)) shouldBe empty
  }
}
