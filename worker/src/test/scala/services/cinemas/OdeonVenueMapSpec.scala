package services.cinemas

import clients.tools.FakeHttpFetch
import models.Cinema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.uk.OdeonClient

import java.time.LocalDate
import scala.io.{Codec, Source}
import scala.util.Using

/**
 * `docs/venue-maps/ODEON-VENUE-MAP.tsv` is a capture of Odeon's live
 * `/ocapi/v1/sites` roster, and it is the source of truth for which `siteId` the
 * catalogue may wire. This spec holds the two together in both directions.
 *
 * The direction that matters is "wired but not in the roster": Odeon closed
 * Basingstoke on 31 Aug 2026 and dropped site `800` from its estate, after which
 * every `film-screening-dates?siteIds=800` answered `400 Invalid site identifier`.
 * Nothing failed — the venue simply went red on /uptime, its flicks fallback had
 * no showtimes to serve either, and it sat there for a week scraping nothing. A
 * re-harvest that drops a site now fails the build instead.
 */
class OdeonVenueMapSpec extends AnyFlatSpec with Matchers {

  // Constructing the catalogue does no network I/O — fetch() is never called here.
  private val catalog = new CinemaScraperCatalog(new FakeHttpFetch("multikino"), LocalDate.of(2026, 6, 8))

  private val wired: Map[String, Cinema] =
    catalog.byCity.values.flatten.collect { case o: OdeonClient => o.siteId -> o.cinema }.toMap

  /** `siteId → case-object name`, skipping comments and the Republic-of-Ireland
   *  rows we deliberately don't model (marked UNMATCHED, no case object). */
  private val roster: Map[String, String] =
    Using.resource(Source.fromFile(OdeonVenueMapSpec.VenueMapPath)(using Codec.UTF8)) { source =>
      source.getLines()
        .filterNot(line => line.startsWith("#") || line.trim.isEmpty)
        .map(_.split("\t", -1))
        .collect { case Array(siteId, _, caseObject, _*) if caseObject != "UNMATCHED" => siteId -> caseObject }
        .toMap
    }

  "Every wired Odeon siteId" should "still be in the captured /ocapi/v1/sites roster" in {
    wired.keySet.diff(roster.keySet) shouldBe empty
  }

  "Every roster row naming a case object" should "be wired in the catalogue" in {
    roster.keySet.diff(wired.keySet) shouldBe empty
  }

  it should "be wired to the cinema the roster names" in {
    val mismatched = wired.collect {
      case (siteId, cinema) if roster.get(siteId).exists(_ != caseObjectName(cinema)) =>
        s"$siteId wired to ${caseObjectName(cinema)}, roster says ${roster(siteId)}"
    }
    assert(mismatched.isEmpty, mismatched.mkString("; "))
  }

  /** The case-object name the venue map's third column carries — `Cinema` is a
   *  sealed abstract class, so the object's own name only shows up reflectively. */
  private def caseObjectName(cinema: Cinema): String =
    cinema.getClass.getSimpleName.stripSuffix("$")
}

object OdeonVenueMapSpec {
  /** Relative to the build root, the working directory every sbt test run uses. */
  val VenueMapPath = "docs/venue-maps/ODEON-VENUE-MAP.tsv"
}
