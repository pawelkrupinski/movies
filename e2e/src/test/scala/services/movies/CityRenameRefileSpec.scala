package services.movies

import models.CityScreening
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CityRenameRefileSpec extends AnyFlatSpec with Matchers {
  private def row(film: String, city: String, cinema: String, url: Option[String] = None) =
    CityScreening(s"$film|$city|$cinema", film, city, cinema, url, Seq.empty)

  /** The convergence suite's original row-by-row application: per rename in order, each row of the
   *  city deleted and its refiled copy upserted. */
  private def rowByRow(rows: Seq[CityScreening], renames: Seq[(String, String)]): Map[String, CityScreening] = {
    val store = scala.collection.mutable.LinkedHashMap.from(rows.map(r => r._id -> r))
    renames.foreach { case (current, former) =>
      store.values.filter(_.city == current).toSeq.foreach { sc =>
        val moved = sc.copy(_id = sc._id.replace(s"|$current|", s"|$former|"), city = former)
        store.remove(sc._id)
        store.update(moved._id, moved)
      }
    }
    store.toMap
  }

  private def applied(rows: Seq[CityScreening], refile: CityRenameRefile): Map[String, CityScreening] =
    rows.map(r => r._id -> r).toMap -- refile.deletes ++ refile.upserts.map(r => r._id -> r)

  "a city rename's refile" should "leave the store exactly as the row-by-row writes did" in {
    val rows = Seq(
      row("f1", "anchorage", "Bear Tooth"), row("f2", "anchorage", "Bear Tooth"),
      row("f1", "oahu", "Kahala"), row("f3", "chicago", "Music Box"),
      // An old-slug row already present: the refiled f4 takes its id and replaces it.
      row("f4", "anchorage", "Alaska Experience", Some("new")), row("f4", "alaska", "Alaska Experience", Some("old")),
      // Renamed on again by a later rename: x → y, then y → z.
      row("f5", "x", "Chain"))
    val renames = Seq("anchorage" -> "alaska", "oahu" -> "hawaii", "x" -> "y", "y" -> "z")
    val refile  = CityRenameRefile.of(rows, renames)
    applied(rows, refile) shouldBe rowByRow(rows, renames)
    refile.upserts.map(_._id) should contain theSameElementsAs Seq(
      "f1|alaska|Bear Tooth", "f2|alaska|Bear Tooth", "f4|alaska|Alaska Experience", "f1|hawaii|Kahala", "f5|z|Chain")
    withClue("an id a later rename moves on is never written: ")(refile.upserts.map(_._id) should not contain "f5|y|Chain")
    refile.upserts.find(_._id == "f4|alaska|Alaska Experience").flatMap(_.filmUrl) shouldBe Some("new")
    refile.deletes should contain theSameElementsAs Seq(
      "f1|anchorage|Bear Tooth", "f2|anchorage|Bear Tooth", "f4|anchorage|Alaska Experience", "f1|oahu|Kahala", "f5|x|Chain")
  }

  it should "write nothing for a city with no rows, and leave every other city's rows alone" in {
    val rows = Seq(row("f1", "chicago", "Music Box"))
    CityRenameRefile.of(rows, Seq("anchorage" -> "alaska")) shouldBe CityRenameRefile(Nil, Nil)
  }
}
