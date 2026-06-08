package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the Wrocław + Warszawa national-chain scrapes (Cinema City, Multikino,
 * Helios) into the shared `08-06-2026` end-to-end corpus, so the full wired
 * `cinemaScrapers` list has fixtures for every cinema. Additive only — these
 * cinemas' URLs don't collide with the Poznań fixtures already there, so
 * existing fixtures (and the page snapshots that depend on them) are untouched.
 * Scrape-only (no enrichment): the e2e specs are Poznań-scoped, so the other
 * cities' films don't need TMDB/IMDb/etc. fixtures.
 */
object RecordWroclawWarszawaChains {
  def main(args: Array[String]): Unit = {
    val rec = new RecordingHttpFetch("08-06-2026", new RealHttpFetch())

    val cc = new CinemaCityClient(rec)
    val cinemaCity = Seq(
      "1097" -> CinemaCityWroclavia, "1067" -> CinemaCityKorona,
      "1074" -> CinemaCityArkadia, "1061" -> CinemaCityBemowo, "1096" -> CinemaCityGaleriaPolnocna,
      "1069" -> CinemaCityJanki, "1070" -> CinemaCityMokotow, "1068" -> CinemaCityPromenada, "1060" -> CinemaCitySadyba
    )
    cinemaCity.foreach { case (id, cinema) =>
      println(s"Cinema City ${cinema.displayName} ($id)…")
      println(s"  ${Try(cc.fetch(id, cinema).size).fold(e => s"FAIL ${e.getMessage}", n => s"$n films")}")
    }

    // Record Multikino directly through `rec` (NOT MultikinoClient.fetchFor,
    // whose Zyte layer fetches outside RecordingHttpFetch and so records
    // nothing). The client's own homepage-warmup retry handles the cold-session
    // 401 against the direct endpoint.
    val mkFetch = rec
    val multikino = Seq(
      "0010" -> MultikinoPasazGrunwaldzki, "0013" -> MultikinoZloteTarasy, "0040" -> MultikinoMlociny,
      "0052" -> MultikinoReduta, "0024" -> MultikinoTargowek, "0025" -> MultikinoWolaPark
    )
    multikino.foreach { case (id, cinema) =>
      println(s"Multikino ${cinema.displayName} ($id)…")
      println(s"  ${Try(new MultikinoClient(mkFetch, id, cinema).fetch().size).fold(e => s"FAIL ${e.getMessage}", n => s"$n films")}")
    }

    Seq(HeliosNuxt.Magnolia, HeliosNuxt.AlejaBielany, HeliosNuxt.BlueCity).foreach { cfg =>
      println(s"Helios ${cfg.cinema.displayName}…")
      println(s"  ${Try(new HeliosClient(rec, cfg).fetch().size).fold(e => s"FAIL ${e.getMessage}", n => s"$n films")}")
    }
  }
}
