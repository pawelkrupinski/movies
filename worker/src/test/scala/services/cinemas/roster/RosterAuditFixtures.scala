package services.cinemas.roster

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Venue source pages recorded 2026-09-23 for the online roster audit's reader
 *  and comparator. Each was fetched from the URL its name gives:
 *    bilety24-organiser-<id>-*.html   https://www.bilety24.pl/kino/organizator/<slug>-<id>
 *                                     (scripts, styles and SVG sprites stripped)
 *    filmweb-cinema-info-<id>-*.json  https://www.filmweb.pl/api/v1/cinema/<id>/info
 */
object RosterAuditFixtures {
  def page(name: String): String =
    new String(Files.readAllBytes(Paths.get(s"worker/src/test/resources/fixtures/roster-audit/$name")), StandardCharsets.UTF_8)

  val Sroda477      = "bilety24-organiser-477-sroda-wielkopolska.html"
  val Ostrowiec1389 = "bilety24-organiser-1389-ostrowiec.html"
  val Konin1626     = "bilety24-organiser-1626-konin.html"
  val EtiudaObk3024 = "filmweb-cinema-info-3024-etiuda-obk.json"
  val Kolo1526      = "filmweb-cinema-info-1526-kolo.json"
  val Braniewo2352  = "filmweb-cinema-info-2352-braniewo.json"
  val Wars2348      = "filmweb-cinema-info-2348-wysokie-mazowieckie.json"
  val Slawa1658     = "filmweb-cinema-info-1658-slawa-miedzyrzec.json"
  val ZaRogiem1850  = "filmweb-cinema-info-1850-za-rogiem-miedzyrzec.json"

  def filmwebInfo(id: Int): String = s"https://www.filmweb.pl/api/v1/cinema/$id/info"
}
