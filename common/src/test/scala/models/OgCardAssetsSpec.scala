package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/**
 * Every shareable page names a committed Open Graph card, and this pins that
 * the named file actually EXISTS — for every country, not just the one whose
 * cards someone remembered to generate.
 *
 * The city index emits `og-{slug}.jpg` and the `/` landing emits
 * [[Country.homeOgImage]], both as absolute prod URLs (see `_ogTagsApp`). A
 * country whose cards were never generated therefore doesn't degrade to a
 * generic card — it points Facebook, Messenger, Slack and X at a 404 and the
 * link preview comes back with no image at all. That is exactly what Germany
 * (158 regions) and the United States (55 states + their metros) shipped with:
 * `og-*.png` covered Poland and the UK alone, because the weekly refresh
 * workflow only ever ran those two legs, and nothing failed when the other two
 * countries went live.
 *
 * A city added by a commit cannot have its card in that same commit: the
 * generator SCREENSHOTS the live page, and the new city's page does not exist
 * upstream until the commit creating it has deployed — and the deploy is gated
 * on this suite, so a red run here would block the very page the card needs.
 * A roster change that adds cities therefore lists their cards in a temporary,
 * exact `awaitingFirstDeploy` set (see 6c45248c0 for the shape); once deployed,
 * generate them with `KINOWO_COUNTRY=<code> sbt "web/PageTest/runMain
 * tools.OgCardGenerator <slug>…"` (about five seconds a card against prod) and
 * delete the set in the same commit as the cards.
 *
 * Runs off the filenames rather than the rendered HTML on purpose: the page
 * specs (`RepertoirePreviewMetaSpec`, `LandingViewSpec`) already pin the
 * URL a page emits, so what is left to prove is that the other end of that URL
 * is on disk — cheap to check for all 739 cards, where rendering 739 pages
 * would not be.
 */
class OgCardAssetsSpec extends AnyFlatSpec with Matchers {

  private val cards: File = testsupport.RepoRoot.file("web/src/main/assets/img")

  private def missing(names: Seq[String]): Seq[String] = names.filterNot(new File(cards, _).exists())

  "every country" should "have the landing card its `/` page names" in {
    missing(Country.all.map(_.homeOgImage)) shouldBe empty
  }

  /** Cities whose card cannot exist yet: their page is not live until the
   *  commit creating them has deployed, and the deploy is gated on this suite.
   *  Asserted to be EXACTLY the set still missing, so a generated card fails
   *  here until its entry is deleted — the list cannot become the place missing
   *  cards quietly go.
   *
   *  2026-10-09 — Spain's 201 new town and cluster pages (provinces → towns). */
  private val awaitingFirstDeploy: Set[String] = Seq(
    "adeje", "aguilar-de-campoo", "aguilas", "alcala-de-henares", "alcala-de-xivert", "alcala-la-real",
    "alcaniz", "alcazar-de-san-juan", "alcobendas", "alcorcon", "alcoy", "algeciras", "alhaurin-el-grande",
    "almazan", "almendralejo", "alzira", "amposta", "andujar", "antequera", "aranda-de-duero",
    "arcos-de-la-frontera", "arenas-de-san-pedro", "arrecife", "arroyo-de-la-encomienda", "astorga",
    "ayamonte", "badalona", "barakaldo", "barbastro", "barbate", "baza", "beasain", "bejar", "benidorm",
    "berga", "bilbao", "binefar", "blanes", "boltana", "bunol", "burgo-de-osma", "calahorra", "calatayud",
    "calpe", "camargo", "carballo", "cartagena", "castellon-de-la-plana", "cee", "ciudad-rodrigo",
    "ciutadella-de-menorca", "coria", "cornella-de-llobregat", "cortegana", "corvera-de-asturias",
    "coslada", "daimiel", "don-benito", "dos-hermanas", "ecija", "eibar", "el-ejido", "el-pont-de-suert",
    "el-puerto-de-santa-maria", "el-vendrell", "elche", "estella-lizarra", "estepa", "ferrol", "figueres",
    "fuengirola", "fuenlabrada", "galdakao", "gandia", "getafe", "gijon", "golmayo", "granollers", "guardo",
    "herrera-del-duque", "huarte", "huetor-tajar", "ibiza", "iniesta", "irun", "jaraiz-de-la-vera", "javea",
    "jerez-de-la-frontera", "l-hospitalet-de-llobregat", "la-orotava", "la-palma-del-condado",
    "la-seu-d-urgell", "la-zubia", "laredo-cantabria", "las-palmas-de-gran-canaria", "leganes", "leiro", "linares", "lleida",
    "logrono", "lorca", "los-llanos-de-aridane", "lucena", "mairena-del-aljarafe", "majadahonda", "manacor",
    "manresa", "mao", "marbella", "marchena", "marratxi", "martos", "mazarron", "medina-de-rioseco",
    "medina-del-campo", "mequinenza", "merida", "miranda-de-ebro", "molina-de-segura", "mollerussa",
    "monforte-de-lemos", "mostoles", "motril", "mungia", "navalmoral-de-la-mata", "navia", "oliva", "olot",
    "orihuela", "oviedo", "palafrugell", "palma-de-mallorca", "pamplona", "paterna",
    "pedrajas-de-san-esteban", "penaranda-de-bracamonte", "penarroya-pueblonuevo", "petrer",
    "pilar-de-la-horadada", "plasencia", "ponferrada", "pozoblanco", "premia-de-mar", "puerto-del-rosario",
    "puertollano", "requena", "reus", "ribadeo", "ronda", "roquetas-de-mar", "rota", "sabadell",
    "sabinanigo", "sagunto", "san-cristobal-de-la-laguna", "san-fernando", "san-javier",
    "san-martin-de-valdeiglesias", "san-sebastian", "san-vicente-del-raspeig", "sant-boi-de-llobregat",
    "sant-cugat-del-valles", "santa-maria-del-paramo", "santa-marta-de-tormes", "santander",
    "santiago-de-compostela", "siero", "sitges", "solsona", "talavera-de-la-reina", "tarancon", "tarrega",
    "telde", "terrassa", "tomelloso", "torrelodones", "totana", "tremp", "tudela", "ubrique",
    "valdemorillo", "valdemoro", "valdepenas", "velez-malaga", "viana", "vic", "vielha", "vigo",
    "vila-real", "vilafranca-del-penedes", "vilagarcia-de-arousa", "villablino", "villarrobledo",
    "villaviciosa-de-odon", "villena", "vinaros", "vitoria-gasteiz", "viveiro", "xinzo-de-limia", "zuera",
    "zumaia",
  ).map(slug => s"og-$slug.jpg").toSet

  "every city, in every country" should "have the card its index page names" in {
    val absent = missing(Country.all.flatMap(_.cities).map(_.shareImage))
    // Only the first few names, or a country that was never swept prints its
    // whole roster — 546 filenames on one assertion line, in the run that
    // introduced this spec.
    withClue(s"${absent.size} cities have no committed share card; first: ") {
      absent.filterNot(awaitingFirstDeploy).take(8) shouldBe empty
    }
    withClue("cards listed as awaiting their first deploy that now exist — delete them from the list: ") {
      awaitingFirstDeploy.diff(absent.toSet) shouldBe empty
    }
  }

  "the cards" should "be the JPEGs the pages name, with no PNG left behind" in {
    // The cards were PNG until the sweep grew from 122 to 739 of them: at
    // ~810 KB each, and rewritten by every weekly refresh, PNG cost ~600 MB a
    // run against JPEG's 77 MB for the same pixels. A leftover `og-*.png` is
    // dead weight nothing serves.
    val strays = Option(cards.listFiles((_, n) => n.startsWith("og-") && n.endsWith(".png"))).toSeq.flatten.map(_.getName)
    withClue(s"${strays.size} stale PNG cards; first: ") { strays.take(8) shouldBe empty }
  }
}
