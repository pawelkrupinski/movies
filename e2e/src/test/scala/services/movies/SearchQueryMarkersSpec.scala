package services.movies

import models.Country

import java.util.regex.Pattern
import scala.util.matching.Regex

/**
 * No screening marker survives into the title search a listing runs, over every listing of every
 * corpus ([[CorpusShapeSpec]]): the Polish fixture boot and each country's recorded corpus.
 *
 * The class of failure: a venue bills a film with a marker of the SCREENING — "– Premiera",
 * "przedpremiera", "Pokaz specjalny", a "| 13+ | PREMIERA!!!" tail, "2D dubbing", a trailing "PL",
 * a "reż. …" credit, "(OmU)", "VOSE", "Parent & Baby", "Open Caption" — and a title rule that did
 * not know that spelling left it in the TMDB / IMDb query, which then found no film (Ekobilet's
 * "Lalka | 13+ | PREMIERA!!!", Kino Gwiazda's senior strand, the "Z klasą do kina:" banner, MSI's
 * distributor tail, a "PL" behind a format word — about twenty fixes, each one more rule entry).
 * Each fix was found by one unresolved film. This holds every corpus to the marker vocabulary of
 * its country ([[SearchQueryMarkersSpec.vocabulary]]), naming the cinema, the listing title and
 * the query it produced.
 */
class SearchQueryMarkersSpec extends CorpusShapeSpec {

  import SearchQueryMarkersSpec._

  private val NoFilmToFind = "an event billing with no film a title search could find"
  private val Strand = "TODO(rule): a strand suffix after the format tag, so FormatTags' end-anchored strip misses it — parked"

  protected def rule = "search every listing without a screening marker"

  protected def findings(corpus: ListingCorpus, listing: CorpusListing): Seq[String] =
    Option(listing.raw).filter(_.nonEmpty).toSeq.flatMap { _ =>
      val query   = corpus.query(listing)
      val markers = markersIn(corpus.country, query)
      Option.when(markers.nonEmpty)(s"searches '$query' (${markers.mkString(", ")})")
    }

  protected def remedy =
    "These listings search with a screening marker left in the query, which then finds no film. Add the spelling " +
      "to the title rule that strips that marker (ExtraTitleRules / TitleRules / FormatTags), or allowlist the " +
      "listing with why the word is the film's own:"

  protected val allowlist: Map[AllowedListing, String] = Map(
    AllowedListing.anywhere("Ród Smoka - premiera pierwszego odcinka 3. Sezonu w kinach Helios") -> s"a TV episode's premiere: $NoFilmToFind",
    AllowedListing("Kino Kinematograf", "Mały Kinematograf: premiera animacji „Kocie obserwacje” i spotkanie z reżyserką Aleksandrą Chrapowicką") ->
      s"a children's-animation premiere with a meeting: $NoFilmToFind",
    AllowedListing.anywhere("Seans w ciemno_6.26") -> s"a blind screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Seans w ciemno_10.26") -> s"a blind screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Seans w ciemno_8.26") -> s"a blind screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Seans w ciemno: 30.07.2026") -> s"a blind screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Siła wspólnoty. Pokaz filmów i spotkanie ze Skyem Hopinką, gościem tegorocznej edycji MFF TAURON Nowe Horyzonty") ->
      s"a programme of several films with a meeting: $NoFilmToFind",
    AllowedListing.anywhere("Lyudyna-pavuk: Absolyutno novyy denʹ - UA - Spetsialnyi pokaz") ->
      "TODO(rule): Helios' transliterated Ukrainian billing — the query is a transliteration TMDB cannot match whatever is stripped; a translit-aware rule is parked",
    AllowedListing.anywhere("Sneak Preview") -> s"a mystery screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("PIĘKNO BEZ ROZMIARU [Pokaz stylu, urody i dobrych manier] i projekcja filmu \" Cruella\"") ->
      "a fashion show billed with a film: an event plus a film matches neither (the double-bill rule)",
    AllowedListing.anywhere("21 i 22 Jump Street – podwójny seans") -> "a double bill: it matches neither film, by design",
    AllowedListing.anywhere("Letnie przesilenie: Pokaz przedpremierowy PRZEKLEŃSTW NIEWINNOŚCI") ->
      "the film is named in the genitive; no strip yields its nominative title",
    AllowedListing.anywhere("DKF ZAPRASZA 19 CZERWCA NA PREMIEROWY POKAZ FILMU „OJCZYZNA” (REŻYSERA „IDY” ORAZ „ZIMNEJ WOJNY”)") ->
      "TODO(rule): xtra-pokaz-filmu wants the quoted film at the title's end; a trailing parenthesis defeats it — parked",
    AllowedListing.anywhere("Premiera biografii M. Breguły") -> s"a book launch: $NoFilmToFind",
    AllowedListing.anywhere("Premiera Camacho Limited Edition 2026 z ambasadorem marki Jackiem Najsztubem") -> s"a product launch: $NoFilmToFind",
    AllowedListing.anywhere("Gdynia Filmowa 2026 | Dobry Chłopiec+napisy SDH") ->
      "TODO(rule): a festival banner AND a glued '+napisy SDH' accessibility tail on one listing — parked",
    AllowedListing.anywhere("WORLD OF ANIMATION: SPIRITED AWAY (SUBTITLED) - DRAG BRUNCH") ->
      s"a drag brunch billed with a strand and a film: an event plus a film matches neither (the double-bill rule)",
  )

  "the marker matcher" should "find each screening marker the rules strip, and nothing in a plain title" in {
    markersIn(Country.Poland, "Lalka | 13+ | PREMIERA!!!") shouldBe Seq("premiera")
    markersIn(Country.Poland, "Minecraft Film 2D dubbing") shouldBe Seq("2d", "dubbing")
    markersIn(Country.Poland, "Pokaz przedpremierowy: Diuna") shouldBe Seq("pokaz", "przedpremierowy")
    markersIn(Country.Poland, "Szybcy i wściekli PL") shouldBe Seq("trailing PL")
    markersIn(Country.Poland, "Baczne oczka reż. Katarzyna Agopsowicz") shouldBe Seq("reż.")
    markersIn(Country.Poland, "Diuna: Część druga") shouldBe empty
    markersIn(Country.Poland, "Plan 9 z kosmosu") shouldBe empty
    markersIn(Country.UnitedKingdom, "Parent & Baby: Wicked") shouldBe Seq("parent & baby")
    markersIn(Country.UnitedKingdom, "Wicked - Autism Friendly Screening") shouldBe Seq("autism friendly")
    markersIn(Country.UnitedKingdom, "Lawrence of Arabia (70mm)") shouldBe Seq("70mm")
    markersIn(Country.UnitedStates, "Sinners Open Caption") shouldBe Seq("open caption")
    markersIn(Country.UnitedStates, "Tron: Ares RPX") shouldBe Seq("rpx")
    markersIn(Country.Germany, "Der Astronaut (OmU)") shouldBe Seq("omu")
    markersIn(Country.Germany, "Sneak Preview") shouldBe Seq("preview", "sneak")
    markersIn(Country.Spain, "Frankenstein (VOSE)") shouldBe Seq("vose")
    markersIn(Country.Spain, "Estreno: Avatar") shouldBe Seq("estreno")
    markersIn(Country.UnitedKingdom, "The Babadook") shouldBe empty
    markersIn(Country.Germany, "Das Lehrerzimmer") shouldBe empty
    markersIn(Country.Spain, "La sociedad de la nieve") shouldBe empty
  }
}

object SearchQueryMarkersSpec {

  /** A marker word or phrase, matched case-insensitively as whole words. */
  private def phrase(text: String): (String, Regex) =
    text -> ("(?iu)(?<![\\p{L}\\p{N}])" + text.split(" ").map(Pattern.quote).mkString("\\s+") + "(?![\\p{L}\\p{N}])").r

  /** The screening's own words, per country — what the title rules strip or a client peels off:
   *  `FormatTags`' format and version words everywhere; then each language's version and event words —
   *  the premiere / special-screening words of `ExtraTitleRules` in Poland, the version abbreviations
   *  `ScreeningTokens` maps (OV/OmU/OmeU in Germany, VO/VOS/VOSE/VOSI in Spain) and the special-audience
   *  labels it refuses as badges (parent & baby, autism friendly, relaxed, open caption…), the premium
   *  formats the chain clients badge (Dolby, RPX, ScreenX, 4DX), and the preview / sneak / estreno words
   *  `NonMovieEventClassifier` and the Webedia clients know. */
  def vocabulary(country: Country): Seq[(String, Regex)] = {
    val english = Seq("parent & baby", "parent and baby", "parents & babies", "parent & toddler", "autism friendly",
      "relaxed screening", "dementia friendly", "open caption", "open captioned", "audio described", "toddler time",
      "silver screen", "q&a", "sneak preview", "dolby", "rpx", "screenx", "4dx", "imax", "35mm", "70mm")
    val byLanguage: Map[String, Seq[String]] = Map(
      "pl" -> Seq("premiera", "przedpremiera", "przedpremierowo", "przedpremierowy", "pokaz", "pokazy", "seans"),
      "en" -> english,
      "de" -> Seq("ov", "omu", "omeu", "original mit untertiteln", "preview", "sneak", "vorpremiere"),
      "es" -> Seq("vo", "vos", "vose", "vosi", "v.o.s.e.", "doblada", "estreno", "preestreno", "versión original"))
    (FormatTags.FormatToken.keySet.toSeq ++ byLanguage.getOrElse(country.language.getLanguage, Nil))
      .distinct.sorted.map(phrase)
  }

  private val Credit = """(?iu)(?:^|\s)reż\.""".r
  private val PlTail = """\s+PL\s*$""".r

  /** The markers `query` still carries, in a stable order. */
  def markersIn(country: Country, query: String): Seq[String] =
    vocabularies(country).collect { case (word, pattern) if pattern.findFirstIn(query).isDefined => word } ++
      (if (country == Country.Poland)
        Credit.findFirstIn(query).map(_ => "reż.") ++ PlTail.findFirstIn(query).map(_ => "trailing PL")
      else Nil)

  private val vocabularies: Map[Country, Seq[(String, Regex)]] = Country.all.map(c => c -> vocabulary(c)).toMap
}
