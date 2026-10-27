package services.movies

import models.Country

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

  protected def rule = "search every listing without a screening marker"

  protected def findings(corpus: ListingCorpus, listing: CorpusListing): Seq[String] =
    tools.SearchQueryMarkers.finding(corpus.country, listing.raw, corpus.query(listing)).toSeq

  protected def remedy =
    "These listings search with a screening marker left in the query, which then finds no film. Add the spelling " +
      "to the title rule that strips that marker (ExtraTitleRules / TitleRules / FormatTags), or allowlist the " +
      "listing with why the word is the film's own:"

  protected val allowlist: Map[AllowedListing, String] = Map(
    AllowedListing.anywhere("Ród Smoka - premiera pierwszego odcinka 3. Sezonu w kinach Helios") -> s"a TV episode's premiere: $NoFilmToFind",
    AllowedListing("Kino Kinematograf", "Mały Kinematograf: premiera animacji „Kocie obserwacje” i spotkanie z reżyserką Aleksandrą Chrapowicką") ->
      s"a children's-animation premiere with a meeting: $NoFilmToFind",
    AllowedListing.anywhere("Siła wspólnoty. Pokaz filmów i spotkanie ze Skyem Hopinką, gościem tegorocznej edycji MFF TAURON Nowe Horyzonty") ->
      s"a programme of several films with a meeting: $NoFilmToFind",
    AllowedListing.anywhere("Lyudyna-pavuk: Absolyutno novyy denʹ - UA - Spetsialnyi pokaz") ->
      "TODO(rule): Helios' transliterated Ukrainian billing — the query is a transliteration TMDB cannot match whatever is stripped; a translit-aware rule is parked",
    AllowedListing.anywhere("Sneak Preview") -> s"a mystery screening whose film is undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("PIĘKNO BEZ ROZMIARU [Pokaz stylu, urody i dobrych manier] i projekcja filmu \" Cruella\"") ->
      "a fashion show billed with a film: an event plus a film matches neither (the double-bill rule)",
    AllowedListing.anywhere("Letnie przesilenie: Pokaz przedpremierowy PRZEKLEŃSTW NIEWINNOŚCI") ->
      "the film is named in the genitive; no strip yields its nominative title",
    AllowedListing.anywhere("Premiera biografii M. Breguły") -> s"a book launch: $NoFilmToFind",
    AllowedListing.anywhere("Premiera Camacho Limited Edition 2026 z ambasadorem marki Jackiem Najsztubem") -> s"a product launch: $NoFilmToFind",
    AllowedListing.anywhere("Gdynia Filmowa 2026 | Dobry Chłopiec+napisy SDH") ->
      "TODO(rule): a festival banner AND a glued '+napisy SDH' accessibility tail on one listing — parked",
    AllowedListing.anywhere("WORLD OF ANIMATION: SPIRITED AWAY (SUBTITLED) - DRAG BRUNCH") ->
      s"a drag brunch billed with a strand and a film: an event plus a film matches neither (the double-bill rule)",
    AllowedListing.anywhere("Hope Sneak") ->
      ("a preview of the film Hope (Na Hong-jin, 2025) billed '<film> Sneak' — the very shape of a venue's own branded sneak that " +
        "names no film ('Movietown Sneak', 'Filmrausch Sneak' below), so no rule can strip the word without searching a venue's " +
        "name as a film; the listing stays unmatched rather than risk a wrong match"),
    AllowedListing.anywhere("Movietown Sneak") -> s"a sneak branded with the venue's name: $NoFilmToFind",
    AllowedListing.anywhere("SPECIAL: Filmrausch Sneak") -> s"a sneak branded with the venue's name: $NoFilmToFind",
    AllowedListing.anywhere("endstation.sneak.abend") -> s"a sneak night branded with the venue's name: $NoFilmToFind",
    AllowedListing("Endstation Kino", "horror.sneak.abend") -> s"Endstation's horror sneak night, its film undisclosed: $NoFilmToFind",
    AllowedListing("Cinema Quadrat e.V.", "DER SUPER 8 ABEND OV, keine Angabe") ->
      s"an evening of Super 8 films, none named (\"keine Angabe\": not given): $NoFilmToFind",
    AllowedListing("Kino NCKF EC1", "Festiwal Pięć Smaków: Pięć smaków Azji, 6 kolorów herbaty- prezentacja i pokaz parzenia herbat") ->
      s"a festival's tea presentation and brewing show (\"pokaz parzenia herbat\"): $NoFilmToFind",
    AllowedListing("BFI London Southbank", "Mark Kermode Live in 3D at the BFI") ->
      s"Mark Kermode's live show, its own name (\"Live in 3D\" is the show's title, no screen format): $NoFilmToFind",
    AllowedListing.anywhere("Sneak_nov + Filmquiz") -> s"November's sneak with a film quiz: $NoFilmToFind",
    AllowedListing.anywhere("Classic Sneak – Couples") -> s"a classics sneak's theme, its film undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Classic Sneak – From Asia with Love") -> s"a classics sneak's theme, its film undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Classic Sneak – Melodrama") -> s"a classics sneak's theme, its film undisclosed: $NoFilmToFind",
    AllowedListing.anywhere("Arsenal Sneak Tübinger Kinos") -> s"a sneak branded with the venue's name: $NoFilmToFind",
    AllowedListing.anywhere("Bowie on the Silver Screen: An Illustrated Talk by Adrian Wootton + The Man Who Fell to Earth") ->
      "an illustrated talk billed with a film: an event plus a film matches neither (the double-bill rule)",
    AllowedListing.anywhere("21 i 22 Jump Street – podwójny seans") -> "a double bill: it matches neither film, by design",
    AllowedListing.anywhere("Sekretny pokaz") -> s"Cinema City's secret screening, its film undisclosed: $NoFilmToFind",
    AllowedListing("Cinexx", "OmU Ebenezer: Eine Weihnachtsgeschichte") ->
      "TODO(rule): a version bare in front of the film with no separator — one listing across the five corpora of 2026-10-13, too rare for a rule of its own",
  ) ++ DroppedByTheClients.keys.map(_ -> DroppedOnReRecord)

  override protected val allowedShapes: Seq[(scala.util.matching.Regex, String)] = Seq(
    BlindScreening -> s"a blind screening whose film is undisclosed, billed anew each month (Forum, Kino Wisła): $NoFilmToFind")

  /** Listings the German clients drop as no screening at all since the 2026-10-12 recording was made
   *  (`NonMovieEventClassifier.isNonScreening`: a sneak billed with no film, a day pass): a re-record stops carrying them. */
  override protected def awaitingReRecord: Set[AllowedListing] = DroppedByTheClients.keySet

  "every listing awaiting a re-record" should "be one the clients now drop as no screening" in {
    DroppedByTheClients.values.filterNot(services.cinemas.pl.NonMovieEventClassifier.isNonScreening) shouldBe empty
  }

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

  "a blind screening" should "be allowed by its shape, whatever month it bills, and no film titled after it" in {
    Seq("Seans w ciemno_11.26", "Seans w ciemno: 29.10.2026", "Seans w ciemno_6.26", "SEANS W CIEMNO").foreach { billing =>
      allowedByShape(billing) shouldBe defined
    }
    allowedByShape("Seanse w ciemnościach") shouldBe empty
    allowedByShape("Lalka - Seans w ciemno") shouldBe empty
  }

  "a listing billing several films" should "be held to no marker: it matches none of them, whatever it searches" in {
    billsSeveralFilms("Thunderbirds - Double Bill 4k Restoration: Atlantic Inferno & Attack of the Alligators") shouldBe true
    billsSeveralFilms("Space: 1999 4K Restoration Double Bill (U)") shouldBe true
    // a marathon of ONE title is that film's, so its query still counts
    billsSeveralFilms("Inna Mamusia - maraton") shouldBe false
    billsSeveralFilms("Lalka | 13+ | PREMIERA!!!") shouldBe false
  }

  "a finding" should "name the rule set whose strip the marker escaped" in {
    strippedBy("dubbing") shouldBe "FormatTags"
    strippedBy("premiera") shouldBe "the country's title rules (TitleRules / ExtraTitleRules)"
    strippedBy("reż.") shouldBe "the country's title rules (TitleRules / ExtraTitleRules)"
  }
}

object SearchQueryMarkersSpec {

  /** "Seans w ciemno_11.26", "Seans w ciemno: 29.10.2026": the blind screening Forum and Kino Wisła bill each month. */
  private val BlindScreening = """(?iu)Seans\s+w\s+ciemno(?![\p{L}\p{N}]).*""".r

  private val DroppedOnReRecord = "no screening, which the German clients now drop (NonMovieEventClassifier.isNonScreening) — the recording predates the drop"

  /** DE sneaks billed with no film that the clients now drop as no screening (`NonMovieEventClassifier.isNonScreening`),
   *  each with the venue's own billing the client judges: still in the recording of run 38355584482 because their
   *  venues' last scrape (2026-10-12 21:22 to 10-13 02:48) predates the drop, and in that of run 38542338732 Filmwerk
   *  Gütersloh's "Sneak_nov", listed under the short kinoheld linked it to ("Sneak Over") until the client stopped
   *  trusting a sneak's link. A re-record stops carrying them. */
  private val DroppedByTheClients: Map[AllowedListing, String] = Map(
    AllowedListing.anywhere("Friday Night Sneak")    -> "Friday Night Sneak",
    AllowedListing.anywhere("Arthaus Sneak Vol. 43") -> "Arthaus Sneak Vol. 43",
    AllowedListing("Filmwerk Gutersloh", "Sneak Over") -> "Sneak_nov")

  export tools.SearchQueryMarkers.{billsSeveralFilms, markersIn, strippedBy, vocabulary}
}
