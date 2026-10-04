package services.movies

import models.Cinema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.FixtureTestWiring

import java.util.Locale

/**
 * No screening marker survives into the title search a listing runs, over every listing title of the
 * recorded corpus.
 *
 * The class of failure: a venue bills a film with a marker of the SCREENING — "– Premiera",
 * "przedpremiera", "Pokaz specjalny", a "| 13+ | PREMIERA!!!" tail, "2D dubbing", a trailing "PL",
 * a "reż. …" credit — and a title rule that did not know that spelling left it in the TMDB / IMDb
 * query, which then found no film (Ekobilet's "Lalka | 13+ | PREMIERA!!!", Kino Gwiazda's senior
 * strand, the "Z klasą do kina:" banner, MSI's distributor tail, a "PL" behind a format word —
 * about twenty fixes, each one more rule entry). Each fix was found by one unresolved film. This
 * holds the whole corpus to the marker vocabulary the rules themselves strip (`FormatTags`'
 * format/version words, and the premiere/screening/credit words of `ExtraTitleRules`), naming the
 * cinema, the listing title and the query it produced.
 */
class SearchQueryMarkersSpec extends AnyFlatSpec with Matchers {

  import SearchQueryMarkersSpec._

  private val NoFilmToFind = "an event billing with no film a title search could find"
  private val Strand = "TODO(rule): a strand suffix after the format tag, so FormatTags' end-anchored strip misses it — parked"

  /** Listing title → why its query may keep the marker. */
  private val Allowlist: Map[String, String] = Map(
    "Ród Smoka - premiera pierwszego odcinka 3. Sezonu w kinach Helios" -> s"a TV episode's premiere: $NoFilmToFind",
    "Mały Kinematograf: premiera animacji „Kocie obserwacje” i spotkanie z reżyserką Aleksandrą Chrapowicką" ->
      s"a children's-animation premiere with a meeting: $NoFilmToFind",
    "Seans w ciemno_6.26" -> s"a blind screening whose film is undisclosed: $NoFilmToFind",
    "PIĘKNO BEZ ROZMIARU [Pokaz stylu, urody i dobrych manier] i projekcja filmu \" Cruella\"" ->
      "a fashion show billed with a film: an event plus a film matches neither (the double-bill rule)",
    "21 i 22 Jump Street – podwójny seans" -> "a double bill: it matches neither film, by design",
    "Letnie przesilenie: Pokaz przedpremierowy PRZEKLEŃSTW NIEWINNOŚCI" ->
      "the film is named in the genitive; no strip yields its nominative title",
    "DKF ZAPRASZA 19 CZERWCA NA PREMIEROWY POKAZ FILMU „OJCZYZNA” (REŻYSERA „IDY” ORAZ „ZIMNEJ WOJNY”)" ->
      "TODO(rule): xtra-pokaz-filmu wants the quoted film at the title's end; a trailing parenthesis defeats it — parked",
    "STRASZNY FILM napisy - Młodzieżowy Klub Filmowy LEŻAK" -> Strand,
    "Chłopiec i czapla (napisy PL) | Poniedziałki ze Studiem Ghibli" -> Strand,
    "Tajemniczy świat Arrietty (napisy PL) | Poniedziałki ze studiem Ghibli" -> Strand,
  )

  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026")
    w.bootStartup()
    w
  }

  private lazy val listings: Seq[(Cinema, String)] =
    ScheduleCorpusText.recordsByFilmId(wiring).values.toSeq.distinct
      .flatMap(_.cinemaData.toSeq.flatMap { case (cinema, slot) => slot.rawTitle.orElse(slot.title).map(cinema -> _) })
      .distinct

  private def query(cinema: Cinema, raw: String): String =
    titleNormalizer.searchQuery(titleNormalizer.listingTitle(cinema, raw)._1)

  "the marker matcher" should "find each screening marker the rules strip, and nothing in a plain title" in {
    markersIn("Lalka | 13+ | PREMIERA!!!") shouldBe Seq("premiera")
    markersIn("Minecraft Film 2D dubbing") shouldBe Seq("2d", "dubbing")
    markersIn("Pokaz przedpremierowy: Diuna") shouldBe Seq("pokaz", "przedpremierowy")
    markersIn("Szybcy i wściekli PL") shouldBe Seq("trailing PL")
    markersIn("Baczne oczka reż. Katarzyna Agopsowicz") shouldBe Seq("reż.")
    markersIn("Diuna: Część druga") shouldBe empty
    markersIn("Plan 9 z kosmosu") shouldBe empty
  }

  "every listing title of the corpus" should "search without a screening marker" in {
    listings.size should be > 1000
    val found = listings.flatMap { case (cinema, raw) =>
      val q = query(cinema, raw)
      Option.when(markersIn(q).nonEmpty && !Allowlist.contains(raw))(
        s"${cinema.slug}: '$raw' searches '$q' (${markersIn(q).mkString(", ")})")
    }.distinct.sorted
    withClue("These listings search with a screening marker left in the query, which then finds no film. Add the " +
      "spelling to the title rule that strips that marker (ExtraTitleRules / TitleRules / FormatTags), or allowlist " +
      "the listing with why the word is the film's own:\n" + found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "keep every allowlist entry still keeping its marker (the backlog only shrinks)" in {
    val live = listings.collect { case (cinema, raw) if markersIn(query(cinema, raw)).nonEmpty => raw }.toSet
    withClue("Allowlisted but now searched clean — drop the entry: ")((Allowlist.keySet -- live).toSeq.sorted shouldBe empty)
  }
}

object SearchQueryMarkersSpec {

  /** The screening's own words the title rules strip: `FormatTags`' format and version words, and the
   *  premiere / special-screening / credit words of `ExtraTitleRules`. */
  private val MarkerWords: Seq[String] =
    (FormatTags.FormatToken.keySet ++ Set(
      "premiera", "przedpremiera", "przedpremierowo", "przedpremierowy", "pokaz", "pokazy", "seans")).toSeq.sorted

  private val Words  = """[\p{L}\p{N}]+""".r
  private val Credit = """(?iu)(?:^|\s)reż\.""".r
  private val PlTail = """\s+PL\s*$""".r

  /** The markers `query` still carries, in a stable order. */
  def markersIn(query: String): Seq[String] = {
    val words = Words.findAllIn(query.toLowerCase(Locale.ROOT)).toSet
    MarkerWords.filter(words.contains) ++
      Credit.findFirstIn(query).map(_ => "reż.") ++
      PlTail.findFirstIn(query).map(_ => "trailing PL")
  }
}
