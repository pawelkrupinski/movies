package services.resolution

import models.{CinemaShowing, Helios, KinoApollo, MovieRecord, Multikino, Source, SourceData, Tmdb, Imdb, Filmweb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

class FilmEvidenceSpec extends AnyFlatSpec with Matchers {

  private def record(data: (Source, SourceData)*): MovieRecord = MovieRecord(data = data.toMap)

  "FilmEvidence.of" should "read the cinemas and never a derived slot" in {
    // The row's previous resolution stamped Haugerud on the Tmdb slot; the only
    // cinema says Michel Franco. Only the cinema counts as evidence.
    val e = FilmEvidence.of(record(
      Helios  -> SourceData(title = Some("Dreams"), director = Seq("Michel Franco"), runtimeMinutes = Some(98), releaseYear = Some(2025), cast = Seq("Jessica Chastain")),
      Tmdb    -> SourceData(title = Some("Drømmer"), director = Seq("Dag Johan Haugerud"), runtimeMinutes = Some(110), releaseYear = Some(2024), originalTitle = Some("Drømmer")),
      Imdb    -> SourceData(director = Seq("Dag Johan Haugerud"), releaseYear = Some(2024)),
      Filmweb -> SourceData(originalTitle = Some("Drømmer"))))
    e.titles         shouldBe Set("Dreams")
    e.directors      shouldBe Seq("Michel Franco")
    e.runtimes       shouldBe Seq(98)
    e.years          shouldBe Seq(2025)
    e.cast           shouldBe Seq("Jessica Chastain")
    e.originalTitle  shouldBe None
    e.directorHint   shouldBe Some("Michel Franco")
  }

  it should "be a pure function of the slot set, whatever order the cinemas arrived in" in {
    val a = Helios     -> SourceData(title = Some("Guru"),   director = Seq(" Yann Gozlan"), runtimeMinutes = Some(100), releaseYear = Some(2026))
    val b = KinoApollo -> SourceData(title = Some("Gourou"), director = Seq("Antoine Fuqua"), runtimeMinutes = Some(94),  releaseYear = Some(2025))
    FilmEvidence.of(record(a, b)) shouldBe FilmEvidence.of(record(b, a))
    FilmEvidence.of(record(a, b)).directors shouldBe Seq("Antoine Fuqua", "Yann Gozlan")
    FilmEvidence.of(record(a, b)).years     shouldBe Seq(2025, 2026)
    FilmEvidence.of(record(a, b)).runtimes  shouldBe Seq(94, 100)
  }

  it should "prefer the higher-priority venue's original title as the search hint" in {
    val e = FilmEvidence.of(record(
      KinoApollo -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled (Apollo)")),
      Multikino  -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled"))))
    e.originalTitle  shouldBe Some("Tangled")
    e.originalTitles shouldBe Seq("Tangled", "Tangled (Apollo)")
  }

  it should "count a title once per slot that publishes it" in {
    // Thirty-eight venues say "Mistyczka"; one lists the film under a banner
    // whose stripped form is a different title.
    val venues = (1 to 3).map(i => (CinemaShowing(Helios, s"mistyczka$i"): Source) -> SourceData(title = Some("Mistyczka")))
    val stray  = (CinemaShowing(KinoApollo, "maryja"): Source) -> SourceData(title = Some("DOBRE Kino - Maryja. Matka Papieża"))
    val votes  = FilmEvidence.of(record((venues :+ stray)*)).titleVotes(titleNormalizer)
    votes(titleNormalizer.sanitize("Mistyczka")) shouldBe 3
    votes(titleNormalizer.sanitize("Maryja. Matka Papieża")) shouldBe 1
  }

  it should "fold an event's director in the same order-free way" in {
    val e = FilmEvidence.of(record(Helios -> SourceData(title = Some("X"), director = Seq("Zhang Yimou"))))
    e.withDirectors(Seq(" Ang Lee ", "")).directors shouldBe Seq("Ang Lee", "Zhang Yimou")
    e.withDirectors(Seq("Zhang Yimou")) shouldBe e
  }

  it should "be empty for a row with no cinema slot" in {
    FilmEvidence.of(record(Tmdb -> SourceData(title = Some("F"), director = Seq("Someone")))) shouldBe FilmEvidence.empty
    FilmEvidence.empty.isEmpty shouldBe true
  }
}
