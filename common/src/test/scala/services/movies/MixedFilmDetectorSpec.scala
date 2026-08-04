package services.movies

import models.{Helios, KinoMuranow, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Both cases here are production rows, with the cinemas' own published fields.
 *
 * The detector's whole job is to distinguish "two cinemas describe one film to
 * different depths" — overwhelmingly the normal case, and one that must never be
 * split — from "two cinemas are describing different films", which no single
 * tmdbId can serve.
 */
class MixedFilmDetectorSpec extends AnyFlatSpec with Matchers {

  private def slot(director: Seq[String], original: Option[String], runtime: Option[Int] = None) =
    SourceData(title = Some("x"), director = director, originalTitle = original, runtimeMinutes = runtime)

  "a row whose cinemas name different directors" should "split, with the odd one out as the stray" in {
    // "Obcy": 2 cinemas on Ozon's L'étranger, 1 on Brandt Andersen's film.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino    -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120)),
      Helios       -> slot(Seq("François Ozon"), Some("L’Étranger"), Some(122)),
      KinoMuranow  -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103))))

    val strays = MixedFilmDetector.strays(record, titleNormalizer)
    strays.map(_._1) shouldBe Seq(KinoMuranow: Source)
  }

  "a row holding two films of the same title" should "split even at one slot each" in {
    // "Joanna d'Arc": Besson's 1999 film and Pálmason's 2025 one.
    val record = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot(Seq("Luc Besson"), Some("Joan of Arc"), Some(160)),
      Helios      -> slot(Seq.empty, Some("Jóhanna af Örk"))))

    MixedFilmDetector.strays(record, titleNormalizer) should have size 1
  }

  // ── What must NOT split ───────────────────────────────────────────────────

  "cinemas describing ONE film to different depths" should "not split" in {
    // The overwhelmingly common shape: one publishes a director, another doesn't.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Michel Franco"), Some("Dreams: Sueños")),
      Helios    -> slot(Seq.empty, None)))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split on punctuation or case in the original title" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger")),
      Helios    -> slot(Seq("François Ozon"), Some("L’Étranger"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split when one cinema omits the director the other publishes" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Luc Besson"), Some("Joan of Arc")),
      Helios    -> slot(Seq.empty, Some("Joan of Arc"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split a row with a single cinema" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Michel Franco"), Some("Dreams: Sueños"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  "the split" should "be a pure function of the row, not of slot order" in {
    val a = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot(Seq("François Ozon"), Some("L'étranger")),
      Helios      -> slot(Seq("François Ozon"), Some("L'étranger")),
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"))))
    val b = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger")),
      Helios      -> slot(Seq("François Ozon"), Some("L'étranger")),
      Multikino   -> slot(Seq("François Ozon"), Some("L'étranger"))))

    MixedFilmDetector.strays(a, titleNormalizer).map(_._1) shouldBe
      MixedFilmDetector.strays(b, titleNormalizer).map(_._1)
  }
}
