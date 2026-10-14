package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleWords.Concept

import java.util.Locale

/** The one table of words billed around a film ([[TitleWords]], `title-words.tsv`): every concept in every language a
 *  country bills in, and the table well-formed. */
class TitleWordsSpec extends AnyFlatSpec with Matchers {

  /** The languages the countries bill in: a country onboarded in a new language fails here until each concept has a word. */
  private val billedLanguages: Set[String] = Country.all.map(_.language.getLanguage.toLowerCase(Locale.ROOT)).toSet

  "Every concept" should "hold a word in every language a country bills in" in {
    billedLanguages should contain allOf ("pl", "de", "en", "es")
    val missing = for {
      concept  <- Concept.values.toSeq
      language <- billedLanguages.toSeq.sorted
      if !TitleWords.languages(concept)(language)
    } yield s"${concept.id}: no $language word"
    missing shouldBe empty
  }

  "The table" should "name each row's language as an ISO code and hold no row twice" in {
    TitleWords.entries.map(_.language).filterNot(_.matches("[a-z]{2}")) shouldBe empty
    TitleWords.entries.groupBy(e => (e.concept, e.language, e.phrase)).collect { case (k, rows) if rows.sizeIs > 1 => k } shouldBe empty
  }

  it should "refuse a row of an unknown concept, or a short one" in {
    an[IllegalArgumentException] should be thrownBy TitleWords.parse(Iterator("anniversaries\ten\tanniversary"))
    an[IllegalArgumentException] should be thrownBy TitleWords.parse(Iterator("anniversary\ten"))
    TitleWords.parse(Iterator("# a comment", "", "anniversary\tde\tJubiläum\ta note")) shouldBe
      Seq(TitleWords.Entry(Concept.Anniversary, "de", "Jubiläum", Some("a note")))
  }

  "A concept read as words" should "hold no stem" in {
    val wordSets = Seq(Concept.Anniversary, Concept.Edition, Concept.Premiere, Concept.Talk, Concept.Screening, Concept.Festival,
      Concept.Performance, Concept.Marathon, Concept.Relay, Concept.SideEvent, Concept.OldFilm, Concept.StageHouse, Concept.Stage,
      Concept.HouseBanner)
    wordSets.foreach(c => noException should be thrownBy TitleWords.tokens(c))
    TitleWords.tokens(Concept.Anniversary) should contain allOf ("jubilaum", "anniversary", "aniversario", "rocznica")
  }

  "The folded pattern" should "read a title's folded words, a stem open-ended and a word whole" in {
    val sneak = TitleWords.foldedPattern(Concept.Sneak).r
    def folded(t: String) = TitleContainment.tokens(t).mkString(" ")
    sneak.findFirstIn(folded("Seans w ciemno")) shouldBe defined
    sneak.findFirstIn(folded("SEANSWCIEMNO")) shouldBe defined
    sneak.findFirstIn(folded("OV Sneak-Previews")) shouldBe defined
    sneak.findFirstIn(folded("CineSneak")) shouldBe defined
    sneak.findFirstIn(folded("CineSneaks")) shouldBe None
    sneak.findFirstIn(folded("Sesión sorpresa")) shouldBe defined
    val bill = TitleWords.foldedPattern(Concept.DoubleBill).r
    bill.findFirstIn(folded("Podwójny seans: Obcy + Decydujące starcie")).map(_.trim) shouldBe Some("podwojny seans")
    bill.findFirstIn(folded("Double Features")) shouldBe Some("double features")
    bill.findFirstIn(folded("Double Featured")) shouldBe None
  }

  "The raw pattern" should "match a phrase as billed, accents and case aside, spaced or hyphenated" in {
    val audience = s"(?iu)${TitleWords.rawPattern(Concept.Audience)}".r
    audience.findFirstIn("Dementia-Friendly") shouldBe Some("Dementia-Friendly")
    audience.findFirstIn("Parents & Babies") shouldBe Some("Parents & Babies")
    audience.findFirstIn("SESION RELAJADA") shouldBe Some("SESION RELAJADA")
    audience.findFirstIn("sesión relajada") shouldBe Some("sesión relajada")
    audience.findFirstIn("Eltern Kind Kino") shouldBe Some("Eltern Kind Kino")
  }
}
