package services.cinemas

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The lowercase genres the prod mirror held on 2026-11-06 (Multikino Stary Browar, UK/US venues), each folded. */
class GenreNamesSpec extends AnyFlatSpec with Matchers {
  private val pl = Country.Poland.language
  private val en = Country.UnitedKingdom.language

  "GenreNames.canonical" should "fold the four lowercase Poznań examples into the vocabulary" in {
    GenreNames.canonical("przygoda", pl) shouldBe "Przygodowy"
    GenreNames.canonical("akcja", pl) shouldBe "Akcja"
    GenreNames.canonical("animacja", pl) shouldBe "Animacja"
    GenreNames.canonical("dla dzieci", pl) shouldBe "Familijny"
    GenreNames.canonical("film TV", pl) shouldBe "Film TV"
  }

  it should "match case-insensitively and capitalise an unknown genre's first letter" in {
    GenreNames.canonical("PRZYGODOWY", pl) shouldBe "Przygodowy"
    GenreNames.canonical("Dla Dzieci", pl) shouldBe "Familijny"
    GenreNames.canonical("KONCERT", pl) shouldBe "Koncert"
    GenreNames.canonical("teatr Improwizowany", pl) shouldBe "Teatr Improwizowany"
    GenreNames.canonical("spotkanie", pl) shouldBe "Spotkanie"
  }

  it should "use each language's own vocabulary" in {
    GenreNames.canonical("science fiction", en) shouldBe "Science Fiction"
    GenreNames.canonical("kids & family", en) shouldBe "Family"
    GenreNames.canonical("live performance", en) shouldBe "Live performance"
    GenreNames.canonical("documentary", en) shouldBe "Documentary"
    GenreNames.canonical("komödie", Country.Germany.language) shouldBe "Komödie"
    GenreNames.canonical("acción", Country.Spain.language) shouldBe "Acción"
  }
}
