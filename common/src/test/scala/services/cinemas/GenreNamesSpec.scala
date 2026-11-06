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
    GenreNames.canonical("warsztaty filmowe", pl) shouldBe "Warsztaty filmowe"
    GenreNames.canonical("spotkanie", pl) shouldBe "Wydarzenie"
  }

  it should "use each language's own vocabulary" in {
    GenreNames.canonical("science fiction", en) shouldBe "Science Fiction"
    GenreNames.canonical("kids & family", en) shouldBe "Family"
    GenreNames.canonical("live performance", en) shouldBe "Live performance"
    GenreNames.canonical("documentary", en) shouldBe "Documentary"
    GenreNames.canonical("komödie", Country.Germany.language) shouldBe "Komödie"
    GenreNames.canonical("acción", Country.Spain.language) shouldBe "Acción"
  }

  it should "give one id to every spelling of a genre, in every language" in {
    Seq("przygoda", "Przygodowy", "Adventure", "Abenteuer", "aventura").flatMap(GenreNames.idOf).distinct shouldBe Seq("adventure")
    Seq("Dla Dzieci", "Familijny", "Family", "Familie", "Kids & Family").flatMap(GenreNames.idOf).distinct shouldBe Seq("family")
    Seq("Opera", "Oper", "Ópera").flatMap(GenreNames.idOf).distinct shouldBe Seq("opera")
    Seq("Science Fiction", "Sci-Fi", "Ciencia ficción").flatMap(GenreNames.idOf).distinct shouldBe Seq("scifi")
    GenreNames.idOf("Warszataty") shouldBe None
  }

  it should "name a master genre in the deployment language, whichever language the source spelled it in" in {
    GenreNames.canonical("Abenteuer", pl) shouldBe "Przygodowy"
    GenreNames.canonical("Przygodowy", Country.Germany.language) shouldBe "Abenteuer"
    GenreNames.canonical("Opera", Country.Germany.language) shouldBe "Oper"
  }

  "the master set" should "have unique ids and a name in every language" in {
    GenreNames.All.map(_.id).distinct should have size GenreNames.All.size
    GenreNames.All.foreach(g => Seq(g.pl, g.en, g.de, g.es).foreach(_.trim should not be empty))
  }

  "a placeholder" should "not be a genre" in {
    Seq("Unknown", "Undefiniert", "Default", "Laufzeit: k. A.").foreach(GenreNames.isPlaceholder(_) shouldBe true)
    GenreNames.isPlaceholder("Drama") shouldBe false
  }
}
