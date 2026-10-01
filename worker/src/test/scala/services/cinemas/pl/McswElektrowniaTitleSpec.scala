package services.cinemas.pl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.McswElektrowniaCinemaClient.{TitleParts, parseDayPage, parseTitle}

import java.nio.file.{Files, Path}
import java.time.LocalDate

/** Kino MCSW Elektrownia writes what it knows of a film into its title — country, genres, age, and a
 *  catalogue code carrying the production year and the version — once after commas, since autumn 2026
 *  also glued to the title by a dash. The strings below are the venue's own (captured 2026-10-01 and
 *  in the 2026-06 fixture day pages). */
class McswElektrowniaTitleSpec extends AnyFlatSpec with Matchers {

  "A dash-glued tail" should "come off the title as the film's genre, country and age" in {
    parseTitle("OBCY-kryminał/Francja/15lat N, KS 2025T2D10184") shouldBe
      TitleParts("OBCY", Some(2025), Seq("Francja"), Seq("kryminał"), Some("15"), List("2D", "NAP"))
    parseTitle("FOLWARK ZWIERZĘCY-animowany/Kanada, Wielka Brytania/10lat N,KS 2026D2D4386") shouldBe
      TitleParts("FOLWARK ZWIERZĘCY", Some(2026), Seq("Kanada", "Wielka Brytania"), Seq("animowany"), Some("10"), List("2D", "DUB"))
    parseTitle("LALKA- Polska, kostiumowy, od 15 lat KS N 2026O2D4432") shouldBe
      TitleParts("LALKA", Some(2026), Seq("Polska"), Seq("kostiumowy"), Some("15"), List("2D"))
  }

  it should "stay the title's when it states something the venue does not write" in {
    parseTitle("SZTUKA NA EKRANIE-HAUSER").title shouldBe "SZTUKA NA EKRANIE-HAUSER"
    parseTitle("MEDYTACJE/NEUROJOGA").title shouldBe "MEDYTACJE/NEUROJOGA"
  }

  "The comma format" should "give the same facts it always carried" in {
    parseTitle("K- POPOWE ŁOWCZYNIE DEMONÓW, USA, animowany, fatasy, dubbing, od 9 lat KS N 2025D2D9996") shouldBe
      TitleParts("K- POPOWE ŁOWCZYNIE DEMONÓW", Some(2025), Seq("USA"), Seq("animowany", "fatasy"), Some("9"), List("2D", "DUB"))
    parseTitle("DRZEWO MAGII, Wlk. Brytania , dubbing, familijny, od 8 lat KS N 2026D2D2251").countries shouldBe Seq("Wielka Brytania")
    // A note in brackets carries a comma of its own; the catalogue code dates the 1953 original.
    val vitelloni = parseTitle("DKF - WAŁKONIE, dramat obyczajowy, od 15 lat (inauguracja przeglądu filmowego - „FEDERICO FELLINI: ciao a tutti!” w programie 6 filmów z kopii cyfrowych 4K,) N 19532DT0004")
    vitelloni.year shouldBe Some(1953)
    vitelloni.genres shouldBe Seq("dramat", "obyczajowy")
    vitelloni.format shouldBe List("2D", "NAP")
  }

  "The day page" should "carry the parsed facts on every screening" in {
    // kino.mcswelektrownia.pl/MSI/mvc/pl?sort=Date&date=2026-10-03&datestart=0, captured 2026-10-01
    val html  = Files.readString(Path.of("test/resources/fixtures/mcsw-elektrownia/day_page_genre_tail_glued_by_dash.html"))
    val slots = parseDayPage(html, LocalDate.of(2026, 10, 3))
    slots.map(_.displayTitle).distinct should contain allOf ("LUNA I ROZGADANA ŚWINKA", "FOLWARK ZWIERZĘCY", "LALKA")
    slots.find(_.displayTitle == "FOLWARK ZWIERZĘCY").get.parts.format shouldBe List("2D", "DUB")
  }
}
