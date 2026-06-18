package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Ground truth is the live prod corpus: every title below was actually scraped
 *  into `movies`. The two lists are the line the product owner drew — "filter
 *  stray live events, keep cinema broadcasts and art documentaries". */
class NonMovieEventClassifierSpec extends AnyFlatSpec with Matchers {

  // Stray live stage/music events that leaked in from small venues' own
  // ticketing — concerts, stand-up, kabaret, recitals, theatre plays.
  private val liveEvents = List(
    "Edyta Geppert - recital",
    "Pani domu jest tylko jedna - spektakl komediowy",
    "SIOSTRZYCZKI - koncert komediowy",
    "\"Seks dla Opornych\"/ Teatr Skene Warszawa",
    "Katarzyna Piasecka - Nowy program stand-up „BAGUS”",
    "Kabaret Chyba - Rodzina to jest siła!",
    "Piotr Bałtroczyk Stand-up",
    "Rafał Rutkowski stand-up: Wehikuł czasu",
    "Tenorzy przy świecach - koncert, który porusza serce",
    "Gwiazdorska gala wiedeńska – \"wszystkie drogi prowadzą do wiednia\"",
    "Gala Baletowa - Viola Dance",
    "Ognisty ptak i lisica Ryśka - Teatralne popołudnie dla dzieci i rodziców",
    "Kabaret młodych panów − z żartami nie ma żartów",
    "Magiczny koncert – bajki świata",
    "Kabaret K2 - Jedziemy na luzie",
    "Mariusz kałamaga stand up",
    "Relaksacyjny koncert kamertonowy",
    "Wiedeńska noc: gala noworoczna z grand étoile orchestra",
    "Światowa gala muzyczna",
    "Koncert magdaleny kumorek i macieja tubisa",
    "Koncert „nie dokazuj! największe przeboje marka grechuty i jana kantego pawluśkiewicza”",
    "Poranek teatralny dla najnajów \"prościutko\"",
    "Koncert Taneczny",
    "10 głosów, jedna scena - koncert 10 tenorów",
    "Anna lipiak i grzegorz niemczuk - solo i w duecie - recital fortepianowy",
    "Diamentowy koncert. 60-lecie - koncert zespołu czerwone gitary",
    "Kochane pieniążki - spektakl komediowy z teatru capitol w warszawie",
    "Miłość i polityka - spektakl komediowy",
    "Orkiestra księżniczek - koncert wiedeński",
    "Rubinowe gody - spektakl komediowy",
    "Wesoła wdówka - operetka franza lehára w wykonaniu teatru muzycznego arte creatura",
    "Wieczorek pożegnalny - spektakl komediowy",
    "Koncert Joscho Stephan Trio",
    "\"Przeboje wiedeńskiej operetki cz.2\" - koncert Sala Widowiskowa w Solcu-Zdroju",
    "Stand-up  Mama na Obrotach",
    "Sobota z teatrem dla dzieci - czerwony kapturek",
    "Okładka „Noc Kupały”  koncert zespołu Wowakin i Niny Kodorskiej",
    "Kabaret Trzecia Strona Medalu - \"Kryzys złotego wieku\"",
    "Koncert studium wokalnego",
    "Spektakl- Pomoc Domowa",
    "Koncert muzyki disneya",
    "Dorota piotrowska & sound circle - voices of human consciousness - zaduszkowy koncert jazzowy",
    "GALA ROZDANIA NAGRÓD - 19. MFFA ANIMATOR",
    "KONCERT ZTL SANOK",
    "Koncertztlsanok",                                  // collapsed spelling, same event
    "Gala uczniów szkoły muzycznej yamaha",
    "Koncert i dyskusja Yannis Patoukas i Mathijs Leeuwis",
    "SPEKTAKL - Moja wersja prawdy",
    "Spektakl - Królowe życia",
    "Spektakl „mglisty skarb tutanchamona”",
    "Spektakl_kubuś fatalista i jego pan",              // underscore separator, still a spektakl
    "Kabaret Paranienormalni"
  )

  // Kept on purpose: "event cinema" broadcasts (opera/ballet/theatre/concert
  // transmissions and concert films) and art DOCUMENTARIES — all real cinema
  // content that merely contains event vocabulary.
  private val cinemaContent = List(
    // Broadcasts — carry a broadcast marker that vetoes the event verdict.
    "Royal Ballet and Opera Sezon Kinowy 2026-27: Carmen",
    "Royal Ballet and Opera Sezon Kinowy 2026-27: Manon",
    "Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów",
    "André Rieu. Niech żyje Maastricht! Retransmisja letniego koncertu z Maastricht",
    "ANDRÉ RIEU W KINIE „Niech żyje Maastricht!” najnowszy letni koncert Króla Walca!",
    "90 URODZINY PAVAROTTIEGO | retransmisja koncertu",
    "„90. urodziny Pavarottiego” Koncert gwiazd z Arena di Verona 2025",
    "90. rocznica urodzin Pavarottiego- Koncert gwiazd z Arena di Verona Napisy PL",
    "SZTUKA NA EKRANIE - KONCERT 90 URODZINY PAVAROTTIEGO",
    "Niebezpieczne związki | TEATR W KINIE | National Theatre Live",
    "Wszyscy moi synowie | TEATR W KINIE | National Theatre Live",
    "Playboy zachodniego świata | TEATR W KINIE | National Theatre Live",
    "Audiencja | TEATR W KINIE | National Theatre Live",
    "Warszawa Konwickiego. Pokaz filmu „Jak daleko stąd, jak blisko” i koncert",
    // Art documentaries — never trip a marker (art words aren't markers; the
    // venue's "Kinoteatrze" survives the \bteatr word boundary).
    "TURNER & CONSTABLE. PRZEŁOMOWA WYSTAWA 2D NAP.",
    "SZTUKA W CENTRUM. NOWOŚCI 2026 | Turner & Constable. Przełomowa wystawa",
    "Wielka Sztuka w Kinoteatrze Rialto - David Hockney. Pejzaże, portrety",
    "Wielka Sztuka w Kinoteatrze Rialto - Hopper. Amerykańska love story",
    "Wielka Sztuka w Kinoteatrze Rialto - Canaletto i sztuka Wenecji",
    "Wielka Sztuka w Kinoteatrze Rialto - Velázquez i jego tajemnica",
    "Wielka Sztuka w Kinoteatrze Rialto - Świt impresjonizmu: Paryż 1874",
    // Real films whose title merely contains event vocabulary.
    "Społeczeństwo spektaklu"
  )

  "isLiveEvent" should "flag every stray live stage/music event" in {
    liveEvents.foreach { t =>
      withClue(s"expected live-event: [$t] ") { NonMovieEventClassifier.isLiveEvent(t) shouldBe true }
    }
  }

  it should "keep broadcasts, art documentaries and films that merely contain event words" in {
    cinemaContent.foreach { t =>
      withClue(s"expected KEPT: [$t] ") { NonMovieEventClassifier.isLiveEvent(t) shouldBe false }
    }
  }

  it should "not flag ordinary film titles" in {
    List(
      "Dune: Part Two", "Diabeł ubiera się u Prady 2", "Avatar",
      "Społeczeństwo spektaklu",          // "spektaklu" inflected — a real film
      "Super Mario Galaxy Film",          // "Galaxy" ≠ \bgala\b
      "Żółtodzioby Galaktyki"             // "Galaktyki" ≠ \bgala\b
    ).foreach(t => withClue(s"[$t] ") { NonMovieEventClassifier.isLiveEvent(t) shouldBe false })
  }
}
