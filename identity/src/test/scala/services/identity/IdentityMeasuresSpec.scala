package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.DurationInt
import services.movies.TitleSeason
import services.identity.IdentityMeasures.{Category, Film, Listing, ListingListing, Missing, Number}
import services.identity.StageRelays.{Billing, Houses}

/** What the calibrated score reads must separate facts that mean different things: a venue's
 *  published year, a year the venue put in its title, a season a broadcast names, and whether a
 *  title DECORATES a film's title or is a FRAGMENT of a longer one. */
class IdentityMeasuresSpec extends AnyFlatSpec with Matchers {

  private def measures(l: Listing, f: Film) = IdentityMeasures.listingFilm(l, f, None, 0, 0)

  /** Two of a title's shapes need its END: a bracketed format ("Vivaldi i ja [2D LEKTOR]") and a screening year ("Ma to
   *  sens 2026"). Their patterns, anchored at the end but scanning from the start, backtrack over every title that has
   *  neither -- 11.8% of a convergence leg's single-threaded take-up and agreement (JFR, run 38553692147) -- so a title
   *  that cannot end so is never scanned. A long title shows the scan: quadratic in its length. */
  "a title's format and screening-year shapes" should "still be cut where the title ends in one" in {
    IdentityMeasures.shapes(Seq("Vivaldi i ja [2D LEKTOR]")) should contain("Vivaldi i ja")
    IdentityMeasures.shapes(Seq("Ma to sens 2026")) should contain("Ma to sens")
    IdentityMeasures.shapes(Seq("Blade Runner 2049")) should not contain "Blade Runner"
  }

  "a billing's piece naming only its event" should "name no film a rule takes, its own title and a banner's words read as before" in {
    // UK The Garden Cinema's "The Assassin + Q&A" searched "Q&A" and stood Q & A (1990) beside Hou Hsiao-hsien's film as
    // the runner-up its own facts did not outweigh; Ciné Lumière's "Inside Gaza + Q&A" took it as its best candidate
    // (prod 2026-11-01)
    def billed(raw: String) = IdentityMeasures.Listing(raw, Some(raw), searchTitles = raw.split("""\s*\+\s*""").toSeq)
    val qa = Film("Q & A", year = Some(1990))
    IdentityMeasures.eventNamed(billed("The Assassin + Q&A"), qa) shouldBe true
    IdentityMeasures.eventNamed(billed("The Assassin + Q&A"), Film("The Assassin", year = Some(2015))) shouldBe false
    IdentityMeasures.eventNamed(billed("Who are you, Polly Maggoo ? + panel"), Film("Panel", year = Some(2026))) shouldBe true
    IdentityMeasures.eventNamed(billed("The Odyssey + Interval"), Film("Interval", year = Some(2019))) shouldBe true
    // UK Picture House Uckfield's "Scottish Ballet: These Violent Delights + Q&A" searched "Q&A", and DCA's
    // "These Violent Delights + Recorded Q&A" names the film only inside its event piece: a run of the event's words
    IdentityMeasures.eventNamed(billed("These Violent Delights + Recorded Q&A"), Film("Q&A", year = Some(2025))) shouldBe true
    // a film called by an event word is billed by its title alone, or beside another work
    IdentityMeasures.eventNamed(billed("Q&A"), qa) shouldBe false
    IdentityMeasures.eventNamed(billed("Rise + Fight Like a Girl"), Film("Rise", year = Some(2022))) shouldBe false
    IdentityMeasures.namesOnlyAnEvent("Writer/Director Q&A") shouldBe false
    IdentityMeasures.namesOnlyAnEvent("The Graduate") shouldBe false
    IdentityMeasures.namesOnlyAnEvent("Q&A") shouldBe true
    // a banner's own event words are its programme's: only a piece joined by a "+" is the event (DE Euromax's "HALLOWEEN -
    // SPECIAL - ZOMBIE-DAWN OF THE DEAD & …", measure 41348646489)
    IdentityMeasures.joinedEvents(billed("HALLOWEEN - SPECIAL - ZOMBIE-DAWN OF THE DEAD")) shouldBe empty
  }



  they should "cost a title that ends in neither no scan of it" in {
    val long = ("Kino Seniora " * 1500).trim
    // CPU, not wall time: a quadratic scan of a 1,500-word title takes seconds of it, a scan that never starts microseconds
    // (wall time read 113 ms of a 100 ms bound at load 150, 2026-10-18)
    tools.costs.CpuMeter.fastest()(IdentityMeasures.shapes(Seq(long))) should be < 100.millis
  }

  "a billing's banner or strand" should "leave the film's title a shape, each case from prod's unmatched listings (2026-11-01)" in {
    def shapes(title: String) = IdentityMeasures.shapes(Seq(title))
    // an edition closing the title after its stop
    shapes("TOUCH OF EVIL: THE RESTORED CUT") should contain("TOUCH OF EVIL")
    shapes("Kult-Special: Blade Runner - The Final Cut") should contain("Blade Runner")
    shapes("The Lord of the Rings: The Return of the King - Extended Edition") should contain("The Lord of the Rings: The Return of the King")
    // a certificate note the billing runs on past
    shapes("Ken Russell's The Devils (18) Directors Cut") should contain("Ken Russell's The Devils")
    shapes("Wicker (15) 105 mins 05.01.27") should contain("Wicker")
    // a case change: a strand closing it, a programme heading it, the film in capitals after the programme
    shapes("Lalka.  DZIEŃ KINA POLSKIEGO") should contain("Lalka")
    shapes("Chłopiec w różowych spodniach-DZIECIŃSTWO BEZ PRZEMOCY") should contain("Chłopiec w różowych spodniach")
    shapes("Godard est mort, vive Godard! DIE PASSION DER JUNGFRAU VON ORLÉANS") should contain("DIE PASSION DER JUNGFRAU VON ORLÉANS")
    shapes("UK PREMIERE For The Plasma") should contain("For The Plasma")
    // the one title a long announcement quotes
    shapes("DKF ZAPRASZA 13 LISTOPADA O GODZ. 18:00 NA FILM „NASZA REWOLUCJA” –  HISTORIĘ MIŁOŚCI SILNIEJSZEJ NIŻ STRACH") should
      contain("NASZA REWOLUCJA")
  }

  it should "keep a film's own subtitle, number and stop whole" in {
    def shapes(title: String) = IdentityMeasures.shapes(Seq(title))
    // a subtitle in capitals after a colon or a spaced dash is the film's, never a case change to split at
    IdentityMeasures.caseSplit("Mission: Impossible – THE FINAL RECKONING") shouldBe empty
    IdentityMeasures.caseSplit("Avatar: FIRE AND ASH") shouldBe empty
    // one word in capitals is a numeral or a name, no strand
    IdentityMeasures.caseSplit("Rocky IV") shouldBe empty
    // a Roman numeral is the film's number, never a word of a run in capitals: DE Babylon's "Halloween II [OV]"
    IdentityMeasures.caseSplit("Halloween II [OV]") shouldBe empty
    IdentityMeasures.caseSplit("Rocky III NEW RESTORATION") should contain("Rocky III")
    IdentityMeasures.caseSplit("NT Live: Hay Fever") shouldBe empty
    IdentityMeasures.caseSplit("M3GAN 2.0") shouldBe empty
    // a sentence's stop between words of one case is the title's ("Szkoła magicznych zwierząt. Tajemnica…")
    IdentityMeasures.caseSplit("Szkoła magicznych zwierząt. Tajemnica szkolnego podwórka") shouldBe empty
    IdentityMeasures.caseSplit("DIUNA: CZĘŚĆ TRZECIA") shouldBe empty
    // a capitals run a sentence goes on from in lower case names no film of its own (PL "VAN GOGH w The National
    // Gallery London" took Pialat's Van Gogh), nor one holding a programme's parts (PL "24. Festiwal TERRA - …" took
    // Terra, 2007), nor a half only of words billed around a film (DE "FREI~QUENZ Anniversary" took Komasa's)
    IdentityMeasures.caseSplit("VAN GOGH w The National Gallery London") shouldBe empty
    IdentityMeasures.caseSplit("24. Festiwal TERRA - BANGLADESZ - NIEZNOŚNY CIĘŻAR ŻYCIA") shouldBe empty
    IdentityMeasures.caseSplit("FREI~QUENZ Anniversary") shouldBe empty
    // a case change inside a bracket is the note's: no split leaves a bracket open
    IdentityMeasures.caseSplit("KADR JUNIOR (6-12 lat): Baranek Shaun i kudłata bestia") shouldBe empty
    // what follows the stop must be an edition's words only
    shapes("Dune: Part Two") should not contain "Dune"
    IdentityMeasures.shapes(Seq("Dune: Part One Reissue")) should not contain "Dune"
    shapes("The Final Cut") shouldBe Seq("The Final Cut")
    // a short billing quoting a work bills a talk or a show about it
    shapes("Wojciech Cejrowski i „Prawo Dżungli”") should not contain "Prawo Dżungli"
    // a short screened before the feature it quotes is no announcement of that feature
    shapes("Mystery-Vorfilm als Warm-up zu „Miles Davis – Birth of The Cool“") should not contain "Miles Davis – Birth of The Cool"
    // a bracketed number is no certificate
    shapes("Sweet Sixteen (16) 2002") should not contain "Sweet Sixteen"
  }

  "an original title" should "share nothing with the billing only when it is another title altogether, not a spelling of it" in {
    // DE kinoheld's event pages: what a search for the billing hit, copied as the original
    Listing("Abdul Kader Chahin", originalTitle = Some("秒速5センチメートル")).originalSharesNothingBilled shouldBe true
    Listing("Dirty Deeds - A Tribute To AC/DC", originalTitle = Some("Salaam-E-Ishq")).originalSharesNothingBilled shouldBe true
    // the billed work in its own language, or the billing restated
    Listing("SEVENTEEN WORLD TOUR 'NEW_'", originalTitle = Some("SEVENTEEN WORLD TOUR [NEW_] ENCORE")).originalSharesNothingBilled shouldBe false
    Listing("Kardes Türküler ile 30 yil", originalTitle = Some("Kardeş Türküler ile 30 Yıl")).originalSharesNothingBilled shouldBe false
    Listing("Puella Magi Madoka Magica Der Film - Walpurgisnacht: Rising",
      originalTitle = Some("Puella Magi Madoka Magica the Movie -Walpurgisnacht: Rising-")).originalSharesNothingBilled shouldBe false
    Listing("Bremer Stadtmusikanten").originalSharesNothingBilled shouldBe true
    // what a billing carries of a record's title: words, not articles or years — two make the record the billing's
    Listing("Madoka Magica - The Movie: Rebellion").wordsBilledOf(
      "Mahou Shoujo Madoka Magica the Movie (Part 3): The Story of the Rebellion") shouldBe 4
    Listing("Über Unterbiberger - Vom Woid in die Welt").wordsBilledOf("Oktober 1917 – Zehn Tage, die die Welt erschütterten") shouldBe 1
    Listing("Zum Dorfkrug").wordsBilledOf("Zum See") shouldBe 0
  }

  "an original title" should "translate the billing word for word only as a cognate of each of its words" in {
    // DE Filmmuseum München: kinoheld copied the Dutch original of the billed German title
    Listing("Mein Opa der Bankräuber", originalTitle = Some("Mijn Opa de Bankrover")).originalTranslatesBilled shouldBe true
    // another title altogether, word counts alike or not
    Listing("Das Auge des Bösen", originalTitle = Some("Eye of the Devil")).originalTranslatesBilled shouldBe false
    Listing("ZIRKUS FUMEUSEMENT", originalTitle = Some("The Circus")).originalTranslatesBilled shouldBe false
    Listing("Abdul Kader Chahin", originalTitle = Some("5 Centimeters per Second")).originalTranslatesBilled shouldBe false
    Listing("Bremer Stadtmusikanten").originalTranslatesBilled shouldBe false
  }

  "a bracketed year in a title" should "be its own measure, never the listing's published year" in {
    // A re-release bracket: the 2015 film shown in 2026.
    val m = measures(Listing("The Hunger Games: Mockingjay - Part 2 (2026)"), Film("The Hunger Games: Mockingjay - Part 2", year = Some(2015)))
    m("year.delta") shouldBe Missing("listing")
    m("titleYear.delta") shouldBe Number(-11)
    measures(Listing("Belle", year = Some(2013)), Film("Belle", year = Some(2013)))("titleYear.delta") shouldBe Missing("listing")
  }

  // prod PL 2026-10-06: Multikino's "Fallen Angels by Noël Coward" (16 venues) — the filmed Broadway production of
  // Coward's play — was taken as Dhont's 2026 "Coward" ("Tchórz"): the playwright's surname, an edge of the title, read
  // as a film's title "decorated" by the rest, as decorated as "Fallen Angels" itself
  "an author's credit after a work ('… by Noël Coward')" should "name no film, and leave the work it credits searched and named" in {
    val credited = Listing("Fallen Angels by Noël Coward")
    IdentityMeasures.titleRelation(credited, Film("Coward")).value shouldBe "overlap"
    IdentityMeasures.titleRelation(credited, Film("Noël Coward")).value shouldBe "overlap"
    IdentityMeasures.titleRelation(credited, Film("Fallen Angels")).value shouldBe "segment"
    IdentityMeasures.searchQueries(credited) should contain ("Fallen Angels")
    // the corpus's learned edge runs never cut into the credit: 16 Multikinos billing the title taught its head as a prefix
    val decorated = credited.copy(decorations = TitleDecorations(Set(Seq("fallen", "angels", "by", "noel")), Set.empty))
    IdentityMeasures.titleRelation(decorated, Film("Tchórz", originalTitle = Some("Coward"))).value shouldBe "overlap"
    // a title whose own words hold "by" is no credit: one word after it, or none capitalised
    IdentityMeasures.titleRelation(Listing("Stand by Me"), Film("Stand by Me")).value shouldBe "exact"
    IdentityMeasures.titleRelation(Listing("Death by Chocolate"), Film("Chocolate")).value shouldBe "decorated"
    IdentityMeasures.searchQueries(Listing("Żyć by tańczyć")) shouldBe Seq("Żyć by tańczyć")
  }

  "an author's credit in any language ('… von Friedrich Dürrenmatt', '… autorstwa …')" should "leave the work it credits searched" in {
    // the billing grammar's Credits reads every language's authorship word; the search once read only English "by"
    IdentityMeasures.searchQueries(Listing("Die Physiker von Friedrich Dürrenmatt")) should contain ("Die Physiker")
    IdentityMeasures.searchQueries(Listing("Wesele autorstwa Stanisława Wyspiańskiego")) should contain ("Wesele")
    IdentityMeasures.creditedWork("Fallen Angels by Noël Coward") shouldBe Some("Fallen Angels")
    IdentityMeasures.creditedWork("Stand by Me") shouldBe None
    // prod DE Kinostar Arthaus Heilbronn (run 1051): the pianist accompanying a silent film is no author of it
    IdentityMeasures.creditedWork("THE GENERAL - Stummfilm mit Klavierbegleitung von Andreas Benz") shouldBe None
    // UK Lowther Pavilion (run 1057): a credit a bracket follows leaves the title whole
    IdentityMeasures.creditedWork("On Screen: Fallen Angels by Noel Coward (15)") shouldBe None
  }

  "a title's credited-work words" should "be its credit-stripped words, read once per title and alike from any thread" in {
    // owner-approved 2026-11-03: `containment` once re-ran the credit's regex for every (listing, film) pair; the form now
    // holds it. Seeded random titles over authorship words, names, brackets and accompaniments: the memo must give
    // exactly the per-pair expression it replaced, the same on every read and from parallel first reads.
    val vocabulary = Seq("Fallen", "Angels", "by", "von", "autorstwa", "Noël", "Coward", "Friedrich", "Dürrenmatt", "Stand",
      "Me", "death", "Chocolate", "mit", "Klavierbegleitung", "live", "music", "Andreas", "Benz", "(15)", "-", ":", "The",
      "Wesele", "Stanisława", "Wyspiańskiego", "Ken", "Russell's", "2026", "&", "Żyć", "tańczyć", "de", "la", "Dracula")
    val random = new scala.util.Random(20261103L)
    val titles = Seq.fill(4000)(Seq.fill(1 + random.nextInt(7))(vocabulary(random.nextInt(vocabulary.size))).mkString(" "))
    def before(text: String) = IdentityMeasures.creditedWork(text).fold(IdentityMeasures.words(text))(IdentityMeasures.words)
    titles.count(t => IdentityMeasures.creditedWork(t).isDefined) should be > 100
    titles.foreach { t =>
      val form = IdentityMeasures.TitleForm(t)
      form.creditedWords shouldBe before(t)
      form.creditedWords shouldBe before(t)
    }
    val shared = titles.map(IdentityMeasures.TitleForm(_))
    val read   = java.util.concurrent.Executors.newFixedThreadPool(8)
    try {
      val reads = (1 to 8).map(_ => read.submit(() => shared.map(_.creditedWords)))
      reads.map(_.get).distinct shouldBe Seq(titles.map(before))
    } finally read.shutdown()
  }

  "a bracket dating the title beside a name" should "date the film by a studio's, never by a month, an event or an edition" in {
    IdentityMeasures.titleYearOf(Seq("DRACULA (Hammer 1958) 4K Restoration - Directors Cut")) shouldBe Some(1958)
    IdentityMeasures.titleYearOf(Seq("Der Physiopath (März 2027)")) shouldBe None
    IdentityMeasures.titleYearOf(Seq("Krimizeit (Krimifestival 2026)")) shouldBe None
    IdentityMeasures.titleYearOf(Seq("Jaws (Re-release 2025)")) shouldBe None
    IdentityMeasures.titleYearOf(Seq("Akira (4K 2025)")) shouldBe None
    // DE Capitol Eisenach's "Metropolis (rest. Fassung 2010)": a German edition's word dates the restoration, not Lang's 1927 film —
    // read as dated, it took Mizue's 2009 short "Metropolis"
    IdentityMeasures.titleYearOf(Seq("Metropolis (rest. Fassung 2010)")) shouldBe None
  }

  // UK Picture House Uckfield's "(ROH)" series (capture 2026-10-13): "Tosca (ROH)" {Oliver Mears} [205′] screens the RBO
  // 2026/27 relay, whose record no search of the work's name reaches; a credit kept the season out of its searches
  "a relay billing its house beside a credit or a running time" should "be searched with the season its screenings fall in" in {
    val tosca = Listing("Tosca (ROH)", runtime = Some(205), directors = Seq("Oliver Mears"), screenedSeason = Some(2026))
    IdentityMeasures.searchQueries(tosca) should contain ("Tosca 2026")
    // a film named like a stage work, no house billed: its season is no search of it
    IdentityMeasures.searchQueries(Listing("Tosca", runtime = Some(105), directors = Seq("Benoît Jacquot"), screenedSeason = Some(2026))) should
      not contain "Tosca 2026"
    // a bare stage work stating only a running time names no staging: UK Empire Theatre Halstead's "Romeo and Juliet"
    // [170′], screening on 25 May 2027, is searched with its season, which reaches the RBO 2026/27 relay aired that day
    IdentityMeasures.searchQueries(Listing("Romeo and Juliet", runtime = Some(170), screenedSeason = Some(2026))) should
      contain ("Romeo and Juliet 2026")
    // …but a film's title going on past the work names no relay: the musical's name is only its head
    IdentityMeasures.searchQueries(Listing("Lord Of The Rings: The Fellowship Of The Ring", runtime = Some(178), screenedSeason = Some(2026))) should
      not contain "Lord of the Rings 2026"
    // Flicks dates the relay by the page it links: the year it states is searched, and the season it screens in beside it
    val carmen = IdentityMeasures.searchQueries(Listing("Royal Opera House: Carmen", year = Some(2024), runtime = Some(220), screenedSeason = Some(2026)))
    carmen should contain allOf ("Carmen 2024", "Carmen 2026")
    // a relay series' banner beside the work bills the relay as a house does: UK Picture House Uckfield's "Otello (MET)"
    // [200′], Savoy Worksop's "MET:Otello" and DCA's "Live from the Met: Manon" (recording 573) screen the Met's 2026/27
    // relays, which no search of the work's name alone reaches
    IdentityMeasures.searchQueries(Listing("Otello (MET)", runtime = Some(200), screenedSeason = Some(2026))) should contain ("Otello 2026")
    IdentityMeasures.searchQueries(Listing("MET:Otello", runtime = Some(200), screenedSeason = Some(2026))) should contain ("Otello 2026")
    IdentityMeasures.searchQueries(Listing("Live from the Met: Manon", runtime = Some(240), screenedSeason = Some(2026))) should
      contain ("Manon 2026")
  }

  // UK Lanternhouse's "Obsession (18)" (wrong-match hunt 10-14): the whole title, searched as billed, found only "80 Day
  // Obsession: Day 18" — the box office's BBFC certificate read as the title's own number
  "a box office's trailing certificate" should "be no part of any title shape or search" in {
    IdentityMeasures.searchQueries(Listing("Obsession (18)")) shouldBe Seq("Obsession")
    Listing("Obsession (18)").shapes shouldBe Seq("Obsession")
    IdentityMeasures.searchQueries(Listing("Romeo & Juliet (12A TBC)", rawTitle = Some("Romeo & Juliet (12A TBC)"))) shouldBe Seq("Romeo & Juliet")
    // a number that is no certificate stays the title's own
    IdentityMeasures.searchQueries(Listing("Sweet Sixteen (16)")) should contain ("Sweet Sixteen (16)")
  }

  // PL Kino Fenix's Konwicki retrospective (review answer): "Konwicki Salto", "Konwicki Ostatni dzień lata" — the
  // director's surname heads each title, and no field credits him
  "a director's surname heading a title" should "credit the film's director whose surname it is, when the rest names the film" in {
    val salto = Film("Salto", directors = Some(Seq("Tadeusz Konwicki")))
    def director(title: String, f: Film) = IdentityMeasures.creditedByTitle(Listing(title), f, measures(Listing(title), f))("director")
    director("Konwicki Salto", salto) shouldBe Category("same_person")
    director("Konwicki: Salto", salto) shouldBe Category("same_person")
    // a surname of four letters names its director as one of five does, as a namesake's banner reads it
    // (Agreement.billsAnothersDirector): DE "Fritz Lang: M" is Lang's 1931 film, not Losey's 1951 remake
    val m = Film("M", directors = Some(Seq("Fritz Lang")))
    director("Fritz Lang: M", m) shouldBe Category("same_person")
    director("Fritz Lang: M", m.copy(directors = Some(Seq("Joseph Losey")))) shouldBe Missing("listing")
    // a record titling the film by its director's name too still names him by the banner: DE Babylon (Mitte)'s
    // "Hitchcock & Chabrol: Spellbound" is the 1945 film, which TMDB also titles "Alfred Hitchcock's Spellbound"
    val spellbound = Film("Ich kämpfe um Dich", originalTitle = Some("Spellbound"), alternativeTitles = Seq("Alfred Hitchcock's Spellbound"),
      directors = Some(Seq("Alfred Hitchcock")))
    director("Hitchcock & Chabrol: Spellbound", spellbound) shouldBe Category("same_person")
    // another director, a rest naming another film, a credit the listing states, a title that is the film's own: untouched
    director("Konwicki Salto", salto.copy(directors = Some(Seq("Jan Kowalski")))) shouldBe Missing("listing")
    director("Konwicki Salto Mortale", salto) shouldBe Missing("listing")
    director("Salto", salto) shouldBe Missing("listing")
    val credited = Listing("Konwicki Salto", directors = Seq("Jan Kowalski"))
    IdentityMeasures.creditedByTitle(credited, salto, measures(credited, salto))("director") should not be Category("same_person")
  }

  // ES taquilla's "Hope" (2026-11-02): no director field, a synopsis "Basada en la propia historia de su directora Maria
  // Sødahl"; UK Chichester's "The Ties That Bind Us": "writer/director Carine Tardieu", L'Attachement
  "a venue's own text naming a director whole" should "credit the film it names, and discredit no other" in {
    val hope     = Film("Hope", year = Some(2019), directors = Some(Seq("Maria Sødahl")))
    val namesake = Film("Hope", year = Some(2026), directors = Some(Seq("Na Hong-jin")))
    def director(text: String, f: Film, l: Listing = Listing("Hope"), pool: Seq[Film] = Seq(hope, namesake)) = {
      val names = VenueNames.of(Seq(text))
      IdentityMeasures.creditedByText(f, IdentityMeasures.textNamedDirectors(l, names.names, pool), measures(l, f))("director")
    }
    val described = "Basada en la propia historia de su directora Maria Sødahl."
    director(described, hope) shouldBe Category("same_person")
    // a text naming someone who directed ANOTHER film says nothing against this one: synopses name stars and crew who
    // direct too -- UK Riverside Woodbridge's "Rocky 50th Anniversary" names Stallone, director of Rocky II-IV, not of
    // Avildsen's 1976 film the measure (2026-11-02) then denied
    director(described, namesake) shouldBe Missing("listing")
    val rocky  = Film("Rocky", year = Some(1976), directors = Some(Seq("John G. Avildsen")))
    val sequel = Film("Rocky II", year = Some(1979), directors = Some(Seq("Sylvester Stallone")))
    director("Sylvester Stallone stars as Rocky Balboa.", rocky, Listing("Rocky 50th Anniversary"), Seq(rocky, sequel)) shouldBe Missing("listing")
    // a surname alone says nothing
    director("La historia de Sødahl.", hope) shouldBe Missing("listing")
    director("La historia de Sødahl.", namesake) shouldBe Missing("listing")
    // a person the billing's title names is its subject, not its credit: the comedian of "NURSE GEORGIE CARROLL:
    // INFECTIOUS", whom IMDb credits as director of an earlier special, takes nothing from this one
    val special   = Film("Infectious", directors = Some(Seq("Jim Hare")))
    val earlier   = Film("Nurse Georgie Carroll: The Gloves Are Off", directors = Some(Seq("Georgie Carroll")))
    val billed    = Listing("NURSE GEORGIE CARROLL: INFECTIOUS")
    val comedian  = VenueNames.of(Seq("Award winning comedian and NHS nurse Georgie Carroll is back on tour."))
    IdentityMeasures.textNamedDirectors(billed, comedian.names, Seq(special, earlier)) shouldBe empty
    // a listing crediting a director of its own keeps its own credit
    val credited = Listing("Hope", directors = Seq("Na Hong-jin"))
    director(described, namesake, credited) shouldBe Category("same_person")
    director(described, hope, credited) should not be Category("same_person")
  }

  // DE Eulenspiegel's "King Kong" (Cineamo 244m, German premiere 1933-12-01): since f5784855d an old premiere states no
  // year, and the listing took the 2005 remake. The premiere still bounds the film: none premieres before it is made.
  "a year upper bound" should "weigh as the year it bounds against a film released after it, and only there" in {
    val bounded = Listing("King Kong", yearUpperBound = Some(1933))
    measures(bounded, Film("King Kong", year = Some(2005)))("year.delta") shouldBe Number(-72)
    measures(bounded, Film("King Kong", year = Some(2005)))("year.distance") shouldBe Number(72)
    // a film from before the bound's year is no match by it, only not ruled out: no year-equality reward
    measures(Listing("Rope", yearUpperBound = Some(1963)), Film("Rope", year = Some(1948)))("year.delta") shouldBe Missing("listing")
    measures(bounded, Film("King Kong", year = Some(1932)))("year.delta") shouldBe Missing("listing")
    // the film made the premiere's very year is read as of the stated year: DE "Der kleine Prinz" (Cineamo 334199m,
    // premiered 1972) is the 1972 film, not the 1962 or the 1974 namesake
    measures(bounded, Film("King Kong", year = Some(1933)))("year.delta") shouldBe Number(0)
    measures(Listing("Der kleine Prinz", yearUpperBound = Some(1972)), Film("Der kleine Prinz", year = Some(1972)))("year.distance") shouldBe Number(0)
    measures(Listing("Der kleine Prinz", yearUpperBound = Some(1972)), Film("Der kleine Prinz", year = Some(1962)))("year.delta") shouldBe Missing("listing")
    // a year past the premiere by the adjacency a festival premiere is dated by, and a release TMDB dates by it, are no contradiction
    measures(Listing("Momo", yearUpperBound = Some(2024)), Film("Momo", year = Some(2025)))("year.delta") shouldBe Missing("listing")
    measures(bounded, Film("King Kong", year = Some(2005), releases = Some("DE1933-")))("year.delta") shouldBe Missing("listing")
    // a year the listing states is its own evidence, the bound unread
    measures(bounded.copy(year = Some(2005)), Film("King Kong", year = Some(2005)))("year.delta") shouldBe Number(0)
  }

  // ES Cines Bulevar's "Drácula. 30 Aniversario" (review answer: Coppola's 1992 film), its original title "Bram Stoker's
  // Dracula 30th Anniversary": the anniversary and the possessive credit are searched without in the original title too
  "an anniversary re-release's original title" should "be searched without its anniversary" in {
    IdentityMeasures.searchQueries(Listing("Drácula. 30 Aniversario", originalTitle = Some("Bram Stoker's Dracula 30th Anniversary"))) should
      contain ("Bram Stoker's Dracula")
  }

  it should "read the year it states as the anniversary's: the film's year that many years before, no other film's" in {
    val dracula = Film("Bram Stoker's Dracula", year = Some(1992))
    val billed  = Listing("Drácula. 30 Aniversario", year = Some(2022), originalTitle = Some("Bram Stoker's Dracula 30th Anniversary"))
    measures(billed, dracula)("year.delta") shouldBe Missing("listing")
    measures(Listing("Dracula 30th Anniversary", year = Some(2022)), dracula)("year.delta") shouldBe Missing("listing")
    // a year that is not the anniversary's stays the listing's fact
    measures(billed, dracula.copy(year = Some(2010)))("year.delta") shouldBe Number(12)
    measures(Listing("Dracula", year = Some(2022)), dracula)("year.delta") shouldBe Number(30)
  }

  // prod DE 2026-10-13 (identity take change log): Apollo Aachen's "Der mit dem Wolf tanzt (35-jähriges Jubiläum)",
  // dated 2026, read 36 years off Costner's 1990 film — its German anniversary unread — and the veto it earned went to
  // every venue's listing joined to it by original title: tmdb 581 lost ×17 ("(OmU)", "Extented Edition" among them)
  it should "be read in the anniversary words of the languages venues bill re-releases in" in {
    val wolves = Film("Der mit dem Wolf tanzt", year = Some(1990), originalTitle = Some("Dances with Wolves"))
    Seq("Der mit dem Wolf tanzt (35-jähriges Jubiläum)", "Der mit dem Wolf tanzt (35- jähriges Jubiläum)",
        "Der mit dem Wolf tanzt - 35. Jubiläum", "Danse avec les loups (35e anniversaire)", "Tańczący z wilkami - 35. rocznica").foreach { t =>
      withClue(t) { measures(Listing(t, year = Some(2026)), wolves)("year.delta") shouldBe Missing("listing") }
    }
    // a number beside no anniversary word, or a year not the anniversary's, stays the listing's fact
    measures(Listing("Der mit dem Wolf tanzt (35 Jahre)", year = Some(2026)), wolves)("year.delta") shouldBe Number(36)
    measures(Listing("Der mit dem Wolf tanzt (20-jähriges Jubiläum)", year = Some(2026)), wolves)("year.delta") shouldBe Number(36)
    // a re-release TMDB dates in the year stated is the year's own evidence: the anniversary never takes it away
    measures(Listing("Der mit dem Wolf tanzt (35-jähriges Jubiläum)", year = Some(2026)), wolves.copy(releases = Some("DE1991-DE2026-")))(
      "year.delta") shouldBe Number(0)
  }

  it should "date two listings of one re-release alike, one by the screening's year and one by the film's" in {
    val reRelease = Listing("Der mit dem Wolf tanzt (35-jähriges Jubiläum)", year = Some(2026))
    val original  = Listing("DER MIT DEM WOLF TANZT (35-JÄHRIGES JUBILÄUM)", year = Some(1991))
    IdentityMeasures.listingListing(reRelease, original, sameVenue = false, None)("year.delta") shouldBe Missing("listing")
    IdentityMeasures.listingListing(original, reRelease, sameVenue = false, None)("year.delta") shouldBe Missing("listing")
    // without an anniversary the 35 years stand
    IdentityMeasures.listingListing(Listing("Der mit dem Wolf tanzt", year = Some(2026)), original.copy(title = "Der mit dem Wolf tanzt"),
      sameVenue = false, None)("year.delta") shouldBe Number(35)
  }

  "a season a broadcast names" should "be read as its own measure, in every spelling, and never as a bracketed year" in {
    val film = Film("Samson et Dalila", year = Some(1949))
    Seq("Samson i dalila | metropolitan opera: live in hd 2026/27", "OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA",
        "Met Opera 2026-27: Samson et Dalila", "Samson i Dalila (sezon 2026/27)", "Sezon 2026-2027 - Samson i Dalila").foreach { t =>
      withClue(t) {
        val m = measures(Listing(t), film)
        m("season.delta") shouldBe Number(-77)
        m("titleYear.delta") shouldBe Missing("listing")
      }
    }
    // Not a season: consecutive digits of one number, a year range that is not one season apart.
    measures(Listing("Blade Runner 2049"), film)("season.delta") shouldBe Missing("listing")
    measures(Listing("Retrospektywa 1990-1999"), film)("season.delta") shouldBe Missing("listing")
  }

  it should "be read in a box office's short spellings" in {
    // Prod UK 2026-10-13: Keswick's "Royal Ballet 2026/7: Romeo & Juliet" named its season, unread, and left the 2026/27
    // record below the threshold.
    TitleSeason.of(Seq("Royal Ballet 2026/7: Romeo & Juliet")) shouldBe Some(2026)
    TitleSeason.of(Seq("Met Opera 2029/0: Tosca")) shouldBe Some(2029)
    // nor two numbers a slash joins, never a season the title states: an age band, a date, and a banner left from the
    // season it was first billed in — DE "Berliner Philharmoniker 25/26" screens the 2026 Silvesterkonzert on 31 December
    // 2026, and read as season 2025 it held ×73 spellings of the concert apart (identity measure, recording 38363428661).
    // A relay's "RB&O 26/27" takes its season from its screenings ([[IdentityMeasures.Listing.broadcastSeason]]).
    TitleSeason.of(Seq("Berliner Philharmoniker 25/26: Silvesterkonzert")) shouldBe None
    TitleSeason.of(Seq("RB&O 26/27: Romeo & Juliet")) shouldBe None
    TitleSeason.of(Seq("Kids Club 12/13")) shouldBe None
    TitleSeason.of(Seq("Live 24/7")) shouldBe None
    TitleSeason.of(Seq("Royal Ballet 2026/8: Romeo & Juliet")) shouldBe None
  }

  "a season production named in another language" should "be the record's, by the stage work both name" in {
    // PL venues bill the RBO's 2026/27 season in Polish: "Dziadek do orzechów" is the record's "The Nutcracker"
    // (Wikidata Q193705, StageWorks), as "Jezioro łabędzie" is Swan Lake — not The Nutcracker
    val nutcracker = Film("Royal Ballet & Opera 2026/27: The Nutcracker", year = Some(2026))
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów"), nutcracker) shouldBe true
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Jezioro łabędzie"), nutcracker) shouldBe false
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2025-26: Dziadek do orzechów"), nutcracker) shouldBe false
    // one work, another house: PL "Balet z Opery Paryskiej 2026-2027: Jezioro łabędzie" ×16 is the Paris Opera Ballet's
    // Swan Lake, which TMDB has no record of — not the Royal Ballet's (recording 37116030016 took it)
    val rboSwanLake = Film("Royal Ballet & Opera 2026/27: Swan Lake", year = Some(2026))
    StageRelays.namesSeasonProduction(Listing("Balet z Opery Paryskiej 2026-2027: Jezioro łabędzie"), rboSwanLake) shouldBe false
    // nor by the art form both houses' names carry: DE "Opera de Paris: Romeo et Juliette (2026/27)" ×7 took the Royal
    // Ballet's 2026/27 record by "opera" (identity measure on recording 38363428661)
    StageRelays.namesSeasonProduction(Listing("Opera de Paris: Romeo et Juliette (2026/27)"),
      Film("RBO 2026/27: Romeo und Julia (The Royal Ballet)", originalTitle = Some("Royal Ballet & Opera 2026/27: Romeo and Juliet"),
        year = Some(2027))) shouldBe false
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Jezioro łabędzie"), rboSwanLake) shouldBe true
    StageRelays.namesSeasonProduction(Listing("OPERA 2026/2027 - MAKBET"),
      Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026))) shouldBe true
    // Wheeldon's Alice, under five sitelinks, from the extra; a work's name running on into a translated subtitle
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Alicja w Krainie Czarów"),
      Film("Royal Ballet & Opera 2026/27: Alice's Adventures in Wonderland", year = Some(2027))) shouldBe true
    StageRelays.namesSeasonProduction(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Cosi fan tutte. Tak czynią wszystkie"),
      Film("Royal Ballet & Opera 2026/27: Così fan tutte", year = Some(2027))) shouldBe true
    // and is searched for by its own name in the season, not only by the whole subtitled piece
    IdentityMeasures.searchQueries(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Cosi fan tutte. Tak czynią wszystkie")) should
      contain ("Così fan tutte 2026")
    // a venue's spelling Wikidata lacks, from the hand-kept extra: "Zaczarowany flet" is The Magic Flute
    StageRelays.namesSeasonProduction(Listing("OPERA 2026/2027 - ZACZAROWANY FLET"),
      Film("The Metropolitan Opera 2026/27: The Magic Flute", year = Some(2026))) shouldBe true
  }

  "a film record of a broadcast's season production" should "be a segment of the listing's title, and only for a film" in {
    val met = Listing("Met Opera 2026-27: Samson et Dalila")
    val record = Film("The Metropolitan Opera 2026/27: Samson et Dalila", year = Some(2026))
    StageRelays.namesSeasonProduction(met, record) shouldBe true
    IdentityMeasures.titleRelation(met, record) shouldBe Category("segment")
    StageRelays.namesSeasonProduction(Listing("Samson i dalila | metropolitan opera: live in hd 2026/27"),
      record.copy(alternativeTitles = Seq("The Metropolitan Opera 2026/27: Samson i Dalila"))) shouldBe true
    // Another season, another work of the season, or no season on one side: not its production.
    IdentityMeasures.titleRelation(met, record.copy(title = "The Metropolitan Opera 2025/26: Samson et Dalila")) shouldBe Category("overlap")
    StageRelays.namesSeasonProduction(met, Film("The Metropolitan Opera 2026/27: Macbeth")) shouldBe false
    StageRelays.namesSeasonProduction(Listing("Samson et Dalila"), record) shouldBe false
    // Two listings' banners do not say which house staged the work: the Met's and the RBO's
    // "Carmen" of one season are not one production by title.
    IdentityMeasures.listingListing(Listing("Met Opera 2026-27: Carmen"), Listing("RBO Cinema Season 2026-27: Carmen"),
      sameVenue = false, sharedChainId = None)("title") shouldBe Category("overlap")
  }

  "a house's record of a work" should "be a segment of a listing spelling it with a bracket year instead of the season" in {
    // US venues list the Met's 2026/27 season as "The Metropolitan Opera: Manon (2027)"; TMDB files
    // it as "The Metropolitan Opera 2026/27: Manon". Neither year is part of how the house bills it.
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Manon (2027)"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe
      Category("segment")
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Così fan tutte (2026)"),
      Film("The Metropolitan Opera 2026/27: Così fan tutte")) shouldBe Category("segment")
    // Another work of the house is not.
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Manon (2027)"), Film("The Metropolitan Opera 2026/27: Otello")) shouldBe
      Category("overlap")
  }

  "a season production" should "be searched by its work and its season, which is how the database finds a house it spells otherwise" in {
    IdentityMeasures.searchQueries(Listing("RBO Cinema Season 2026-27: Manon")) should contain ("Manon 2026")
    IdentityMeasures.searchQueries(Listing("Samson i dalila | metropolitan opera: live in hd 2026/27")) should contain ("Samson i dalila 2026")
    IdentityMeasures.searchQueries(Listing("NT Live: Manon")).exists(_.contains("20")) shouldBe false
    // Only the title naming the season asks, and a bracketed year is no part of the work.
    val cleaned = Listing("The Metropolitan Opera: Manon (2027)", rawTitle = Some("Met Opera 2026-27: Manon (2027)"))
    // ("Manon" names Massenet's opera and MacMillan's ballet, whose own name is asked beside it)
    IdentityMeasures.searchQueries(cleaned).filter(_.endsWith(" 2026")) shouldBe Seq("Manon 2026", "L'histoire de Manon 2026", "Manon Lescaut 2026")
  }
  it should "be searched by its work and the year its title or banner dates it" in {
    // DE "RBO live 2027: Tosca": the relay's record reads the banner's year, so the search must ask it too, or the house's
    // 2026/27 record never reaches the pool
    IdentityMeasures.searchQueries(Listing("RBO live 2027: Tosca")) should contain ("Tosca 2027")
    IdentityMeasures.searchQueries(Listing("Royal Opera: Tosca (2027)")) should contain ("Tosca 2027")
  }


  it should "be searched by every name of its work, whatever language the venue names it in" in {
    // TMDB titles the Met's record "The Metropolitan Opera 2026/27: Samson et Dalila": neither "Samson i dalila 2026"
    // nor the English "Samson and Delilah 2026" finds it
    IdentityMeasures.searchQueries(Listing("Samson i dalila | metropolitan opera: live in hd 2026/27")) should contain ("Samson et Dalila 2026")
    // the work named within a piece: after its composer (DE), before the venue's tag (PL)
    IdentityMeasures.searchQueries(Listing("Met Opera 2026/27: Camille Saint-Saëns SAMSON ET DALILA")) should contain ("Samson et Dalila 2026")
    IdentityMeasures.searchQueries(Listing("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA")) should contain ("Samson et Dalila 2026")
  }

  "a stage work billed with neither its season nor a year" should "be searched by its work's names and the season it is broadcast in" in {
    // PL Kino Amok's "Samson i Dalila", Kino Powiśle's "OPERA-MAKBET - retransmisja", screening in the 2026/27 season
    IdentityMeasures.searchQueries(Listing("Samson i Dalila", broadcastSeason = Some(2026))) should contain allOf ("Samson et Dalila 2026", "Samson and Delilah 2026")
    IdentityMeasures.searchQueries(Listing("Opera-makbet - retransmisja", broadcastSeason = Some(2026))) should contain ("Macbeth 2026")
    // with no broadcast season, no season is searched
    IdentityMeasures.searchQueries(Listing("Samson i Dalila")).exists(_.endsWith(" 2026")) shouldBe false
    // a work only contained mid-title, or opening a title naming no season, is no stage work billed: "Manon des sources"
    IdentityMeasures.searchQueries(Listing("Manon des sources", broadcastSeason = Some(2026))).exists(_.endsWith(" 2026")) shouldBe false
  }

  "a double bill" should "search each work on its own" in {
    // PL Kinoteka's "Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi": TMDB finds
    // no record by the whole bill, so the one its first work names was never a candidate, and the
    // series director's walk handed it another Basia film.
    val bill = IdentityMeasures.searchQueries(Listing("Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi"))
    bill should contain allOf ("Basia. Humor w paski mam", "Kocia Szajka. Tajemnica zniknięcia śledzi")
    // Only a spaced "+" joins works: "Romeo+Juliet" is one title.
    IdentityMeasures.searchQueries(Listing("Romeo+Juliet")) shouldBe Seq("Romeo+Juliet")
  }

  "a title dating itself in a bracket" should "also be searched by what precedes the year" in {
    // PL Kino Kosmos's "Akademia Kina Polskiego: Człowiek z żelaza (1981) 4K": TMDB answers nothing
    // for "Człowiek z żelaza (1981)" or its "… 4K", and finds Wajda's film for the bare title — a
    // search is asked without a year, the year being measured apart.
    IdentityMeasures.searchQueries(Listing("Akademia Kina Polskiego: Człowiek z żelaza (1981) 4K")) should contain ("Człowiek z żelaza")
    IdentityMeasures.searchQueries(Listing("Krzyżacy [1960]")) should contain ("Krzyżacy")
    // …and that part is a whole piece of the title, naming the film as a banner segment does: the
    // "4K" after the year left Wajda's film only an overlap (−3.95), rejected at 11.7%.
    IdentityMeasures.titleRelation(Listing("Akademia Kina Polskiego: Człowiek z żelaza (1981) 4K"), Film("Człowiek z żelaza")) shouldBe
      Category("segment")
    // A bracket that is not a year stays the title's: "Kura (Mała Sala)" asks nothing new.
    IdentityMeasures.searchQueries(Listing("Kura (Mała Sala)")).exists(q => !q.contains("Kura")) shouldBe false
    // Nor does a year with nothing before it name a title.
    IdentityMeasures.searchQueries(Listing("(2026)")).exists(_.isEmpty) shouldBe false
  }

  "a title crediting its director and dating an anniversary" should "also be searched without them" in {
    // US Showcase ×6 "Guillermo del Toro's Pan's Labyrinth 20th Anniversary": TMDB's search finds no
    // record by the whole title, and none of its pieces is one.
    IdentityMeasures.searchQueries(Listing("Guillermo del Toro's Pan's Labyrinth 20th Anniversary")) should contain ("Pan's Labyrinth")
    IdentityMeasures.searchQueries(Listing("Rocky 50th Anniversary")) should contain ("Rocky")
    // A one-word possessive is the title's own ("Schindler's List"), never a credit.
    IdentityMeasures.searchQueries(Listing("Schindler's List")) shouldBe Seq("Schindler's List")
  }

  "a title" should "name the same work with or without a possessive" in {
    // UK Odeon's "Andre Rieu 2026 Christmas Concert: Let it Snow" ×76 was only an overlap of TMDB's
    // "Andre Rieu's 2026 Christmas Concert Let It Snow", and vetoed it for want of other facts.
    IdentityMeasures.titleRelation(Listing("Andre Rieu 2026 Christmas Concert: Let it Snow"),
      Film("Andre Rieu's 2026 Christmas Concert Let It Snow")) shouldBe Category("exact")
    // Only after an apostrophe, straight or curly; a word ending in "s" keeps it.
    IdentityMeasures.withoutPossessives("Andre Rieu’s Let's Go Pops") shouldBe "Andre Rieu Let Go Pops"
    IdentityMeasures.withoutPossessives("Schindlers List") shouldBe "Schindlers List"
  }

  "a title containing another" should "say which way: the listing decorates the film, or is a fragment of a longer title" in {
    IdentityMeasures.titleRelation(Listing("Ken Russell's The Devils"), Film("The Devils")) shouldBe Category("decorated")
    IdentityMeasures.titleRelation(Listing("It"), Film("It Ends with Us")) shouldBe Category("fragment")
    // …also behind a programme banner: UK "Horror Season 2026 Manhunter: The Final Cut" ×81 was only an
    // overlap of "Manhunter" — the banner-free shape decorates it with an edition label — and was vetoed.
    val season = TitleDecorations(Set(Seq("horror", "season", "2026")), Set.empty)
    // Since its edition is read off as one (2026-11-01, `editionHead`), "Manhunter" is a whole piece of it: a segment.
    IdentityMeasures.titleRelation(Listing("Horror Season 2026 Manhunter: The Final Cut", decorations = season), Film("Manhunter")) shouldBe
      Category("segment")
    // Its original title decorates the film on both sides: a director's possessive before, an edition after.
    IdentityMeasures.originalTitleRelation(Some("Michael Mann's Manhunter: The Final Cut"), Seq("Manhunter")) shouldBe Category("decorated")
  }

  "the original-title relation" should "read a film title's trailing bracketed gloss as that title again" in {
    // PL Helios lists Solange Cicurel's "Nie martw się, nic mi nie jest" with original title "TKT";
    // TMDB's is "TKT (T'inquiète)", the title with its gloss. As a fragment (−4.53) it sank an exact,
    // rank-1 title to 3.4% on all 10 listings.
    IdentityMeasures.originalTitleRelation(Some("TKT"), Seq("Nie martw się, nic mi nie jest", "TKT (T'inquiète)")) shouldBe Category("match")
    // Only a trailing gloss, the whole of what precedes it: a piece before a colon is still a segment.
    IdentityMeasures.originalTitleRelation(Some("Your Name"), Seq("Your Name: Director's Cut (2016)")) should not be Category("match")
  }

  "own agreement" should "count a bracketed year that matches, and deny only on a published year" in {
    IdentityMeasures.ownAgreement(measures(Listing("It (1990)"), Film("It", year = Some(1990))))._1 should contain ("year")
    IdentityMeasures.ownAgreement(measures(Listing("Toy Story (2026)"), Film("Toy Story", year = Some(1995))))._2 should not contain ("year")
    IdentityMeasures.ownAgreement(measures(Listing("Toy Story", year = Some(2026)), Film("Toy Story", year = Some(1995))))._2 should contain ("year")
  }

  "a published year beside the same director" should "date a screening, not the film, when it denies the film" in {
    val devils = Film("Diabły", year = Some(1971), directors = Some(Seq("Ken Russell")))
    val screened = measures(Listing("Diabły", year = Some(2026), directors = Seq("Ken Russell")), devils)
    IdentityMeasures.PublishedYear.foreach(m => screened(m) shouldBe IdentityMeasures.MissingListing)
    // A title that does not name the film leaves the year a fact: it tells the director's films
    // apart (KINOMUZEUM's 2026 "Błotem w twarz" is Jaak Kilmi's 2026 film, not his 2017 "Sangarid").
    measures(Listing("Błotem w twarz", year = Some(2026), directors = Seq("Jaak Kilmi")),
      Film("Sangarid", year = Some(2017), directors = Some(Seq("Jaak Kilmi"))))("year.delta") shouldBe Number(9)
    // Nor does a title that only starts with the film's ("Basia. Humor w paski mam + Kocia Szajka…"
    // beside Wasilewski's 2018 "Basia"): it is not the film's title, so the year is not its screening.
    measures(Listing("Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi", year = Some(2026),
      directors = Seq("Marcin Wasilewski")), Film("Basia", year = Some(2018), directors = Some(Seq("Marcin Wasilewski"))))("year.delta") shouldBe Number(8)
    // Agreeing, it stays the fact it is; beside another director, a year decades off denies.
    measures(Listing("Diabły", year = Some(1971), directors = Seq("Ken Russell")), devils)("year.delta") shouldBe Number(0)
    measures(Listing("Diabły", year = Some(2026), directors = Seq("Someone Else")), devils)("year.delta") shouldBe Number(55)
    measures(Listing("Diabły", year = Some(2026)), devils)("year.delta") shouldBe Number(55)
  }

  "venues corroborating a film" should "count only venues whose own title names it, not every venue crediting its director" in {
    // 237 Regal venues list "Candyman (1992)" by Bernard Rose: they back his Candyman, but not
    // every other film the director walk turns up — a director alone does not pick his film.
    val group = (1 to 5).map(i => s"Venue $i" -> Listing("Candyman (1992)", directors = Seq("Bernard Rose")))
    val candyman = Film("Candyman", year = Some(1992), directors = Some(Seq("Bernard Rose")))
    val other    = Film("Paperhouse", year = Some(1988), directors = Some(Seq("Bernard Rose")))
    val backing  = new VenueBacking(Map("candyman" -> group))
    backing.corroborating(Seq("candyman"), candyman, "Venue 1") shouldBe 4
    backing.corroborating(Seq("candyman"), other, "Venue 1") shouldBe 0
  }

  it should "count the venues reading one listings site's entry page once, and never the listing's own page" in {
    // US Burns Court Sarasota and Film Streams Omaha read ONE Flicks page for "NT Live: Hamlet", its 2015 year
    // Cumberbatch's: Film Streams is no second venue dating it so
    val page   = SourceReliability.pageClaim("https://www.flicks.us/movie/nt-live-hamlet/").get
    val hamlet = Listing("NT Live: Hamlet", year = Some(2015))
    val group  = Seq(Backer("Burns Court", page) -> hamlet, Backer("Film Streams", page) -> hamlet, Backer("Another", page) -> hamlet,
      "The Loft" -> hamlet)
    val backing = new VenueBacking(Map("ntlivehamlet" -> group))
    backing.corroborating(Seq("ntlivehamlet"), Film("National Theatre Live: Hamlet", year = Some(2015)), "Burns Court", page) shouldBe 1
    backing.corroborating(Seq("ntlivehamlet"), Film("National Theatre Live: Hamlet", year = Some(2015)), "The Loft", "The Loft") shouldBe 1
    SourceReliability.pageClaim("https://www.flicks.us/movie/hamlet-2026/") should not be Some(page)
  }

  "a group's venue backing" should "count, for every member and film, what a backing that never asked before counts" in {
    val films = Seq(Film("Candyman", year = Some(2021), directors = Some(Seq("Nia DaCosta"))),
      Film("Candyman", year = Some(1992), directors = Some(Seq("Bernard Rose"))), Film("Candy", year = Some(2006)))
    val group = (1 to 12).map { i =>
      s"Venue ${i % 5}" -> Listing(if (i % 4 == 0) "Candyman (1992)" else "Candyman", year = Option.when(i % 3 == 0)(if (i % 2 == 0) 2021 else 1992),
        directors = if (i % 5 == 1) Seq("Nia DaCosta") else Nil)
    }
    val backing = new VenueBacking(Map("candyman" -> group))
    for (f <- films; (venue, _) <- group :+ ("Elsewhere" -> Listing("Candyman")))
      withClue(s"$venue ${f.year}")(backing.corroborating(Seq("candyman"), f, venue) shouldBe
        new VenueBacking(Map("candyman" -> group)).corroborating(Seq("candyman"), f, venue))
  }

  it should "count nothing for a title group no venue lists" in {
    // PL, a decorated "Pucio ... 2D DUB" whose undecorated title no other venue lists bare: its
    // undecorated group is empty, not missing (the calibration's refit threw on it).
    val decorated = Listing("Pucio 2D DUB", decorations = TitleDecorations(Set.empty, Set(Seq("2d", "dub"))))
    val groups    = IdentityMeasures.titleGroups(decorated)
    groups should contain (IdentityMeasures.key("Pucio"))
    val backing = new VenueBacking(Map(IdentityMeasures.key(decorated.title) -> Seq("Kino 1" -> decorated)))
    backing.corroborating(groups, Film("Pucio", year = Some(2026)), "Kino 2") shouldBe 0
  }

  "a title's billing" should "be the work two titles share and how each spells its house, and a banner's house the one most of its works name" in {
    StageRelays.billing(Listing("RBO Cinema Season 2026-27: Manon"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe
      Some(Billing(Seq("rbo", "cinema", "season"), Seq("the", "metropolitan", "opera"), "manon"))
    StageRelays.billing(Listing("NT Live: Les Liaisons Dangereuses (2025)"), Film("National Theatre Live: Les Liaisons Dangereuses")) shouldBe
      Some(Billing(Seq("nt", "live"), Seq("national", "theatre", "live"), "lesliaisonsdangereuses"))
    // The work alone on either side bills no house.
    StageRelays.billing(Listing("Manon"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe None
    StageRelays.billing(Listing("Throwback: Manon"), Film("Manon")) shouldBe None
    def b(banner: String, house: String, work: String) = Billing(Seq(banner), Seq(house), work)
    val houses = Houses.learn(Seq(b("rbo", "royal", "swanlake"), b("rbo", "royal", "alice"), b("rbo", "met", "manon"), b("rbo", "royal", "swanlake"),
      b("met", "met", "manon"), b("met", "met", "otello"), b("tie", "royal", "a"), b("tie", "met", "b"), b("tie", "royal", "c"), b("tie", "met", "d"),
      b("once", "nt", "hamlet")))
    houses shouldBe Houses(Map("rbo" -> "royal", "met" -> "met"))
    houses.other(b("rbo", "met", "manon")) shouldBe true
    houses.same(b("rbo", "royal", "manon")) shouldBe true
    houses.same(b("once", "once", "x")) shouldBe true
    houses.other(b("once", "nt", "hamlet")) shouldBe false
    // A banner's own words name its house before any count of works does.
    def w(banner: String, house: String, work: String) = Billing(banner.split(" ").toSeq, house.split(" ").toSeq, work)
    Houses.learn(Seq(w("metropolitan opera live in hd", "royal ballet opera", "carmen"), w("metropolitan opera live in hd", "royal ballet opera", "cosi"),
      w("metropolitan opera live in hd", "the metropolitan opera", "cosi"))) shouldBe Houses(Map("metropolitanoperaliveinhd" -> "themetropolitanopera"))
    // Words both rivals share decide nothing, and a house outside the rivalry does not make them decide.
    Houses.learn(Seq(w("met opera", "the metropolitan opera", "cosi"), w("met opera", "the metropolitan opera", "manon"),
      w("met opera", "the metropolitan opera", "otello"), w("met opera", "royal ballet opera", "cosi"), w("met opera", "royal ballet opera", "manon"),
      w("met opera", "salzburger festspiele", "carmen"))) shouldBe Houses(Map("metopera" -> "themetropolitanopera"))
    Houses.learn(Seq(w("nt live", "national theatre live", "earnest"), w("nt live", "national theatre live", "playboy"),
      w("nt live", "rsc live", "macbeth"))) shouldBe Houses(Map("ntlive" -> "nationaltheatrelive"))
    // One shared word does not NAME a house of more: PL "ANDRÉ RIEU - NIECH ŻYJE MAASTRICHT! retransmisja koncertu"
    // read its title as a banner over the work "André Rieu", learned it as "André Rieu - Love in Maastricht"'s house
    // by "maastricht" alone, and took the 2019 concert for the 2026 one.
    Houses.learn(Seq(w("niech zyje maastricht retransmisja koncertu", "love in maastricht", "andrerieu"))) shouldBe Houses.Unknown
    // ...but a one-word house its banner spells is named
    Houses.learn(Seq(w("bolshoi ballet live", "bolshoi", "swanlake"))) shouldBe Houses(Map("bolshoiballetlive" -> "bolshoi"))
  }

  "a banner abbreviating its house" should "bill the work under that house, whatever the house was learned as" in {
    // UK recording 38351832400: one candidate filed "NT Live: …" among its alternatives, so the "NT Live" banner was
    // learned as itself; Flicks' "NT Live: Hay Fever" ×131 then read its own record, "National Theatre Live: Hay Fever",
    // as an overlap, and the director's "National Theatre at Home: Samira" pooled in at 49.9%.
    val learnedAsItself = Houses(Map("ntlive" -> "ntlive"))
    IdentityMeasures.titleRelation(Listing("NT Live: Hay Fever"), Film("National Theatre Live: Hay Fever"), learnedAsItself) should not be
      Category("overlap")
    // an abbreviation names one house only by its initials, never by a word two houses share
    IdentityMeasures.titleRelation(Listing("NT Live: Samira"), Film("National Theatre at Home: Samira"), learnedAsItself) shouldBe
      IdentityMeasures.titleRelation(Listing("NT Live: Samira"), Film("National Theatre at Home: Samira"), Houses.Unknown)
    IdentityMeasures.titleRelation(Listing("Royal Shakespeare Company: Macbeth"), Film("Royal Ballet & Opera 2026/27: Macbeth"), Houses.Unknown) shouldBe
      Category("overlap")
    // ...nor overrules the house a banner was learned to be: "New Theatre" shares only NT's initials
    IdentityMeasures.titleRelation(Listing("NT Live: Hay Fever"), Film("New Theatre: Hay Fever"), Houses(Map("ntlive" -> "nationaltheatrelive"))) shouldBe
      Category("overlap")
  }

  "a venue dropping a film's leading article" should "still list the film's exact title — a plain title of three words or more" in {
    // US Metrograph's "Brides of Dracula" {Terence Fisher} read a `fragment` of "The Brides of Dracula" and took Fisher's "Dracula".
    def rel(l: String, f: String) = IdentityMeasures.titleRelation(Listing(l), Film(f)).value
    rel("Brides of Dracula", "The Brides of Dracula") shouldBe "exact"
    // not a short title: "Spookies" is no more "The Spookies" than any other
    rel("Spookies", "The Spookies") should not be "exact"
    // not a listing's own article dropped by the film
    rel("A Bay of Blood", "Bay of Blood") should not be "exact"
    // not a banner's title: "Royal Ballet: Swan Lake" is not the house's 2024 "The Royal Ballet: Swan Lake"
    rel("Royal Ballet: Swan Lake", "The Royal Ballet: Swan Lake") should not be "exact"
  }

  "a decorated spelling" should "be corroborated by the venues listing its search form, when that is the film's title" in {
    // PL Kino Oskard's "Kino Konesera: Róża" and seven spellings like it: other venues list "Róża" [2026] {Markus
    // Schleinzer}, TMDB's "Rose" (2026) under its Polish title; searched as "Róża", the decorated listing counted
    // none of them — the old pipeline folded it by that search key.
    val rose     = Film("Rose", year = Some(2026), alternativeTitles = Seq("Róża"), directors = Some(Seq("Markus Schleinzer")))
    val konesera = Listing("Kino Konesera: Róża", searchTitles = Seq("Róża"))
    IdentityMeasures.searchGroups(konesera, rose) shouldBe Seq("roza")
    val plain   = Seq("Rialto", "Apollo").map(venue => venue -> Listing("Róża", year = Some(2026), directors = Seq("Markus Schleinzer")))
    val backing = new VenueBacking(Map("roza" -> plain))
    backing.corroborating(IdentityMeasures.titleGroups(konesera), rose, "Oskard") shouldBe 0
    backing.corroborating(IdentityMeasures.titleGroups(konesera) ++ IdentityMeasures.searchGroups(konesera, rose), rose, "Oskard") shouldBe 2
    // a search form that is no title of the film names no group: "ANDRÉ RIEU - NIECH ŻYJE MAASTRICHT!", searched
    // as "André Rieu", is not "André Rieu - Love in Maastricht"
    IdentityMeasures.searchGroups(Listing("ANDRÉ RIEU - NIECH ŻYJE MAASTRICHT!", searchTitles = Seq("André Rieu")),
      Film("André Rieu - Love in Maastricht", year = Some(2019))) shouldBe Nil
  }

  "a season-free listing" should "take its house's current-season record only when its banner spells that house" in {
    // UK venues list "Royal Ballet and Opera: Così fan tutte" beside TMDB's "Royal Ballet & Opera
    // 2026/27: Così fan tutte"; the Paris Opera's banner, learned as the Met, shares only "opera".
    val houses = Houses(Map("royalballetandopera" -> "royalballetopera", "operanationaldeparis" -> "themetropolitanopera"))
    StageRelays.billsUnderItsHouse(Listing("Royal Ballet and Opera: Così fan tutte"),
      Film("Royal Ballet & Opera 2026/27: Così fan tutte"), houses) shouldBe true
    StageRelays.billsUnderItsHouse(Listing("Opéra National de Paris: La fanciulla del West"),
      Film("The Metropolitan Opera 2026/27: La Fanciulla del West"), houses) shouldBe false
  }

  "a record billing the listing's work under several banners" should "bill it under the listing's house when any of its titles does" in {
    // TMDB's 1693710 carries the National Theatre's broadcast title and, as alternatives, its "At
    // Home" streaming title and the bare play: Flicks' "NT Live: The Misanthrope" is its house's.
    val record = Film("National Theatre Live: The Misanthrope", originalTitle = Some("National Theatre Live: The Misanthrope"),
      alternativeTitles = Seq("National Theatre at Home: The Misanthrope", "The Misanthrope"), year = Some(2026), runtime = Some(105))
    StageRelays.billsUnderItsHouse(Listing("NT Live: The Misanthrope"), record, Houses(Map("ntlive" -> "nationaltheatrelive"))) shouldBe true
  }

  "a title transliterated from another script" should "name the record carrying it in that script" in {
    // Helios lists Oleksii Esakov's Ukrainian film as "Potyag Chervona ruta" (and in Cyrillic with that
    // as its original title); TMDB titles it "Потяг «Червона Рута»".
    val record = Film("Потяг «Червона Рута»", originalTitle = Some("Потяг «Червона Рута»"), year = Some(2026))
    IdentityMeasures.titleRelation(Listing("Potyag Chervona ruta"), record) shouldBe Category("exact")
    IdentityMeasures.originalTitleRelation(Some("Potyag Chervona ruta"), Seq(record.title)) shouldBe Category("match")
    // Only the whole title: a transliteration naming another film is not one.
    IdentityMeasures.titleRelation(Listing("Potyag"), record) should not be Category("exact")
  }

  "a title writing an \"&\" or an \"ß\" the record spells out" should "name the record exactly" in {
    // DE Metropol Stuttgart ×5 billed Georgia Oakley's 2026 "Sinn & Sinnlichkeit", TMDB's "Sinn und Sinnlichkeit": an
    // `overlap`, refused below the cannot-link cut; Kulturzentrum Linse's "DER RHEIN FLIESST INS MITTELMEER" writes in
    // capitals the "ß" of "Der Rhein fließt ins Mittelmeer" (prod 2026-11-01).
    IdentityMeasures.titleRelation(Listing("Sinn & Sinnlichkeit"), Film("Sinn und Sinnlichkeit", year = Some(2026))) shouldBe Category("exact")
    IdentityMeasures.titleRelation(Listing("Music & Lyrics"), Film("Music and Lyrics", year = Some(2007))) shouldBe Category("exact")
    // never the other way: a billing spelling out a record's "&" is read as before (DE FFSBC's "… Agnes und Amir")
    IdentityMeasures.titleRelation(Listing("Renoir and Love"), Film("Renoir & Love", year = Some(2027))) should not be Category("exact")
    IdentityMeasures.titleRelation(Listing("DER RHEIN FLIESST INS MITTELMEER"), Film("Der Rhein fließt ins Mittelmeer", year = Some(2023))) shouldBe
      Category("exact")
    // Only the spelling: the "&" joins the same words, or it names another title.
    IdentityMeasures.titleRelation(Listing("Sinn & Sinnlichkeit"), Film("Sinnlichkeit", year = Some(2026))) should not be Category("exact")
    IdentityMeasures.titleRelation(Listing("Fast & Furious"), Film("Fast Five", year = Some(2011))) should not be Category("exact")
    IdentityMeasures.titleRelation(Listing("Sinn Sinnlichkeit"), Film("Sinn und Sinnlichkeit", year = Some(2026))) should not be Category("exact")
  }

  "a banner's contending houses" should "be ranked by the words they share with it, then their works, and a tie on both be no house" in {
    def billed(banner: String, house: String, work: String) = Billing(banner.split(" ").toSeq, house.split(" ").toSeq, work)
    val tied = Houses.ranking(Seq(billed("nt live", "national theatre live", "misanthrope"), billed("nt live", "rsc live", "macbeth")))
    tied("ntlive").map(_.render) shouldBe Seq("nationaltheatrelive (words 1, works 1)", "rsclive (words 1, works 1)")
    Houses.chosen(tied("ntlive")) shouldBe None
    Houses.learn(Seq(billed("nt live", "national theatre live", "misanthrope"), billed("nt live", "rsc live", "macbeth"))) shouldBe Houses.Unknown
  }

  "a banner learned as one spelling of a house" should "not make another spelling of its own name another house" in {
    // DE recording 38196151886: "RBO Live" was learned as the house records spelling it so bill, and "RBO Live 2026/27:
    // Schwanensee (Royal Ballet)" ×80 was then denied its season's record billing the work "RBO 26/27: …" as another
    // house's season production; the Met's billing of a work stays another house's.
    def billed(banner: String, house: String, work: String) = Billing(banner.split(" ").toSeq, house.split(" ").toSeq, work)
    val houses = Houses(Map("rbolive" -> "rbolive", "metopera" -> "metopera"))
    houses.other(billed("rbo live", "rbo 26 27", "schwanenseeroyalballet")) shouldBe false
    houses.other(billed("rbo live", "royal ballet opera", "manon")) shouldBe false
    houses.other(billed("met opera", "the metropolitan opera", "manon")) shouldBe false
    houses.other(billed("rbo live", "the metropolitan opera", "manon")) shouldBe true
    houses.other(billed("met opera", "rbo 26 27", "manon")) shouldBe true
  }

  "two names of one renamed house" should "name one house, and no other house that shares a word with one of them" in {
    // UK "Tosca (ROH)" beside TMDB's "Royal Ballet & Opera 2026/27: Tosca": the Royal Opera House renamed itself in 2024
    def words(name: String) = name.split(" ").toSeq
    StageRelays.namesOneHouse(words("roh"), words("royal ballet opera")) shouldBe true
    StageRelays.namesOneHouse(words("rbo live"), words("royal opera house")) shouldBe true
    StageRelays.namesOneHouse(words("roh"), words("rbo")) shouldBe true
    StageRelays.namesOneHouse(words("roh"), words("royal shakespeare company")) shouldBe false
    StageRelays.namesOneHouse(words("roh"), words("the metropolitan opera")) shouldBe false
  }

  "a house's initials split by punctuation" should "name the house they spell" in {
    // DE Kinopolis' "Carmen (RB+O 2026/27)" ×2 was denied its season's "RBO 2026/27: Carmen (The Royal Opera)" as another
    // house's season production, and took the Salzburg Festival's 2026 "Carmen": "RB+O" folds to the words "rb" and "o"
    def words(name: String) = name.split(" ").toSeq
    StageRelays.namesOneHouse(words("rb o 2026 27"), words("rbo 2026 27 the royal opera")) shouldBe true
    StageRelays.namesOneHouse(words("rb o live"), words("royal ballet opera")) shouldBe true
    StageRelays.namesOneHouse(words("rb o"), words("the metropolitan opera")) shouldBe false
    // short words that are no initials stay apart: "de la" spells no house
    StageRelays.namesOneHouse(words("opera de la bastille"), words("teatro de la zarzuela")) shouldBe false
  }

  "a stage house's relay" should "deny a record of another work, in no language its work goes by" in {
    val macbeth = Listing("MET Opera: Macbeth 2026", originalTitle = Some("The Metropolitan Opera: Eugene Onegin"))
    StageRelays.namesAnotherWork(macbeth, Film("MET Opera: Eugen Onegin", Some("The Metropolitan Opera: Eugene Onegin"))) shouldBe true
    StageRelays.namesAnotherWork(macbeth, Film("The Metropolitan Opera 2026/27: Macbeth")) shouldBe false
    val swan = Listing("RBO Live 2026/27: Schwanensee (Royal Ballet)")
    StageRelays.namesAnotherWork(swan, Film("RBO 2026/27: Tosca (The Royal Opera)", Some("Royal Ballet & Opera 2026/27: Tosca"))) shouldBe true
    StageRelays.namesAnotherWork(swan, Film("Royal Ballet & Opera 2026/27: Swan Lake")) shouldBe false
    // a title naming nothing but its house names no work, and a film's title naming no house is no relay's
    StageRelays.namesAnotherWork(Listing("Royal Ballet & Opera Live"), Film("RBO 2026/27: Tosca (The Royal Opera)")) shouldBe false
    StageRelays.namesAnotherWork(Listing("Das Phantom der Oper"), Film("The Phantom of the Opera")) shouldBe false
    StageRelays.namesAnotherWork(Listing("Macbeth 2026"), Film("MET Opera: Eugen Onegin")) shouldBe false
    // a venue's composer, tag or translation beside the work's name, and a work billed whole under a programme banner
    val met = Film("The Metropolitan Opera 2026/27: Macbeth")
    Seq("The Metropolitan Opera: Macbeth ENCORE (2026)", "Met Opera 2026/27: Giuseppe Verdi MACBETH", "MET Opera: Macbeth-Verdi (2026)")
      .foreach(title => withClue(title)(StageRelays.namesAnotherWork(Listing(title), met) shouldBe false))
    StageRelays.namesAnotherWork(Listing("Royal Ballet and Opera Sezon Kinowy 2026-27: Cosi fan tutte. Tak czynią wszystkie"),
      Film("Royal Ballet & Opera 2026/27: Così fan tutte")) shouldBe false
    StageRelays.namesAnotherWork(Listing("MUSICAL MITTWOCH - Das Phantom der Oper"), Film("Das Phantom der Oper")) shouldBe false
    StageRelays.namesAnotherWork(Listing("MUSICAL MITTWOCH - Das Phantom der Oper"), Film("Phantom der Oper")) shouldBe false
    // a fragment naming only the art form is no title a record can carry whole: UK Riverside Theatre Woodbridge's "Macbeth -
    // Opera 2026" (recording 38326582997) took Argento's 1987 "Opera" by its "Opera" alone
    StageRelays.namesAnotherWork(Listing("Macbeth - Opera 2026"), Film("Opera")) shouldBe true
    StageRelays.namesAnotherWork(Listing("Macbeth - Opera 2026"), Film("The Metropolitan Opera 2026/27: Macbeth")) shouldBe false
    // a box office's "(Cine)" says how the relay screens, no work: ES taquilla's "Ballet El Cascanueces (Cine)" ×14 and
    // "Ópera Cosi fan tutte (Cine)" ×5 (prod mirror 2026-10-21) denied their season's records as another work's
    def cine(title: String) = Listing(title, rawTitle = Some(s"$title (Cine)"))
    StageRelays.namesAnotherWork(cine("Ballet El Cascanueces"), Film("Royal Ballet & Opera 2026/27: The Nutcracker")) shouldBe false
    StageRelays.namesAnotherWork(cine("Ópera Cosi fan tutte"), Film("Royal Ballet & Opera 2026/27: Così fan tutte")) shouldBe false
  }

  "a stage house's relay" should "deny another production of its work than the season its screenings fall in" in {
    def relay(title: String, year: Option[Int] = None) = Listing(title, year = year, screenedSeason = Some(2026))
    def another(listing: Listing, film: Film) = StageRelays.anotherProduction(listing, film)
    // UK Savoy Boston's "Opéra De Paris Ballet - Notre-Dame de Paris" ×6, screening March 2027 (the Paris Opera's Dec 2025
    // recording), took La Scala's 2013 "La Scala Ballet: Notre-Dame de Paris" (790330) by the work's name alone
    val paris   = relay("Opéra De Paris Ballet - Notre-Dame de Paris")
    val scala   = Film("La Scala Ballet: Notre-Dame de Paris", Some("Roland Petit: Notre-Dame de Paris (Teatro alla Scala)"), year = Some(2013))
    another(paris, scala) shouldBe true
    // another house's record of the season it screens in stays: UK's "RBO Cinema Season" relays the Met's productions of
    // that season — and so does its own house's of any season, a house encoring its recordings for years (UK "NT Live: A
    // Streetcar Named Desire" is the 2014 broadcast, "Royal Ballet: La Bayadère" the 2018 recording)
    another(relay("RBO Cinema Season 2026-27: Tosca"), Film("The Metropolitan Opera: Tosca", year = Some(2027))) shouldBe false
    another(paris, Film("Paris Opera Ballet: Notre-Dame de Paris", year = Some(2013))) shouldBe false
    another(relay("Royal Ballet: La Bayadère"), Film("La Bayadère (Royal Ballet)", year = Some(2018))) shouldBe false
    another(relay("NT Live: A Streetcar Named Desire"), Film("National Theatre Live: A Streetcar Named Desire", year = Some(2014))) shouldBe false
    // nor is a banner a film series shares a house: DE "Der Herr der Ringe - Die Gefährten (Extended Version)"
    another(relay("Der Herr der Ringe - Die Gefährten (Extended Version)"), Film("Der Herr der Ringe - Die Gefährten", year = Some(2001))) shouldBe false
    // a billing naming no house in particular may be the record's house: a record long before is another production (UK
    // Film Theatre Leiston's "Ballet - Notre-Dame de Paris", screening March 2027), one a year or two away its encore or
    // recording (DE Kleines Theater's "RB&O - Ballett "La Fille mal gardée"", its split initials the Royal Ballet's; DE
    // Cinema Wolfhagen's Paris Opera "Schwanensee" of 2024, screening December 2026), and a year of its own dates it so
    another(relay("Ballet - Notre-Dame de Paris"), scala) shouldBe true
    another(relay("Ballet - Notre-Dame de Paris", year = Some(2013)), scala) shouldBe false
    val fille = Film("RBO 2025/26: La Fille Mal Gardée (The Royal Ballet)", year = Some(2025))
    another(relay("RB&O - Ballett \"La Fille mal gardée\""), fille) shouldBe false
    another(relay("Ballett: La Fille mal gardée"), fille) shouldBe false
    another(relay("Ballett: Schwanensee"), Film("Opéra national de Paris 2024: Schwanensee", year = Some(2024))) shouldBe false
    // a title whose house word is the work's own bills no banner: UK Chichester's "Phantom of the Opera", screening December
    // 2026, lost the 2004 film so (recording 38666576506)
    another(relay("Phantom of the Opera"), Film("The Phantom of the Opera", year = Some(2004))) shouldBe false
    another(relay("Phantom of the Opera"), Film("The Phantom of the Opera (Royal Albert Hall)", year = Some(2011))) shouldBe false
    // a record naming no house, or no screenings to date the relay, denies nothing
    another(paris, Film("Notre-Dame de Paris", year = Some(1956))) shouldBe false
    another(paris.copy(screenedSeason = None), scala) shouldBe false
  }

  "a stage house's relay" should "read a house's initials that punctuation splits" in {
    StageRelays.billsAStageHouse(Seq("RB&O 26/27: Carmen")) shouldBe true
    StageRelays.billsAStageHouse(Seq("Carmen (RB+O 2026/27)")) shouldBe true
    StageRelays.billsAStageHouse(Seq("Jin-Roh - Die Wolfsbrigade")) shouldBe false
    StageRelays.billsAStageHouse(Seq("R&B Night: Soul Classics")) shouldBe false
    StageRelays.billsItsHouse(Listing("RB&O 26/27: Carmen"), Film("Royal Ballet & Opera 2026/27: Carmen")) shouldBe true
  }

  it should "derive any house's initials from its names, never a list of them" in {
    def names(line: String) = services.movies.StageHouse.parse(Iterator(line))
    services.movies.StageHouse.initials shouldBe Set("rbo", "roh")
    // any house of the table: three initialled words or more, its articles and conjunctions skipped
    services.movies.StageHouse.initialsOfHouses(names("The English National Opera|ENO Live") ++ names("Metropolitan Opera|The Met")) shouldBe Set("eno")
  }

  "a stage house's relay" should "know a record billing its house from one billing another's" in {
    val enb = Listing("English National Ballet: The Sleeping Beauty")
    StageRelays.billsItsHouse(enb, Film("English National Ballet presents The Sleeping Beauty")) shouldBe true
    StageRelays.billsAnotherHouse(enb, Film("The Sleeping Beauty", alternativeTitles = Seq("The Sleeping Beauty (The Royal Ballet)"))) shouldBe true
    StageRelays.billsAnotherHouse(enb, Film("The Sleeping Beauty")) shouldBe false
    val rbo = Listing("RBO Live 2026/27: Manon (Royal Ballet)")
    StageRelays.billsItsHouse(rbo, Film("RBO 2026/27: Manon (The Royal Ballet)", Some("Royal Ballet & Opera 2026/27: Manon"))) shouldBe true
    StageRelays.billsAnotherHouse(rbo, Film("The Metropolitan Opera 2026/27: Manon")) shouldBe true
    StageRelays.billsItsHouse(Listing("MET Opera Live im Kino: Manon"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe true
    StageRelays.billsItsHouse(Listing("ROH: Tosca"), Film("Royal Opera House: Tosca")) shouldBe true
    // a work billed in the house's word names no house: PL "OPERA-MANON" is no relay of Royal Opera House's "Manon"
    StageRelays.billsItsHouse(Listing("OPERA-MANON"), Film("BALLET LIVE. MANON. ROYAL ÓPERA HOUSE")) shouldBe false
    StageRelays.billsItsHouse(Listing("OPERA- SILENT NIGHT"), Film("Silent Night (Opera North)")) shouldBe false
    // the kind of institution names no house: records the unmatched-cluster captures hold of other national companies
    StageRelays.billsItsHouse(enb, Film("The Sleeping Beauty (Dutch National Ballet)")) shouldBe false
    StageRelays.billsAnotherHouse(enb, Film("The Sleeping Beauty (Dutch National Ballet)")) shouldBe true
    StageRelays.billsItsHouse(Listing("English National Ballet: Swan Lake"), Film("Swan Lake - Ballet Company of the National Opera of Ukraine")) shouldBe false
    StageRelays.billsItsHouse(Listing("Wiener Staatsoper: Tosca"), Film("Bayerische Staatsoper: Tosca")) shouldBe false
    StageRelays.billsItsHouse(Listing("Royal Opera House: Carmen"), Film("Sydney Opera House: Carmen")) shouldBe false
  }

  "a record billing the listing's work under its learned house" should "be a segment of its title, and only under that house" in {
    val nt = Listing("NT Live: The Importance of Being Earnest")
    val record = Film("National Theatre Live: The Importance of Being Earnest")
    val houses = Houses(Map("ntlive" -> "nationaltheatrelive"))
    // a banner abbreviating the record's house names it unlearned ("NT" the initials of "National Theatre")
    IdentityMeasures.titleRelation(nt, record) shouldBe Category("segment")
    IdentityMeasures.titleRelation(nt, record, houses) shouldBe Category("segment")
    IdentityMeasures.titleRelation(nt, Film("RSC Live: The Importance of Being Earnest"), houses) shouldBe Category("overlap")
    IdentityMeasures.listingFilm(nt, record, None, 0, 0, houses)("title") shouldBe Category("segment")
  }

  "a record billing the listing's work under the listing's own house" should "be one of the house, not a season's or another edition's" in {
    val houses = Houses(Map("ntlive" -> "nationaltheatrelive",
      // TMDB files the Paris Opera's broadcasts' works only under the Met's records, so its banner is
      // learned as the Met; League of Legends' two finals banners share every word but the number.
      "operanationaldeparis" -> "themetropolitanopera", "leagueoflegendsworlds26" -> "leagueoflegendsworlds25"))
    StageRelays.billsUnderItsHouse(Listing("NT Live: The Misanthrope"), Film("National Theatre Live: The Misanthrope"), houses) shouldBe true
    StageRelays.billsUnderItsHouse(Listing("NT Live: The Misanthrope"), Film("The Misanthrope"), houses) shouldBe false
    // A record of one SEASON's broadcast names its production by the season, which the listing does not.
    StageRelays.billsUnderItsHouse(Listing("Opéra National de Paris: La fanciulla del West"),
      Film("The Metropolitan Opera 2026/27: La Fanciulla del West"), houses) shouldBe false
    // A listing naming a season takes its house's record naming none (seasonsApart bounds its year), but never one
    // naming a season: that is the season rule's to read.
    StageRelays.billsUnderItsHouse(Listing("MetOpera 2025-26: La Sonnambula"), Film("The Metropolitan Opera: La Sonnambula"),
      Houses(Map("metopera" -> "themetropolitanopera"))) shouldBe true
    StageRelays.billsUnderItsHouse(Listing("MetOpera 2025-26: La Sonnambula"), Film("The Metropolitan Opera 2024/25: La Sonnambula"),
      Houses(Map("metopera" -> "themetropolitanopera"))) shouldBe false
    // A banner numbering its edition otherwise than the record's is another edition.
    StageRelays.billsUnderItsHouse(Listing("League of Legends Worlds 26 | Finals in Cinema"),
      Film("League of Legends Worlds25 - Finals in Cinema"), houses) shouldBe false
    StageRelays.billsUnderItsHouse(Listing("Berliner Philharmoniker LIVE: New Year’s Eve Concert 2025"),
      Film("Berliner Philharmoniker: New Year’s Eve Concert 2025"), Houses(Map("berlinerphilharmonikerlive" -> "berlinerphilharmoniker"))) shouldBe true
    // A cut or an edition is no house: DE Movietown Brandenburg's "Das Boot - Director s Cut" took TMDB's yearless "Das
    // Boot - Directors Cut" record as the house production of "Das Boot", not Petersen's film.
    StageRelays.billsUnderItsHouse(Listing("Das Boot - Director s Cut"), Film("Das Boot - Directors Cut"), Houses.Unknown) shouldBe false
    StageRelays.billsUnderItsHouse(Listing("DAS BOOT (DIRECTOR'S CUT)(WA:2023)"), Film("Das Boot - Directors Cut"), Houses.Unknown) shouldBe false
  }

  "qualifiers" should "be the pieces records bill beside more works than the rest of the listing's title" in {
    val q = IdentityMeasures.Qualifiers.learn(Seq("Chocolate - Director's Cut",
      "The Great War: Director's Cut", "The Promise (Director's Cut)", "Director's Cut", "Dark City", "Manhunter").map(Film(_)))
    q.of(Listing("Dark City: Director's Cut")) shouldBe Set("directorscut")
    // A bare title, and a title whose only piece is its bracketed year, have no qualifier.
    q.of(Listing("Director's Cut")) shouldBe empty
    q.of(Listing("Manhunter (1986)")) shouldBe empty
    // A record titled only the qualifier names nothing; the work's record still names the listing.
    val cut = Listing("Dark City: Director's Cut")
    IdentityMeasures.titleRelation(cut, Film("Director's Cut")) shouldBe Category("segment")
    IdentityMeasures.titleRelation(cut, Film("Director's Cut"), Houses.Unknown, q) shouldBe Category("overlap")
    // the work's record is its title whole once the version tag is dropped (`VersionTags`)
    IdentityMeasures.titleRelation(cut, Film("Dark City"), Houses.Unknown, q) shouldBe Category("exact")
    IdentityMeasures.titleRelation(cut, Film("Dark City: Director's Cut"), Houses.Unknown, q) shouldBe Category("exact")
  }

  it should "tie to nothing when both pieces are billed alike" in {
    val q = IdentityMeasures.Qualifiers.learn(Seq("Heat: Director's Cut", "Heat - Extended", "Alien: Director's Cut", "Heat").map(Film(_)))
    // "directorscut" trailing two works, "heat" leading two pieces: neither is the listing's qualifier.
    q.of(Listing("Heat: Director's Cut")) shouldBe empty
  }

  it should "leave a work its record under a banner when the records bill it on the other side" in {
    // PL "Tani wtorek: OBCY": the records bill the work BEFORE its sequels, the listing after its banner.
    val q = IdentityMeasures.Qualifiers.learn(Seq("Obcy", "Obcy: Przymierze", "Obcy: Romulus", "Obcy - 8. pasażer Nostromo").map(Film(_)))
    q.of(Listing("Tani wtorek: Obcy")) shouldBe empty
    IdentityMeasures.titleRelation(Listing("Tani wtorek: Obcy"), Film("Obcy"), Houses.Unknown, q) shouldBe Category("segment")
  }

  it should "not be a record's whole title when the rest of the listing's names no record's work" in {
    // UK Everyman/Cineworld "Dracula (4K Restoration)": TMDB bills "Dracula" before many sequels, so
    // it read as a banner and left the listing no work — the Hammer record only overlapped it.
    val draculas = Seq(Film("Dracula", year = Some(1958)), Film("Dracula", year = Some(1931)), Film("Dracula: Prince of Darkness"),
      Film("Dracula: A Love Tale"), Film("Dracula: Dead and Loving It"))
    val q = IdentityMeasures.Qualifiers.learn(draculas)
    q.of(Listing("Dracula (4K Restoration)")) shouldBe empty
    IdentityMeasures.titleRelation(Listing("Dracula (4K Restoration)"), draculas.head, Houses.Unknown, q) shouldBe Category("exact")
    // A work records bill it before stays one: "Dracula: Prince of Darkness" is not the 1958 film.
    IdentityMeasures.titleRelation(Listing("Dracula: Prince of Darkness"), draculas.head, Houses.Unknown, q) should not be Category("segment")
  }

  it should "not be a record's whole title trailing a banner that names no work" in {
    // PL Kino Luna "KINO SENIORA | Primetime": TMDB bills "Primetime" after two works ("EliteXC: Primetime",
    // "Dateline: Primetime"), so it read as an edition label and the listing's own film only overlapped it.
    val q    = IdentityMeasures.Qualifiers.learn(Seq("Primetime", "EliteXC: Primetime", "Dateline: Primetime").map(Film(_)))
    val luna = Listing("KINO SENIORA | Primetime", decorations = TitleDecorations(Set(Seq("kino", "seniora")), Set.empty))
    q.of(luna) shouldBe empty
    IdentityMeasures.titleRelation(luna, Film("Primetime"), Houses.Unknown, q) shouldBe Category("segment")
    // Before anything not learned as a venue's, it stays an edition: "EliteXC: Primetime" is not the 2026 film.
    q.of(Listing("EliteXC: Primetime")) shouldBe Set("primetime")
  }

  it should "never be the piece a listing publishes as its original title" in {
    // UK Cineworld's "Cineworld 30: The Dark Knight" (x87), originally "The Dark Knight": TMDB bills
    // the work after two banners too, but the venue names it as the film.
    val q = IdentityMeasures.Qualifiers.learn(Seq("The Dark Knight", "Enter the World of Hans Zimmer: The Dark Knight",
      "GARO - Kiba: The Dark Knight").map(Film(_)))
    q.of(Listing("Cineworld 30: The Dark Knight")) shouldBe Set("thedarkknight")
    val cineworld = Listing("Cineworld 30: The Dark Knight", originalTitle = Some("The Dark Knight"))
    q.of(cineworld) shouldBe empty
    IdentityMeasures.titleRelation(cineworld, Film("The Dark Knight"), Houses.Unknown, q) shouldBe Category("segment")
  }

  "an edition" should "be a later record carrying the work's title under a qualifier, never a namesake" in {
    val murnau = Film("Nosferatu", alternativeTitles = Seq("Nosferatu: A Symphony of Horror"), year = Some(1922))
    IdentityMeasures.editionOf(Film("Radiohead X Nosferatu: A Symphony of Horror", year = Some(2025)), murnau) shouldBe true
    IdentityMeasures.editionOf(Film("Nosferatu: A Symphony of Horror", year = Some(2023)), murnau) shouldBe false
    IdentityMeasures.editionOf(Film("Nosferatu", year = Some(2024)), murnau) shouldBe false
    IdentityMeasures.editionOf(murnau, Film("Radiohead X Nosferatu: A Symphony of Horror", year = Some(2025))) shouldBe false
    // DE "Die Puppe" (1975, originally "The Doll") is a namesake of Has's "Lalka", which TMDB also
    // calls "The Doll": an original title is a whole title, never a piece of the record's own.
    val has = Film("Lalka", alternativeTitles = Seq("The Doll"), year = Some(1968))
    IdentityMeasures.editionOf(Film("Die Puppe", originalTitle = Some("The Doll"), year = Some(1975)), has) shouldBe false
  }

  "title shapes" should "de-decorate the parts a banner leaves, as well as the whole title" in {
    // "Throwback: Donnie Darko (25th Anniversary)": the banner split leaves "Donnie Darko (25th
    // Anniversary)", whose trailing bracket is itself a decoration.
    Listing("Throwback: Donnie Darko (25th Anniversary)").shapes should contain ("Donnie Darko")
    IdentityMeasures.searchQueries(Listing("Throwback: Donnie Darko (25th Anniversary)")) should contain ("Donnie Darko")
  }

  they should "split a spaced slash and a short code before an unspaced colon, as the banner separators" in {
    // Kinoteatr Pasja bills "MISTYCZKA /film polski/"; Kino Millenium "MS:HOT SPOT": each piece is searched.
    Listing("MISTYCZKA /film polski/").shapes should contain ("MISTYCZKA")
    Listing("Róża / Spotkanie Filozoficzne").shapes should contain allOf ("Róża", "Spotkanie Filozoficzne")
    Listing("MS:HOT SPOT").shapes should contain ("HOT SPOT")
    // Fregata's "Fregata dla seniorów- 500 Mil": a dash spaced on one side only separates too.
    Listing("Fregata dla seniorów- 500 Mil").shapes should contain ("500 Mil")
    Listing("Spider-Man").shapes shouldBe Seq("Spider-Man")
    // Jaworzyna's "Ma to sens 2026": a screening year after the title is no part of it; "2046" and
    // "Blade Runner 2049" are titles, not screening years.
    Listing("Ma to sens 2026").shapes should contain ("Ma to sens")
    Listing("Blade Runner 2049").shapes shouldNot contain ("Blade Runner")
    Listing("2046").shapes shouldBe Seq("2046")
    // A slash or colon inside a word is the title's own: "Face/Off", "AC/DC", "Star Wars:Episode".
    Listing("Face/Off").shapes shouldBe Seq("Face/Off")
    Listing("Star Wars:Episode I").shapes shouldNot contain ("Episode I")
  }

  they should "take a programme's head-quoted title and a square-bracketed format off" in {
    // Kino Świt: "„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe"
    Listing("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe").shapes should contain ("Baranek Shaun i kudłata bestia")
    // two quoted titles bill two works; a quote further in names what a talk or a concert is about
    Listing("„Bestia”, reż. Aleksander Hertz, „Cud nad Wisłą”").shapes shouldNot contain ("Bestia")
    Listing("Wojciech Cejrowski i „Prawo Dżungli”").shapes shouldNot contain ("Prawo Dżungli")
    // Kinematograf's lecture bills two quoted films; its cut title quotes one, its raw title both
    Listing("Wykład: „Bestia”", rawTitle = Some("Wykład: „Bestia”, reż. Aleksander Hertz, „Cud nad Wisłą”")).shapes shouldNot
      contain ("Bestia")
    // Bałtyk: "Weekend Seniora z Kulturą: Vivaldi i ja [2D LEKTOR]"
    Listing("Weekend Seniora z Kulturą: Vivaldi i ja [2D LEKTOR]").shapes should contain ("Vivaldi i ja")
    Listing("[REC]").shapes shouldBe Seq("[REC]")
  }

  they should "take the title an upper-case banner runs into with a colon and one quoted at the tail after a banner, never a sentence's head" in {
    // Kino Millenium "KINO NA NIEDZIELE:CAMINO DLA OPORNYCH": a programme's banner, no space after its colon
    Listing("KINO NA NIEDZIELE:CAMINO DLA OPORNYCH").shapes should contain ("CAMINO DLA OPORNYCH")
    Listing("Star Wars:Episode I").shapes shouldNot contain ("Episode I")
    // Kino w Ratuszu 'Kino Kobiet - KLAPS! "Jak żyć żeby nie zwariować"': the one quoted title ends the billing, after its banner
    Listing("Kino Kobiet - KLAPS! \"Jak żyć żeby nie zwariować\"").shapes should contain ("Jak żyć żeby nie zwariować")
    Listing("ZADUSZKI JAZZOWE GABA JANUSZ \"NIEOBECNI\"").shapes shouldNot contain ("NIEOBECNI")
    // a sentence's head is a franchise's first film as often as the title: the sequel "Szkoła magicznych zwierząt.
    // Tajemnica szkolnego podwórka" is not "Szkoła magicznych zwierząt"
    Listing("SZKOŁA MAGICZNYCH ZWIERZĄT. TAJEMNICA SZKOLNEGO PODWÓRKA").shapes shouldNot contain ("SZKOŁA MAGICZNYCH ZWIERZĄT")
    Listing("Vincent. Legenda oceanu").shapes shouldNot contain ("Vincent")
  }

  "a listing's comparable facts" should "be every measure but the title relation, the ranking priors and the pooled count" in {
    IdentityMeasures.FactMeasures shouldBe Set("originalTitle", "year.delta", "year.distance", "titleYear.delta", "season.delta",
      "director", "runtime.delta", "country")
    val film = Film("The Last Whale Singer", year = Some(2025), directors = Some(Seq("Reza Memari")))
    // A title and nothing else: no fact to compare, whatever the titles' relation.
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent. Legenda oceanu"), film)) shouldBe false
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Donnie Darko 25th Anniversary"), Film("Donnie Darko"))) shouldBe false
    // A published fact the film cannot be compared on (no year in its record) is not a comparison.
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", year = Some(2025)), Film("Vincent"))) shouldBe false
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", year = Some(1975)), film)) shouldBe true
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent (1975)"), film)) shouldBe true
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", originalTitle = Some("Vincent")), film)) shouldBe true
  }

  private def pair(a: Listing, b: Listing) = IdentityMeasures.listingListing(a, b, sameVenue = false, sharedChainId = None)

  "two listings' title relation" should "read one title as a whole delimited piece of the other's in either order" in {
    def title(a: String, b: String) = pair(Listing(a), Listing(b))("title")
    title("Astra Seniora - Lalka", "Lalka") shouldBe Category("segment")
    title("Lalka", "Astra Seniora - Lalka") shouldBe Category("segment")
    title("Tony", "Kino bez barier: Tony (AD + CC)") shouldBe Category("segment")
    // A token run that is no delimited piece stays what it was, and two banners' shared words are
    // no segment of either title.
    title("It", "It Ends with Us") shouldBe Category("fragment")
    title("Kino bez barier: Tony", "Kino bez barier: Lalka") shouldBe Category("overlap")
  }

  "two listings' comparable facts" should "be every listing-listing measure but the title relation and the venue" in {
    IdentityMeasures.ListingFactMeasures shouldBe Set("originalTitle", "year.delta", "titleYear.delta", "season.delta",
      "director", "runtime.delta", "chainId")
    // A fact one side publishes and the other does not — a year in one's bracket, the other's field —
    // is no comparison.
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka (2026)"),
      Listing("Filmowy Klub Seniora: LALKA", year = Some(2026), directors = Seq("Maciej Kawalski")))) shouldBe false
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka", year = Some(2026)), Listing("Lalka | PREMIERA", year = Some(2026)))) shouldBe true
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka", directors = Seq("Maciej Kawalski")),
      Listing("Lalka", directors = Seq("Wojciech Has")))) shouldBe true
  }

  "an edition's runtime" should "be neutral when longer than every runtime TMDB states, and still count when shorter" in {
    // TMDB keeps no record of "The Return of the King"'s extended edition (searches for it find nothing): its one record,
    // 122, states the 201-minute theatrical cut in en, pl and de, while the extended cut runs 263.
    val film = Film("The Lord of the Rings: The Return of the King", year = Some(2003), runtime = Some(201))
    val extended = "The Lord of the Rings: The Return of the King (Extended Edition)"
    val longer = measures(Listing(extended, runtime = Some(263)), film)
    longer("runtime.delta") shouldBe Missing("listing")
    IdentityMeasures.runtimeContradicts(longer) shouldBe false
    // An edition never makes a film shorter.
    val shorter = measures(Listing(extended, runtime = Some(120)), film)
    shorter("runtime.delta") shouldBe Number(-81)
    IdentityMeasures.runtimeContradicts(shorter) shouldBe true
    // No edition billed: 62 minutes longer is another film's runtime.
    val plain = measures(Listing("The Lord of the Rings: The Return of the King", runtime = Some(263)), film)
    plain("runtime.delta") shouldBe Number(62)
    IdentityMeasures.runtimeContradicts(plain) shouldBe true
    // Nor is a word the film's own title carries an edition: "The Final Cut" is the film, not a cut of it.
    measures(Listing("The Final Cut", runtime = Some(150)), Film("The Final Cut", runtime = Some(95)))("runtime.delta") shouldBe Number(55)
  }

  "the listing-film runtime.delta" should "be signed: the venue's minutes less the closest of the film's" in {
    val film = Film("Anora", year = Some(2024), runtime = Some(139), alternativeRuntimes = Seq(132))
    // a venue billing fewer minutes than every runtime TMDB states reads below 0, one billing more above it
    measures(Listing("Anora", runtime = Some(120)), film)("runtime.delta") shouldBe Number(-12)
    measures(Listing("Anora", runtime = Some(150)), film)("runtime.delta") shouldBe Number(11)
    // closest by size: 135 is 3 above the 132-minute cut, not 4 below the 139
    measures(Listing("Anora", runtime = Some(135)), film)("runtime.delta") shouldBe Number(3)
    // the hand-written rules read its size either way
    IdentityMeasures.runtimeGap(measures(Listing("Anora", runtime = Some(100)), film)) shouldBe Some(32.0)
    IdentityMeasures.runtimeContradicts(measures(Listing("Anora", runtime = Some(100)), film)) shouldBe true
    // between two listings it stays a distance
    IdentityMeasures.listingListing(Listing("Anora", runtime = Some(120)), Listing("Anora", runtime = Some(139)),
      sameVenue = false, sharedChainId = None)("runtime.delta") shouldBe Number(19)
  }

  "a runtime a title brackets with a minute mark" should "be the listing's runtime when it publishes none" in {
    // KinoPort prints the running time into the title: "DZIADKU WIEJEMY (97’)".
    val film = Film("Dziadku, wiejemy!", year = Some(2025), runtime = Some(97))
    Seq("DZIADKU WIEJEMY (97’)", "Dziadku wiejemy (97')", "Dziadku wiejemy (97′)", "Dziadku wiejemy [97 min]").foreach { t =>
      withClue(t)(measures(Listing(t), film)("runtime.delta") shouldBe Number(0))
    }
    // A published runtime wins; a year, a sequel number or a bare number is not a runtime.
    measures(Listing("Dziadku wiejemy (97’)", runtime = Some(99)), film)("runtime.delta") shouldBe Number(2)
    Seq("Dziadku wiejemy (2025)", "Ocean's 11", "Kino 60 Krzeseł", "Dziadku wiejemy (97)").foreach { t =>
      withClue(t)(measures(Listing(t), film)("runtime.delta") shouldBe Missing("listing"))
    }
    // Between two listings too: the bracketed runtime is the listing's runtime.
    IdentityMeasures.listingListing(Listing("DZIADKU WIEJEMY (97’)"), Listing("Dziadku wiejemy", runtime = Some(97)),
      sameVenue = false, sharedChainId = None)("runtime.delta") shouldBe Number(0)
  }

  "a listing naming several directors" should "be the same person as a film crediting any ONE of them" in {
    // Flicks lists "A Night at the Opera" as Edmund Goulding (uncredited) and Sam Wood; TMDB credits
    // only Sam Wood. One person in common is a match — not first-only, not all-must-match.
    IdentityMeasures.directorRelation(Seq("Edmund Goulding", "Sam Wood"), Seq("Sam Wood")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Sam Wood"), Seq("Edmund Goulding", "Sam Wood")) shouldBe Category("same_person")
    PinnedGateMeasures.directorRelation(Seq("Edmund Goulding", "Sam Wood"), Seq("Sam Wood")) shouldBe Category("same_person")
  }

  "a director credited in another script" should "be compared in Latin letters, not read as incomparable" in {
    // TMDB credits a film's director in the deployment language's name for them, which is often
    // the native one ("毕赣", "Яков Протазанов"); the venue prints the Latin spelling.
    IdentityMeasures.directorRelation(Seq("Bi Gan"), Seq("毕赣")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Giorgos Lanthimos"), Seq("Γιώργος Λάνθιμος")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Kira Muratova"), Seq("Кира Муратова")) shouldBe Category("same_person")
    // A transliteration convention apart (Yakov / Akov): the surname is shared.
    IdentityMeasures.directorRelation(Seq("Yakov Protazanov"), Seq("Яков Протазанов")) shouldBe Category("shared_name")
    // A letter apart in a long word (Ljubomir / Lubomir) is one spelling of the same name.
    IdentityMeasures.directorRelation(Seq("Ljubomir Stefanov"), Seq("Љубомир Стефанов")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Ljubomir Stefanov", "Tamara Kotevska"), Seq("Љубомир Стефанов", "Тамара Котевска")) shouldBe
      Category("same_person")
  }

  "a director's name spelt a letter apart in each word" should "be the same person" in {
    // UK Flicks' "Aaram" ×20 credits "Rajeesh Parmeswaran"; TMDB has "Rajesh Parameswaran". Read as
    // different directors, the credit vetoed the very film.
    IdentityMeasures.directorRelation(Seq("Rajeesh Parmeswaran"), Seq("Rajesh Parameswaran")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Parmeswaran Rajeesh"), Seq("Rajesh Parameswaran")) shouldBe Category("same_person")
    // Short words are other names, not spellings; a word two letters off is another word.
    IdentityMeasures.directorRelation(Seq("Jan Kowal"), Seq("Jon Kowal")) should not be Category("same_person")
    IdentityMeasures.directorRelation(Seq("Rajeesh Parmeswaran"), Seq("Ramesh Parameswaran")) should not be Category("same_person")
  }

  it should "still tell different people apart, as a disagreement across scripts of its own" in {
    // A mismatch after transliteration is weaker than a Latin-to-Latin one (a Japanese reading of
    // Kanji is not its pinyin), so the calibration weighs it apart from `different`.
    IdentityMeasures.directorRelation(Seq("Bi Gan"), Seq("张艺谋")) shouldBe Category("different_script")
    IdentityMeasures.directorRelation(Seq("Kira Muratova"), Seq("Андрей Тарковский")) shouldBe Category("different_script")
    // Both in one script: compared as ever.
    IdentityMeasures.directorRelation(Seq("Wong Kar Wai"), Seq("Kim Jeong-hwan")) shouldBe Category("different")
    IdentityMeasures.directorRelation(Seq("Кира Муратова"), Seq("Кира Муратова")) shouldBe Category("same_person")
  }

  "a credited director naming the film's own house" should "be no director at all, not a different one" in {
    // UK venues' "Met Opera 2026-27: Così fan tutte" ×97 credit "The Metropolitan Opera", which TMDB's record names
    // in its title, not as its director (Phelim McDermott): read as a different person, it vetoed the film.
    val met = Film("The Metropolitan Opera: Così fan tutte", year = Some(2026), directors = Some(Seq("Phelim McDermott")))
    measures(Listing("Met Opera 2026-27: Così fan tutte", directors = Seq("The Metropolitan Opera")), met)("director") shouldBe IdentityMeasures.MissingListing
    // beside a person the venue credits too, the person is compared
    measures(Listing("Met Opera 2026-27: Così fan tutte", directors = Seq("The Metropolitan Opera", "Phelim McDermott")), met)("director") shouldBe
      Category("same_person")
    // a director the title names who directed it stays a person: "Guillermo del Toro's Pinocchio"
    val pinocchio = Film("Guillermo del Toro's Pinocchio", year = Some(2022), directors = Some(Seq("Guillermo del Toro", "Mark Gustafson")))
    measures(Listing("Pinocchio", directors = Seq("Guillermo del Toro")), pinocchio)("director") shouldBe Category("same_person")
  }

  "a director's name" should "be the same person whatever its order, case and splitting" in {
    // BTS São Paulo: the venue writes the given name joined and the surname last in capitals.
    IdentityMeasures.directorRelation(Seq("Jungjae HA"), Seq("Ha Jung-jae")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Makoto Shinkai"), Seq("Shinkai Makoto")) shouldBe Category("same_person")
    // Every letter must be accounted for: a name word missing is not the same written name.
    IdentityMeasures.directorRelation(Seq("Jung Ha"), Seq("Ha Jung-jae")) should not be Category("same_person")
    IdentityMeasures.directorRelation(Seq("Jungjae Kim"), Seq("Ha Jung-jae")) should not be Category("same_person")
  }

  "a decorated original title" should "read as naming the film, as a decorated title does, not as a word overlap" in {
    val yourName = Seq("Twoje imię", "君の名は。", "Your Name.")
    IdentityMeasures.originalTitleRelation(Some("Your Name (Dolby Night)"), yourName) shouldBe Category("segment")
    IdentityMeasures.originalTitleRelation(Some("Your Name"), yourName) shouldBe Category("match")
    IdentityMeasures.originalTitleRelation(Some("Ken Russell's The Devils"), Seq("The Devils")) shouldBe Category("decorated")
    IdentityMeasures.originalTitleRelation(Some("It"), Seq("It Ends with Us")) shouldBe Category("fragment")
    IdentityMeasures.originalTitleRelation(Some("Stalker"), Seq("Solaris")) shouldBe Category("disjoint")
    // The listing-film measure reads it the same way.
    measures(Listing("Twoje imię", originalTitle = Some("Your Name (Dolby Night)")),
      Film("Twoje imię", Some("君の名は。"), alternativeTitles = Seq("Your Name.")))("originalTitle") shouldBe Category("segment")
  }

  "a version-tagged title" should "name the film's own title whole, as a cut of it, not a segment or an overlap of it" in {
    // DE CineStar's "Herr der Ringe - Rückkehr des Königs (ext.)", originally "The Lord of the Rings: The Return of the King –
    // Extended": `originalTitle=segment` (-2.52) put Jackson's film under the learned listing-film cut, and its `denies-film`
    // edges vetoed every other venue's Extended billing (recording 528).
    val returnOfTheKing = Film("Der Herr der Ringe - Die Rückkehr des Königs", Some("The Lord of the Rings: The Return of the King"), year = Some(2003))
    measures(Listing("Herr der Ringe - Rückkehr des Königs (ext.)", originalTitle = Some("The Lord of the Rings: The Return of the King – Extended")),
      returnOfTheKing)("originalTitle") shouldBe Category("match")
    IdentityMeasures.originalTitleRelation(Some("Your Name (re-release)"), Seq("Your Name.")) shouldBe Category("match")
    // DE Capitol's and Kinorama's billings name its German title whole once the tag is dropped
    measures(Listing("DER HERR DER RINGE - DIE RÜCKKEHR DES KÖNIGS - EXTENDED VERSION"), returnOfTheKing)("title") shouldBe Category("exact")
    measures(Listing("Der Herr der Ringe: Die Rückkehr des Königs EXTENDED VERSION"), returnOfTheKing)("title") shouldBe Category("exact")
    // a tag is no other title: "Pink Floyd: The Final Cut" is not "Pink Floyd"
    measures(Listing("Pink Floyd: The Final Cut"), Film("Pink Floyd"))("title") should not be Category("exact")
  }

  "an original title copying the listing's own title" should "repeat it whether whole or a truncation" in {
    // UK BTS relays: the venue's original-title field is its own title cut short ("…: Live").
    val bts = "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING"
    IdentityMeasures.repeatsItsTitle(Listing(bts, originalTitle = Some("BTS World Tour 'ARIRANG' In Buenos Aires: Live"))) shouldBe true
    IdentityMeasures.repeatsItsTitle(Listing(bts, originalTitle = Some(bts))) shouldBe true
    IdentityMeasures.repeatsItsTitle(Listing("Diabły | Splat!FilmFest", originalTitle = Some("Diabły"))) shouldBe true
    // Anything the title does not already carry is a second fact: another language's title, a
    // shared word, or a LONGER title the venue shortened for display.
    IdentityMeasures.repeatsItsTitle(Listing("Diabły | Splat!FilmFest", originalTitle = Some("The Devils"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("De Gaulle: Part 2 - Liberte", originalTitle = Some("La Bataille de Gaulle - Partie 2 : J’écris ton nom"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("Alien", originalTitle = Some("Alien: Romulus"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("Alien")) shouldBe false
    // UK Everyman: an original title that holds the title as one piece of more is not a copy. Read as
    // one, it stopped counting against Zeffirelli's 1968 film and the Royal Ballet's staging took it.
    IdentityMeasures.repeatsItsTitle(Listing("Royal Ballet and Opera: Romeo and Juliet",
      originalTitle = Some("Royal Ballet and Opera: Romeo and Juliet (INACTIVE)"))) shouldBe false
  }

  it should "be absent against a film the title names, and stay what it measures against one it does not" in {
    val bts = Listing("BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", originalTitle = Some("BTS World Tour 'ARIRANG' In Buenos Aires: Live"))
    measures(bts, Film("BTS World Tour 'Arirang' in Buenos Aires: Live Viewing"))("originalTitle") shouldBe IdentityMeasures.MissingListing
    // UK Everyman: the repeat against The Mummy, which its title does not name, is still a disjoint title.
    val dracula = Listing("Dracula (4K Restoration)", originalTitle = Some("Dracula 4k Restoration"))
    measures(dracula, Film("The Mummy"))("originalTitle") shouldBe Category("disjoint")
  }

  "a listing's exact top hits" should "be the films its title names exactly that a title search returned FIRST" in {
    val listing = Listing("Godzilla vs. Megalon")
    val megalon = Film("Godzilla vs. Megalon", year = Some(1973))
    val remake  = Film("Godzilla vs. Megalon", year = Some(2031))
    val other   = Film("Godzilla vs. Mothra", year = Some(1964))
    IdentityMeasures.exactTopHits(listing, Seq((1, megalon, Some(1)), (2, remake, Some(2)), (3, other, Some(1)))) shouldBe Seq(1)
    // First only under a banner segment's search, or not first at all: no exact top hit.
    IdentityMeasures.exactTopHits(Listing("Kino Nocne: Godzilla vs. Megalon"), Seq((1, megalon, Some(1)))) shouldBe Nil
    IdentityMeasures.exactTopHits(listing, Seq((1, megalon, Some(2)), (3, other, Some(1)), (4, megalon, None))) shouldBe Nil
  }

  "a sequel numeral" should "be compared by the NUMBER each title carries, however it is written" in {
    def numeral(listing: String, film: Film) = IdentityMeasures.numeralRelation(Listing(listing), film)
    // US, the recorded "The Texas Chainsaw Massacre 2" ×4 (labelled 16337): TMDB's 1974 original
    // (30497, alternative title "The Texas Chainsaw Massacre") is the series without the number.
    val original = Film("The Texas Chain Saw Massacre", alternativeTitles = Seq("Leatherface", "The Texas Chainsaw Massacre"))
    val sequel   = Film("The Texas Chainsaw Massacre Part 2", alternativeTitles = Seq("TCM 2", "The Texas Chain Saw Massacre Part 2"))
    numeral("The Texas Chainsaw Massacre 2", original) shouldBe Category("listing_only")
    numeral("The Texas Chainsaw Massacre 2", sequel) shouldBe Category("same")
    numeral("The Texas Chainsaw Massacre", sequel) shouldBe Category("film_only")
    numeral("Mortal Kombat 2", Film("Mortal Kombat II")) shouldBe Category("same")
    numeral("Rocky II", Film("Rocky")) shouldBe Category("listing_only")
    numeral("Toy Story 2", Film("Toy Story 3")) shouldBe Category("different")
    numeral("Kill Bill: Vol. 2 (2026)", Film("Kill Bill: Vol. 1")) shouldBe Category("different")
    numeral("Star Wars: Episode IV - A New Hope", Film("Star Wars", alternativeTitles = Seq("Star Wars: Episode IV - A New Hope"))) shouldBe Category("same")
    // PL Cinema City and UK Odeon, recorded 2026-10: the 2026 final billed against TMDB's record of the 2025 one, its
    // number run into the word before it.
    val worlds25 = Film("League of Legends Worlds25 - Finals in Cinema")
    numeral("League of Legends Worlds 26 | Finals in Cinema", worlds25) shouldBe Category("different")
    numeral("League of Legends Worlds 26 - Finals in Cinema", worlds25) shouldBe Category("different")
    // DE UFA Palast, recorded 2026-10-15: the feed copied the 2025 record's title as the original, number and all — it
    // never numbers the billing the record's way; an original title of its own still does.
    IdentityMeasures.numeralRelation(Listing("League of Legends Worlds 26 - Finals in Cinema",
      originalTitle = Some("League of Legends Worlds25 - Finals in Cinema")), worlds25) shouldBe Category("different")
    IdentityMeasures.numeralRelation(Listing("Szybcy i wściekli 7", originalTitle = Some("Furious 7")), Film("Furious 7")) shouldBe Category("same")
    numeral("League of Legends Worlds 25 - Finals in Cinema", worlds25) shouldBe Category("same")
    numeral("Rambo3", Film("Rambo III")) shouldBe Category("same")
  }

  it should "have nothing to compare in a remake, a title whose number is its name, or a decoration's number" in {
    def numeral(listing: String, film: String) = IdentityMeasures.numeralRelation(Listing(listing), Film(film))
    numeral("Suspiria", "Suspiria") shouldBe Missing("none")
    numeral("2001: A Space Odyssey", "2001: A Space Odyssey") shouldBe Missing("none")
    numeral("1917", "1917") shouldBe Missing("none")
    numeral("Se7en", "Se7en") shouldBe Missing("none")
    numeral("M3GAN", "M3GAN") shouldBe Missing("none")
    numeral("R2D2", "R2D2") shouldBe Missing("none")
    numeral("Ocean's Eleven", "Ocean's Eleven") shouldBe Missing("none")
    numeral("2046", "2046") shouldBe Missing("none")
    numeral("9 to 5", "9 to 5") shouldBe Category("same")
    numeral("Cineworld 30: The Matrix", "The Matrix") shouldBe Missing("none")
    numeral("Sense and Sensibility (2026)", "Sense and Sensibility") shouldBe Missing("none")
    numeral("The Metropolitan Opera 2026/27: Manon", "Manon") shouldBe Missing("none")
    // A lone "i" inside a title is a word (Polish "and"), and L, C, D, M spell words, not instalments.
    numeral("Vivaldi i ja", "Vivaldi") shouldBe Missing("none")
    numeral("Listy do M. 5", "Listy do M. 5") shouldBe Category("same")
    numeral("Listy do M 5", "Listy do M. 5") shouldBe Category("same")
    numeral("Zodiac", "Fight Club") shouldBe Missing("unrelated")
    // a relay numbers its house's season, no instalment
    numeral("RBO: Tosca 27", "Royal Ballet & Opera 2026/27: Tosca") shouldBe Missing("season")
  }

  it should "keep a title that numbers another instalment from NAMING the film its words decorate" in {
    val original = Film("The Texas Chain Saw Massacre", alternativeTitles = Seq("The Texas Chainsaw Massacre"))
    val listing  = Listing("The Texas Chainsaw Massacre 2")
    IdentityMeasures.titleRelation(listing, original) shouldBe Category("decorated")
    IdentityMeasures.namesFilm(listing, original) shouldBe false
    IdentityMeasures.namesFilm(Listing("Throwback: The Texas Chainsaw Massacre"), original) shouldBe true
    IdentityMeasures.namesFilm(Listing("Cineworld 30: The Matrix"), Film("The Matrix")) shouldBe true
    // So no venue's own facts back the original on a sequel's title.
    val group = Seq("a", "b").map(_ -> listing.copy(directors = Seq("Tobe Hooper")))
    new VenueBacking(Map("sequel" -> group)).corroborating(Seq("sequel"), original.copy(directors = Some(Seq("Tobe Hooper"))), "") shouldBe 0
  }

  "a listing that publishes nothing but its title" should "be told from one that publishes a fact" in {
    Listing("Sense and Sensibility").publishesAFact shouldBe false
    Listing("Relaxed Screening: Sense and Sensibility").publishesAFact shouldBe false
    Listing("Sense and Sensibility", originalTitle = Some("Sense and Sensibility")).publishesAFact shouldBe false
    Listing("Sense and Sensibility (2026)").publishesAFact shouldBe true
    Listing("Sense and Sensibility", directors = Seq("Georgia Oakley")).publishesAFact shouldBe true
    Listing("Sense and Sensibility", runtime = Some(132)).publishesAFact shouldBe true
  }

  "a title naming two films" should "name them apart when their pieces sit at spans of it that do not overlap" in {
    val gruffalo = Film("The Gruffalo"); val child = Film("The Gruffalo's Child")
    // A double bill: the two titles share words, but each has its own place in the title.
    IdentityMeasures.namedApart(Listing("The Gruffalo + The Gruffalo's Child"), gruffalo, child) shouldBe true
    // Disjoint words, as before.
    IdentityMeasures.namedApart(Listing("Lalka (Dolly)"), Film("Lalka"), Film("Dolly")) shouldBe true
    // Nested: one film's title is part of the other's, which the whole title names.
    IdentityMeasures.namedApart(Listing("Joker: Folie à deux"), Film("Joker"), Film("Joker: Folie à deux")) shouldBe false
    IdentityMeasures.namedApart(Listing("The Gruffalo's Child"), gruffalo, child) shouldBe false
    // A film named by both of its titles leaves no place of its own for a third title's word.
    IdentityMeasures.namedApart(Listing("Tokyo Story (Tôkyô monogatari)"), Film("Tokyo Story", Some("Tôkyô monogatari")), Film("Tokyo")) shouldBe false
    // A film whose own title joins two others' is named by the whole title, which overlaps both.
    IdentityMeasures.namedApart(Listing("Romeo + Juliet"), Film("Romeo + Juliet"), Film("Romeo")) shouldBe false
    IdentityMeasures.namedApart(Listing("Fast & Furious"), Film("Fast & Furious"), Film("Furious")) shouldBe false
  }

  "the titles venues publish for a record" should "be a title published beside one of its titles as the original, when unanimous" in {
    val concert = Film("André Rieu's 2026 Summer Concert: Viva Maastricht!")
    val listings = Seq(
      Listing("Andre Rieu. Niech żyje Maastricht!", originalTitle = Some("Andre Rieu's 2026 Summer Concert: Viva Maastricht!")),
      Listing("André Rieu. Niech żyje Maastricht!"),
      // An original title repeating the listing's own title translates nothing.
      Listing("Nosferatu", originalTitle = Some("Nosferatu")))
    IdentityMeasures.venueTitles(listings, Seq(1 -> concert, 2 -> Film("Nosferatu"))) shouldBe
      Map(1 -> Seq("Andre Rieu. Niech żyje Maastricht!"))
    IdentityMeasures.titleRelation(Listing("André Rieu. Niech żyje Maastricht!"),
      IdentityMeasures.withVenueTitles(concert, Seq("Andre Rieu. Niech żyje Maastricht!"))) shouldBe Category("alternative")
    // Two venues giving one title two originals name two films by it: it translates neither.
    val invitation = Seq(Listing("La invitación", originalTitle = Some("The Invitation")), Listing("La invitación", originalTitle = Some("The Invite")))
    IdentityMeasures.venueTitles(invitation, Seq(1 -> Film("The Invitation"), 2 -> Film("The Invite"))) shouldBe Map.empty
    // A record already titled so gains nothing, nor one the title names already.
    IdentityMeasures.venueTitles(Seq(Listing("Diuna", originalTitle = Some("Dune"))), Seq(1 -> Film("Diuna", Some("Dune")))) shouldBe Map.empty
    IdentityMeasures.venueTitles(Seq(Listing("Coraline (2009)", originalTitle = Some("Coraline"))), Seq(1 -> Film("Coraline"))) shouldBe Map.empty
    // An original naming several records names none of them: the facts pick among namesakes.
    IdentityMeasures.venueTitles(Seq(Listing("Niebo nad Normandią", originalTitle = Some("Pressure"))),
      Seq(1 -> Film("Pressure"), 2 -> Film("Pressure"))) shouldBe Map.empty
    IdentityMeasures.venueTitles(Seq(Listing("Diabły", originalTitle = Some("The Devils"))),
      Seq(31767 -> Film("Diabły", Some("The Devils")), 1491681 -> Film("The Devils"))) shouldBe Map.empty
    // DE UFA Palast, recorded 2026-10-15: an original title copied from last year's record numbers another instalment
    // than the billing — no translation of it.
    IdentityMeasures.venueTitles(Seq(Listing("League of Legends Worlds 26 - Finals in Cinema",
      originalTitle = Some("League of Legends Worlds25 - Finals in Cinema"))),
      Seq(1552059 -> Film("League of Legends Worlds25 - Finals in Cinema"))) shouldBe Map.empty
  }
  they should "also be a title whose listings' own director and year single out one record" in {
    // PL "Vincent. Legenda oceanu" [2025] {Reza Memari}: TMDB titles it "The Last Whale Singer" only, so its
    // bare spellings at other venues only overlapped the record the facts had already named.
    val whale = Film("The Last Whale Singer", year = Some(2026), directors = Some(Seq("Reza Memari")))
    val vincent = Seq(Listing("Vincent. Legenda oceanu", year = Some(2025), directors = Seq("Reza Memari")),
      // credits singling out two records here say nothing; credits singling out none say nothing either
      Listing("Vincent. Legenda oceanu", year = Some(2025), directors = Seq("Reza Memari", "Steven Majaury")),
      Listing("Vincent. Legenda oceanu", year = Some(2025), directors = Seq("Someone Else")))
    IdentityMeasures.titlesByFacts(vincent, Seq(677558 -> whale, 9 -> Film("Other", year = Some(2025), directors = Some(Seq("Steven Majaury"))))) shouldBe
      Seq(677558 -> "Vincent. Legenda oceanu")
    // A title some record carries whole stays that record's: a 2026 "Obcy" by Ozon is not his "La Catastrophe".
    val ozon = Seq(Listing("Obcy", year = Some(2026), directors = Seq("François Ozon")))
    IdentityMeasures.titlesByFacts(ozon, Seq(1 -> Film("Obcy", year = Some(2025), directors = Some(Seq("François Ozon"))),
      2 -> Film("La Catastrophe", year = Some(2027), directors = Some(Seq("François Ozon"))))) shouldBe empty
    // A title pointing at another record its years do not rule out names that one, whoever its credit picks.
    IdentityMeasures.titlesByFacts(Seq(Listing("BTS World Tour 'ARIRANG' In Buenos Aires: Live", year = Some(2026), directors = Seq("Jungjae Ha"))),
      Seq(1 -> Film("BTS World Tour 'Arirang' In São Paulo: Live Viewing", year = Some(2026), directors = Some(Seq("Ha Jung-jae"))),
        2 -> Film("BTS World Tour 'Arirang' in Buenos Aires: Live Viewing", year = Some(2026)))) shouldBe empty
    // A piece of the record's own title names the work it belongs to, never the record.
    IdentityMeasures.titlesByFacts(Seq(Listing("BTS World Tour 'ARIRANG'", year = Some(2026), directors = Seq("Jungjae Ha"))),
      Seq(1 -> Film("BTS World Tour 'Arirang' In São Paulo: Live Viewing", year = Some(2026), directors = Some(Seq("Ha Jung-jae"))))) shouldBe empty
    // A double bill's facts may single out one of its films; its title is neither's.
    IdentityMeasures.titlesByFacts(Seq(Listing("Słonik w lesie + Tańczący przyjaciel", year = Some(2025), directors = Seq("Jane Doe"))),
      Seq(1 -> Film("Olifantje in het bos", year = Some(2025), directors = Some(Seq("Jane Doe"))))) shouldBe empty
    // Two listings of one title singling out two different records: the title names neither.
    IdentityMeasures.titlesByFacts(Seq(Listing("Nowy film", year = Some(2025), directors = Seq("Anna Nowak")),
      Listing("Nowy film", year = Some(2024), directors = Seq("Jan Kowalski"))),
      Seq(1 -> Film("A", year = Some(2025), directors = Some(Seq("Anna Nowak"))), 2 -> Film("B", year = Some(2024), directors = Some(Seq("Jan Kowalski"))))) shouldBe empty
  }


  "a venue's one-letter typo in a long word" should "still name the film's title exactly" in {
    // US: "Shaun the Sheep: The Beast of Mossy Botton" (×6) and "Pradhama Drishtiya Kuttakkar" (×16)
    // found the film but measured the title as unrelated; the pipeline's fuzzy TMDB search took them.
    IdentityMeasures.titleRelation(Listing("Shaun the Sheep: The Beast of Mossy Botton"), Film("Shaun the Sheep: The Beast of Mossy Bottom")) shouldBe IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("Pradhama Drishtiya Kuttakkar"), Film("Pradhama Drishtya Kuttakkar")) shouldBe IdentityMeasures.Category("exact")
  }

  it should "not reach a sequel's number, a short word, or two words" in {
    IdentityMeasures.titleRelation(Listing("Scary Movie 3"), Film("Scary Movie 4")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("Mission: Impossible II"), Film("Mission: Impossible III")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("The Hunt"), Film("The Hurt")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("The Beast of Mossy Botton"), Film("The Feast of Mossy Bottom")) should not be IdentityMeasures.Category("exact")
  }

  it should "also name the film from a shape of the title, and from the original title" in {
    // UK, Vue's "Pradhama Drishtiya Kuttakkar (Malayalam)" (×13), originally "Pradhama Drishtiya
    // Kuttakkar": the language tag is a shape away, and the original title carries the same typo.
    IdentityMeasures.titleRelation(Listing("Pradhama Drishtiya Kuttakkar (Malayalam)"), Film("Pradhama Drishtya Kuttakkar")).value should
      (be("exact") or be("segment"))
    IdentityMeasures.originalTitleRelation(Some("Pradhama Drishtiya Kuttakkar"), Seq("Pradhama Drishtya Kuttakkar")) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("Scary Movie 3"), Seq("Scary Movie 4")) should not be IdentityMeasures.Category("match")
  }

  it should "not reach a one-word title: a letter apart there is another film" in {
    // UK, 64 "Lalka" / "Lalka (The Doll)" listings, originally "Lalka", were taken for "Lalkar" (1972).
    IdentityMeasures.originalTitleRelation(Some("Lalka"), Seq("Lalkar")) should not be IdentityMeasures.Category("match")
    IdentityMeasures.titleRelation(Listing("Lalka"), Film("Lalkar")) should not be IdentityMeasures.Category("exact")
  }

  "an original title" should "match a film's title that differs only by a year or a season" in {
    // DE, 153 "MET Opera Live im Kino: Così Fan Tutte" listings, originally "The Metropolitan Opera: Così
    // fan tutte (2026)", against the record "The Metropolitan Opera 2026/27: Così fan tutte".
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Così fan tutte (2026)"),
      Seq("The Metropolitan Opera 2026/27: Così fan tutte"), filmYear = Some(2026)) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("Dune (2021)"), Seq("Dune"), filmYear = Some(2021)) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Carmen (2026)"),
      Seq("Royal Ballet & Opera 2026/27: Carmen")) should not be IdentityMeasures.Category("match")
  }

  it should "not match when the year it drops disagrees with the film's" in {
    // UK, Cineworld's "The Royal Ballet: The Nutcracker", originally "... (2024)", went to the 2015 recording
    // "The Royal Ballet: The Nutcracker" instead of the 2024/25 production.
    IdentityMeasures.originalTitleRelation(Some("The Royal Ballet: The Nutcracker (2024)"), Seq("The Royal Ballet: The Nutcracker"),
      filmYear = Some(2015)) should not be IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Così fan tutte (2026)"),
      Seq("The Metropolitan Opera 2026/27: Così fan tutte"), filmYear = Some(2026)) shouldBe IdentityMeasures.Category("match")
  }

  // The scorer asks the shapes of a listing's billed title again for every candidate it weighs
  // (`CandidateScoring.namesOnlyATag`, `Qualifiers.split`): ~6% of a US replay's CPU (run 37517196329). Kept on the
  // listing, split once each — never in a process-wide cache.
  "a listing's own titles' shapes" should "be split once per listing, the same as split afresh" in {
    val listing = IdentityMeasures.Listing("Mistyczka", rawTitle = Some("MISTYCZKA /film polski/ [2D LEKTOR] | DKF"))
    val billed  = listing.rawTitle.get
    val first   = listing.shapesOfOwn(billed)
    IdentityMeasures.capitalisedTags(listing)
    listing.shapesOfOwn(billed) should be theSameInstanceAs first
    first shouldBe IdentityMeasures.shapes(Seq(billed))
    listing.shapesOfOwn("another title") shouldBe IdentityMeasures.shapes(Seq("another title"))
  }

  "a work a slash joins to another" should "deny the film it is not, but not for a banner the title rules peel off its head" in {
    val fish = Film("The Fish with One Sleeve", year = Some(2025))
    IdentityMeasures.joinsAnotherWork(Listing("SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE", searchTitles = Seq("SHINJUKUY BOYS")), fish) shouldBe true
    // Kino Sokolnia Kępno: the retrospective banner before the slash is searched off (xtra-pp-wajda-rewizje-slash)
    val wajda = Film("Bez znieczulenia", year = Some(1978), directors = Some(Seq("Andrzej Wajda")))
    val banner = Listing("„WAJDA: re-wizje” 2026 / \"BEZ ZNIECZULENIA\" (1985)", searchTitles = Seq("\"BEZ ZNIECZULENIA\" (1985)"))
    IdentityMeasures.joinsAnotherWork(banner, wajda) shouldBe false
    IdentityMeasures.joinsAnotherWork(banner.copy(searchTitles = Nil), wajda) shouldBe true
  }

  "a part a title numbers and subtitles" should "be another film's part only when the subtitle holds no word of the film's titles" in {
    val lang = Film("Das indische Grabmal", year = Some(1959), directors = Some(Seq("Fritz Lang")))
    IdentityMeasures.titlesAnotherPart(Listing("DAS INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)"), lang) shouldBe true
    // a bare number among the film's titles names no part: TMDB's "2 - Das indische Grabmal", Danish "… gravmæle II"
    val langTitled = lang.copy(alternativeTitles = Seq("2 - Das indische Grabmal", "Det indiske gravmæle II"))
    IdentityMeasures.titlesAnotherPart(Listing("DAS INDISCHE GRABMAL (TEIL 2, DER TIGER VON ESCHNAPUR)"), langTitled) shouldBe true
    IdentityMeasures.titlesAnotherPart(Listing("SHOAH (TEIL I)"), Film("Shoah", year = Some(1985))) shouldBe false
    IdentityMeasures.titlesAnotherPart(Listing("De Gaulle Part Two: Liberté"), Film("De Gaulle: Liberté", year = Some(2026))) shouldBe false
    IdentityMeasures.titlesAnotherPart(Listing("Ramayana: Rise Of A Legend - Part 1 Hindi"), Film("Ramayana: Rise of a Legend", year = Some(2026))) shouldBe false
    // a film numbered as the part is that part, whatever note follows it
    IdentityMeasures.titlesAnotherPart(Listing("Dune: Part Three - ERLY FAN PREVIEW"), Film("Dune: Part Three", year = Some(2026))) shouldBe false
    // however the number is spelled
    IdentityMeasures.titlesAnotherPart(Listing("Dune: Part 3 - The IMAX Experience"), Film("Dune: Part Three", year = Some(2026))) shouldBe false
    IdentityMeasures.titlesAnotherPart(Listing("Der Pate Teil 2 - Restaurierte Fassung"),
      Film("The Godfather Part II", year = Some(1974), alternativeTitles = Seq("Der Pate – Teil II"))) shouldBe false
    // an episode, named whether or not it is numbered
    IdentityMeasures.titlesAnotherPart(Listing("LOVE ISLAND - EPISODE NATURE"), Film("Love Island", year = Some(1952))) shouldBe true
    IdentityMeasures.titlesAnotherPart(Listing("Star Wars: Episode VII - The Force Awakens"),
      Film("Star Wars: The Force Awakens", year = Some(2015))) shouldBe false
  }

  "a numbered title whose original title gives a subtitle" should "be another instalment only when none of the film's titles holds it" in {
    // DE Kinopolis' "Der Herr der Ringe 2 (Extended Version)" (Forgejo recording 528) took Bakshi's 1978 film by its bare top hit
    val lotr2    = Listing("Der Herr der Ringe 2 (Extended Version)", originalTitle = Some("The Lord of the Rings: The Two Towers – Extended"))
    val bakshi   = Film("Der Herr der Ringe", originalTitle = Some("The Lord of the Rings"), year = Some(1978))
    val twoTowers = Film("Der Herr der Ringe - Die zwei Türme", originalTitle = Some("The Lord of the Rings: The Two Towers"), year = Some(2002))
    IdentityMeasures.numbersAnotherSubtitle(lotr2, bakshi) shouldBe true
    // an alternative title's number is a catalogue's, not the film's: TMDB's Spanish one for Bakshi's
    IdentityMeasures.numbersAnotherSubtitle(lotr2,
      bakshi.copy(alternativeTitles = Seq("El Señor De Los Anillos (Animación) 2 - El Señor De Los Anillos"))) shouldBe true
    IdentityMeasures.numbersAnotherSubtitle(lotr2, twoTowers) shouldBe false
    // the subtitle held only by an alternative title, in another language than the film's own titles
    IdentityMeasures.numbersAnotherSubtitle(lotr2, Film("Der Herr der Ringe - Die zwei Türme", originalTitle = Some("Il Signore degli Anelli - Le due torri"),
      alternativeTitles = Seq("The Lord of the Rings: The Two Towers"), year = Some(2002))) shouldBe false
    // a subtitle that is but a version tag names no instalment
    IdentityMeasures.numbersAnotherSubtitle(Listing("Der Herr der Ringe 2", originalTitle = Some("The Lord of the Rings - Extended Edition")), bakshi) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("Alien 2", originalTitle = Some("Aliens: Director's Cut – Remastered 4K")),
      Film("Aliens - Die Rückkehr", originalTitle = Some("Aliens"), year = Some(1986))) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("Terminator 2 (4K)", originalTitle = Some("Terminator 2: Judgment Day – Remastered")),
      Film("Terminator", originalTitle = Some("The Terminator"), year = Some(1984))) shouldBe true
    // a number that is no part's: a year, a name, a spelled-out series number
    IdentityMeasures.numbersAnotherSubtitle(Listing("2001: Odyssee im Weltraum", originalTitle = Some("2001: A Space Odyssey")),
      Film("2001: Odyssee im Weltraum", originalTitle = Some("2001: A Space Odyssey"), year = Some(1968))) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("Ocean's 11", originalTitle = Some("Ocean's Eleven: Remastered")),
      Film("Ocean's Eleven", year = Some(2001))) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("Fantastic 4", originalTitle = Some("The Fantastic Four: First Steps")),
      Film("Fantastic Four", year = Some(2005))) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("9½ Wochen", originalTitle = Some("9½ Weeks: Uncut")),
      Film("9½ Wochen", originalTitle = Some("Nine 1/2 Weeks"), year = Some(1986))) shouldBe false
    // a number mid-title, in an original title that only repeats the billing: PL Kinoteka's Wajda retrospective
    val wajda = "BRZEZINA | WAJDA: re- wizje. Przegląd filmów Andrzeja Wajdy w 100. rocznicę urodzin"
    IdentityMeasures.numbersAnotherSubtitle(Listing(wajda, originalTitle = Some(wajda)), Film("Brzezina", year = Some(1970))) shouldBe false
    IdentityMeasures.numbersAnotherSubtitle(Listing("Der Herr der Ringe Teil Zwei", originalTitle = Some("The Lord of the Rings: The Two Towers")), bakshi) shouldBe true
    // a series' slot number, not the film's: the subtitle is the film
    IdentityMeasures.numbersAnotherSubtitle(Listing("KinoKult 3 - Metropolis"), Film("Metropolis", year = Some(1927))) shouldBe false
    // the billing's own subtitle is the venue's language, which a record in another need not hold (DE Neues Regina)
    IdentityMeasures.numbersAnotherSubtitle(Listing("HOBBIT 2: SMAUGS EINÖDE (EXTENDED)"),
      Film("The Hobbit: The Desolation of Smaug", year = Some(2013))) shouldBe false
    // a year with its century elided numbers no instalment (PL Kino Światowid)
    IdentityMeasures.numbersAnotherSubtitle(Listing("Halloween '26: Inni (re- release)", originalTitle = Some("Halloween: The Others")),
      Film("Halloween", year = Some(1978))) shouldBe false
    // a part the original title numbers, not subtitles
    IdentityMeasures.numbersAnotherSubtitle(Listing("Harry Potter 7", originalTitle = Some("Harry Potter and the Deathly Hallows: Part 1")),
      Film("Harry Potter und die Heiligtümer des Todes - Teil 1", originalTitle = Some("Harry Potter and the Deathly Hallows: Part 1"), year = Some(2010))) shouldBe false
    // a film numbered as the billing's instalment is that instalment, whatever its titles say
    IdentityMeasures.numbersAnotherSubtitle(Listing("Shrek 2", originalTitle = Some("Shrek 2: The Sing-Along")), Film("Shrek 2", year = Some(2004))) shouldBe false
    // no number, nothing to compare
    IdentityMeasures.numbersAnotherSubtitle(Listing("Der Herr der Ringe (Extended Version)", originalTitle = Some("The Lord of the Rings: The Two Towers – Extended")),
      bakshi) shouldBe false
  }
}

