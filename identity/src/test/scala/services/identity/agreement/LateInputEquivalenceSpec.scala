package services.identity.agreement

import models.{KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CatalogueAnswers, CatalogueHit, CatalogueId, DetailFacts, FilmTable, ForwardingIdentityLookups, Hit, IdentityLookups,
  IdentityMeasures, Listing, Pin, PinClaim, PosterHash, ProseWords, ScreeningDays, UnifiedRules}
import tools.IndependentCases

import java.time.LocalDate

/**
 * Per input kind, the no-drift principle (docs/design/identity-no-drift.md, "The harness"): decide without an input, let
 * it arrive, and the incremental path takes exactly what a whole resolve with it from the start takes — grouping and
 * takes, no tolerance ([[LateInputs]]). Each kind's cases run under every absence the kind can have (not filed, asked and
 * failed, not existing yet) and every restart (none, while it is withheld, after it arrived); each case must have teeth
 * (the late input moves the whole resolve), so a green says something.
 *
 * The production drift rows (`scripts.IdentityLiveDrift`, 2026-10-25: 39 listings prod's live takes differ on from a whole
 * resolve over prod's own inputs) are cases here by their shape — their titles, films and families as the live check
 * named them, their inputs reduced to what decides them ([[LateInputEquivalenceSpec.Drift]]).
 *
 * A case the incremental path does not cure yet is `pendingUntilFixed`, its root cause named beside it: once the fix
 * cures it, the spec fails until the mark comes off.
 */
class LateInputEquivalenceSpec extends AnyFlatSpec with Matchers {
  import LateInputEquivalenceSpec._
  import LateInputs.{Kind, Restart}

  /** Every case of `kind`, under every absence it can have and every restart: the divergences. */
  private def divergences(cases: Seq[LateInputs.Scenario]): Seq[String] =
    IndependentCases.flatMap(for { c <- cases; restart <- Restart.values.toSeq } yield c -> restart) { case (c, restart) =>
      LateInputs.divergence(c, restart)
    }

  private def toothless(cases: Seq[LateInputs.Scenario]): Seq[String] = cases.filterNot(LateInputs.matters).map(_.name)

  "every input kind" should "have cases, each moving the whole resolve when it arrives" in {
    Kind.values.toSeq.filterNot(kind => everyCase.exists(_.kind == kind)) shouldBe empty
    toothless(everyCase) shouldBe empty
  }

  /** `cases` held to the whole resolve — as a known failure when the incremental path does not cure them yet. */
  private def holds(title: String, behaviour: String, cases: Seq[LateInputs.Scenario], pending: Boolean): Unit =
    if (cases.nonEmpty) {
      if (pending) title should behaviour in pendingUntilFixed { divergences(cases) shouldBe empty }
      else title should behaviour in { divergences(cases) shouldBe empty }
    }

  Kind.values.foreach { kind =>
    val (open, cured) = everyCase.filter(_.kind == kind).partition(c => Pending.contains(c.name))
    holds(s"a late ${kind.toString} input", "leave the incremental path where a whole resolve with it from the start would be", cured, pending = false)
    open.foreach(c => holds(s"a late ${kind.toString} input (${c.name}, ${Pending(c.name)})", "leave it there too", Seq(c), pending = true))
  }

  "the production drift rows" should "each be a case with teeth" in {
    toothless(Drift.all) shouldBe empty
  }

  Drift.all.foreach(row => holds(s"drift row ${row.name}", "take what a whole resolve over the same inputs takes", Seq(row), Pending.contains(row.name)))
}

object LateInputEquivalenceSpec {
  import LateInputs.{Absence, Input, Kind, Scenario}
  private val normalizer = LateInputs.normalizer

  /**
   * The cases the incremental path does not cure yet, by name, each with its root cause — for the fix to take the mark
   * off. None: the last, UK "CBeebies Panto: Treasure Island" ×11 (a film new in TMDB, filled by venues.current by a fresh
   * stage, by no live one), was the model keeping its decisions without their trace, so the fill's `rule.*` signals read
   * none in prod; it keeps what a stage reads of it now ([[services.identity.DecisionTrace.kept]]).
   */
  val Pending: Map[String, String] = Map.empty

  private def silent: Map[VoterFamily, FamilyAnswers] =
    VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap
  private def holding(records: (VoterFamily, Map[String, SourceRecord])*): Map[VoterFamily, FamilyAnswers] =
    silent ++ records.map { case (family, held) => family -> new HeldFamilyAnswers(family, held) }
  private def of(kind: Kind): Input => Boolean = _.kind == kind
  /** `table` with only the searches `reach` lets through answering: a film its title search and IMDb's suggestions miss
   *  is reached only by what `reach` allows — an IMDb id's find, a person's filmography — its record answering all the same. */
  private def reaching(table: FilmTable)(reach: services.identity.CandidateQuery => Boolean): IdentityLookups = new ForwardingIdentityLookups(table) {
    override def candidates(query: services.identity.CandidateQuery): Answer[Seq[Hit]] = if (reach(query)) super.candidates(query) else Answer.Known(Nil)
  }
  private def unsearched(table: FilmTable): IdentityLookups = reaching(table)(_ => false)
  private def everyAbsence(s: Scenario): Seq[Scenario] = Absence.values.toSeq.map(s.withAbsence)

  // ── the families agreeing on a film TMDB's search misses: Klondike (AgreementStageSpec) ────────────────────────

  private val klondike = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), Some(100), Some(Seq("Maryna Er Gorbach")), None, None)
  private val klondikeRecord = SourceRecord(klondike, Map("imdb" -> "tt16315948"))
  private val klondikeFamilies = holding(VoterFamily.Imdb -> Map("tt16315948" -> klondikeRecord), VoterFamily.Wiki -> Map("Q1" -> klondikeRecord),
    VoterFamily.Filmweb -> Map("880000" -> SourceRecord(klondike)), VoterFamily.RottenTomatoes -> Map("klondike_2022" -> SourceRecord(klondike)))
  /** TMDB holds Klondike, but neither its title search nor IMDb's suggestions reach it: only the agreed IMDb id's find. */
  private val klondikeTable = new FilmTable(Seq(FilmTable.F(913760, "Klondike", 2022, "Maryna Er Gorbach", 100)), normalizer)
  private val klondikeTmdb  = unsearched(klondikeTable)
  private val klondikeListings = Seq(KinoMuza, Multikino).map(venue => FilmTable.listing(venue, "Klondike", year = Some(2022)))
  private def klondikeCase(name: String, kind: Kind) = Scenario(name, kind, klondikeListings, klondikeTmdb, klondikeFamilies,
    finds = imdb => Answer.Known(Option.when(imdb == "tt16315948")(913760)), late = of(kind))

  val familyAnswers: Seq[Scenario] = everyAbsence(klondikeCase("Klondike's families", Kind.FamilyAnswer))
  val finds: Seq[Scenario]         = everyAbsence(klondikeCase("Klondike's find", Kind.TmdbFind))
  /** The broadcast's TMDB record: the day join reads every candidate's record, none while one is not known. */
  val records: Seq[Scenario] = everyAbsence(samson("Samson i Dalila's broadcast record", Kind.Record, { case Input.Record(id) => id == 1703624; case _ => false }))
  /** A person's search: the director's filmography reaches Klondike where its title does not. */
  val people: Seq[Scenario] = everyAbsence(Scenario("Klondike's director", Kind.People,
    Seq(FilmTable.listing(KinoMuza, "Klondike", year = Some(2022), director = Some("Maryna Er Gorbach"))),
    reaching(klondikeTable)(_.isInstanceOf[services.identity.CandidateQuery.Director]), silent, late = of(Kind.People)))

  // ── two Lalkas: posters, venue pages, pins, searches ──────────────────────────────────────────────────────────

  private val venuePoster = "https://kino.example/lalka.jpg"
  private val shown       = PosterHash(0x5a5a5a5a5a5aL)
  private def near(bits: Int) = PosterHash(shown.bits ^ ((1L << bits) - 1))
  private val lalkas = new FilmTable(Seq(FilmTable.F(1001, "Lalka", 2026, "Maciej Kawalski", 120), FilmTable.F(1002, "Lalka", 1968, "Wojciech Has", 159)), normalizer)
  private val bareLalka = FilmTable.listing(Multikino, "Lalka").copy(poster = Some(venuePoster))
  private val lalkaPosters = new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(PosterHash(0L), near(3)), 1002 -> Seq(PosterHash(-1L))))

  /** The venue's poster votes for the 2026 Lalka (AgreementPosterSpec); and vetoes the 1968 one IMDb, Wikidata and RT agree on. */
  val posters: Seq[Scenario] = everyAbsence(Scenario("Lalka's poster vote", Kind.Poster, Seq(bareLalka), lalkas, silent, posters = lalkaPosters,
    late = of(Kind.Poster)))

  /** The venue's detail page states the year and director: the 1968 Lalka. */
  val pages: Seq[Scenario] = everyAbsence {
    val paged = bareLalka.copy(page = Some("https://kino.example/lalka"), poster = None)
    val tmdb  = new ForwardingIdentityLookups(lalkas) {
      override def hasDetail(listing: Listing): Boolean = listing.page.isDefined
      override def detail(listing: Listing): Answer[Option[DetailFacts]] =
        Answer.Known(listing.page.map(_ => DetailFacts(Some(1968), Seq("Wojciech Has"), Some(159), None)))
    }
    Scenario("Lalka's venue page", Kind.VenuePage, Seq(paged), tmdb, silent, late = of(Kind.VenuePage))
  }

  /** A curator's pin: the bare Lalka is Has's. */
  val pins: Seq[Scenario] = Seq(Scenario("Lalka pinned", Kind.Pin, Seq(bareLalka), lalkas, silent,
    pins = Seq(Pin(Seq(bareLalka.key), PinClaim.IsFilm(1002), "curator", "Has's", java.time.Instant.parse("2026-10-25T00:00:00Z")))))

  /** UK "CBeebies Panto: Treasure Island" ×11: a film new in TMDB (1785679), billed widely; a fresh stage fills it by
   *  venues.current, prod's stage — its verdict decided before TMDB held the film — none. */
  private lazy val cbeebiesName = "CBeebies ×11 (Panto: Treasure Island)"
  val cbeebies: Seq[Scenario] = {
    val panto = FilmTable.F(1785679, "Cbeebies Panto: Treasure Island", 2026, "", 0, popularity = 1.0)
    val venues = Seq(KinoMuza, Multikino, KinoPalacowe, models.Rialto, models.Helios, models.KinoApollo, models.CinemaCityKinepolis,
      models.KinoAmok, models.KinoKijow, models.KinoPowisle, models.KinoTon)
    val listings = venues.map(FilmTable.listing(_, "CBeebies Panto: Treasure Island"))
    Seq(Scenario(cbeebiesName, Kind.Search, listings, new FilmTable(Seq(panto), normalizer), silent,
      late = { case Input.NewFilm(id) => id == panto.id; case _ => false }, absence = Absence.Missing))
  }

  /** A TMDB search answered late, and a film new in TMDB (US-shaped "CBeebies Panto", filled by venues.current). */
  val searches: Seq[Scenario] = everyAbsence(Scenario("Lalka's searches", Kind.Search,
    Seq(FilmTable.listing(Multikino, "Lalka", year = Some(2026), director = Some("Maciej Kawalski"))), lalkas, silent, late = of(Kind.Search))) ++
    cbeebies

  // ── the synopsis refusing an agreement: Binti (AgreementStageSpec) ─────────────────────────────────────────────

  val synopses: Seq[Scenario] = everyAbsence {
    val shamte = IdentityMeasures.Film("Binti", Some("Binti"), Nil, Some(2021), Some(98), Some(Seq("Seko Shamte")), None, None)
    val migom  = IdentityMeasures.Film("Binti", Some("Binti"), Nil, Some(2019), Some(90), Some(Seq("Frederike Migom")), None, None)
    val families = holding(VoterFamily.Imdb -> Map("tt11867140" -> SourceRecord(shamte, Map("imdb" -> "tt11867140"))),
      VoterFamily.Wiki -> Map("Q108940090" -> SourceRecord(shamte, Map("imdb" -> "tt11867140", "tmdb" -> "855156"))),
      VoterFamily.Filmweb -> Map("10016218" -> SourceRecord(shamte)))
    val page = "Binti, nastoletnia imigrantka nielegalnie mieszkająca w Belgii, nie pamięta Konga. Gdy poznaje Eliasa, młodego obrońcę praw zwierząt."
    val overviews = Map(
      855156 -> "Four Tanzanian women are unknowingly connected through their ability to persevere extreme hardships in the city of Dar-es-Salaam.",
      577285 -> "Binti is a 12-year-old girl of Congolese origin. Binti meets Elias and befriends her, so they can stay in Belgium.")
    val tmdb = new IdentityLookups {
      def hasDetail(listing: Listing): Boolean = true
      def detail(listing: Listing) = Answer.Known(Some(DetailFacts(None, Nil, None, None, synopsis = Some(page))))
      def candidates(query: services.identity.CandidateQuery) = query match {
        case services.identity.CandidateQuery.Title(title) if title.equalsIgnoreCase("Binti") =>
          Answer.Known(Seq(Hit(855156, "Binti", Some("Binti"), Some(2021), 1.5), Hit(577285, "Binti", Some("Binti"), Some(2019), 1.1)))
        case _ => Answer.Known(Nil)
      }
      def film(tmdbId: Int) = Answer.Known(Map(855156 -> shamte, 577285 -> migom).get(tmdbId))
      override def overview(tmdbId: Int) = Answer.Known(overviews.get(tmdbId).map(text => ProseWords.ofDescription(Seq(text))))
    }
    Scenario("Binti's TMDB descriptions", Kind.Synopsis, Seq(FilmTable.listing(KinoMuza, "Binti")), tmdb, families,
      finds = imdb => Answer.Known(Option.when(imdb == "tt11867140")(855156)), late = of(Kind.Synopsis))
  }

  // ── a broadcast's day: Samson i Dalila (AgreementBroadcastSpec) ────────────────────────────────────────────────

  private def samson(name: String, kind: Kind, late: Input => Boolean): Scenario = {
    val met  = FilmTable.F(1703624, "The Metropolitan Opera 2026/27: Samson et Dalila", 2026, "", 0, popularity = 1.0, released = Some(LocalDate.of(2026, 12, 5)))
    val film = FilmTable.F(29993, "Samson and Delilah", 1949, "Cecil B. DeMille", 131, alternatives = Seq("Samson i Dalila"), released = Some(LocalDate.of(1949, 12, 21)))
    Scenario(name, kind, Seq(FilmTable.listing(Multikino, "Samson i Dalila").copy(screenings = ScreeningDays.of(Seq(LocalDate.parse("2026-12-05"))))),
      new FilmTable(Seq(met, film), normalizer), silent, late = late)
  }
  /** The Met's air date: DeMille's 1949 film, the model's take, is corrected to the broadcast screening that day. */
  val broadcastDays: Seq[Scenario] = everyAbsence(samson("Samson i Dalila's broadcast day", Kind.BroadcastDay, of(Kind.BroadcastDay)))

  // ── a listing's catalogue id: Fantasy (AgreementCatalogueSpec) ────────────────────────────────────────────────

  val catalogueIds: Seq[Scenario] = everyAbsence {
    val fantasy = CatalogueId("webedia", "325279")
    val item    = SourceRecord(IdentityMeasures.Film("Fantasy", None, Nil, Some(2025), None, Some(Seq("Kukla Kesting")), None),
      Map("wikidata" -> "Q135441923", "tmdb" -> "1318829", "imdb" -> "tt36112899"))
    val hits    = Map(fantasy -> Seq(CatalogueHit(Some("Q135441923"), Some(1318829), Some("tt36112899"), "Wikidata P1265")))
    val catalogue = new CatalogueAnswers {
      def linked(page: String): Answer[Seq[CatalogueId]]     = Answer.Known(Nil)
      def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] = Answer.Known(hits.getOrElse(id, Nil))
    }
    Scenario("Fantasy's catalogue id", Kind.Catalogue,
      Seq(FilmTable.listing(Multikino, "Fantasy").copy(catalogueIds = Seq(fantasy))),
      // two Fantasies the model cannot tell apart; the listing's feed id, corroborated by the title's most popular hit, can
      new FilmTable(Seq(FilmTable.F(1318829, "Fantasy", 2025, "Kukla Kesterović", 99, popularity = 20.0), FilmTable.F(9999, "Fantasy", 1999, "Someone Else", 90, popularity = 5.0)), normalizer),
      holding(VoterFamily.Wiki -> Map("Q135441923" -> item)), finds = imdb => Answer.Known(Option.when(imdb == "tt36112899")(1318829)),
      // no fill selected: Wikidata's lone pick would fill the cluster before the catalogue take reads it
      catalogue = catalogue, rules = UnifiedRules.resolver.copy(fill = Nil), late = of(Kind.Catalogue))
  }

  // ── listings joining and leaving ──────────────────────────────────────────────────────────────────────────────

  /** A listing stating the year joins the bare Lalka's cluster, and leaves it again; a cluster sorting first leaves
   *  before a restart (the run 1530 shape: the key counter moved under a corrected take). */
  val listings: Seq[Scenario] = {
    val dated = FilmTable.listing(KinoMuza, "Lalka", year = Some(1968), director = Some("Wojciech Has"))
    val akira = FilmTable.listing(Multikino, "Akira")
    val correction = IncrementalWholeEquivalenceSpec.Recorded.correction
    Seq(
      Scenario("Lalka's dated listing joining", Kind.Listing, Seq(bareLalka, dated), lalkas, silent, posters = lalkaPosters, joining = Seq(dated)),
      Scenario("Lalka's dated listing leaving", Kind.Listing, Seq(bareLalka), lalkas, silent, posters = lalkaPosters, leaving = Seq(dated)),
      Scenario("the corrected Lalka, a cluster before it gone", Kind.Listing, correction.listings, correction.tmdb,
        correction.families.map { case (family, held) => family -> new HeldFamilyAnswers(family, held) },
        posters = new HeldPosters(correction.venuePosters.view.mapValues(Some(_)).toMap, correction.filmPosters), leaving = Seq(akira)))
  }

  val everyCase: Seq[Scenario] = familyAnswers ++ finds ++ records ++ searches ++ people ++ posters ++ catalogueIds ++ pages ++ synopses ++
    broadcastDays ++ pins ++ listings

  /**
   * The production drift rows by their shape (`scripts.IdentityLiveDrift` over the 2026-10-25 recording, 39 listings): each
   * named by its title and count, with the films and families the live check named.
   */
  object Drift {
    /** Families agree on a film TMDB's search misses; the IMDb id's find lands AFTER the verdict was decided without
     *  it — prod's stored verdict carried no TMDB link while its own store held the find and the record. Root cause
     *  on main: `AgreementStage.verdictOf` kept a verdict while its `reads` matched, and `reads` recorded only family
     *  answers, never finds or records. */
    private def agreedFindLate(name: String, title: String, year: Int, imdb: String, tmdb: Int, venues: Int, families: Seq[VoterFamily]): Seq[Scenario] = {
      val film    = IdentityMeasures.Film(title, None, Nil, Some(year), Some(100), None, None, None)
      val record  = SourceRecord(film, Map("imdb" -> imdb))
      val table   = unsearched(new FilmTable(Seq(FilmTable.F(tmdb, title, year, "", 100)), normalizer))
      val held    = holding(families.map(family => family -> Map(s"${family.label}-$imdb" -> record))*)
      val listing = Seq(KinoMuza, Multikino, KinoPalacowe, models.Rialto, models.Helios, models.KinoApollo, models.CinemaCityKinepolis,
        models.KinoAmok, models.KinoKijow, models.KinoPowisle, models.KinoTon)
      Seq(Absence.Attempted, Absence.Missing).map(absence => Scenario(name, Kind.TmdbFind, listing.take(venues).map(FilmTable.listing(_, title)),
        table, held, finds = found => Answer.Known(Option.when(found == imdb)(tmdb)), late = of(Kind.TmdbFind)).withAbsence(absence))
    }
    private val reviewers = Seq(VoterFamily.Filmweb, VoterFamily.Metacritic, VoterFamily.RottenTomatoes)

    val noTmdbLink: Seq[Scenario] =
      agreedFindLate("Dolly ×10", "Dolly", 2025, "tt32639175", 1309083, 10, reviewers :+ VoterFamily.Imdb) ++
      agreedFindLate("Rental ×8 (Rental Family)", "Rental Family", 2025, "tt14142060", 1208348, 8, reviewers ++ Seq(VoterFamily.Imdb, VoterFamily.Wiki)) ++
      agreedFindLate("Ice ×4 (Ice Cream Man)", "Ice Cream Man", 2026, "tt36893729", 1477712, 4, reviewers) ++
      agreedFindLate("Jump (Jump Scare)", "Jump Scare", 2025, "tt29650028", 1560615, 1, Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes, VoterFamily.Imdb)) ++
      agreedFindLate("Miss (Miss Mary)", "Miss Mary", 1986, "tt0091528", 241603, 1, Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes, VoterFamily.Imdb)) ++
      agreedFindLate("Karmele", "Karmele", 2025, "tt8690422", 1495245, 1, Seq(VoterFamily.Imdb, VoterFamily.RottenTomatoes, VoterFamily.Filmweb, VoterFamily.Wiki)) ++
      agreedFindLate("I.K.U", "IKU", 2001, "tt0255233", 37982, 1, Seq(VoterFamily.Wiki, VoterFamily.RottenTomatoes, VoterFamily.Imdb))

    val cbeebies: Seq[Scenario] = LateInputEquivalenceSpec.cbeebies

    /** PL "Festiwal Pięć Smaków: Ostrze" ×2: prod kept a take of TMDB 42845 a fresh stage refuses — evidence against the
     *  take that arrived after it was decided: here the venue's poster, hashed late, matching the other Ostrze, which
     *  IMDb and Wikidata take. */
    val ostrze: Seq[Scenario] = {
      val other   = IdentityMeasures.Film("Ostrze", None, Nil, Some(2024), Some(95), Some(Seq("Jan Kowalski")), None, None)
      val table   = new FilmTable(Seq(FilmTable.F(42845, "Ostrze", 1995, "Tsui Hark", 105), FilmTable.F(42846, "Ostrze", 2024, "Jan Kowalski", 95)), normalizer)
      val url     = "https://ec1lodz.pl/ostrze.jpg"
      val record  = SourceRecord(other, Map("tmdb" -> "42846"))
      val listing = Seq(FilmTable.listing(KinoPalacowe, "Festiwal Pięć Smaków: Ostrze").copy(poster = Some(url)),
        FilmTable.listing(Multikino, "Festiwal Pięć Smaków: Ostrze").copy(poster = Some(url)))
      Seq(Absence.Unfiled, Absence.Attempted).map(absence => Scenario("Ostrze ×2 (Festiwal Pięć Smaków)", Kind.Poster, listing, table,
        holding(VoterFamily.Imdb -> Map("tt1" -> record), VoterFamily.Wiki -> Map("Q1" -> record)),
        posters = new HeldPosters(Map(url -> Some(shown)), Map(42845 -> Seq(PosterHash(-1L)), 42846 -> Seq(near(3)))),
        late = of(Kind.Poster)).withAbsence(absence))
    }

    val all: Seq[Scenario] = noTmdbLink ++ cbeebies ++ ostrze
  }
}
