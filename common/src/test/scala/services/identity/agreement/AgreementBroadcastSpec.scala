package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CandidateQuery, DetailFacts, FilmTable, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures, Listing, Resolution,
  ResolverDecision, ScreeningDays}
import services.movies.SingleCountryNormalizer

import java.time.LocalDate

/** The broadcast date join on the agreement's way to the projection: a cluster billing a stage work that screens on the
 *  day one record of it was broadcast takes that record; an encore, only when its title bills the house or season. */
class AgreementBroadcastSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val metSamson = FilmTable.F(1703624, "The Metropolitan Opera 2026/27: Samson et Dalila", 2026, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2026, 12, 5)))
  private val metMacbeth = FilmTable.F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2026, 10, 17)))
  private val metFanciulla = FilmTable.F(1703629, "The Metropolitan Opera 2026/27: La Fanciulla del West", 2027, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2027, 1, 23)))
  private val rboSamson = FilmTable.F(1800001, "Royal Ballet & Opera 2026/27: Samson et Dalila", 2027, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2027, 3, 2)))
  private val film1949 = FilmTable.F(29993, "Samson and Delilah", 1949, "Cecil B. DeMille", 131, alternatives = Seq("Samson i Dalila"),
    released = Some(LocalDate.of(1949, 12, 21)))
  private val table = new FilmTable(Seq(metSamson, metMacbeth, metFanciulla, rboSamson, film1949), normalizer)

  private def silentFamilies = VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap[VoterFamily, FamilyAnswers]
  private def screening(title: String, days: String*): Listing =
    FilmTable.listing(Multikino, title).copy(screenings = ScreeningDays.of(days.map(LocalDate.parse)))

  private def decided(listing: Listing): ResolverDecision = {
    val resolution = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()),
      1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    new AgreementStage(silentFamilies, table, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      clock = _root_.tools.SpecClock.Pinned, tmdb = Some(table)).apply(resolution, Map(listing.key -> listing).get, version = 1).decisions.head
  }

  "a stage work screening on a production's broadcast day" should "take that production, whatever its title leaves out" in {
    // PL Kino Amok's "Samson i Dalila" on 5 December 2026: the Met's live broadcast, not DeMille's 1949 film nor RBO's staging
    val taken = decided(screening("Samson i Dalila", "2026-12-05"))
    (taken.film, taken.basis) shouldBe ((Some(metSamson.id), ResolverDecision.Basis.Broadcast))
    taken.explanation.last should include ("2026-12-05")
    // Kino Powiśle bills it run into the house's word
    decided(screening("Opera-samson i dalila", "2026-12-05")).film shouldBe Some(metSamson.id)
  }

  it should "take none on another day, unless the title bills the production's house or season" in {
    // a bare title days after the broadcast says nothing of which staging it is
    decided(screening("Samson i Dalila", "2026-12-26")).film shouldBe None
    // PL Kino Kijów's retransmission three weeks on, its title naming the season
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2026-12-26")).film shouldBe Some(metSamson.id)
    // …but none long after the broadcast, nor before it
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2027-06-26")).film shouldBe None
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2026-12-01")).film shouldBe None
    // UK Flicks venues' encores three days on, the house run into one camel-cased word
    decided(screening("MetOpera: La Fanciulla del West", "2027-01-26")).film shouldBe Some(metFanciulla.id)
    decided(screening("MetOpera: Samson et Dalila", "2026-12-08")).film shouldBe Some(metSamson.id)
  }

  it should "take none for a title billing another house, though the work and day are the record's" in {
    // US Regal's "Opéra National de Paris: La fanciulla del West" on the Met's broadcast day shares only "opera" with it
    decided(screening("Opéra National de Paris: La fanciulla del West", "2027-01-23")).film shouldBe None
    decided(screening("The Metropolitan Opera: La Fanciulla del West", "2027-01-23")).film shouldBe Some(metFanciulla.id)
  }

  /** A film database holding the Met's productions under the house's own title, found by the work's name. */
  private final class Productions(val family: VoterFamily, records: Map[String, SourceRecord]) extends FamilyAnswers {
    def titled(text: String): Answer[Seq[SourceHit]] = Answer.Known(records.toSeq.sortBy(_._1).collect {
      case (id, record) if IdentityMeasures.key(record.film.title).endsWith(IdentityMeasures.key(text)) =>
        SourceHit(id, record.film.title, None, record.film.year) })
    def directedBy(name: String): Answer[Seq[SourceHit]] = Answer.Known(Nil)
    def record(id: String): Answer[Option[SourceRecord]] = Answer.Known(records.get(id))
  }
  private val rtProductions = Map(
    "the_metropolitan_opera_la_fanciulla_del_west" -> SourceRecord(IdentityMeasures.Film("The Metropolitan Opera: La Fanciulla del West",
      runtime = Some(195), directors = Some(Seq("Richard Jones")))),
    "the_metropolitan_opera_macbeth" -> SourceRecord(IdentityMeasures.Film("The Metropolitan Opera: Macbeth", runtime = Some(209),
      directors = Some(Seq("Louisa Proske")))),
    "royal_opera_macbeth" -> SourceRecord(IdentityMeasures.Film("Royal Opera House: Macbeth", year = Some(2018), directors = Some(Seq("Phyllida Lloyd")))))
  private def credited(listing: Listing, records: Map[String, SourceRecord] = rtProductions): ResolverDecision = {
    val families = silentFamilies + (VoterFamily.RottenTomatoes -> new Productions(VoterFamily.RottenTomatoes, records))
    val resolution = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()),
      1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    new AgreementStage(families, table, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      clock = _root_.tools.SpecClock.Pinned, tmdb = Some(table)).apply(resolution, Map(listing.key -> listing).get, version = 1).decisions.head
  }
  private def rbo(work: String, director: Option[String], days: String*): Listing =
    FilmTable.listing(Multikino, s"RBO Cinema Season 2026-27: $work", director = director).copy(screenings = ScreeningDays.of(days.map(LocalDate.parse)))

  "a title billing another house" should "take the production a film database credits with the very director the venue credits" in {
    // UK Flicks' "RBO Cinema Season 2026-27" relays the Met's 2026/27 productions (labels.tsv, the review page 10-06): the
    // venues credit Richard Jones and Louisa Proske, who staged the Met's — the banner names a distributor, not the house
    val fanciulla = credited(rbo("La Fanciulla Del West", Some("Richard Jones"), "2027-01-26"))
    (fanciulla.film, fanciulla.basis) shouldBe ((Some(metFanciulla.id), ResolverDecision.Basis.Broadcast))
    credited(rbo("Macbeth", Some("Louisa Proske"), "2026-10-20")).film shouldBe Some(metMacbeth.id)
  }

  it should "take none where the venue credits nobody, or another director, or only another house's production credits its director" in {
    // the banner alone still names another house (US Regal's "Opéra National de Paris: La fanciulla del West" credits nobody)
    credited(rbo("La Fanciulla Del West", None, "2027-01-26")).film shouldBe None
    credited(rbo("Macbeth", Some("Phyllida Lloyd"), "2026-10-20")).film shouldBe None
    // the credit must be the RECORD's house's production: a Royal Opera Macbeth crediting the venue's director is no Met's
    credited(rbo("Macbeth", Some("Louisa Proske"), "2026-10-20"),
      Map("royal_opera_macbeth" -> SourceRecord(IdentityMeasures.Film("Royal Opera House: Macbeth", directors = Some(Seq("Louisa Proske")))))).film shouldBe None
    credited(FilmTable.listing(Multikino, "Opéra National de Paris: La fanciulla del West")
      .copy(screenings = ScreeningDays.of(Seq(LocalDate.of(2027, 1, 23))))).film shouldBe None
  }

  it should "take none against a fact the listing states, nor for a title billing no stage work" in {
    val dated = FilmTable.listing(KinoMuza, "Samson i Dalila", year = Some(1949)).copy(screenings = ScreeningDays.of(Seq(LocalDate.of(2026, 12, 5))))
    decided(dated).basis should not be ResolverDecision.Basis.Broadcast
    decided(screening("Lalka", "2026-12-05")).film shouldBe None
  }

  /** `table` as a store holding `undated`'s records as they were filed before records kept the whole day: their year
   *  alone, their day `Unknown` until [[reread]]. */
  private final class YearOnlyRecords(table: FilmTable, undated: Set[Int]) extends IdentityLookups {
    private val stale = scala.collection.mutable.Set.from(undated)
    def reread(id: Int): Unit = { stale -= id; () }
    def hasDetail(listing: Listing): Boolean                  = table.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]] = table.detail(listing)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]   = table.candidates(query)
    def film(id: Int): Answer[Option[IdentityMeasures.Film]]  =
      if (stale(id)) table.film(id) match { case Answer.Known(f) => Answer.Known(f.map(_.copy(released = None))); case other => other } else table.film(id)
    override def releaseDay(id: Int): Answer[Option[LocalDate]] = if (stale(id)) Answer.Unknown else super.releaseDay(id)
  }

  "a stored record of the billed work holding only its year" should "be read again before the day decides, and then taken" in {
    // prod 2026-10-05: 88% of the season relays' stored records predate the whole day (PL "Samson i Dalila" never fired)
    val listing = screening("Samson i Dalila", "2026-12-05")
    val stored  = new YearOnlyRecords(table, Set(metSamson.id, metMacbeth.id))
    val asked   = scala.collection.mutable.Buffer.empty[AgreementStage.Open]
    val stage   = new AgreementStage(silentFamilies, stored, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = asked += _, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(stored))
    val resolution = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()),
      1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    // the Met's record states no day: no take yet — it is asked again, and only it (Macbeth bills another work)
    stage.apply(resolution, Map(listing.key -> listing).get, version = 1).decisions.head.film shouldBe None
    stage.wantedRecords shouldBe Set(metSamson.id)
    asked.flatMap(_.records) shouldBe Seq(metSamson.id)
    // read again, it states its day, and the screening day takes it
    stored.reread(metSamson.id)
    val taken = stage.apply(resolution, Map(listing.key -> listing).get, version = 2).decisions.head
    (taken.film, taken.basis) shouldBe ((Some(metSamson.id), ResolverDecision.Basis.Broadcast))
    stage.wantedRecords shouldBe empty
  }

  "a stage relay whose days could not be read" should "wait, asking nothing, and be taken once a read gives its days" in {
    val unread  = FilmTable.listing(Multikino, "Samson i Dalila").copy(screenings = ScreeningDays.Unknown)
    val asked   = scala.collection.mutable.Buffer.empty[AgreementStage.Open]
    val stage   = new AgreementStage(silentFamilies, table, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = asked += _, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(table))
    def decided(listing: Listing, version: Long) = stage.apply(Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1,
      ResolverDecision.Basis.BelowThreshold, Nil)()), 1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty),
      Map(listing.key -> listing).get, version).decisions.head
    decided(unread, 1).film shouldBe None
    (stage.wantedRecords, asked.flatMap(_.records)) shouldBe ((Set.empty, Nil))
    decided(unread.copy(screenings = ScreeningDays.of(Seq(LocalDate.of(2026, 12, 5)))), 2).film shouldBe Some(metSamson.id)
    // unknown days are no days and no season, but never "screens on none"
    (ScreeningDays.Unknown.isEmpty, ScreeningDays.Unknown.days, unread.broadcastSeason) shouldBe ((false, Nil, None))
    (ScreeningDays.Unknown ++ ScreeningDays.of(Seq(LocalDate.of(2026, 12, 5)))).isUnknown shouldBe true
  }

  "a stored record of the billed work holding only its year" should "be read again once only: a record TMDB still dates by its year is then taken as dating none" in {
    val listing = screening("Samson i Dalila", "2026-12-05")
    val stored  = new YearOnlyRecords(table, Set(metSamson.id))   // never gains its day
    val filed   = scala.collection.mutable.Set.empty[String]
    val asked   = scala.collection.mutable.Buffer.empty[AgreementStage.Open]
    val stage   = new AgreementStage(silentFamilies, stored, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = asked += _, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(stored),
      changes = _ => Some(filed.toSet))
    val resolution = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()),
      1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    def apply(version: Long) = stage.apply(resolution, Map(listing.key -> listing).get, version).decisions.head
    apply(1).film shouldBe None
    // other answers filed meanwhile: still waiting on the read, and not asked twice
    filed += "imdb|title|Samson i Dalila"
    apply(2).film shouldBe None
    stage.wantedRecords shouldBe Set(metSamson.id)
    // the read filed, the record still year-only: waited on no more, and never asked again
    filed += AgreementStage.recordReadId(metSamson.id)
    apply(3).film shouldBe None
    apply(4).film shouldBe None
    stage.wantedRecords shouldBe empty
    asked.flatMap(_.records) shouldBe Seq(metSamson.id)
  }
}
