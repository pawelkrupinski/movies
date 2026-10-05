package services.identity.agreement

import models.{Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CandidateQuery, CatalogueAnswers, CatalogueHit, CatalogueId, CatalogueQuestion, DetailFacts, FilmTable, Hit, IdentityCalibration,
  IdentityLookups, IdentityMeasures, Listing, Resolution, ResolverDecision}
import services.movies.SingleCountryNormalizer

/** A listing's own catalogue id — the venue's exact naming of its film in another film database — on the agreement's way
 *  to the projection: a cluster nothing else took takes the film the id maps to, unless the listing's own year or director
 *  contradicts it or its ids name two films; a mapping not asked yet holds the cluster as the model left it and is
 *  asked for. */
class AgreementCatalogueSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val table      = new FilmTable(Nil, normalizer)

  private val fantasy = CatalogueId("webedia", "325279")
  /** Wikidata's record of the 2025 "Fantasy", by its item. */
  private val fantasyItem = SourceRecord(IdentityMeasures.Film("Fantasy", None, Nil, Some(2025), None, Some(Seq("Kukla Kesting")), None),
    Map("wikidata" -> "Q135441923", "tmdb" -> "1318829", "imdb" -> "tt36112899"))
  private val fantasyHit = CatalogueHit(Some("Q135441923"), Some(1318829), Some("tt36112899"), "Wikidata P1265")

  private def listing(year: Option[Int] = Some(2025), director: Option[String] = None, ids: Seq[CatalogueId] = Seq(fantasy),
                      page: Option[String] = None): Listing =
    FilmTable.listing(Multikino, "Fantasy", year, director).copy(catalogueIds = ids, page = page)

  private final class HeldCatalogue(pages: Map[String, Seq[CatalogueId]], hits: Map[CatalogueId, Seq[CatalogueHit]]) extends CatalogueAnswers {
    def linked(page: String): Answer[Seq[CatalogueId]]    = pages.get(page).fold[Answer[Seq[CatalogueId]]](Answer.Unknown)(Answer.Known(_))
    def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] = hits.get(id).fold[Answer[Seq[CatalogueHit]]](Answer.Unknown)(Answer.Known(_))
  }

  private def families(wiki: Map[String, SourceRecord] = Map("Q135441923" -> fantasyItem), imdb: Map[String, SourceRecord] = Map.empty) =
    VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, family match {
      case VoterFamily.Wiki => wiki
      case VoterFamily.Imdb => imdb
      case _                => Map.empty
    })).toMap[VoterFamily, FamilyAnswers]

  private def resolutionOf(listing: Listing) = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.4, ResolverDecision.Basis.BelowThreshold, Nil)()),
    1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  /** TMDB's find of the IMDb ids these specs name. */
  private val finds: String => Answer[Option[Int]] =
    Map("tt36112899" -> 1318829, "tt0015175" -> 31506, "tt37150957" -> 1445025).get.andThen(Answer.Known(_))

  /** The venue pages' facts, by page. */
  private final class Paged(pages: Map[String, DetailFacts]) extends IdentityLookups {
    def hasDetail(l: Listing): Boolean                           = l.page.exists(pages.contains)
    def detail(l: Listing): Answer[Option[DetailFacts]]          = Answer.Known(l.page.flatMap(pages.get))
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = Answer.Known(Nil)
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
  }

  private def stage(catalogue: CatalogueAnswers, fams: Map[VoterFamily, FamilyAnswers] = families(), tmdbOf: String => Answer[Option[Int]] = finds,
                    ask: AgreementStage.Open => Unit = _ => (), venues: IdentityLookups = table) =
    new AgreementStage(fams, venues, normalizer, IdentityCalibration.resolver, tmdbOf = tmdbOf, new InMemoryAgreementVerdicts,
      ask = ask, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(table), catalogue = catalogue)

  private def decided(stage: AgreementStage, l: Listing) = stage.apply(resolutionOf(l), Map(l.key -> l).get, version = 1).decisions.head

  "an unmatched cluster's catalogue id" should "take the TMDB film it maps to, saying which id and which property" in {
    val l = listing()
    val d = decided(stage(new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit)))), l)
    (d.film, d.basis) shouldBe ((Some(1318829), ResolverDecision.Basis.Catalogue))
    d.explanation.last shouldBe "catalogue id webedia:325279 → TMDB 1318829 via Wikidata P1265 (Q135441923) 'Fantasy' (2025)"
  }

  it should "take none when the listing's own year or director contradicts the record it maps to" in {
    decided(stage(new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit)))), listing(year = Some(2019))).film shouldBe None
    decided(stage(new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit)))), listing(director = Some("Wes Anderson"))).film shouldBe None
  }

  it should "take none when its ids name two films, or the id maps to no film item" in {
    val other = CatalogueId("webedia", "1")
    val two   = new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit), other -> Seq(CatalogueHit(Some("Q1"), Some(7), None, "Wikidata P1265"))))
    decided(stage(two, families(wiki = Map("Q135441923" -> fantasyItem, "Q1" -> fantasyItem))), listing(ids = Seq(fantasy, other))).film shouldBe None
    decided(stage(new HeldCatalogue(Map.empty, Map(fantasy -> Nil))), listing()).film shouldBe None
    decided(stage(new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit))), families(wiki = Map.empty)), listing()).film shouldBe None
  }

  it should "take the film TMDB's find names for the item's IMDb id, not a TMDB id Wikidata keeps for a record since deleted" in {
    // DE "Dann passiert das Leben": Wikidata's P4947 is 1517080, which TMDB no longer has; TMDB finds tt37150957 as 1445025
    val id   = CatalogueId("webedia", "1000007718")
    val item = SourceRecord(IdentityMeasures.Film("Dann passiert das Leben", None, Nil, Some(2025), None, Some(Seq("Neele Vollmar")), None),
      Map("wikidata" -> "Q135779106"))
    val l = FilmTable.listing(Multikino, "Dann passiert das Leben", Some(2025)).copy(catalogueIds = Seq(id))
    decided(stage(new HeldCatalogue(Map.empty, Map(id -> Seq(CatalogueHit(Some("Q135779106"), Some(1517080), Some("tt37150957"), "Wikidata P8531")))),
      families(wiki = Map("Q135779106" -> item))), l).film shouldBe Some(1445025)
  }

  it should "take none when the venue's own page contradicts the record its link names, though the listing states nothing" in {
    // PL Kinoteka "Czarne zombie": its page credits Bedward's 2026 "Black Zombie" and links Corman's 1963 "X" on IMDb
    val page = "https://kinoteka.pl/film/czarne-zombie-splatfilmfest/"
    val tt   = CatalogueId("imdb", "tt0057693")
    val x    = SourceRecord(IdentityMeasures.Film("X: The Man with the X-Ray Eyes", None, Nil, Some(1963), None, Some(Seq("Roger Corman")), None))
    val l    = FilmTable.listing(Kinoteka, "Czarne zombie | Splat!FilmFest").copy(page = Some(page))
    val held = new HeldCatalogue(Map(page -> Seq(tt)), Map.empty)
    val fams = families(imdb = Map("tt0057693" -> x))
    val stated = new Paged(Map(page -> DetailFacts(Some(2026), Seq("Maya Annik Bedward"), Some(90), None)))
    decided(stage(held, fams, tmdbOf = _ => Answer.Known(Some(32569)), venues = stated), l).film shouldBe None
    decided(stage(held, fams, tmdbOf = _ => Answer.Known(Some(32569))), l).film shouldBe Some(32569)   // what a page stating nothing leaves
  }

  it should "take none for a listing billing several works, but name a film whatever a stage work its title spells" in {
    val held = new HeldCatalogue(Map.empty, Map(fantasy -> Seq(fantasyHit)))
    decided(stage(held), listing().copy(title = "Fantasy + Wicked", rawTitle = "Fantasy + Wicked", cleanTitle = "Fantasy + Wicked")).film shouldBe None
    // DE "Die Nibelungen - Teil 1: Siegfried": Lang's 1924 film by its Filmstarts id, though a search reads Wagner's opera
    val siegfried = CatalogueId("webedia", "49986")
    val lang = SourceRecord(IdentityMeasures.Film("Die Nibelungen: Siegfried", None, Nil, Some(1924), None, Some(Seq("Fritz Lang")), None),
      Map("wikidata" -> "Q18341272"))
    val l = FilmTable.listing(Multikino, "Die Nibelungen - Teil 1: Siegfried", Some(1924), Some("Fritz Lang")).copy(catalogueIds = Seq(siegfried))
    decided(stage(new HeldCatalogue(Map.empty, Map(siegfried -> Seq(CatalogueHit(Some("Q18341272"), Some(31506), Some("tt0015175"), "Wikidata P1265")))),
      families(wiki = Map("Q18341272" -> lang))), l).film shouldBe Some(31506)
  }

  it should "hold the cluster while its id is not mapped yet, and hand the id to the queue" in {
    val handed = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    val s      = stage(new HeldCatalogue(Map.empty, Map.empty), ask = handed += _)
    decided(s, listing()).film shouldBe None
    s.wantedCatalogue shouldBe Set(CatalogueQuestion.Id(fantasy))
    handed.flatMap(_.catalogue).toSet shouldBe Set(CatalogueQuestion.Id(fantasy))
  }

  "a catalogue id the venue's page links" should "map through the page's own read, an IMDb id through TMDB's find of it" in {
    val page = "https://kinoteka.pl/film/the-taxidermist/"
    val tt   = CatalogueId("imdb", "tt40381362")
    val l    = FilmTable.listing(Kinoteka, "The Taxidermist | Splat!FilmFest").copy(page = Some(page))
    val taxidermist = SourceRecord(IdentityMeasures.Film("The Taxidermist", None, Nil, Some(2025), None, Some(Seq("Paulo Nascimento")), None),
      Map("imdb" -> "tt40381362"))
    val waiting = stage(new HeldCatalogue(Map.empty, Map.empty))
    decided(waiting, l).film shouldBe None
    waiting.wantedCatalogue shouldBe Set(CatalogueQuestion.Page(page))
    val d = decided(stage(new HeldCatalogue(Map(page -> Seq(tt)), Map.empty), families(imdb = Map("tt40381362" -> taxidermist)),
      tmdbOf = { case "tt40381362" => Answer.Known(Some(1686904)); case _ => Answer.Known(None) }), l)
    (d.film, d.basis) shouldBe ((Some(1686904), ResolverDecision.Basis.Catalogue))
    d.explanation.last shouldBe "catalogue id imdb:tt40381362 (linked from the venue's page) → TMDB 1686904 via TMDB's find 'The Taxidermist' (2025)"
  }

  "a catalogue film TMDB holds no record of" should "stand on its Wikidata item" in {
    val mein = CatalogueId("webedia", "1000032825")
    val item = SourceRecord(IdentityMeasures.Film("Mein neues altes Ich", None, Nil, Some(2025), None, None, None), Map("wikidata" -> "Q138644118"))
    val l    = FilmTable.listing(Multikino, "Mein neues altes Ich", Some(2025)).copy(catalogueIds = Seq(mein))
    val d    = decided(stage(new HeldCatalogue(Map.empty, Map(mein -> Seq(CatalogueHit(Some("Q138644118"), None, None, "Wikidata P8531")))),
      families(wiki = Map("Q138644118" -> item))), l)
    (d.film, d.basis, d.fallback.map(f => (f.source, f.id))) shouldBe ((None, ResolverDecision.Basis.Catalogue, Some(("wikidata", "Q138644118"))))
  }
}
