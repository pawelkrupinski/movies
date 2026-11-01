package services.identity

import models.{Cinema, Helios, KinoApollo, KinoBulgarska, KinoMikro, KinoMuza, KinoOaza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.GroupingCoherence.{Coherence, Kind}
import services.movies.{SingleCountryNormalizer, TitleNormalizer}

/**
 * The calibration of [[GroupingCoherence]] over the known groupings (docs/investigations/2026-11-01-grouping-coherence.md).
 *
 * KNOWN BAD: each a grouping the resolver made before its fix, rebuilt as it stood -- its listings as the investigations
 * and the specs of its fix hold them, grouped under the film it took. The grouping is forced with a curator's pin over
 * its members (`PinClaim.IsFilm`: the members of one cluster, its film), and read as the resolver's own (no pin), so the
 * members' traces are what the current resolver's lone-node scoring says of each against the historical film.
 *
 * KNOWN GOOD: groupings the hand judgements call right (identity-judgements.tsv), on the shapes the bad ones bill: the
 * whole resolve's own clusters, read as the corpus run reads them.
 */
class GroupingCoherenceCalibrationSpec extends AnyFlatSpec with Matchers {
  import FilmTable.{F, listing}

  private val Year = 2026

  /** The grouping `cluster` under `film`, the rest of `listings` present beside it: resolved with the cluster pinned to
   *  the film, then read as an own match. */
  private def historical(listings: Seq[Listing], cluster: Seq[Listing], film: Int, films: Seq[F], normalizer: TitleNormalizer): Coherence = {
    val pin    = Pin(cluster.map(_.key), PinClaim.IsFilm(film), "calibration", "the historical grouping", java.time.Instant.EPOCH)
    val inputs = ResolveInputs(PinConstraints(Seq(pin)), IdentityCalibration.resolver, TitleDecorations.None)
    val r      = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, inputs)
    val keys   = cluster.map(_.key).toSet
    val traced = r.decisions.filter(_.members.exists(keys)).flatMap(_.trace.nodes).filter { case (k, _) => keys(k) }.toMap
    val record = r.films.getOrElse(film, fail(s"no record of $film"))
    val d      = ResolverDecision(cluster.map(_.key).sorted, Some(film), 0.9, ResolverDecision.Basis.OwnMatch, Nil)(DecisionTrace(None, None, traced))
    GroupingCoherence.of(d, s"tmdb:$film", record, cluster, Year)
  }

  /** Every cluster of a whole resolve of `listings`, as the corpus run reads them. */
  private def resolved(listings: Seq[Listing], films: Seq[F], normalizer: TitleNormalizer): Seq[Coherence] = {
    val r = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
    val byKey = listings.map(l => l.key -> l).toMap
    val takes = SuspectTakes.takes(r.decisions, byKey.get, r.films.get)
    GroupingCoherence.corpus(takes, Year)
  }

  private def told(c: Coherence) = s"score ${c.score}: ${c.findings.map(_.render).mkString("; ")}"

  // ── known bad ─────────────────────────────────────────────────────────────────────────────────────────────────

  // UK "Prima Facie", drift uk seed 17 (docs/investigations/2026-11-01-relay-apart-from-film.md, fixed by e9337f0beec):
  // Chichester's bare billing, whose own text credits Susanna White's 2026 film, grouped by its search form with Power
  // Station's "NT Live: Prima Facie" and the bare Kendal and Bo'ness billings, under the National Theatre's 2022 recording
  private val uk         = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
  private val primaFilms = Seq(F(963568, "National Theatre Live: Prima Facie", 2022, "Justin Martin", 108, 4.0),
    F(1127282, "Prima Facie", 2026, "Susanna White", 106, 9.0), F(1089557, "Prima Facie", 2022, "Someone Else", 95, 1.0))
  private val synopsis   = "A young, ambitious trial lawyer (Cynthia Erivo) brilliantly navigates the ruthless London court system. " +
    "Unlike Suzie Miller's one-woman West End play of the same name starring Jodie Comer, this is an ensemble film, with " +
    "an ensemble cast which includes Simon Russell Beale. UK 2026 Susanna White 106m"
  private val chichester = listing(KinoMuza, "Prima Facie").copy(names = VenueNames.of(Seq(synopsis)))
  private val ntLive     = listing(Multikino, "NT Live: Prima Facie", Some(2022), Some("Justin Martin"), Some(108))
  private val primaBare  = Seq(listing(KinoPalacowe, "Prima Facie"), listing(Helios, "Prima Facie (15)", runtime = Some(109)))

  "Prima Facie's grouping (uk)" should "be flagged: Chichester's own evidence takes the 2026 film, or denies the recording" in {
    val all = chichester +: ntLive +: primaBare
    val c   = historical(all, all, 963568, primaFilms, uk)
    withClue(told(c)) {
      c.flagged shouldBe true
      c.findings.filter(_.kind.signal == GroupingCoherence.Signal.Alone).flatMap(_.members) should contain(chichester.key)
    }
  }

  // DE "Eternity", prod-latest 41265286306 (docs/investigations/2026-11-01-kept-overruled-apart.md, fixed by 871125cf71d):
  // the bare "Eternity" [2025] Cineamo links to Freyne's 2025 film (its year copied) at the venues screening Meures'
  // 2026 film, grouped with Caligari's KEPT kinoheld listing of Freyne's film, under Freyne's film
  private val de            = SingleCountryNormalizer.titleNormalizer
  private val eternityFilms = Seq(F(1259102, "Eternity", 2025, "David Freyne", 114, 20.0), F(1769681, "Eternity", 2026, "Susanne Regina Meures", 90, 0.4))
  private def fed(l: Listing, source: String, id: String) = l.copy(catalogueIds = Seq(CatalogueId(source, id)))
  private val cineamo  = Seq(KinoMikro, KinoOaza, KinoPalacowe, Kinoteka).map(v => fed(listing(v, "Eternity", Some(2025)), "cineamo", "1259102m"))
  private val meures   = Seq(
    listing(KinoApollo, "Eternity", Some(2026), Some("Susanne Regina Meures"), Some(90)).copy(page = Some("https://www.kinoheld.de/film/eternity-2026")),
    fed(listing(KinoMuza, "Eternity", Some(2026), Some("Susanne Regina Meures"), Some(90)), "webedia", "1000052001"))
  private val freyne   = ListingRetention.kept(
    listing(KinoBulgarska, "Eternity", Some(2025), Some("David Freyne"), Some(114)).copy(page = Some("https://www.kinoheld.de/film/eternity")))

  "Eternity's grouping (de)" should "be flagged when it holds a listing of Meures' film: its facts and its own take both say so" in {
    // the shape the owner named: the kept Freyne listing grouped with a listing crediting Meures
    val cluster = cineamo :+ freyne :+ meures.head
    val c       = historical(cineamo ++ meures :+ freyne, cluster, 1259102, eternityFilms, de)
    withClue(told(c)) {
      c.flagged shouldBe true
      c.findings.map(_.kind) should contain(Kind.Director)
    }
  }

  it should "be flagged as the prod grouping stood: the linked billings' own evidence denies Freyne's film" in {
    // the linked listings state Freyne's copied year and nothing else, the kept one Freyne's facts -- no fact conflicts;
    // but alone each linked listing's Cineamo link is overruled by the present sources backing Meures' film, which denies
    // it Freyne's: the grouping merged billings of Meures' film with a listing of Freyne's
    val c = historical(cineamo ++ meures :+ freyne, cineamo :+ freyne, 1259102, eternityFilms, de)
    withClue(told(c)) {
      c.flagged shouldBe true
      c.findings.filter(_.kind == Kind.Denies).flatMap(_.members) should contain allElementsOf cineamo.map(_.key)
    }
  }

  // UK "The Ladykillers", convergence #624 uk seed 32 (drift slice nerve-centre-londonderry-the-ladykillers-039701b7, fixed
  // by 8dfe177522a): Nerve Centre's "The Ladykillers | Cinematic Breakfast" with Keighley's "The Ladykillers (1955)", and,
  // Keighley gone, alone under the 2004 remake by its pooled match
  private val ladyFilms = Seq(F(5506, "The Ladykillers", 1955, "Alexander Mackendrick", 91, 8.0),
    F(5516, "The Ladykillers", 2004, "Joel Coen", 104, 12.0))
  private val nerve     = listing(Multikino, "The Ladykillers | Cinematic Breakfast")
  private val keighley  = listing(Helios, "The Ladykillers (1955)", None, Some("Alexander Mackendrick"), Some(91))

  "The Ladykillers' grouping (uk)" should "be flagged when Keighley's 1955 billing is grouped under the remake" in {
    val c = historical(Seq(nerve, keighley), Seq(nerve, keighley), 5516, ladyFilms, uk)
    withClue(told(c))(c.flagged shouldBe true)
  }

  it should "not be flagged alone: one listing is no grouping, and its pooled take is a take for SuspectTakes" in {
    val c = historical(Seq(nerve), Seq(nerve), 5516, ladyFilms, uk)
    withClue(told(c))(c.flagged shouldBe false)
  }

  // ── known good ────────────────────────────────────────────────────────────────────────────────────────────────

  "a right grouping" should "not be flagged: Prima Facie's bare billings and Chichester under the 2026 film" in {
    // identity-judgements.tsv: uk * "Prima Facie" tmdb:1127282 right
    val all = chichester +: ntLive +: primaBare
    resolved(all, primaFilms, uk).filter(_.flagged).map(told) shouldBe empty
  }

  it should "not be flagged: Eternity's Meures listings with the linked billings whose link they overrule" in {
    resolved(cineamo ++ meures :+ freyne, eternityFilms, de).filter(_.flagged).map(told) shouldBe empty
  }

  it should "not be flagged: DE Karla, a feed's copied runtime beside the 2025 film's stated facts" in {
    // IdentityResolverCasesSpec: Cineamo's bare "Karla" [99'] copied from the 2006 film, every listing taking the 2025 one
    val films  = Seq(F(9695, "Karla", 2006, "Joel Bender", 99, 2.102), F(1480424, "Karla", 2025, "Christina Tournatzés", 104, 1.7239))
    def stated(v: Cinema) = listing(v, "Karla", Some(2025), Some("Christina Tournatzés"), Some(104))
    val linked = Seq(KinoMikro, KinoOaza, KinoPalacowe).map(v => fed(listing(v, "Karla", runtime = Some(99)), "cineamo", "9695m"))
    val others = Seq(KinoMuza, Helios, Multikino).map(v => fed(stated(v), "webedia", "1000023315")) ++
      Seq(stated(KinoApollo).copy(page = Some("https://www.kinoheld.de/film/karla-68660bfe987df")),
        fed(listing(KinoBulgarska, "Karla", Some(2025)), "cineamo", "1480424m"))
    resolved(linked ++ others, films, de).filter(_.flagged).map(told) shouldBe empty
  }

  it should "not be flagged: The Ladykillers at Nerve Centre and Keighley under the 1955 film" in {
    resolved(Seq(nerve, keighley), ladyFilms, uk).filter(_.flagged).map(told) shouldBe empty
  }
}
