package services.identity

import models.{Cinema, City, Country, Helios, HeliosMagnolia, KinoApollo, KinoBulgarska, KinoMikro, KinoMuza, KinoOaza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{SingleCountryNormalizer, TitleNormalizer}

/**
 * A BARE listing -- no fact of its own, no own match, nothing but its title joining it to a film -- whose title family is
 * CONTESTED takes no film (docs/investigations/2026-11-01-flip-unification.md, stage 5; the owner: "unmatched beats
 * wrong"): two works, each backed by more than one venue's own statements, and nothing of the bare listing's to tell them
 * apart by. It followed the family's majority, whose bound a venue arriving crossed either way: UK Beehive Honiton's
 * "Nostalgic Cinema: The Addams Family" went 2907 -> 481084 -> 2907 as venues crediting Sonnenfeld's 1991 film and
 * Vernon's 2019 one arrived (drift seed 58). As listings arrive, a bare listing's take can now only go to none.
 */
class ContestedFamilySpec extends AnyFlatSpec with Matchers {
  import FilmTable.{F, listing}
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val films = Seq(F(2907, "The Addams Family", 1991, "Barry Sonnenfeld", 99, 30.0), F(481084, "The Addams Family", 2019, "Conrad Vernon", 87, 40.0))
  private def sonnenfeld(venues: Seq[models.Cinema]) = venues.map(listing(_, "The Addams Family", Some(1991), Some("Barry Sonnenfeld"), Some(99)))
  private def vernon(venues: Seq[models.Cinema]) = venues.map(listing(_, "The Addams Family", Some(2019), Some("Conrad Vernon"), Some(87)))
  private val bare = listing(Kinoteka, "Nostalgic Cinema: The Addams Family")

  private def whole(listings: Seq[Listing]) = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
  private def rendered(r: Resolution, ls: Seq[Listing]) = ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")

  private val contested = sonnenfeld(Seq(KinoMuza, Helios)) ++ vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska))

  "A bare listing in a contested title family" should "take no film, its siblings their own" in {
    val r = whole(contested :+ bare)
    withClue(rendered(r, contested :+ bare)) {
      r.decisionOf(bare.key).film shouldBe None
      r.decisionOf(bare.key).explanation.exists(_.startsWith("contested title family")) shouldBe true
      contested.take(2).map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(2907))
      contested.drop(2).map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(481084))
    }
  }

  // Contested is absorbing: each work's venues only grow as listings arrive, so once both are well supported the bare
  // listing stays on none. Before that it follows its siblings as before (one venue each side contests nothing).
  it should "stay on no film once its family is contested, whatever order the family's listings arrive in" in {
    val lookups = new FilmTable(films, normalizer)
    def contestedAt(prefix: Seq[Listing]) =
      Seq(1991, 2019).forall(year => prefix.count(l => l.year.contains(year)) >= ClusterVoting.WellSupported)
    val left = (1L to 8L).flatMap { seed =>
      PopulationMonotonicity.chains(contested :+ bare, seed, 8, 8).flatMap { chain =>
        chain.filter(_.contains(bare)).dropWhile(prefix => !contestedAt(prefix)).flatMap { prefix =>
          val r = IdentityResolver.resolve(prefix, lookups, normalizer, ResolveInputs.Unpinned)
          r.decisionOf(bare.key).film.map(film => s"seed $seed, ${prefix.size} listings: $film")
        }
      }
    }
    left shouldBe empty
  }

  // the guards: a family one venue's statement contests is not contested; a family backing one film lends it
  "A bare listing beside one venue's other film" should "still follow its family: one venue's credit may be its own slip" in {
    val siblings = sonnenfeld(Seq(KinoMuza)) ++ vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska, HeliosMagnolia, KinoApollo))
    val r = whole(siblings :+ bare)
    withClue(rendered(r, siblings :+ bare))(r.decisionOf(bare.key).explanation.exists(_.startsWith("contested title family")) shouldBe false)
  }

  "A bare listing beside one film's venues alone" should "take that film" in {
    val siblings = vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska))
    val r = whole(siblings :+ bare)
    withClue(rendered(r, siblings :+ bare))(r.decisionOf(bare.key).film shouldBe Some(481084))
  }

  // ── the rivals are readings of THIS listing (measure 41430307403: the first rule lost 38 right takes) ──────────────────
  // A rival counts only when it is the same kind of screening, the bare title does not rule it out, and more than one
  // SOURCE states it: a feed's or a listings site's entry, copied to every venue it lists, is one statement.

  private def venuesIn(country: Country) = Cinema.all.filter(c => City.forCinema(c).exists(_.country == country))
  private val uk = venuesIn(Country.UnitedKingdom)
  private val de = venuesIn(Country.Germany)
  private def resolveIn(country: Country, films: Seq[F], listings: Seq[Listing]) = {
    val normalizer = TitleNormalizer.forCountry(country)
    IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
  }
  private def stated(venue: Cinema, title: String, year: Int, director: String, page: Option[String] = None) =
    listing(venue, title, Some(year), Some(director)).copy(page = page)
  private def contestedVeto(r: Resolution, l: Listing) = r.decisionOf(l.key).explanation.exists(_.startsWith("contested title family"))

  // DE CinemaxX ×27 "BTS WORLD TOUR 'ARIRANG' IN SÃO PAULO: LIVE" (recording 573): the Buenos Aires concert is no rival
  // reading of a billing naming São Paulo -- each title names a city the other does not. A banner or a shorter billing
  // still reads as the film.
  "A bare billing naming its edition" should "read as no other edition of its title family" in {
    def node(title: String) = { val l = listing(de(0), title); new EvidenceNode(Evidence.of(l, None, TitleDecorations.None), Seq(l)) }
    def film(title: String, year: Int) = IdentityMeasures.Film(title, year = Some(year))
    val saoPaulo = node("BTS WORLD TOUR 'ARIRANG' IN SÃO PAULO: LIVE")
    ClusterVoting.readable(saoPaulo, film("BTS WORLD TOUR [ARIRANG] in São Paulo", 2026)) shouldBe true
    ClusterVoting.readable(saoPaulo, film("BTS WORLD TOUR [ARIRANG] in Buenos Aires", 2026)) shouldBe false
    ClusterVoting.readable(node("Nostalgic Cinema: The Addams Family"), film("The Addams Family", 1991)) shouldBe true
    ClusterVoting.readable(node("Nostalgic Cinema: The Addams Family"), film("The Addams Family", 2019)) shouldBe true
    ClusterVoting.readable(node("Shaun the Sheep"), film("Shaun the Sheep Movie", 2015)) shouldBe true
  }

  // UK "Fallen Angels by Noël Coward" ×6: the billing credits its work to Coward, which rules out Wong Kar-wai's 1995
  // "Fallen Angels" however many venues credit him (ListingShape.creditDenies).
  private val coward = F(1702350, "Fallen Angels", 2026, "Scott Ellis", 120, 40.0)
  private val wongKarWai = F(11220, "Fallen Angels", 1995, "Wong Kar-wai", 99, 20.0)
  "A bare billing crediting its work to an author" should "not count the author's older namesake as a rival" in {
    val play  = uk.slice(0, 4).map(stated(_, "Fallen Angels by Noël Coward", 2026, "Scott Ellis"))
    val wkw   = uk.slice(4, 6).map(stated(_, "Fallen Angels", 1995, "Wong Kar Wai"))
    val bare  = listing(uk(6), "Fallen Angels by Noël Coward").copy(screenings = ScreeningDays.of(Seq(java.time.LocalDate.of(2026, 11, 2))))
    val all = play ++ wkw :+ bare
    val r = resolveIn(Country.UnitedKingdom, Seq(coward, wongKarWai), all)
    withClue(rendered(r, all)) {
      contestedVeto(r, bare) shouldBe false
      r.decisionOf(bare.key).film shouldBe Some(1702350)
    }
  }

  // UK Ilkley's and Teignmouth's "Fallen Angels": the 1995 film's eleven venues were ONE feed's credit copied to them.
  "A bare billing beside one feed's copied statement" should "not count the feed's venues as a second source" in {
    val play = uk.slice(0, 4).map(stated(_, "Fallen Angels by Noël Coward", 2026, "Scott Ellis"))
    val feed = uk.slice(4, 8).map(stated(_, "Fallen Angels", 1995, "Wong Kar Wai", Some("https://www.flicks.co.uk/movie/fallen-angels-1995/")))
    val bare = listing(uk(8), "Fallen Angels")
    val all = play ++ feed :+ bare
    val r = resolveIn(Country.UnitedKingdom, Seq(coward, wongKarWai), all)
    withClue(rendered(r, all))(contestedVeto(r, bare) shouldBe false)
  }

  // UK The Cinema Newton Stewart's "Shaun the Sheep": the 2015 film's two venues read one Flicks entry.
  it should "lend a bare title its film when the other film's venues read one listings site's entry" in {
    // TMDB's search for the bare title finds both films: the bare billing has no sole hit of its own
    val films = Seq(F(1477104, "Shaun the Sheep: The Beast of Mossy Bottom", 2026, "Steve Cox", 90, 60.0, alternatives = Seq("Shaun the Sheep")),
      F(263109, "Shaun the Sheep Movie", 2015, "Mark Burton", 85, 30.0, alternatives = Seq("Shaun the Sheep")))
    // the new film billed under the series' bare title at most of its venues, dated and credited
    val mossy = uk.slice(0, 3).map(stated(_, "Shaun the Sheep", 2026, "Steve Cox"))
    val movie = uk.slice(3, 5).map(stated(_, "Shaun the Sheep (2015)", 2015, "Mark Burton", Some("https://www.flicks.co.uk/movie/shaun-the-sheep-movie/")))
    val bare  = listing(uk(5), "Shaun the Sheep")
    val all = mossy ++ movie :+ bare
    val r = resolveIn(Country.UnitedKingdom, films, all)
    withClue(rendered(r, all))(contestedVeto(r, bare) shouldBe false)
  }

  // DE Lumos's "Sinn und Sinnlichkeit (Strick-Kino)": Ang Lee's 1995 film's nine venues were one kinoheld entry.
  it should "lend a banner's bare billing the current film beside one feed's repertory entry" in {
    val films = Seq(F(1503762, "Sinn und Sinnlichkeit", 2026, "Georgia Oakley", 110, 80.0), F(4584, "Sinn und Sinnlichkeit", 1995, "Ang Lee", 136, 20.0))
    val current = de.slice(0, 4).map(stated(_, "Sinn und Sinnlichkeit", 2026, "Georgia Oakley"))
    val feed    = de.slice(4, 7).map(stated(_, "Sinn und Sinnlichkeit", 1995, "Ang Lee", Some("https://www.kinoheld.de/film/sinn-und-sinnlichkeit-1995")))
    val bare    = listing(de(7), "Sinn und Sinnlichkeit (Strick-Kino)")
    val all = current ++ feed :+ bare
    val r = resolveIn(Country.Germany, films, all)
    withClue(rendered(r, all))(contestedVeto(r, bare) shouldBe false)
  }

  // UK Old Market Hall Shrewsbury's "Dracula (12A)": the venue's 12A is the Hammer restoration's certificate, not
  // Browning's PG 1931 film's -- no rival reading, however many venues bill the 1931 film.
  private def shrewsbury = listing(uk(7), "Dracula").copy(rawTitle = "Dracula (12A)", certificate = Some("12A"))
  "A bare billing with its certificate" should "not count a namesake rated on another step as a rival" in {
    val films = Seq(F(11868, "Dracula", 1958, "Terence Fisher", 82, 30.0, certificates = Some("GB=12A")),
      F(138, "Dracula", 1931, "Tod Browning", 75, 25.0, certificates = Some("GB=PG")))
    // Quad Derby's "Dracula (15)" and Plough Torrington's "Dracula" bill the restoration under the bare title too
    val hammer   = uk.slice(0, 4).map(stated(_, "Dracula (4K Restoration)", 1958, "Terence Fisher")) ++
      uk.slice(8, 10).map(stated(_, "Dracula", 1958, "Terence Fisher"))
    val browning = uk.slice(4, 7).map(stated(_, "Dracula (1931)", 1931, "Tod Browning"))
    val bare     = shrewsbury
    val all = hammer ++ browning :+ bare
    val r = resolveIn(Country.UnitedKingdom, films, all)
    withClue(rendered(r, all) + r.decisionOf(bare.key).explanation) {
      contestedVeto(r, bare) shouldBe false
      r.decisionOf(bare.key).film shouldBe Some(11868)
    }
  }

  it should "still be contested when its certificate is both namesakes' step" in {
    val films = Seq(F(11868, "Dracula", 1958, "Terence Fisher", 82, 30.0, certificates = Some("GB=12A")),
      F(138, "Dracula", 1931, "Tod Browning", 75, 25.0, certificates = Some("GB=12A")))
    val hammer   = uk.slice(0, 4).map(stated(_, "Dracula (4K Restoration)", 1958, "Terence Fisher"))
    val browning = uk.slice(4, 7).map(stated(_, "Dracula (1931)", 1931, "Tod Browning"))
    val bare     = shrewsbury
    val all = hammer ++ browning :+ bare
    val r = resolveIn(Country.UnitedKingdom, films, all)
    withClue(rendered(r, all) + r.decisionOf(bare.key).explanation)(r.decisionOf(bare.key).film shouldBe None)
  }
}
