package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityMeasures.{Category, Film, Listing, ListingFilm, ListingListing, Missing, Number}

/**
 * `identity-weights.json` is DATA fitted by `scripts.IdentityCalibrate`
 * (docs/design/identity-resolver.md §14). These checks pin that it loads, that it is
 * internally sound, and that it puts the HISTORICAL cases on the right side. The cases are test
 * labels only — the calibration never reads them — each written from its incident as the venues and
 * TMDB published it.
 */
class IdentityCalibrationSpec extends AnyFlatSpec with Matchers {

  private val model = IdentityCalibration.resolver

  private def film(l: Listing, f: Film): Double =
    model.probability(ListingFilm, IdentityMeasures.listingFilm(l, f, searchRank = None, rivals = 0, corroboratingVenues = 0))

  private def listings(a: Listing, b: Listing, sameVenue: Boolean): Double =
    model.probability(ListingListing, IdentityMeasures.listingListing(a, b, sameVenue, sharedChainId = None))

  "identity-weights.json" should "load, with both scopes, their calibration and their thresholds" in {
    model.scopes.keySet shouldBe Set(ListingFilm, ListingListing)
    model.scopes.values.foreach { s =>
      s.signals should not be empty
      s.calibration.logOdds should not be empty
      s.calibration.probabilities shouldBe sorted
    }
    model.scopes(ListingFilm).thresholds.keySet should contain allOf ("showRatings", "cannotLink")
    model.provenance.keySet should contain allOf ("script", "labels", "splits", "epsilon")
  }

  it should "only carry cannot-link rules whose measured false-veto bound is under the stated epsilon" in {
    val epsilon = model.provenance("epsilon").toDouble
    model.cannotLinks should not be empty
    model.cannotLinks.foreach { r =>
      r.scope should (be(ListingFilm) or be(ListingListing))
      r.all should not be empty
      r.support should be > 0
      withClue(r.name)(r.falseVetoBound should be <= epsilon)
    }
  }

  it should "weigh a category naming more of the other side no lower than one naming less, in every scope" in {
    // IdentityMeasures.EvidenceOrder, fitted by pool-adjacent-violators in IdentityCalibrate: an
    // unconstrained table once put "Ken Russell's The Devils" (decorated) below Tommy (no shared word).
    for ((scope, m) <- model.scopes; (signal, order) <- IdentityMeasures.EvidenceOrder; w <- m.signals.get(signal)) {
      val weights = order.flatMap(c => w.categories.get(c).map(c -> _))
      weights.sliding(2).foreach {
        case Seq((stronger, a), (weaker, b)) => withClue(s"$scope $signal: $stronger $a vs $weaker $b")(a should be >= b)
        case _                               => ()
      }
    }
  }

  it should "weigh missing evidence as its own value, never as agreement" in {
    val bare = Listing("Anything")
    val m = IdentityMeasures.listingFilm(bare, Film("Anything", year = Some(2020)), None, 0, 0)
    m("year.delta") shouldBe Missing("listing")
    m("director") shouldBe Missing("listing")
    val director = model.contributions(ListingFilm, m).toMap.apply("director")
    director shouldBe model.scopes(ListingFilm).signals("director").missing.getOrElse("listing", 0.0)
  }

  "an evidence class" should "lend its measured probability only when every condition holds, never on missing evidence" in {
    import IdentityCalibration.{Condition, EvidenceClass}
    val topHit = EvidenceClass("title=exact AND search.rank<=1 AND rivals<=0.5", ListingFilm,
      Seq(Condition("title", in = Seq("exact")), Condition("search.rank", atMost = Some(1)), Condition("rivals", atMost = Some(0.5))),
      probability = 0.99)
    val m = model.copy(evidenceClasses = Seq(topHit))
    def measured(rank: Option[Int], rivals: Int) = IdentityMeasures.listingFilm(Listing("Aaram"), Film("Aaram"), rank, rivals, 0)
    m.classProbability(ListingFilm, measured(Some(1), 0)) shouldBe Some(0.99)
    m.classProbability(ListingFilm, measured(Some(2), 0)) shouldBe None
    m.classProbability(ListingFilm, measured(Some(1), 1)) shouldBe None
    m.classProbability(ListingFilm, measured(None, 0)) shouldBe None
    m.classProbability(ListingListing, measured(Some(1), 0)) shouldBe None
    model.copy(evidenceClasses = Nil).classProbability(ListingFilm, measured(Some(1), 0)) shouldBe None
  }

  "identity-weights.json's evidence classes" should "each be measured within the wrong rate ratings are shown at" in {
    val target = model.scopes(ListingFilm).thresholds("showRatings").measured("targetWrongRate")
    model.evidenceClasses should not be empty
    model.evidenceClasses.foreach { c =>
      withClue(c.name) {
        c.all should not be empty
        c.measured("fittingWrongUpper") should be <= target
        c.probability shouldBe (1 - c.measured("fittingWrongUpper")) +- 1e-12
        model.showsRatings(c.probability) shouldBe true
      }
    }
  }

  // ── the historical cases (docs/design/identity-resolver.md §1.1) ──────────────────────

  private val samsonMet   = Listing("Samson i Dalila", year = Some(2026), directors = Seq("Darko Tresnjak"))
  private val deMille     = Film("Samson i Dalila", Some("Samson and Delilah"), year = Some(1949), runtime = Some(131),
                                 directors = Some(Seq("Cecil B. DeMille")), countries = Some(Seq("US")))
  private val faustMeda   = Listing("Zärtlich kreist die Faust", year = Some(1990), runtime = Some(70),
                                    directors = Seq("Hilde Bechert", "Klaus Dexel"))
  private val murnau      = Film("Faust – Eine deutsche Volkssage", Some("Faust"), year = Some(1926), runtime = Some(107),
                                 directors = Some(Seq("F. W. Murnau")), countries = Some(Seq("DE")))
  private val itEndsWithUs = Listing("It Ends with Us", runtime = Some(130), directors = Seq("Justin Baldoni"))
  private val itEnds       = Film("It Ends", year = Some(2025), runtime = Some(89), directors = Some(Seq("Alexander Ullom")))
  private val itEndsWithUsFilm = Film("It Ends with Us", year = Some(2024), runtime = Some(131), directors = Some(Seq("Justin Baldoni")))
  private val starIsBorn2018 = Listing("A Star is Born (2018)", runtime = Some(136), directors = Seq("Bradley Cooper"))
  private val cukor        = Film("A Star Is Born", year = Some(1954), runtime = Some(176), directors = Some(Seq("George Cukor")))
  private val cooper       = Film("A Star Is Born", year = Some(2018), runtime = Some(136), directors = Some(Seq("Bradley Cooper")))
  private val rboTosca     = Listing("RBO Cinema Season 2026-27: Tosca", runtime = Some(195), directors = Seq("Oliver Mears"))
  private val tosca1941    = Film("Tosca", Some("Tosca"), year = Some(1941), runtime = Some(105), directors = Some(Seq("Carl Koch")))
  private val happyKinoteka = Listing("Happy Together", year = Some(1997), directors = Seq("Wong Kar Wai"))
  private val happy2018    = Film("Happy Together", Some("해피 투게더"), year = Some(2018), runtime = Some(112), directors = Some(Seq("Kim Jeong-hwan")))
  private val rozmowa      = Listing("Rozmowa", originalTitle = Some("The Conversation"), year = Some(2026), runtime = Some(113),
                                     directors = Seq("Francis Ford Coppola"))
  private val conversation = Film("Rozmowa", Some("The Conversation"), year = Some(1974), runtime = Some(113),
                                  directors = Some(Seq("Francis Ford Coppola")))
  private val yourNameNh   = Listing("Twoje imię", originalTitle = Some("Your Name (re-release)"), runtime = Some(83),
                                     directors = Seq("Makoto Shinkai"))
  private val yourName     = Film("Twoje imię", Some("君の名は。"), alternativeTitles = Seq("Your Name."), year = Some(2016),
                                  runtime = Some(106), directors = Some(Seq("Makoto Shinkai")))

  private val differentFilms: Seq[(String, Listing, Film)] = Seq(
    ("the Met's 2026 Samson i Dalila is not DeMille's 1949 film", samsonMet, deMille),
    ("Zärtlich kreist die Faust is not Murnau's Faust", faustMeda, murnau),
    ("It Ends with Us is not It Ends", itEndsWithUs, itEnds),
    ("Bradley Cooper's A Star Is Born is not Cukor's", starIsBorn2018, cukor),
    ("the RBO's 2026 Tosca relay is not the 1941 Tosca", rboTosca, tosca1941),
    ("Wong Kar Wai's Happy Together is not Kim Jeong-hwan's", happyKinoteka, happy2018))

  private val sameFilms: Seq[(String, Listing, Film)] = Seq(
    ("It Ends with Us is It Ends with Us", itEndsWithUs, itEndsWithUsFilm),
    ("Bradley Cooper's A Star Is Born is the 2018 film", starIsBorn2018, cooper))

  // A KNOWN REGRESSION of the r5 refit (docs/design/identity-resolver.md §15.8), pinned pending so
  // the refit that scores it right flips it red: the resolver now VETOES this listing (its own facts'
  // probability, 0.06, is under the certified cut) — a 23-minute runtime gap weighs −3.26 and
  // "Your Name (re-release)" against "Your Name." weighs −2.52, where the same director weighs
  // +4.22. The old artefact's +5.27 director weight was inflated by unfetched candidate credits.
  it should "join: an 83-minute Your Name re-release is still Shinkai's film (known regression, pending)" in {
    pendingUntilFixed {
      film(yourNameNh, yourName) should be > 0.5
      model.cannotLink(ListingFilm, IdentityMeasures.listingFilm(yourNameNh, yourName, None, 0, 0)) shouldBe None
    }
  }

  differentFilms.foreach { case (name, l, f) =>
    it should s"keep apart: $name" in {
      val p = film(l, f)
      withClue(s"p=$p ${model.explain(ListingFilm, IdentityMeasures.listingFilm(l, f, None, 0, 0))}") {
        model.showsRatings(p) shouldBe false
        p should be < 0.5
      }
    }
  }

  sameFilms.foreach { case (name, l, f) =>
    it should s"join: $name" in {
      val p = film(l, f)
      withClue(s"p=$p ${model.explain(ListingFilm, IdentityMeasures.listingFilm(l, f, None, 0, 0))}") {
        p should be > 0.5
        model.cannotLink(ListingFilm, IdentityMeasures.listingFilm(l, f, None, 0, 0)) shouldBe None
      }
    }
  }

  // A KNOWN LIMITATION, pinned so a recalibration that learns better flips it: Kinoteka lists
  // Coppola's 1974 "Rozmowa" at its 2026 screening year. No same-film unit of the recorded data sits
  // that far from its film, so a certified learned year veto forbids it — the DECISION, unchanged
  // since the first artefact.
  it should "keep Kinoteka's screening-year Rozmowa from Coppola's film by the certified year veto (known limitation)" in {
    val m = IdentityMeasures.listingFilm(rozmowa, conversation, None, 0, 0)
    model.cannotLink(ListingFilm, m).map(_.all.map(_.signal)) shouldBe Some(Seq("year.distance"))
  }

  // The SCORE, which the first artefact put above even odds (0.90), is 0.41 under r5 (§15.8): the
  // director and runtime weights shrank once candidates' credits were recorded. Pending until a refit
  // scores it right again; the decision above does not depend on it.
  it should "score Kinoteka's screening-year Rozmowa as Coppola's film (known regression, pending)" in {
    pendingUntilFixed(film(rozmowa, conversation) should be > 0.5)
  }

  private val differentListings: Seq[(String, Listing, Listing, Boolean)] = Seq(
    ("Arc Blackpool's Belle (2013) and Belle (2021)",
      Listing("Belle (2013)", runtime = Some(104), directors = Seq("Amma Asante")),
      Listing("Belle (2021)", runtime = Some(122), directors = Seq("Mamoru Hosoda")), true),
    ("Marion Theatre's Planet of the Apes and Planet of the Apes (2001)",
      Listing("Planet of the Apes", runtime = Some(112), directors = Seq("Franklin J. Schaffner")),
      Listing("Planet of the Apes (2001)", runtime = Some(119), directors = Seq("Tim Burton")), true),
    ("Ang Lee's and Georgia Oakley's Sinn und Sinnlichkeit",
      Listing("Sinn und Sinnlichkeit", Some("Sense and Sensibility"), year = Some(1995), runtime = Some(135), directors = Seq("Ang Lee")),
      Listing("Sinn und Sinnlichkeit", Some("Sense and Sensibility"), year = Some(2026), runtime = Some(132), directors = Seq("Georgia Oakley")), false),
    ("Lalka (2026) and Lalka (ale to horror)",
      Listing("Lalka", year = Some(2026), runtime = Some(162)),
      Listing("Lalka (ale to horror)", year = Some(2025), runtime = Some(82)), true))

  differentListings.foreach { case (name, a, b, sameVenue) =>
    it should s"keep apart the listings: $name" in {
      listings(a, b, sameVenue) should be < 0.5
    }
  }

  it should "join a banner-decorated spelling that states its plain sibling's year" in {
    listings(Listing("Lalka", year = Some(2026), runtime = Some(162)), Listing("Lalka | PREMIERA", year = Some(2026)),
      sameVenue = false) should be > 0.5
  }

  private def listingsApart(a: Listing, b: Listing, sameVenue: Boolean) =
    services.movies.ListingConstraints.learnedListingListing(model, IdentityMeasures.listingListing(a, b, sameVenue, sharedChainId = None))

  "a learned listing-listing cannot-link" should "never keep apart two listings that compare no fact both published" in {
    // PL: a bare or bannered spelling beside its credited decorated siblings — the title relation
    // (and where they are) is all the pair measures, and that is a score, never a veto.
    listingsApart(Listing("Tony"), Listing("Kino bez barier: Tony (AD + CC)", year = Some(2026), directors = Seq("Matt Johnson")),
      sameVenue = false) shouldBe None
    listingsApart(Listing("Lalka (2026)"), Listing("Filmowy Klub Seniora: LALKA", year = Some(2026), directors = Seq("Maciej Kawalski")),
      sameVenue = false) shouldBe None
    listingsApart(Listing("Wajda: Re-wizje. Bez znieczulenia"), Listing("BEZ ZNIECZULENIA (1978) | „PRZEGLĄD WAJDA: re-wizje”"),
      sameVenue = false) shouldBe None
  }

  it should "never keep apart one venue's two spellings, one carrying the other's whole title, whose facts agree" in {
    // PL, one venue's "Lalka (2026)" and "Lalka (2026) | seans DKF Projekcja": the same year in
    // each, and a venue listing two spellings is all that weighs against one film.
    listingsApart(Listing("Lalka (2026)"), Listing("Lalka (2026) | seans DKF Projekcja", year = Some(2026)), sameVenue = true) shouldBe None
    listingsApart(Listing("Pucio kocha zwierzaki", year = Some(2026), runtime = Some(60)),
      Listing("PUCIO KOCHA ZWIERZAKI 2D DUB. SPS", year = Some(2026), runtime = Some(60)), sameVenue = true) shouldBe None
  }

  it should "still keep apart two listings a fact both published contradicts" in {
    listingsApart(Listing("Lalka", year = Some(2026), runtime = Some(162)), Listing("Lalka (ale to horror)", year = Some(2025), runtime = Some(82)),
      sameVenue = true) shouldBe defined
    // UK, one venue's two live-viewing events: the same crew and running time, and titles naming two
    // cities — the venue lists them apart.
    listingsApart(Listing("BTS 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", runtime = Some(195), directors = Seq("Jungjae HA")),
      Listing("BTS 'ARIRANG' IN SÃO PAULO: LIVE VIEWING", runtime = Some(195), directors = Seq("Jungjae HA")), sameVenue = true) shouldBe defined
    // DE: Sheri Hagen's "Billie" (2025) and James Erskine's "Billie – Legende des Jazz" (2020) — the
    // shorter title a whole segment of the longer, measured in either order, a year and a director apart.
    val hagen   = Listing("Billie", year = Some(2025), directors = Seq("Sheri Hagen"))
    val erskine = Listing("Billie – Legende des Jazz", year = Some(2020), directors = Seq("James Erskine"))
    listingsApart(hagen, erskine, sameVenue = false) shouldBe defined
    listingsApart(erskine, hagen, sameVenue = false) shouldBe defined
    // The director alone, when one states its year in a bracket and the other in a field.
    listingsApart(hagen, Listing("Billie – Legende des Jazz (2020)", directors = Seq("James Erskine")), sameVenue = false) shouldBe defined
  }

  "a listing's title search" should "rank the film at its best over the listing's own queries and count the films its title names as closely" in {
    val l = Listing("Kill Bill", rawTitle = Some("Kill Bill: The Whole Bloody Affair"))
    val answers = Map(
      "Kill Bill" -> Seq(Hit(24, "Kill Bill: Vol. 1", None, Some(2003), 50), Hit(414419, "Kill Bill: The Whole Bloody Affair", None, Some(2011), 5)),
      "Kill Bill: The Whole Bloody Affair" -> Seq(Hit(414419, "Kill Bill: The Whole Bloody Affair", None, Some(2011), 5)))
    val s = IdentityMeasures.titleSearch(l, 414419, answers.get).get
    s shouldBe models.TitleSearch(IdentityMeasures.key("Kill Bill"), Some(1), 0)
    IdentityMeasures.titleSearch(l, 7, answers.get).get.rank shouldBe None
    IdentityMeasures.titleSearch(l, 414419, _ => None) shouldBe None
    val namesakes = Map("Lalka" -> Seq(Hit(1, "Lalka", None, Some(1968), 3), Hit(2, "Lalka", None, Some(2026), 9)))
    IdentityMeasures.titleSearch(Listing("Lalka"), 2, namesakes.get).get shouldBe models.TitleSearch("lalka", Some(2), 1)
  }

  "a learned cannot-link" should "never fire on missing evidence" in {
    val rule = IdentityCalibration.Condition("director", in = Seq("different"))
    rule.holds(Map("director" -> Missing("listing"))) shouldBe false
    rule.holds(Map("director" -> Category("different"))) shouldBe true
    IdentityCalibration.Condition("year.distance", atLeast = Some(6)).holds(Map("year.distance" -> Number(7))) shouldBe true
    IdentityCalibration.Condition("year.distance", atLeast = Some(6)).holds(Map("year.distance" -> Missing("film"))) shouldBe false
  }
}
