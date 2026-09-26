package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityMeasures.{Category, Film, Listing, ListingFilm, ListingListing, Missing, Number}

/**
 * `identity-weights.json` is DATA fitted by `scripts.IdentityCalibrate`
 * (docs/design/identity-resolver.md §calibration). These checks pin that it loads, that it is
 * internally sound, and that it puts the HISTORICAL cases on the right side. The cases are test
 * labels only — the calibration never reads them — each written from its incident as the venues and
 * TMDB published it.
 */
class IdentityCalibrationSpec extends AnyFlatSpec with Matchers {

  private val model = IdentityCalibration.default

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

  it should "weigh missing evidence as its own value, never as agreement" in {
    val bare = Listing("Anything")
    val m = IdentityMeasures.listingFilm(bare, Film("Anything", year = Some(2020)), None, 0, 0)
    m("year.delta") shouldBe Missing("listing")
    m("director") shouldBe Missing("listing")
    val director = model.contributions(ListingFilm, m).toMap.apply("director")
    director shouldBe model.scopes(ListingFilm).signals("director").missing.getOrElse("listing", 0.0)
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
    ("Bradley Cooper's A Star Is Born is the 2018 film", starIsBorn2018, cooper),
    ("an 83-minute Your Name re-release is still Shinkai's film", yourNameNh, yourName))

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
  // Coppola's 1974 "Rozmowa" at its 2026 screening year. The score still joins it (director, runtime
  // and original title outweigh 52 years), but no same-film unit of the recorded data sits 49+ years
  // from its film, so the certified learned veto `year.distance >= 49` forbids it.
  it should "score Kinoteka's screening-year Rozmowa as Coppola's film, though the certified year veto still forbids it" in {
    val m = IdentityMeasures.listingFilm(rozmowa, conversation, None, 0, 0)
    withClue(model.explain(ListingFilm, m))(film(rozmowa, conversation) should be > 0.5)
    model.cannotLink(ListingFilm, m).map(_.all.map(_.signal)) shouldBe Some(Seq("year.distance"))
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

  "a learned cannot-link" should "never fire on missing evidence" in {
    val rule = IdentityCalibration.Condition("director", in = Seq("different"))
    rule.holds(Map("director" -> Missing("listing"))) shouldBe false
    rule.holds(Map("director" -> Category("different"))) shouldBe true
    IdentityCalibration.Condition("year.distance", atLeast = Some(6)).holds(Map("year.distance" -> Number(7))) shouldBe true
    IdentityCalibration.Condition("year.distance", atLeast = Some(6)).holds(Map("year.distance" -> Missing("film"))) shouldBe false
  }
}
