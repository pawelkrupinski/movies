package services.identity

import models.{Helios, KinoApollo, KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.GroupingCoherence.Kind

/** The grouping-coherence signals ([[GroupingCoherence]]), each on the real listings it was drawn from and on the right
 *  groupings that bill the same shapes. */
class GroupingCoherenceSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val Year = 2026
  private def film(title: String, year: Int, runtime: Option[Int] = None, directors: Seq[String] = Nil, releases: Option[String] = None) =
    IdentityMeasures.Film(title, year = Some(year), runtime = runtime, directors = Some(directors), releases = releases)
  private def decision(members: Seq[Listing], film: Int, basis: ResolverDecision.Basis = ResolverDecision.Basis.OwnMatch,
                       nodes: Map[services.movies.ListingKey, DecisionTrace.Node] = Map.empty) =
    ResolverDecision(members.map(_.key).sorted, Some(film), 0.9, basis, Nil)(DecisionTrace(None, None, nodes))
  private def coherence(members: Seq[Listing], record: IdentityMeasures.Film, d: Option[ResolverDecision] = None) =
    GroupingCoherence.of(d.getOrElse(decision(members, 1)), "tmdb:1", record, members, Year, Some("DE"))
  private def kinds(members: Seq[Listing], record: IdentityMeasures.Film): Set[Kind] = coherence(members, record).findings.map(_.kind).toSet

  // ── (1) facts ─────────────────────────────────────────────────────────────────────────────────────────────────

  "a year conflict" should "be two members stating years more than one apart, neither a release of the film" in {
    // DE "Eternity": Caligari's kept kinoheld page dates Freyne's 2025 film, Zazie's Meures' 2026 one -- a year apart, no
    // conflict; the 1999 namesake is one
    val freyne = listing(KinoMuza, "Eternity", Some(2025), Some("David Freyne"), Some(114))
    val meures = listing(KinoApollo, "Eternity", Some(2026), Some("Susanne Regina Meures"), Some(90))
    kinds(Seq(freyne, meures), film("Eternity", 2025)) shouldNot contain(Kind.Year)
    val old = listing(Helios, "Eternity", Some(1999))
    val found = coherence(Seq(freyne, old), film("Eternity", 2025)).findings.filter(_.kind == Kind.Year)
    found.map(_.weight) shouldBe Seq(1.0)
    found.head.why should include("1999")
  }

  it should "not be a re-release TMDB dates, nor an old film a venue dates to its re-release" in {
    // "Apocalypse Now" billed 2019 is the 1979 record through its 2019 Final Cut release
    val finalCut = listing(KinoMuza, "Apocalypse Now", Some(2019))
    val original = listing(Helios, "Apocalypse Now", Some(1979))
    kinds(Seq(finalCut, original), film("Apocalypse Now", 1979, releases = Some("US1979-US2019-"))) shouldNot contain(Kind.Year)
    // a 4K re-release a venue dates this year beside one dating the 1955 film
    val reissue = listing(KinoMuza, "The Ladykillers", Some(2026))
    val dated   = listing(Helios, "The Ladykillers (1955)", Some(1955))
    kinds(Seq(reissue, dated), film("The Ladykillers", 1955)) shouldNot contain(Kind.Year)
  }

  it should "read a title's year marker as the year it states" in {
    val marked = listing(KinoMuza, "Suspiria (1977)")
    val remake = listing(Helios, "Suspiria", Some(2018))
    kinds(Seq(marked, remake), film("Suspiria", 2018)) should contain(Kind.Year)
  }

  "a director conflict" should "be two members crediting no person in common" in {
    val freyne = listing(KinoMuza, "Eternity", Some(2025), Some("David Freyne"))
    val meures = listing(KinoApollo, "Eternity", Some(2025), Some("Susanne Regina Meures"))
    coherence(Seq(freyne, meures), film("Eternity", 2025, directors = Seq("David Freyne"))).findings.map(_.kind) shouldBe Seq(Kind.Director)
  }

  it should "not be a co-direction each venue credits one half of, nor a relay's staging beside its screen director" in {
    val one = listing(KinoMuza, "Asterix", Some(2026), Some("Joel Coen"))
    val two = listing(Helios, "Asterix", Some(2026), Some("Ethan Coen"))
    kinds(Seq(one, two), film("Asterix", 2026, directors = Seq("Joel Coen", "Ethan Coen"))) shouldNot contain(Kind.Director)
    val relay = listing(KinoMuza, "NT Live: Prima Facie", Some(2022), Some("Justin Martin"))
    val bare  = listing(Helios, "Prima Facie", Some(2022), Some("Suzie Miller"))
    kinds(Seq(relay, bare), film("National Theatre Live: Prima Facie", 2022)) shouldNot contain(Kind.Director)
  }

  "a runtime conflict" should "be a member running far shorter than another, which no cut of the film explains" in {
    val feature = listing(KinoMuza, "Lalka", Some(2026), runtime = Some(118))
    val other   = listing(Helios, "Lalka", Some(2026), runtime = Some(75))
    kinds(Seq(feature, other), film("Lalka", 2026, runtime = Some(118))) should contain(Kind.Runtime)
  }

  it should "not be a slot around the film, a short's programme, nor rounding" in {
    val film120 = film("Psycho", 1960, runtime = Some(109))
    // the film with its wine, 170 minutes
    kinds(Seq(listing(KinoMuza, "Psycho", runtime = Some(109)), listing(Helios, "Psycho with wine", runtime = Some(170))), film120) shouldNot
      contain(Kind.Runtime)
    kinds(Seq(listing(KinoMuza, "Psycho", runtime = Some(109)), listing(Helios, "Psycho", runtime = Some(99))), film120) shouldNot contain(Kind.Runtime)
    kinds(Seq(listing(KinoMuza, "Psycho", runtime = Some(30)), listing(Helios, "Psycho", runtime = Some(109))), film120) shouldNot contain(Kind.Runtime)
  }

  "a chain billing one title alike" should "be one statement, whatever venues bill it" in {
    val chain = Seq(KinoMuza, Helios, Multikino).map(v => listing(v, "Eternity", Some(2025)))
    coherence(chain, film("Eternity", 2025)).findings shouldBe empty
  }

  // ── (2) a member on its own ───────────────────────────────────────────────────────────────────────────────────

  private val chichester = listing(KinoMuza, "Prima Facie")
  private val ntLive     = listing(Multikino, "NT Live: Prima Facie", Some(2022), Some("Justin Martin"), Some(108))
  private val relay      = film("National Theatre Live: Prima Facie", 2022, Some(108), Seq("Justin Martin"))
  private def node(alone: Option[DecisionTrace.Alone] = None, score: Option[Double] = None, denied: Option[DecisionTrace.Denied] = None,
                   candidate: Int = 963568) =
    DecisionTrace.Node(None, Nil, Nil, candidate = Some(candidate), score = score, alone = alone, denied = denied)

  "a member alone" should "be suspect when its own rules confidently take another film than the cluster's" in {
    // UK Chichester's bare "Prima Facie" takes the 2026 film alone; its cluster took the National Theatre's recording
    val d = decision(Seq(chichester, ntLive), 963568, nodes = Map(
      chichester.key -> node(Some(DecisionTrace.Alone(1127282, 0.91, "exact-top-hit")), Some(0.12)),
      ntLive.key     -> node(Some(DecisionTrace.Alone(963568, 0.95, "exact-top-hit")), Some(0.95))))
    val found = GroupingCoherence.of(d, "tmdb:963568", relay, Seq(chichester, ntLive), Year).findings
    found.map(_.kind) shouldBe Seq(Kind.Prefers)
    found.head.members shouldBe Seq(chichester.key)
    found.head.why should (include("tmdb:1127282") and include("91"))
    GroupingCoherence.of(d, "tmdb:963568", relay, Seq(chichester, ntLive), Year).flagged shouldBe true
  }

  it should "not be suspect when its own take of another film is unsure, or no surer than its own score of the cluster's" in {
    val unsure = decision(Seq(chichester, ntLive), 963568, nodes = Map(chichester.key -> node(Some(DecisionTrace.Alone(1127282, 0.45, "r")), Some(0.1))))
    GroupingCoherence.alone(unsure, Seq(chichester, ntLive)) shouldBe empty
    val close = decision(Seq(chichester, ntLive), 963568, nodes = Map(chichester.key -> node(Some(DecisionTrace.Alone(1127282, 0.8, "r")), Some(0.7))))
    GroupingCoherence.alone(close, Seq(chichester, ntLive)) shouldBe empty
  }

  it should "be suspect when its own evidence denies the cluster's film, weakly by the probability cut alone" in {
    def denied(cutOnly: Boolean) = decision(Seq(chichester, ntLive), 963568, nodes = Map(
      chichester.key -> node(denied = Some(DecisionTrace.Denied("a relay's record its own text contradicts", cutOnly)))))
    GroupingCoherence.alone(denied(cutOnly = false), Seq(chichester, ntLive)).map(f => f.kind -> f.weight) shouldBe Seq(Kind.Denies -> 1.0)
    GroupingCoherence.alone(denied(cutOnly = true), Seq(chichester, ntLive)).map(f => f.kind -> f.weight) shouldBe Seq(Kind.Denies -> 0.4)
  }

  it should "never question a pinned cluster: the curator's pin is the judgement" in {
    val pinned = decision(Seq(chichester, ntLive), 963568, ResolverDecision.Basis.Pinned,
      Map(chichester.key -> node(Some(DecisionTrace.Alone(1127282, 0.91, "r")), Some(0.1))))
    GroupingCoherence.alone(pinned, Seq(chichester, ntLive)) shouldBe empty
  }

  "the score" should "sum each kind's heaviest finding, and rank the most suspect first" in {
    val freyne = listing(KinoMuza, "Eternity", Some(2025), Some("David Freyne"))
    val meures = listing(KinoApollo, "Eternity", Some(2018), Some("Susanne Regina Meures"))
    val both   = coherence(Seq(freyne, meures), film("Eternity", 2025))
    both.findings.map(_.kind).toSet shouldBe Set(Kind.Year, Kind.Director)
    both.score shouldBe 2.0
    both.flagged shouldBe true
    val rec = film("Eternity", 2025)
    val takes = Seq(freyne, meures).map(l => SuspectTakes.Take(l, "tmdb:1", Some(1), rec, decision(Seq(freyne, meures), 1))) ++
      Seq(listing(Helios, "Lalka", Some(2026)), listing(Multikino, "Lalka", Some(2023))).pipe { ls =>
        ls.map(l => SuspectTakes.Take(l, "tmdb:2", Some(2), film("Lalka", 2026), decision(ls, 2))) }
    GroupingCoherence.corpus(takes, Year).map(_.film) shouldBe Seq("tmdb:1", "tmdb:2")
  }

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)
}
