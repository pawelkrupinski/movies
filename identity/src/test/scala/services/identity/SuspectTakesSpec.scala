package services.identity

import models.{Cinema, Helios, KinoApollo, KinoMuza, Multikino, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.SuspectTakes.{Billings, Signal, Take}

/** The suspect-take signals ([[SuspectTakes]]) on the wrong takes a capture surfaced on 2026-10-12, and on the right
 *  takes beside them that bill the same shapes. */
class SuspectTakesSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val Year = 2026
  private def film(title: String, year: Int, runtime: Option[Int] = None, directors: Seq[String] = Nil) =
    IdentityMeasures.Film(title, year = Some(year), runtime = runtime, directors = Some(directors))
  private def take(l: Listing, f: IdentityMeasures.Film, basis: ResolverDecision.Basis = ResolverDecision.Basis.OwnMatch,
                   tmdb: Int = 1, explanation: Seq[String] = Nil) =
    Take(l, s"tmdb:$tmdb", Some(tmdb), f, ResolverDecision(Seq(l.key), Some(tmdb), 0.9, basis, explanation)())
  private def signals(t: Take, billings: Billings = Billings.None): Set[Signal] = SuspectTakes.of(t, Year, billings).map(_.signal).toSet
  /** A corpus of `takes`. */
  private def corpus(takes: Take*): Billings = Billings.of(takes.map(_.listing), takes)
  private def billed(venue: Cinema, title: String, tmdb: Int, year: Int) = take(listing(venue, title), film(title, year), tmdb = tmdb)

  "a fragment" should "be an older film whose title is a piece of the billing beside another work's words" in {
    signals(take(listing(KinoMuza, "Giorgia Fumo - OUT OF OFFICE"), film("Out of Office", 2022))) should contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "The Tea Alchemist - World Premiere + Q&A"), film("World Premiere", 1941))) should contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "Jan Logemann LIVE: Herr Logemann hebt ab! -AUSVERKAUFT!-"), film("Ausverkauft!", 1999))) should
      contain(Signal.Fragment)
    // "A" is no event word: a tribute show's subtitle beside the 1992 film it shares a title with (convergence 38687290714, UK)
    signals(take(listing(KinoMuza, "Straight Talk: A Tribute to Dolly Parton"), film("Straight Talk", 1992))) should contain(Signal.Fragment)
    // a double bill names neither of its works alone
    signals(take(listing(KinoMuza, "The Gruffalo + The Gruffalo's Child"), film("The Gruffalo", 2009))) should contain(Signal.Fragment)
  }

  it should "be a film none of whose title's words the billing bills" in {
    signals(take(listing(KinoMuza, "AKW: Powoli"), film("The Proposal", 2009))) should contain(Signal.Fragment)
    // a respelling shares a word, a stage relay's record titles the house's words: the title relation's to weigh
    signals(take(listing(KinoMuza, "NT Live: Hayfever"), film("National Theatre Live: Hay Fever", 2027))) shouldNot contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "OPERA-MANON"), film("The Metropolitan Opera 2026/27: Manon", 2026))) shouldNot contain(Signal.Fragment)
  }

  it should "not be a current release, nor a film billed beside its programme's banner, an event, a format, its director or its other titles" in {
    signals(take(listing(KinoMuza, "HAMNET - Halte dein Herz offen"), film("Hamnet", 2025))) shouldNot contain(Signal.Fragment)
    val banner = corpus(billed(KinoMuza, "Akademia Kina Polskiego: Psy", 1, 1992), billed(KinoMuza, "Akademia Kina Polskiego: Krzyżacy", 2, 1960))
    signals(take(listing(KinoMuza, "Akademia Kina Polskiego: Psy"), film("Psy", 1992)), banner) shouldNot contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "Nigdy w życiu! + spotkanie z reżyserką"), film("Nigdy w życiu!", 2004))) shouldNot contain(Signal.Fragment)
    // a talk's whole phrase in a piece of its own words, "Q&A" — its letters no event words (convergence 38687290714, DE)
    signals(take(listing(KinoMuza, "SPECIAL: RASENDES GRÜN MIT PFERDEN (Anschließendes Q&A mit Regisseurin Ute Aurand)"),
      film("Rasendes Grün mit Pferden", 2021))) shouldNot contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "Lalka 2D NAPISY"), film("Lalka", 1968))) shouldNot contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "Konwicki: Salto"), film("Salto", 1965, directors = Seq("Tadeusz Konwicki")))) shouldNot contain(Signal.Fragment)
    signals(take(listing(KinoMuza, "Clockwork Orange - Uhrwerk Orange"), film("Uhrwerk Orange", 1971).copy(originalTitle = Some("A Clockwork Orange")))) shouldNot
      contain(Signal.Fragment)
  }

  "an era mismatch" should "be an old film whose title other billings take as a film of today, nothing billing it as old" in {
    val michael = corpus(billed(Helios, "Michael", 2, 2026), billed(Multikino, "Michael", 2, 2026))
    signals(take(listing(KinoMuza, "Michael"), film("Michael", 1924)), michael) should contain(Signal.Era)
    // an old film alone is the venue's programme: a classic, a Christmas favourite
    signals(take(listing(KinoMuza, "Die Hard"), film("Die Hard", 1988)), corpus(billed(Helios, "Die Hard", 1, 1988))) shouldNot contain(Signal.Era)
  }

  it should "be a film newer than the year the listing states, or older than it unless billed as a classic" in {
    signals(take(listing(KinoMuza, "Nosferatu", year = Some(1922)), film("Nosferatu", 2024))) should contain(Signal.Era)
    signals(take(listing(KinoMuza, "Lalka", year = Some(2026)), film("Lalka", 1968))) should contain(Signal.Era)
    signals(take(listing(KinoMuza, "Diabły | KLASYKA", year = Some(2026)), film("Diabły", 1971))) shouldNot contain(Signal.Era)
    // a festival's or a print's year beside the release's
    signals(take(listing(KinoMuza, "Podglądacz", year = Some(1957)), film("Podglądacz", 1960))) shouldNot contain(Signal.Era)
  }

  it should "not be an old film billed as one: silent, a classic, an anniversary, a re-release, its director" in {
    val newer = corpus(billed(Helios, "Nosferatu", 2, 2024), billed(Helios, "Quo vadis?", 3, 2001), billed(Helios, "Auta", 4, 2006),
      billed(Helios, "Salto", 5, 2020), billed(Helios, "Harry Angel", 6, 2026))
    signals(take(listing(KinoMuza, "Nosferatu"), film("Nosferatu", 1922)), newer) should contain(Signal.Era)
    signals(take(listing(KinoMuza, "Stummfilm mit Live-Musik: Nosferatu"), film("Nosferatu", 1922)), newer) shouldNot contain(Signal.Era)
    signals(take(listing(KinoMuza, "Dni Kina Niemego - Quo vadis?"), film("Quo Vadis?", 1913)), newer) shouldNot contain(Signal.Era)
    signals(take(listing(KinoMuza, "Auta - 20th anniversary"), film("Auta", 1960)), newer) shouldNot contain(Signal.Era)
    signals(take(listing(KinoMuza, "Konwicki Salto"), film("Salto", 1965, directors = Seq("Tadeusz Konwicki"))), newer) shouldNot contain(Signal.Era)
    signals(take(listing(KinoMuza, "Harry Angel"), film("Harry Angel", 1987).copy(releases = Some("PL2026-"))), newer) shouldNot contain(Signal.Era)
    // ...but a word of the film's own title bills it as nothing: "Rewind" taken for the 1971 namesake of a 2026 film
    signals(take(listing(KinoMuza, "Rewind"), film("Rewind", 1971)), corpus(billed(Helios, "Rewind", 2, 2026))) should contain(Signal.Era)
  }

  "a runtime mismatch" should "be a film far from the listing's running time, or a short taken for a feature or a title widely billed" in {
    signals(take(listing(KinoMuza, "Herr der Ringe", runtime = Some(263)), film("Herr der Ringe", 2025, runtime = Some(14)))) should contain(Signal.Runtime)
    val short = film("Herr der Ringe", 2025, runtime = Some(14))
    val wide  = corpus(Seq(KinoMuza, Helios, Multikino, Rialto).map(venue => take(listing(venue, "Herr der Ringe - Rückkehr des Königs (ext.)"), short, tmdb = 9))*)
    signals(take(listing(KinoApollo, "Herr der Ringe - Rückkehr des Königs (ext.)"), short), wide) should contain(Signal.Runtime)
    signals(take(listing(KinoApollo, "Herr der Ringe - Rückkehr des Königs (ext.)"), short)) shouldNot contain(Signal.Runtime)
    signals(take(listing(KinoMuza, "Dune", runtime = Some(155)), film("Dune", 2021, runtime = Some(90)))) should contain(Signal.Runtime)
    signals(take(listing(KinoMuza, "Dune", runtime = Some(155)), film("Dune", 1984, runtime = Some(137)))) shouldNot contain(Signal.Runtime)
    // a venue billing a short states the programme's time, not the film's
    signals(take(listing(KinoMuza, "World Champion", runtime = Some(36)), film("World Champion", 2021, runtime = Some(110)))) shouldNot contain(Signal.Runtime)
  }

  "a sequel mismatch" should "be a billing numbering another instalment, never a relay numbering its season" in {
    signals(take(listing(KinoMuza, "Begegnungen der Seele 2"), film("Begegnungen der Seele", 2024))) should contain(Signal.Sequel)
    signals(take(listing(KinoMuza, "Zwierzogród 2"), film("Zwierzogród 2", 2025))) shouldNot contain(Signal.Sequel)
    signals(take(listing(KinoMuza, "RBO: Cosi fan Tutte 27"), film("Royal Ballet & Opera 2026/27: Così fan tutte", 2027))) shouldNot contain(Signal.Sequel)
  }

  "the feed's own id" should "suspect a take on the stage's weaker evidence of another film than the feed links" in {
    val linked = listing(KinoMuza, "Michael").copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, "9695m")))
    signals(take(linked, film("Michael", 2026), ResolverDecision.Basis.Agreed, tmdb = 1)) should contain(Signal.FeedId)
    signals(take(linked, film("Michael", 2026), ResolverDecision.Basis.Agreed, tmdb = 9695)) shouldNot contain(Signal.FeedId)
    // the model's own take overrules a link on its evidence (FeedLink)
    signals(take(linked, film("Michael", 2026), tmdb = 1)) shouldNot contain(Signal.FeedId)
  }

  "the route" should "suspect a take a fill rule alone made" in {
    val filled = take(listing(KinoMuza, "Lalka"), film("Lalka", 2026), ResolverDecision.Basis.Filled,
      explanation = Seq("filled by venues.current: 'Lalka (2026)' tmdb:1 — guards passed: …"))
    SuspectTakes.of(filled, Year, Billings.None).filter(_.signal == Signal.WeakRoute).map(_.why) shouldBe Seq("filled by venues.current")
    signals(take(listing(KinoMuza, "Lalka"), film("Lalka", 2026))) shouldBe empty
  }

  "a take" should "be every listing of a decision with a film, its record, or a fallback's title and year" in {
    val a = listing(KinoMuza, "Lalka"); val b = listing(KinoMuza, "Lalka 2D")
    val took = ResolverDecision(Seq(a.key, b.key), Some(7), 0.9, ResolverDecision.Basis.OwnMatch, Nil)()
    val fell = ResolverDecision(Seq(listing(KinoMuza, "Inna").key), None, 0.9, ResolverDecision.Basis.BelowThreshold, Nil,
      fallback = Some(ResolverDecision.Fallback("imdb", "tt0064570", 0.9, Some("Lalka"), Some(1968))))()
    val none = ResolverDecision(Seq(listing(KinoMuza, "Nic").key), None, 0.9, ResolverDecision.Basis.NoCandidate, Nil)()
    val byKey = Seq(a, b, listing(KinoMuza, "Inna"), listing(KinoMuza, "Nic")).map(l => l.key -> l).toMap
    val takes = SuspectTakes.takes(Seq(took, fell, none), byKey.get, id => Option.when(id == 7)(film("Lalka", 2026)))
    takes.map(t => (t.listing.rawTitle, t.film, t.record.year)) shouldBe Seq(("Lalka", "tmdb:7", Some(2026)), ("Lalka 2D", "tmdb:7", Some(2026)),
      ("Inna", "imdb:tt0064570", Some(1968)))
  }
}
