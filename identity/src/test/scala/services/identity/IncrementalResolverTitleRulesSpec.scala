package services.identity

import models.{Country, Helios}
import services.identity.measures.Listing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer
import services.titles.TitleNormalizer
import services.titles.rules.{RuleScope, TitleRule, TitleRuleKey, TitleRuleSet}
import tools.IndependentCases

import scala.util.Random
import services.identity.vocabulary.TitleDecorations

/** The incremental model restored after its country's title rules changed, nothing else of its rules version moving:
 *  it decides as a whole resolve under the new rules -- the takes and families, whatever kind of rule moved -- and
 *  re-resolves only the families that read what moved (TitleRuleChange), none for an edit no title reads. */
class IncrementalResolverTitleRulesSpec extends AnyFlatSpec with Matchers {

  private val before      = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds       = 1L to 16L

  /** `before` with its rules edited by `edit`, as a deploy of another title-rule table builds it. */
  private def edited(edit: Seq[TitleRule] => Seq[TitleRule]): TitleNormalizer =
    new TitleNormalizer(TitleRuleSet(edit(before.rules.rules)), facts = before.facts, country = before.country)

  private def rule(id: String, scope: RuleScope, pattern: String, replacement: String = "", cinema: Option[String] = None) =
    TitleRule(id, scope, cinema, pattern, replacement, applyAll = false, order = Int.MaxValue, countries = Some(Set(Country.Poland)))

  /** A query rule taking off the release-night prefix "Oficjalna premiera: " the corpus bills: dropped, those listings
   *  search as billed, their search titles and search forms moving. */
  private val premiereDropped = edited(_.filterNot(_.id == "xtra-premiere-prefix"))
  /** A key rule folding "It Ends with Us" into "It Ends": the two films' titles share a key form -- a film's title a
   *  family reads off its candidates, where no listing bills it, read otherwise. */
  private val endsFolded = edited(_ :+ rule("spec-ends-folded", RuleScope.Canonical, "(?iu)^It Ends with Us$", "It Ends"))
  /** One venue's rule taking off the "(2026)" a re-release bills: what that venue's listings fire, as their traces name. */
  private val heliosYear = edited(_ :+ rule("spec-helios-year", RuleScope.PerCinema, "\\s*\\(2026\\)$", cinema = Some(TitleRuleKey.of(Helios))))
  /** A query rule for a billing no title of the corpus carries. */
  private val unread = edited(_ :+ rule("spec-unread", RuleScope.GlobalStructural, "(?iu)^Zzyzx Festival:\\s*"))
  /** Every rule's note rewritten and its order spaced out, as a table edit elsewhere renumbers them: nothing folds otherwise. */
  private val renumbered = edited(_.map(r => r.copy(note = Some("edited"), order = r.order * 3 + 7)))

  /** A listing as the archive builds it under `normalizer`: the search titles its rules ask ([[Listing.of]]). */
  private def built(listing: Listing, normalizer: TitleNormalizer): Listing = {
    val (search, proper) = Listing.searchTitlesOf(listing.title, normalizer)
    listing.copy(searchTitle = search, properTitle = proper)
  }

  private def model(lookups: IdentityLookups, normalizer: TitleNormalizer, store: IdentityModelStore,
                    mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None) =
    new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, rules = "v1",
      mutation = mutation)

  /** A model takes half a seed's events into a store under `before`; the rest happen while it is down, and it is
   *  restored under `after`, every held listing built again under it. Whether it then decides as a whole resolve under
   *  `after`, how many families it re-resolved, and how many a whole resolve holds. */
  private def restart(seed: Long, after: TitleNormalizer, mutation: IncrementalResolver.Mutation,
                      moved: Listing => Listing): (Boolean, Int, Int) = {
    val corpus   = GeneratedIdentityCorpus.generate(seed, before, films = 12, listings = 48)
    val lookups  = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val store    = new InMemoryIdentityModelStore
    val random   = new RandomIdentityEvents(corpus.listings.map(built(_, before)), lookups, seed, steps = 60)
    val events   = random.events
    val first    = model(lookups, before, store)
    events.take(30).foreach {
      case IdentityEvent.Seen(listings) => first.batch(seen = listings)
      case IdentityEvent.Gone(keys)     => first.batch(gone = keys)
      case IdentityEvent.Answered(c)    => first.batch(answered = c)
    }
    events.foreach(_ => ())
    val held     = random.held.values.toSeq.map(listing => moved(built(listing, after)))
    val restored = model(lookups, after, store, mutation)
    restored.restore(held)
    val expected = IdentityResolver.resolveWith(held, lookups, after, calibration, IdentityResolver.Mutation.None)
    store.titleRules shouldBe Some(TitleRuleRecord.of(after.rules))
    (ResolutionSignature.of(restored) == ResolutionSignature.of(expected), restored.familiesResolved, expected.families)
  }

  /** A model takes a seed's whole corpus into a store under `before`; nothing moves while it is down but the rules, and
   *  it is restored under `after`. As [[restart]]'s. */
  private def rulesOnly(seed: Long, after: TitleNormalizer, mutation: IncrementalResolver.Mutation,
                        moved: Listing => Listing): (Boolean, Int, Int) = {
    val corpus   = GeneratedIdentityCorpus.generate(seed, before, films = 12, listings = 48)
    val store    = new InMemoryIdentityModelStore
    model(corpus.lookups, before, store).batch(seen = corpus.listings.map(built(_, before)))
    val held     = corpus.listings.map(listing => moved(built(listing, after)))
    val restored = model(corpus.lookups, after, store, mutation)
    restored.restore(held)
    val expected = IdentityResolver.resolveWith(held, corpus.lookups, after, calibration, IdentityResolver.Mutation.None)
    (ResolutionSignature.of(restored) == ResolutionSignature.of(expected), restored.familiesResolved, expected.families)
  }

  /** The seeds `scenario` decides otherwise than a whole resolve in, for each of [[restart]] and [[rulesOnly]]. */
  private def diverging(after: TitleNormalizer, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None,
                        moved: Listing => Listing = identity): Seq[(String, Long)] =
    IndependentCases.flatMap(Seeds)(seed => Seq("restart" -> restart(seed, after, mutation, moved), "rules only" -> rulesOnly(seed, after, mutation, moved))
      .collect { case (scenario, (false, _, _)) => scenario -> seed })

  /** Over every seed, the families [[rulesOnly]] re-resolves and those a whole resolve holds. */
  private def resolvedOf(after: TitleNormalizer, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None,
                         moved: Listing => Listing = identity): (Int, Int) =
    IndependentCases.map(Seeds)(seed => rulesOnly(seed, after, mutation, moved)).foldLeft((0, 0)) {
      case ((r, h), (_, resolves, families)) => (r + resolves, h + families) }

  "a model restored under other title rules" should "decide as a whole resolve under them, a query rule dropped" in {
    diverging(premiereDropped) shouldBe empty
  }

  it should "decide as a whole resolve under them, a key rule added that folds two films' titles" in {
    diverging(endsFolded) shouldBe empty
  }

  it should "decide as a whole resolve under them, a venue's own rule added" in {
    diverging(heliosYear) shouldBe empty
  }

  it should "re-resolve some families, and fewer than a whole resolve holds, when a rule reaches some titles" in {
    Seq(premiereDropped, endsFolded, heliosYear).map(resolvedOf(_)).foreach { case (resolved, held) =>
      resolved should be > 0
      resolved should be < held
    }
  }

  it should "re-resolve nothing for a rule no title reads, nor after a restart under the same rules" in {
    resolvedOf(unread)._1 shouldBe 0
    resolvedOf(before)._1 shouldBe 0
  }

  it should "record rules that fold alike alike, so a renumbering or a note re-resolves nothing beyond the same rules'" in {
    TitleRuleRecord.of(renumbered.rules) shouldBe TitleRuleRecord.of(before.rules)
    TitleRuleRecord.of(unread.rules) should not be TitleRuleRecord.of(before.rules)
    resolvedOf(renumbered)._1 shouldBe 0
  }

  it should "rebuild the normaliser its record names, reading every title as the rules recorded" in {
    val again = TitleRuleRecord.normalizer(TitleRuleRecord.of(premiereDropped.rules), before).get
    TitleRuleRecord.of(again.rules) shouldBe TitleRuleRecord.of(premiereDropped.rules)
    Seq("Oficjalna premiera: Lalka", "Filmowy Klub Seniora - Belle", "Gwiezdne Wojny: Nowa Nadzieja").foreach { title =>
      again.searchQuery(title) shouldBe premiereDropped.searchQuery(title)
      again.sanitize(title) shouldBe premiereDropped.sanitize(title)
    }
    TitleRuleRecord.normalizer("not a record", before) shouldBe None
  }

  /** A seed's corpus taken into a store under `before`, its stored families then edited by `stored`, and restored under
   *  `unread` (a rule no title reads, so every check but the edited one passes): the families re-resolved, and how many
   *  the store held. */
  private def tampered(stored: StoredFamily => StoredFamily, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None): (Int, Int) = {
    val corpus = GeneratedIdentityCorpus.generate(6L, before, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    model(corpus.lookups, before, store).batch(seen = corpus.listings)
    val families = store.families()
    store.replace(Set.empty, families.map(stored))
    val again  = model(corpus.lookups, unread, store, mutation)
    again.restore(corpus.listings)
    (again.familiesResolved, families.size)
  }

  // What a family read through the title forms that the corpus context never reads -- a candidate film's title no listing
  // bills, a listing's original title -- only the recorded titles tell. Here one family's are said to hold a title the
  // rule moved.
  it should "re-resolve a family that read a title through the title forms whose form moved, and be caught when it does not (the teeth)" in {
    val billed = (f: StoredFamily) => f.copy(titles = f.titles.map(t => t.copy(searches = t.searches + "Zzyzx Festival: Lalka")))
    tampered(identity)._1 shouldBe 0
    val (resolved, held) = tampered(billed)
    resolved should (be > 0 and be >= held)
    tampered(billed, IncrementalResolver.Mutation.IgnoreTitleForms)._1 shouldBe 0
  }

  // The archive builds every listing's fields by the rules; a member whose fields moved outside its evidence node reads
  // otherwise. Here every family's members are said to have read otherwise when it was stored.
  it should "re-resolve a family whose members read otherwise than when it was stored, and be caught when it does not (the teeth)" in {
    val (resolved, held) = tampered(_.copy(members = Some(0L)))
    resolved should be >= held
    tampered(_.copy(members = Some(0L)), IncrementalResolver.Mutation.IgnoreMembers)._1 shouldBe 0
    // a search title moved, as a query rule moves it, reads as another member
    val searchedOtherwise = (l: Listing) => l.copy(searchTitle = Some(s"${l.title} otherwise"))
    IndependentCases.map(Seeds)(seed => rulesOnly(seed, unread, IncrementalResolver.Mutation.None, searchedOtherwise))
      .collect { case (agrees, resolved, held) if !agrees || resolved < held => (resolved, held) } shouldBe empty
  }

  it should "re-resolve a family stored before its title forms were kept" in {
    val corpus = GeneratedIdentityCorpus.generate(3L, before, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    model(corpus.lookups, before, store).batch(seen = corpus.listings)
    store.replace(Set.empty, store.families().map(_.copy(titles = None)))
    val again  = model(corpus.lookups, unread, store)
    again.restore(corpus.listings)
    again.familiesResolved shouldBe IdentityResolver.resolveWith(corpus.listings, corpus.lookups, unread, calibration, IdentityResolver.Mutation.None).families
  }

  it should "re-resolve every family when the store holds no title rules it can read" in {
    val corpus = GeneratedIdentityCorpus.generate(4L, before, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    model(corpus.lookups, before, store).batch(seen = corpus.listings)
    store.recordTitleRules("not a record")
    val again  = model(corpus.lookups, before, store)
    again.restore(corpus.listings)
    val whole  = IdentityResolver.resolveWith(corpus.listings, corpus.lookups, before, calibration, IdentityResolver.Mutation.None)
    again.familiesResolved should be >= whole.families
    ResolutionSignature.of(again) shouldBe ResolutionSignature.of(whole)
  }

  "the rules version" should "digest the relay rows, which the resolver reads outside the country's normaliser" in {
    val version = IncrementalResolver.rulesVersion("code", calibration, TitleDecorations.None, vocabulary.PinConstraints.None)
    version should endWith (s":${TitleRuleRecord.relayVersion}")
    TitleRuleRecord.relayVersion should fullyMatch regex "[0-9a-f]+"
  }
}
