package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for the rule-set abstraction itself — the behaviours the migration
 *  golden (which only exercises the default tiers) doesn't reach: per-cinema
 *  application, disabled / invalid-pattern no-ops, ordering, and the
 *  install/reset swap on `TitleNormalizer`. */
class TitleRuleSetSpec extends AnyFlatSpec with Matchers {
  import RuleScope._

  private def rule(id: String, scope: RuleScope, pattern: String, repl: String,
                   applyAll: Boolean = false, order: Int = 10, enabled: Boolean = true,
                   cinemaId: Option[String] = None, tag: Option[String] = None) =
    TitleRule(id, scope, cinemaId, pattern, repl, applyAll, order, enabled = enabled, tag = tag)

  "perCinema" should "apply only the rules scoped to that cinema, in order, then trim" in {
    val rs = TitleRuleSet(Seq(
      rule("a", PerCinema, "^Ladies Night - ", "", cinemaId = Some("cc"), order = 10),
      rule("b", PerCinema, " - powrót do kin$", "", cinemaId = Some("cc"), order = 20),
      rule("c", PerCinema, """\s*\|\s*""", ": ", applyAll = true, cinemaId = Some("bok"), order = 10)
    ))
    rs.perCinema("cc", "Ladies Night - Wicked - powrót do kin") shouldBe "Wicked"
    rs.perCinema("bok", "Diuna | CZ.2") shouldBe "Diuna: CZ.2"
    rs.perCinema("unknown", "Untouched Title") shouldBe "Untouched Title"
  }

  // Kino Wybrzeże appends its venue name to every listing, splitting a film off
  // its canonical row ("Dzień objawienia-kino wybrzeże" sanitises to a different
  // key than "Dzień objawienia"). The seeded `wybrzeze-venue-suffix` rule strips
  // it. Runs against the REAL default rule set so the seed itself is covered.
  "the default Kino Wybrzeże rule" should "strip the trailing venue name so the film keys to its canonical row" in {
    val rs = TitleRuleDefaults.ruleSet
    rs.perCinema("wybrzeze", "Dzień objawienia-kino wybrzeże") shouldBe "Dzień objawienia"
    // The all-caps raw form: suffix stripped, casing left to canonicalizeBySanitize.
    rs.perCinema("wybrzeze", "DZIEŃ OBJAWIENIA-KINO WYBRZEŻE") shouldBe "DZIEŃ OBJAWIENIA"
    // Both now sanitise to the SAME key as the bare title — so they merge.
    def key(t: String): String = services.movies.TitleNormalizer.sanitize(t)
    key(rs.perCinema("wybrzeze", "Dzień objawienia-kino wybrzeże")) shouldBe key("Dzień objawienia")
    key(rs.perCinema("wybrzeze", "DZIEŃ OBJAWIENIA-KINO WYBRZEŻE")) shouldBe key("Dzień objawienia")
    // A title without the suffix is untouched.
    rs.perCinema("wybrzeze", "Inny film") shouldBe "Inny film"
  }

  // ── placeholders: a rule referencing {{SEP}} expands before it compiles ─────
  "a rule using {{SEP}}" should "match any banner separator with optional spaces" in {
    val rs = TitleRuleSet(Seq(rule("dkf", GlobalStructural, """(?i){{SEP}}DKF\b.*$""", "")))
    rs.structural("Ojczyzna | DKF KOT")  shouldBe "Ojczyzna"   // pipe
    rs.structural("Ojczyzna - DKF III W") shouldBe "Ojczyzna"  // hyphen
    rs.structural("Ojczyzna_DKF")         shouldBe "Ojczyzna"  // underscore, no spaces
    rs.structural("Ojczyzna : DKF")       shouldBe "Ojczyzna"  // colon
  }

  it should "be valid (NOT surface in invalidRules) — the raw {{SEP}} expands to a real regex" in {
    val rs = TitleRuleSet(Seq(rule("dkf", GlobalStructural, """{{SEP}}DKF$""", "")))
    rs.invalidRules shouldBe empty
  }

  "a rule referencing an UNKNOWN placeholder" should "surface in invalidRules with its RAW pattern" in {
    val rs = TitleRuleSet(Seq(rule("oops", GlobalStructural, """{{NOPE}}DKF$""", "")))
    rs.invalidRules.map(_.id)      shouldBe Seq("oops")
    rs.invalidRules.map(_.pattern) shouldBe Seq("""{{NOPE}}DKF$""")   // raw token, for the editor
    rs.structural("x {{NOPE}}DKF") shouldBe "x {{NOPE}}DKF"           // no-op, didn't throw
  }

  "a disabled rule" should "be a no-op" in {
    val rs = TitleRuleSet(Seq(rule("x", GlobalStructural, "^Strip ", "", enabled = false)))
    rs.structural("Strip Me") shouldBe "Strip Me"
  }

  "an invalid pattern" should "be a no-op and surface in invalidRules" in {
    val bad = rule("bad", GlobalStructural, "(unclosed", "")
    val rs = TitleRuleSet(Seq(bad))
    rs.structural("(unclosed group title") shouldBe "(unclosed group title"
    rs.invalidRules.map(_.id) shouldBe Seq("bad")
  }

  // A replacement with a bare `$` (or trailing `\`) is, to Java's Matcher, an
  // illegal group reference and throws IllegalArgumentException mid-replace. Like
  // an invalid pattern, it must degrade to a no-op rather than take down the
  // normalisation hot path and the admin "affected" preview. See Sentry KINOWO-Y.
  "an invalid replacement" should "be a no-op rather than throw (replaceFirstIn)" in {
    val rs = TitleRuleSet(Seq(rule("price", GlobalStructural, "Me", "$")))
    noException should be thrownBy rs.structural("Strip Me")
    rs.structural("Strip Me") shouldBe "Strip Me"
  }

  it should "be a no-op rather than throw (replaceAllIn)" in {
    val rs = TitleRuleSet(Seq(rule("price", GlobalStructural, "x", "$9.99 $", applyAll = true)))
    noException should be thrownBy rs.structural("xx")
    rs.structural("xx") shouldBe "xx"
  }

  it should "still honour valid group references" in {
    val rs = TitleRuleSet(Seq(rule("grp", GlobalStructural, """(\d+)D""", "$1 D", applyAll = true)))
    rs.structural("Avatar 3D") shouldBe "Avatar 3 D"
  }

  "ordering" should "respect the order field (lower runs first)" in {
    // Rule 1 turns "AB" → "B" (strip A); rule 2 turns "B" → "" (strip B). Order
    // matters only in that both must run; assert the composed result.
    val rs = TitleRuleSet(Seq(
      rule("second", GlobalStructural, "B$", "", order = 20),
      rule("first", GlobalStructural, "^A", "", order = 10)
    ))
    rs.structural("AB") shouldBe ""
  }

  "programmePrefix" should "extract only tagged GlobalStructural rules' prefixes" in {
    val rs = TitleRuleSet(Seq(
      rule("prog", GlobalStructural, "(?i)^Klub: ", "", tag = Some("programmePrefix")),
      rule("other", GlobalStructural, """\s*\(AD\)$""", "")  // untagged, must not be extracted
    ))
    rs.programmePrefix("Klub: Vertigo") shouldBe Some("Klub: ")
    rs.programmePrefix("Vertigo (AD)") shouldBe None
  }

  "perCinema with no rules for a key" should "be an identity transform" in {
    TitleRuleSet.empty.perCinema("anything", "Untouched - X") shouldBe "Untouched - X"
  }

  // ── transientAffected: the per-rule "affected films" preview ───────────────
  private val previewSet = TitleRuleSet(Seq(
    rule("g-restored", GlobalStructural, "(?i)\\s*-\\s*restored$", ""),
    rule("g-noop",     GlobalStructural, "(?i)\\s*-\\s*director's cut$", ""),
    rule("s-klub",     GlobalStructural, "(?i)^Klub:\\s*", "", order = 30),
    rule("c-amp",      Canonical, " & ", " i ", applyAll = true),
    rule("p-strip",    PerCinema, "^X ", "", cinemaId = Some("cc"))
  ))
  private val previewTitles = Seq("Top Gun - Restored", "Top Gun", "Klub: Vertigo", "Batman & Robin")
  private def affectedFor(id: String) =
    previewSet.transientAffected(previewTitles).find(_.ruleId == id).get

  "transientAffected" should "only cover the scopes that don't rewrite the stored record" in {
    previewSet.transientAffected(previewTitles).map(_.ruleId) should contain theSameElementsAs
      Seq("g-restored", "g-noop", "s-klub")     // no Canonical, no PerCinema
  }

  it should "credit a structural rule the exact corpus titles it rewrites, with the result" in {
    val a = affectedFor("g-restored")
    a.scope shouldBe GlobalStructural
    a.changes shouldBe Seq(TitleRuleSet.Change("Top Gun - Restored", "Top Gun")) // not the bare "Top Gun"
  }

  it should "credit a search rule its own strip" in {
    affectedFor("s-klub").changes shouldBe Seq(TitleRuleSet.Change("Klub: Vertigo", "Vertigo"))
  }

  it should "leave a rule that matches nothing in the corpus with an empty change list" in {
    affectedFor("g-noop").changes shouldBe empty
  }

  it should "attribute each step to its own rule when several rules in a tier fire on one title" in {
    val rs = TitleRuleSet(Seq(
      rule("a", GlobalStructural, "^A ", "", order = 10),
      rule("b", GlobalStructural, " B$", "", order = 20)))
    val byId = rs.transientAffected(Seq("A Film B")).map(a => a.ruleId -> a.changes).toMap
    byId("a") shouldBe Seq(TitleRuleSet.Change("A Film B", "Film B")) // original, after rule a
    byId("b") shouldBe Seq(TitleRuleSet.Change("A Film B", "Film"))   // original, after rule b
  }

  // ── transientTierAffected: the tier-level "all affected films" rollup ───────
  private def tierFor(scope: RuleScope) =
    previewSet.transientTierAffected(previewTitles).find(_.scope == scope).get

  "transientTierAffected" should "only cover the scopes that don't rewrite the stored record" in {
    previewSet.transientTierAffected(previewTitles).map(_.scope) should contain theSameElementsAs
      Seq(GlobalStructural)     // no Canonical, no PerCinema
  }

  it should "map each affected film to its FINAL form after the whole tier folds, dropping untouched titles" in {
    // "Top Gun" is untouched → omitted; "Batman & Robin" is Canonical-only → not
    // in the structural tier. Only the two structural strips remain.
    tierFor(GlobalStructural).changes should contain theSameElementsAs Seq(
      TitleRuleSet.Change("Top Gun - Restored", "Top Gun"),
      TitleRuleSet.Change("Klub: Vertigo", "Vertigo"))
  }

  it should "collapse a chain of rules on one title to a single original → final pair" in {
    val rs = TitleRuleSet(Seq(
      rule("a", GlobalStructural, "^A ", "", order = 10),
      rule("b", GlobalStructural, " B$", "", order = 20)))
    rs.transientTierAffected(Seq("A Film B")).find(_.scope == GlobalStructural).get.changes shouldBe
      Seq(TitleRuleSet.Change("A Film B", "Film")) // net of both rules, not one pair per rule
  }
}
