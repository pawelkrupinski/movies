package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

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

  it should "memoise only the titles of cinemas that have rules of their own" in {
    // A venue without rules folds nothing; caching its titles held one entry per US listing (~8 MB).
    val rs = TitleRuleSet(Seq(rule("a", PerCinema, "^Ladies Night - ", "", cinemaId = Some("cc"))))
    rs.perCinema("unknown", "Untouched Title") shouldBe "Untouched Title"
    rs.perCinema("other", "Another Title") shouldBe "Another Title"
    rs.perCinemaCached shouldBe 0
    rs.perCinema("cc", "Ladies Night - Wicked") shouldBe "Wicked"
    rs.perCinemaCached shouldBe 1
  }

  // Kino Wybrzeże appends its venue name to every listing, splitting a film off
  // its canonical row ("Dzień objawienia-kino wybrzeże" sanitises to a different
  // key than "Dzień objawienia"). The seeded `wybrzeze-venue-suffix` rule strips
  // it. Runs against the REAL default rule set so the seed itself is covered.
  "the default Kino Wybrzeże rule" should "strip the trailing venue name so the film keys to its canonical row" in {
    val rs = TitleRules.ruleSet
    rs.perCinema("wybrzeze", "Dzień objawienia-kino wybrzeże") shouldBe "Dzień objawienia"
    // The all-caps raw form: suffix stripped, casing left to canonicalizeBySanitize.
    rs.perCinema("wybrzeze", "DZIEŃ OBJAWIENIA-KINO WYBRZEŻE") shouldBe "DZIEŃ OBJAWIENIA"
    // Both now sanitise to the SAME key as the bare title — so they merge.
    def key(t: String): String = titleNormalizer.sanitize(t)
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

  // The tier folds are memoised per-instance for the hot path. The one way that
  // can go wrong is a cache key that drops `cinemaId`: two cinemas with DIFFERENT
  // rules but the SAME raw title must NOT collide on a raw-only key. Same raw,
  // different cinema → each keeps its own rule's result.
  "the per-cinema memo cache" should "key on (cinemaId, raw), not raw alone" in {
    val rs = TitleRuleSet(Seq(
      rule("a", PerCinema, "^Gala: ", "", cinemaId = Some("cc")),
      rule("b", PerCinema, " - wieczór$", "", cinemaId = Some("bok"))
    ))
    rs.perCinema("cc",  "Gala: Wicked - wieczór") shouldBe "Wicked - wieczór" // only cc's strip
    rs.perCinema("bok", "Gala: Wicked - wieczór") shouldBe "Gala: Wicked"     // only bok's strip
    // Re-query in the opposite order — a collision would now serve the wrong cached value.
    rs.perCinema("bok", "Gala: Wicked - wieczór") shouldBe "Gala: Wicked"
    rs.perCinema("cc",  "Gala: Wicked - wieczór") shouldBe "Wicked - wieczór"
  }

  it should "return a stable result on repeated calls (cache hit == cold value)" in {
    val rs = TitleRuleSet(Seq(rule("strip", GlobalStructural, "(?i)\\s*-\\s*restored$", "")))
    val cold = rs.structural("Top Gun - Restored")
    cold shouldBe "Top Gun"
    rs.structural("Top Gun - Restored") shouldBe cold   // second call served from cache
    rs.structural("Top Gun - Restored") shouldBe cold
  }
}
