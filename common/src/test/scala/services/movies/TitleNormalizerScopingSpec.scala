package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRules, TitleRuleSet}

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

/** Regression guard for the cross-suite title-rules flake. ScalaTest runs suites
 *  in parallel; a test installing a custom rule set used to swap a global var, so
 *  while it held the swap a suite running BESIDE it saw the foreign rules and an
 *  unrelated `sanitize`-based assertion failed intermittently (observed once in
 *  `StagingStepsSpec`). `withRules` now scopes the install to the installing
 *  thread; this proves a concurrent thread is unaffected. */
class TitleNormalizerScopingSpec extends AnyFlatSpec with Matchers {

  private val title = "Testfilm Zzdropzz"
  // A Canonical-tier rule that strips the "Zzdropzz" suffix, so the title
  // sanitises to a DIFFERENT key under it than under the defaults.
  private val dropRule = TitleRule("test-zzdrop", RuleScope.Canonical, None,
    """(?i)\s*Zzdropzz\s*$""", "", applyAll = false, order = 100)
  private val customSet = TitleRuleSet(TitleRules.all :+ dropRule)

  "withRules" should "scope a custom rule set to the installing thread, invisible to a concurrent thread" in {
    val defaultKey = TitleNormalizer.sanitize(title)                  // no scope → defaults
    // Sanity: the rule really does change the key when active on the calling thread.
    TitleNormalizer.withRules(customSet)(TitleNormalizer.sanitize(title)) should not be defaultKey

    val installed = new CountDownLatch(1)
    val release   = new CountDownLatch(1)
    val onThreadA = new AtomicReference[String]()
    val other = new Thread(() =>
      TitleNormalizer.withRules(customSet) {
        onThreadA.set(TitleNormalizer.sanitize(title))               // A sees the custom rule
        installed.countDown()
        release.await()                                              // hold the scope OPEN
      })
    other.start()
    installed.await()
    // While thread A holds the custom scope, THIS thread must still see the
    // defaults — the property that was false under the old global swap.
    TitleNormalizer.sanitize(title) shouldBe defaultKey
    release.countDown()
    other.join()
    onThreadA.get should not be defaultKey                           // A really had the custom rule active
  }

  it should "restore the prior scope after the body returns" in {
    val defaultKey = TitleNormalizer.sanitize(title)
    TitleNormalizer.withRules(customSet)(())
    TitleNormalizer.sanitize(title) shouldBe defaultKey               // no leak past the scope
  }

  // Guards the `sanitize` memo cache: it's keyed on the raw title and scoped to the
  // GLOBAL rule set, so a thread-local `withRules` override must BYPASS it (never
  // serve the cached global value, never write the scoped value back). A naive
  // `computeIfAbsent(title, …)` without the bypass fails both assertions below.
  "the sanitize memo cache" should "be bypassed under a scoped rule set and never poisoned by it" in {
    val defaultKey = TitleNormalizer.sanitize(title)                  // prime the global cache
    TitleNormalizer.withRules(customSet)(TitleNormalizer.sanitize(title)) should not be defaultKey  // scope bypasses the cached default
    TitleNormalizer.sanitize(title) shouldBe defaultKey               // the scoped call didn't overwrite the global cache
  }

  /** The canonical " & " → " i " unification is POLISH ("i" = "and"). It used to
   *  run for every country, so the German film reported by CinemaxX Würzburg as
   *  "Minions & Monster" (see the checked-in Filmstarts capture,
   *  `test/resources/fixtures/webedia-de/.../theater-A0263/d-2026-07-11/p-1.json`)
   *  was stored — and SERVED to German users — as "Minions i Monster", keyed
   *  `minionsimonster`. No German cinema slot can ever produce that key, so the
   *  row's key and its own cinemas' spellings disagreed permanently. */
  "the Polish ' & ' → ' i ' unification" should "not touch a German title" in {
    val de = TitleRuleSet.forCountry(Country.Germany)
    TitleNormalizer.withRules(de) {
      TitleNormalizer.sanitize("Minions & Monster") shouldBe "minionsmonster"
    }
  }

  it should "still apply in Poland, where 'i' IS the conjunction" in {
    val pl = TitleRuleSet.forCountry(Country.Poland)
    TitleNormalizer.withRules(pl) {
      // "Mandalorian & Grogu" and "Mandalorian i Grogu" must keep merging.
      TitleNormalizer.sanitize("Mandalorian & Grogu") shouldBe
        TitleNormalizer.sanitize("Mandalorian i Grogu")
    }
  }
}
