package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicReference

/** The shared business logic of the rules cache, against the in-memory fake repo
 *  and a capturing `install` sink — so the spec never touches the process-global
 *  `TitleNormalizer` and can't race other suites. */
class TitleRulesCacheSpec extends AnyFlatSpec with Matchers {

  private def capturing(): (TitleRuleSet => Unit, () => TitleRuleSet) = {
    val ref = new AtomicReference[TitleRuleSet](TitleRuleSet.empty)
    ((rs: TitleRuleSet) => ref.set(rs), () => ref.get())
  }

  "reload" should "install the store's rules, replacing the defaults" in {
    val (install, last) = capturing()
    val repo = new InMemoryTitleRulesRepo(Seq(
      TitleRule("custom", RuleScope.GlobalStructural, None, " ZZZ$", "", applyAll = false, order = 10)))
    new TitleRulesCache(repo, install = install).reload()
    last().structural("Film ZZZ") shouldBe "Film"
    // The default anniversary strip is absent under the custom-only set.
    last().structural("Top Gun 40th Anniversary") shouldBe "Top Gun 40th Anniversary"
  }

  it should "fall back to the in-code defaults when the store is empty" in {
    val (install, last) = capturing()
    new TitleRulesCache(new InMemoryTitleRulesRepo(), install = install).reload()
    last().structural("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  "start with seedIfEmpty" should "populate an empty enabled store with the defaults and install them" in {
    val (install, last) = capturing()
    val repo = new InMemoryTitleRulesRepo()
    repo.findAll() shouldBe empty
    val cache = new TitleRulesCache(repo, seedIfEmpty = true, install = install)
    try cache.start() finally cache.stop()
    repo.findAll().map(_.id) should contain("structural-anniversary-suffix")
    repo.findAll().size shouldBe TitleRuleDefaults.all.size
    last().structural("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  "start without seedIfEmpty" should "leave an empty store untouched" in {
    val (install, _) = capturing()
    val repo = new InMemoryTitleRulesRepo()
    val cache = new TitleRulesCache(repo, seedIfEmpty = false, install = install)
    try cache.start() finally cache.stop()
    repo.findAll() shouldBe empty
  }

  "onRulesChanged" should "fire on a real change but NOT on the first load" in {
    val (install, _) = capturing()
    val repo = new InMemoryTitleRulesRepo()
    var fired = 0
    val cache = new TitleRulesCache(repo, install = install, onRulesChanged = () => fired += 1)

    cache.reload()            // first load (defaults) — must NOT fire
    fired shouldBe 0
    cache.reload()            // unchanged — must NOT fire
    fired shouldBe 0
    repo.upsert(TitleRule("new", RuleScope.Search, None, "x$", "", applyAll = false, order = 1))
    cache.reload()            // changed — fires once
    fired shouldBe 1
    cache.reload()            // unchanged again — no further fire
    fired shouldBe 1
  }
}
