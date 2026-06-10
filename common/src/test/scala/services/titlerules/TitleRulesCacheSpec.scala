package services.titlerules

import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/** The shared business logic of the rules cache, exercised against the in-memory
 *  fake repo (no Mongo): install-from-store, fall-back-to-defaults, and the
 *  worker's seed-if-empty. */
class TitleRulesCacheSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  override def afterEach(): Unit = TitleNormalizer.resetToDefaults()

  "reload" should "install the store's rules, replacing the defaults" in {
    val repo = new InMemoryTitleRulesRepo(Seq(
      TitleRule("custom", RuleScope.GlobalStructural, None, " ZZZ$", "", applyAll = false, order = 10)))
    new TitleRulesCache(repo).reload()
    TitleNormalizer.searchTitle("Film ZZZ") shouldBe "Film"
    // The default anniversary strip is absent under the custom-only set.
    TitleNormalizer.searchTitle("Top Gun 40th Anniversary") shouldBe "Top Gun 40th Anniversary"
  }

  it should "fall back to the in-code defaults when the store is empty" in {
    TitleNormalizer.installRules(TitleRuleSet.empty) // pollute
    new TitleRulesCache(new InMemoryTitleRulesRepo()).reload()
    TitleNormalizer.searchTitle("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  "start with seedIfEmpty" should "populate an empty enabled store with the defaults and install them" in {
    val repo = new InMemoryTitleRulesRepo()
    repo.findAll() shouldBe empty
    val cache = new TitleRulesCache(repo, seedIfEmpty = true)
    try cache.start() finally cache.stop()
    repo.findAll().map(_.id) should contain("structural-anniversary-suffix")
    repo.findAll().size shouldBe TitleRuleDefaults.all.size
    TitleNormalizer.searchTitle("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  "start without seedIfEmpty" should "leave an empty store untouched" in {
    val repo = new InMemoryTitleRulesRepo()
    val cache = new TitleRulesCache(repo, seedIfEmpty = false)
    try cache.start() finally cache.stop()
    repo.findAll() shouldBe empty
  }
}
