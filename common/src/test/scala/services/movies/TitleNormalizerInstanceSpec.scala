package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The property the process-global object could never provide: TWO countries'
 * rule sets live in one JVM at the same time, each keying its own titles.
 *
 * Every case here is deliberately simultaneous — holding both normalizers at
 * once — because the old process-global rule set could only ever make one set
 * current, which is why a multi-country worker had to be refused.
 */
class TitleNormalizerInstanceSpec extends AnyFlatSpec with Matchers {

  private val pl = TitleNormalizer.forCountry(Country.Poland)
  private val de = TitleNormalizer.forCountry(Country.Germany)
  private val uk = TitleNormalizer.forCountry(Country.UnitedKingdom)

  "A country's normalizer" should "be a new instance each time it is built, never one the whole JVM shares" in {
    // Every instance fills memo caches as it runs; a process-wide memo handed every
    // wiring and every spec the same caches.
    TitleNormalizer.forCountry(Country.Poland) should not be theSameInstanceAs (TitleNormalizer.forCountry(Country.Poland))
  }

  "two countries' normalizers" should "key the same title differently, at the same time" in {
    // The Polish " & " → " i " unification ("i" = "and") must not reach a German
    // listing: CinemaxX Würzburg's "Minions & Monster" was stored and served as
    // "Minions i Monster", a key no German cinema slot can produce.
    pl.sanitize("Minions & Monster") shouldBe "minionsimonster"
    de.sanitize("Minions & Monster") shouldBe "minionsmonster"
    uk.sanitize("Minions & Monster") shouldBe "minionsmonster"
  }

  /** The keys that actually broke, from the 2026-08-04 convergence run: the
   *  replay harness handed the UK and Germany legs POLAND's instance, so the
   *  Polish " & " -> " i " unification keyed British and German films through a
   *  conjunction those languages do not use. Verbatim, because "& -> i" reads
   *  harmless until you see  in a UK corpus. */
  it should "not put the Polish conjunction into a British or German title" in {
    val wallace = "Wallace & Gromit: The Curse of the Were-Rabbit"
    pl.sanitize(wallace) shouldBe "wallaceigromitthecurseofthewererabbit"  // what the leg WRONGLY produced
    uk.sanitize(wallace) shouldBe "wallacegromitthecurseofthewererabbit"   // what it must produce
    de.sanitize(wallace) shouldBe "wallacegromitthecurseofthewererabbit"

    pl.sanitize("Pat Garrett & Billy the Kid") shouldBe "patgarrettibillythekid"
    uk.sanitize("Pat Garrett & Billy the Kid") shouldBe "patgarrettbillythekid"

    pl.sanitize("B-Movie: Lust & Sound in West Berlin 1979") shouldBe "bmovielustisoundinwestberlin1979"
    de.sanitize("B-Movie: Lust & Sound in West Berlin 1979") shouldBe "bmovielustsoundinwestberlin1979"
  }

  it should "scope the Poland-only Mandalorian rewrite too" in {
    // German cinemas list this as "The Mandalorian And Grogu"; the Poland-only
    // rule maps it onto the Polish canonical, which pinned a Berlin row.
    pl.sanitize("The Mandalorian and Grogu") shouldBe pl.sanitize("Mandalorian i Grogu")
    de.sanitize("The Mandalorian and Grogu") should not be de.sanitize("Mandalorian i Grogu")
  }

  it should "still merge the Polish spellings in Poland" in {
    pl.sanitize("Mandalorian & Grogu") shouldBe pl.sanitize("Mandalorian i Grogu")
  }

  it should "agree on titles no country-scoped rule touches" in {
    Seq(pl, de, uk).map(_.sanitize("Top Gun: Maverick")).distinct should have size 1
  }

  it should "give each country its own instance" in {
    pl should not be theSameInstanceAs(de)
  }

  "the sanitize memo cache" should "belong to the instance, so one country cannot serve another's key" in {
    // Priming Poland's cache with the ampersand title is exactly the sequence that
    // would poison a shared, title-keyed cache: the raw string is identical, only
    // the rules differ. Germany must still compute its own answer afterwards.
    val title = "Alvin & the Chipmunks"
    pl.sanitize(title) shouldBe "alvinithechipmunks"
    de.sanitize(title) shouldBe "alvinthechipmunks"
    pl.sanitize(title) shouldBe "alvinithechipmunks" // and Poland's entry survived
  }

  // `CacheKey` equality IS identity in the corpus — it decides whether two
  // cinema spellings are one film. It used to normalise inside its own
  // constructor, so a key silently adopted whatever rule set was global on the
  // thread that built it, including Mongo change-stream driver threads.
  "a CacheKey" should "take its identity from the country whose rules built it" in {
    CacheKey("Minions & Monster", Some(2025), pl) should
      not be CacheKey("Minions & Monster", Some(2025), de)
  }

  it should "merge the ampersand and conjunction spellings only where that rule applies" in {
    CacheKey("Minions & Monster", Some(2025), pl) shouldBe
      CacheKey("Minions i Monster", Some(2025), pl)
    CacheKey("Minions & Monster", Some(2025), de) should
      not be CacheKey("Minions i Monster", Some(2025), de)
  }

  it should "still key identically across countries when no scoped rule applies" in {
    CacheKey("Top Gun: Maverick", Some(2022), pl) shouldBe
      CacheKey("Top Gun: Maverick", Some(2022), de)
  }

  "the rules a normalizer exposes" should "be that country's filtered set" in {
    // Poland carries the three country-scoped rules the others drop.
    pl.rules.rules.size should be > de.rules.rules.size
    de.rules.rules.size shouldBe uk.rules.rules.size
  }
}
