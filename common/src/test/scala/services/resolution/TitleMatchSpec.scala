package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.TextNormalization

/**
 * One table per helper, lifted from the site specs that used to reach these
 * rules through `MetacriticClient` / `TitleCorroboration` / `MovieService`
 * (MetacriticClientSpec, RottenTomatoesClientSpec, FilmwebClientSpec,
 * ImdbClientSpec, OMDbClientSpec, CinemetaClientSpec, DirectorWalk*Spec).
 * Every row is an answer a site already depends on; a consolidation that
 * changes one has changed a rule.
 */
class TitleMatchSpec extends AnyFlatSpec with Matchers {

  /** Filmweb's fold, the `sanitize` its corroboration passes in. */
  private val deburr: String => String = s => TextNormalization.deburr(s).toLowerCase.trim

  // ── foldDashes / fold / deburredFold ───────────────────────────────────────

  "foldDashes" should "fold every Unicode dash variant to ASCII '-' and nothing else" in {
    val cases = Seq(
      "Chainsaw Man – The Movie: Reze Arc" -> "Chainsaw Man - The Movie: Reze Arc", // en dash
      "Spider–Man"                         -> "Spider-Man",
      "a—b"                                -> "a-b",  // em dash
      "a‐b"                                -> "a-b",  // hyphen
      "a‑b"                                -> "a-b",  // non-breaking hyphen
      "a‒b"                                -> "a-b",  // figure dash
      "a―b"                                -> "a-b",  // horizontal bar
      "a−b"                                -> "a-b",  // minus sign
      "Zażółć – Gęślą JAŹŃ"                -> "Zażółć - Gęślą JAŹŃ" // case + diacritics kept
    )
    for ((in, out) <- cases) withClue(s"$in: ") { TitleMatch.foldDashes(in) shouldBe out }
  }

  it should "return a dash-free string unchanged" in {
    val plain = "Top Gun: Maverick - Re-Release"
    TitleMatch.foldDashes(plain) should be theSameInstanceAs plain
  }

  "fold" should "lower-case, trim and dash-fold while KEEPING diacritics" in {
    val cases = Seq(
      "  Chainsaw Man – The Movie: Reze Arc " -> "chainsaw man - the movie: reze arc",
      "Basia. Radzę sobie!"                  -> "basia. radzę sobie!",
      "Sirât"                                -> "sirât",
      "TOP GUN"                              -> "top gun"
    )
    for ((in, out) <- cases) withClue(s"$in: ") {
      TitleMatch.fold(in) shouldBe out
      TitleMatch.fold(out) shouldBe out // idempotent: a pre-folded query folds to itself
    }
  }

  "deburredFold" should "strip diacritics (including ł) on top of fold" in {
    val cases = Seq(
      "Chłopiec na krańcach świata" -> "chlopiec na krancach swiata",
      "Basia. Radzę sobie!"         -> "basia. radze sobie!",
      "Spider–Man"                  -> "spider-man",
      " Łódź "                      -> "lodz"
    )
    for ((in, out) <- cases) withClue(s"$in: ") { TitleMatch.deburredFold(in) shouldBe out }
  }

  // ── exact ──────────────────────────────────────────────────────────────────

  "exact" should "ignore case, surrounding whitespace and the dash glyph, but not diacritics or a suffix" in {
    val cases = Seq(
      ("Chainsaw Man – The Movie: Reze Arc", "Chainsaw Man - The Movie: Reze Arc", true),
      ("Top Gun", "top gun", true),
      ("Top Gun", " Top Gun ", true),
      ("Top Gun", "Top Gun: Maverick", false),
      ("La Dolce Vita", "La Dolce Vita - Re-Release", false),
      ("Sirât", "Sirat", false),
      ("Faworyta", "Carska faworyta", false)
    )
    for ((a, b, expected) <- cases) withClue(s"$a / $b: ") {
      TitleMatch.exact(a, b) shouldBe expected
      TitleMatch.exact(b, a) shouldBe expected
    }
  }

  // ── isModifierSuffix ───────────────────────────────────────────────────────

  "isModifierSuffix" should "accept a punctuation-led suffix after the query and nothing else" in {
    val cases = Seq(
      ("I Vitelloni - Re-Release", "i vitelloni", true),
      ("La Dolce Vita - Re-Release", "la dolce vita", true),
      ("Lalka: Restored", "lalka", true),
      ("Lalka - Restored", "lalka", true),
      ("Top Gun (Anniversary Edition)", "top gun", true),
      ("Top Gun: Maverick", "top gun", true),               // colon-subtitled counts; the caller ranks exact first
      ("Chainsaw Man – The Movie: Reze Arc", "chainsaw man - the movie", true), // dash folded on both sides
      ("Chainsaw Man - The Movie: Reze Arc", "chainsaw man – the movie", true),
      ("Deaf President Now!", "deaf", false),               // next char alphanumeric: a different film
      ("Belleville Cop", "belle", false),
      ("Beauty and the Belle", "belle", false),             // not a prefix at all
      ("Top Gun", "top gun", false),                        // exact equals are the caller's
      ("Lalka", "lalka - restored", false)
    )
    for ((title, query, expected) <- cases) withClue(s"$title / $query: ") {
      TitleMatch.isModifierSuffix(title, query) shouldBe expected
    }
  }

  // ── oneStartsWithTheOther ──────────────────────────────────────────────────

  "oneStartsWithTheOther" should "hold when either folded title prefixes the other, never for an empty side" in {
    val cases = Seq(
      ("varavuthebeginning", "varavu", true),
      ("varavu", "varavuthebeginning", true),
      ("alphasomethingelse", "alpha", true),
      ("cactuspears", "cactuspears", true),
      ("faworyta", "carskafaworyta", false),
      ("", "alpha", false),
      ("alpha", "", false)
    )
    for ((a, b, expected) <- cases) withClue(s"$a / $b: ") {
      TitleMatch.oneStartsWithTheOther(a, b) shouldBe expected
    }
  }

  // ── close ──────────────────────────────────────────────────────────────────

  "close" should "tie titles within 2 edits and a third of the longer, and no further" in {
    val cases = Seq(
      ("guru", "gourou", true),                       // the case it was written for
      ("guru", "dalloway", false),
      ("guru", "guru", true),
      ("diabeł ubiera się u prady", "diabeł ubiera się u prady 2", true), // a sequel IS close — exact outranks it upstream
      ("mistyczka", "maryja matka papieża", false),
      ("giulietta i duchy", "giulietta degli spiriti", false), // a translation is out of reach; corroboration covers it
      ("kitten", "sitting", false),                   // 3 edits
      ("abcd", "abcdef", true),                       // 2 edits, 2 * 3 <= 6
      ("abc", "abcde", false),                        // 2 edits, 2 * 3 > 5
      ("ab", "abc", true),                            // 1 edit, 1 * 3 <= 3
      ("a", "ab", false)                              // 1 edit, 1 * 3 > 2
    )
    for ((a, b, expected) <- cases) withClue(s"$a / $b: ") {
      TitleMatch.close(a, b) shouldBe expected
      TitleMatch.close(b, a) shouldBe expected
    }
  }

  // ── dropLeadingArticle ─────────────────────────────────────────────────────

  "dropLeadingArticle" should "strip the/a/an only when followed by the site's separator" in {
    val cases = Seq(
      ("the-odyssey", '-', Some("odyssey")),
      ("a-star-is-born", '-', Some("star-is-born")),
      ("an_education", '_', Some("education")),
      ("the_bodyguard", '_', Some("bodyguard")),
      ("top_gun", '_', None),
      ("theatre-of-blood", '-', None),  // "the" is a word start, not an article
      ("the_odyssey", '-', None),       // wrong separator
      ("a", '-', None)
    )
    for ((slug, sep, expected) <- cases) withClue(s"$slug / '$sep': ") {
      TitleMatch.dropLeadingArticle(slug, sep) shouldBe expected
    }
  }

  // ── yearSuffixedFirst ──────────────────────────────────────────────────────

  "yearSuffixedFirst" should "put each year-suffixed form immediately before its bare form" in {
    TitleMatch.yearSuffixedFirst(Seq("the-odyssey", "odyssey"), Some(2026), '-') shouldBe
      Seq("the-odyssey-2026", "the-odyssey", "odyssey-2026", "odyssey")
    TitleMatch.yearSuffixedFirst(Seq("top_gun"), Some(1986), '_') shouldBe
      Seq("top_gun_1986", "top_gun")
  }

  it should "leave the ladder unchanged without a year and drop duplicate forms with one" in {
    TitleMatch.yearSuffixedFirst(Seq("the-odyssey", "odyssey"), None, '-') shouldBe Seq("the-odyssey", "odyssey")
    TitleMatch.yearSuffixedFirst(Seq.empty, Some(2026), '-') shouldBe Seq.empty
    TitleMatch.yearSuffixedFirst(Seq("x", "x"), Some(2026), '-') shouldBe Seq("x-2026", "x")
  }

  // ── latinise / distinctiveTokens ───────────────────────────────────────────

  "latinise" should "romanise Cyrillic (lower-casing the whole string) and leave Latin text as it was" in {
    TitleMatch.latinise("Мавка. Справжній міф") shouldBe "mavka. spravzhnii mif"
    TitleMatch.latinise("Щука") shouldBe "shchuka"      // the digraph rule runs before ш→sh
    TitleMatch.latinise("Ваяна 3D") shouldBe "vaiana 3d"
    val latin = "Mawka. Prawdziwy mit"
    TitleMatch.latinise(latin) should be theSameInstanceAs latin
  }

  "distinctiveTokens" should "keep only the words long enough to identify a film" in {
    TitleMatch.distinctiveTokens("Giulietta i duchy", deburr) shouldBe Set("giulietta", "duchy")
    TitleMatch.distinctiveTokens("Le città di pianura", deburr) shouldBe Set("citta", "pianura")
    TitleMatch.distinctiveTokens("Maryja. Matka Papieża", deburr) shouldBe Set("maryja", "matka", "papieza")
    TitleMatch.distinctiveTokens("The End", deburr) shouldBe Set.empty
  }

  // ── sharesDistinctiveToken ─────────────────────────────────────────────────

  "sharesDistinctiveToken" should "tie a translation on its surviving proper noun and nothing weaker" in {
    val cases = Seq(
      (Seq("Giulietta i duchy"), Seq("Giulietta degli spiriti"), 0, true),
      (Seq("Mistyczka"), Seq("Maryja. Matka Papieża"), 0, false),
      (Seq("Mistyczka"), Seq("Maryja. Matka Papieża"), 1, false),
      (Seq("The End"), Seq("The Beginning"), 0, false),               // "the" is too short to count
      (Seq("Mawka. Prawdziwy mit"), Seq("Mavka. Spravzhnij mif"), 0, false),
      (Seq("Mawka. Prawdziwy mit"), Seq("Mavka. Spravzhnij mif"), 1, true), // one letter of romanisation
      (Seq("Mavka"), Seq("Мавка. Справжній міф"), 0, true),                // latinised, so no edits needed
      (Seq("Mawka"), Seq("Мавка. Справжній міф"), 0, false),
      (Seq("Mawka"), Seq("Мавка. Справжній міф"), 1, true),
      (Seq("Miasta na równinie", "Cinema Italia Oggi: Miasta na równinie", "Le città di pianura"),
        Seq("Le città di pianura"), 1, true),                            // any name the row is known by
      (Seq("Miasta na równinie"), Seq("Le città di pianura"), 1, false),
      (Seq.empty, Seq("Anything"), 1, false)
    )
    for ((left, right, edits, expected) <- cases) withClue(s"$left / $right within $edits: ") {
      TitleMatch.sharesDistinctiveToken(left, right, deburr, maxTokenEdits = edits) shouldBe expected
    }
  }
}
