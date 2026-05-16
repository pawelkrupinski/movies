package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TitleNormalizerSpec extends AnyFlatSpec with Matchers {

  import TitleNormalizer.mergeKey

  // ── The Mandalorian merge — the headline scenario ─────────────────────────

  "mergeKey" should "collapse all three Mandalorian variants to one key" in {
    val titles = Seq(
      "Mandalorian & Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Mandalorian i Grogu"
    )
    titles.map(t => mergeKey(t, titles)).toSet should have size 1
  }

  it should "collapse 'Mandalorian & Grogu' with 'Mandalorian i Grogu'" in {
    val titles = Seq("Mandalorian & Grogu", "Mandalorian i Grogu")
    mergeKey("Mandalorian & Grogu", titles) shouldBe mergeKey("Mandalorian i Grogu", titles)
  }

  it should "collapse 'Gwiezdne Wojny: Mandalorian i Grogu' with 'Mandalorian i Grogu'" in {
    val titles = Seq("Gwiezdne Wojny: Mandalorian i Grogu", "Mandalorian i Grogu")
    mergeKey("Gwiezdne Wojny: Mandalorian i Grogu", titles) shouldBe
      mergeKey("Mandalorian i Grogu", titles)
  }

  it should "collapse 'Gwiezdne Wojny: Mandalorian & Grogu' with 'Mandalorian i Grogu' (both rules)" in {
    val titles = Seq("Gwiezdne Wojny: Mandalorian & Grogu", "Mandalorian i Grogu")
    mergeKey("Gwiezdne Wojny: Mandalorian & Grogu", titles) shouldBe
      mergeKey("Mandalorian i Grogu", titles)
  }

  // ── Safety: only merge when there's actually a partner title ──────────────

  it should "keep 'Gwiezdne Wojny: A New Hope' intact when nothing else reduces to it" in {
    val titles = Seq("Gwiezdne Wojny: A New Hope")
    mergeKey("Gwiezdne Wojny: A New Hope", titles) shouldBe "gwiezdne wojny: a new hope"
  }

  it should "keep 'Pizza & Pasta' intact when nothing else reduces to 'Pizza i Pasta'" in {
    val titles = Seq("Pizza & Pasta", "Some Other Film")
    mergeKey("Pizza & Pasta", titles) shouldBe "pizza & pasta"
  }

  it should "not strip the prefix when only the prefixed variant exists" in {
    val titles = Seq("Gwiezdne Wojny: Solo", "Diabeł ubiera się u Prady 2")
    mergeKey("Gwiezdne Wojny: Solo", titles) shouldBe "gwiezdne wojny: solo"
  }

  // ── Pre-existing Arabic→Roman behaviour still applies ─────────────────────

  // The decoration patterns now live in TitleNormalizer (formerly only in
  // MovieService.searchTitle) and feed into `canonical`, so anniversary
  // and wersja variants collapse with their base film for merging.

  it should "merge 'Top Gun 40th Anniversary' with 'Top Gun' via the decoration-stripped canonical" in {
    val titles = Seq("Top Gun 40th Anniversary", "Top Gun")
    mergeKey("Top Gun 40th Anniversary", titles) shouldBe mergeKey("Top Gun", titles)
  }

  it should "merge a Polish 'Rocznica' variant with the base film" in {
    val titles = Seq("Top gun | 40 rocznica", "Top Gun")
    mergeKey("Top gun | 40 rocznica", titles) shouldBe mergeKey("Top Gun", titles)
  }

  it should "merge 'Wersja zremasterowana' with the base film" in {
    val titles = Seq("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana",
                     "Żywot Briana Grupy Monty Pythona")
    mergeKey(titles.head, titles) shouldBe mergeKey(titles.last, titles)
  }

  it should "leave the anniversary title untouched when no base film is in the corpus" in {
    val titles = Seq("Top Gun 40th Anniversary")
    mergeKey("Top Gun 40th Anniversary", titles) shouldBe "top gun 40th anniversary"
  }

  it should "still collapse 'Mortal Kombat 2' and 'Mortal Kombat II' via Roman normalisation" in {
    val titles = Seq("Mortal Kombat 2", "Mortal Kombat II")
    mergeKey("Mortal Kombat 2", titles) shouldBe mergeKey("Mortal Kombat II", titles)
  }

  // ── Punctuation-only duplicates ───────────────────────────────────────────
  //
  // Different cinemas format the same title differently — same words and
  // word order, only punctuation/whitespace differs (one omits a colon,
  // another uses a dash, etc.). Merge them.

  it should "collapse 'Top Gun Maverick' and 'Top Gun: Maverick' (punctuation-only diff)" in {
    val titles = Seq("Top Gun Maverick", "Top Gun: Maverick")
    mergeKey("Top Gun Maverick", titles) shouldBe mergeKey("Top Gun: Maverick", titles)
  }

  it should "leave 'Top Gun: Maverick' intact when no punctuation-stripped sibling exists" in {
    val titles = Seq("Top Gun: Maverick", "Top Gun")
    // No sibling reduces to "topgunmaverick", so the key stays the romanized
    // lower-case form of the input (no spurious cross-film merge with Top Gun).
    mergeKey("Top Gun: Maverick", titles) shouldBe "top gun: maverick"
  }

  it should "not merge films that differ in actual words, only matching siblings" in {
    val titles = Seq("Mortal Kombat II", "Mortal Kombat 2", "Mortal Kombat")
    // II ≡ 2 still collapses (Roman normalisation already handled this).
    // "Mortal Kombat" stays separate — its punctuation-strip "mortalkombat"
    // has no sibling, only "mortalkombatii" does.
    mergeKey("Mortal Kombat II", titles) shouldBe mergeKey("Mortal Kombat 2", titles)
    mergeKey("Mortal Kombat", titles)    should not be mergeKey("Mortal Kombat II", titles)
  }

  // ── No interaction between unrelated films ───────────────────────────────

  it should "give different keys to unrelated films sharing the corpus" in {
    val titles = Seq(
      "Mandalorian & Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Mandalorian i Grogu",
      "Drama",
      "Top Gun"
    )
    val mandalorianKey = mergeKey("Mandalorian i Grogu", titles)
    mergeKey("Drama", titles)        should not be mandalorianKey
    mergeKey("Top Gun", titles)      should not be mandalorianKey
  }

  it should "be case-insensitive in its output (so toLowerCase comparison still works)" in {
    val titles = Seq("Mandalorian & Grogu", "Mandalorian i Grogu")
    val keys   = titles.map(t => mergeKey(t, titles))
    keys.foreach(k => k shouldBe k.toLowerCase)
  }

  // ── preferredDisplay ──────────────────────────────────────────────────────

  import TitleNormalizer.preferredDisplay

  "preferredDisplay" should "prefer 'i' over '&' when both spellings are present" in {
    preferredDisplay(Seq("Mandalorian & Grogu", "Mandalorian i Grogu")) shouldBe Some("Mandalorian i Grogu")
  }

  it should "still prefer 'i' regardless of input order" in {
    preferredDisplay(Seq("Mandalorian i Grogu", "Mandalorian & Grogu")) shouldBe Some("Mandalorian i Grogu")
  }

  it should "fall back to '&' form when no 'i' alternative exists" in {
    preferredDisplay(Seq("Mandalorian & Grogu")) shouldBe Some("Mandalorian & Grogu")
  }

  it should "return None for an empty group" in {
    preferredDisplay(Nil) shouldBe None
  }

  it should "prefer 'Mandalorian i Grogu' over 'Gwiezdne Wojny: Mandalorian i Grogu' when both exist" in {
    preferredDisplay(Seq(
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Mandalorian i Grogu"
    )) shouldBe Some("Mandalorian i Grogu")
  }

  it should "deterministically pick 'Mandalorian i Grogu' from all three Mandalorian variants" in {
    preferredDisplay(Seq(
      "Mandalorian & Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Mandalorian i Grogu"
    )) shouldBe Some("Mandalorian i Grogu")
  }

  it should "be order-independent for the three-variant Mandalorian case" in {
    val variants = Seq(
      "Mandalorian & Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu",
      "Mandalorian i Grogu"
    )
    variants.permutations.foreach { perm =>
      preferredDisplay(perm) shouldBe Some("Mandalorian i Grogu")
    }
  }

  it should "synthesise 'Mandalorian i Grogu' when only '& ' and prefixed 'i' variants are present" in {
    // Neither "Mandalorian i Grogu" form is literally in the input — the
    // canonical form is constructed on the fly so the prefix never leaks
    // into a merged schedule's display title.
    preferredDisplay(Seq(
      "Mandalorian & Grogu",
      "Gwiezdne Wojny: Mandalorian i Grogu"
    )) shouldBe Some("Mandalorian i Grogu")
  }

  it should "leave a standalone 'Gwiezdne Wojny:' title untouched (no merge → no transformation)" in {
    preferredDisplay(Seq("Gwiezdne Wojny: A New Hope")) shouldBe Some("Gwiezdne Wojny: A New Hope")
  }

  it should "leave a standalone '&' title untouched (no merge → no transformation)" in {
    preferredDisplay(Seq("Pizza & Pasta")) shouldBe Some("Pizza & Pasta")
  }

  // ── preferredDisplay for punctuation-only duplicates ──────────────────────

  it should "prefer the colon variant when both 'Top Gun Maverick' and 'Top Gun: Maverick' are present" in {
    preferredDisplay(Seq("Top Gun Maverick", "Top Gun: Maverick")) shouldBe Some("Top Gun: Maverick")
  }

  it should "prefer 'Top Gun: Maverick' regardless of input order" in {
    preferredDisplay(Seq("Top Gun: Maverick", "Top Gun Maverick")) shouldBe Some("Top Gun: Maverick")
  }
}
