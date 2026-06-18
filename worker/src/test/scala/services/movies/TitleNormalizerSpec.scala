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

  // ── Decoration is NOT part of identity ────────────────────────────────────

  // The structural decoration strip (anniversary / "- wersja X" / restored /
  // Cykl / slash) feeds the external-lookup tiers (searchTitle / apiQuery) but
  // NOT `canonical`. So a decoration edition keys by its OWN form and stays a
  // SEPARATE row from the base film — the same rule the cache enforces via
  // keyOf → sanitize. (`searchTitle` itself still strips these — see below.)

  it should "NOT merge 'Top Gun 40th Anniversary' with 'Top Gun' (decoration keys on its own)" in {
    val titles = Seq("Top Gun 40th Anniversary", "Top Gun")
    mergeKey("Top Gun 40th Anniversary", titles) should not be mergeKey("Top Gun", titles)
  }

  it should "NOT merge a Polish 'Rocznica' variant with the base film" in {
    val titles = Seq("Top gun | 40 rocznica", "Top Gun")
    mergeKey("Top gun | 40 rocznica", titles) should not be mergeKey("Top Gun", titles)
  }

  it should "NOT merge 'Wersja zremasterowana' with the base film" in {
    val titles = Seq("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana",
                     "Żywot Briana Grupy Monty Pythona")
    mergeKey(titles.head, titles) should not be mergeKey(titles.last, titles)
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

  it should "preserve the full accessibility tag on a Kino bez barier screening" in {
    // Simulates `MovieRecord.displayTitle`, which feeds the union of the
    // cinema's raw reported title and the row's cleanTitle (= recase of the
    // same raw title) into preferredDisplay. Both must end up as the full
    // "(AD + CC + PJM)" form so the user-visible title isn't truncated.
    val raw        = "Kino bez barier: Arco (AD + CC + PJM)"
    val cleanTitle = TitleNormalizer.recase(raw)
    preferredDisplay(Seq(raw, cleanTitle)) shouldBe Some(raw)
  }

  it should "prefer 'Top Gun: Maverick' regardless of input order" in {
    preferredDisplay(Seq("Top Gun: Maverick", "Top Gun Maverick")) shouldBe Some("Top Gun: Maverick")
  }

  it should "prefer the diacritic spelling over a scraper-flattened ASCII one" in {
    // The old lexicographic tiebreak picked "Diabel" ('l' < 'ł'); the diacritic
    // axis must keep the proper Polish letter.
    preferredDisplay(Seq("Diabel", "Diabeł")) shouldBe Some("Diabeł")
    preferredDisplay(Seq("Diabeł", "Diabel")) shouldBe Some("Diabeł")
  }

  it should "treat trailing junk as junk, not as richer punctuation" in {
    // A stray trailing "." must not outrank the clean form via the punctuation
    // axis — interior punctuation is what that axis rewards.
    preferredDisplay(Seq("Werdykt.", "Werdykt")) shouldBe Some("Werdykt")
  }

  "wellFormedTitle" should "reject ALL-CAPS, double-space, and edge-junk titles" in {
    import TitleNormalizer.wellFormedTitle
    wellFormedTitle("Drzewo magii")             shouldBe true
    wellFormedTitle("Diabeł ubiera się u Prady 2") shouldBe true
    wellFormedTitle("ALL YOU NEED IS KILL")     shouldBe false  // all-caps
    wellFormedTitle("Super Mario  Galaxy Film") shouldBe false  // double space
    wellFormedTitle("Zaproszenie.")             shouldBe false  // trailing junk
    wellFormedTitle("„Arco”")                   shouldBe false  // wrapping quotes
  }

  // ── apiQuery (merged GlobalStructural tier) ────────────────────────────────
  //
  // apiQuery is the aggressive strip every external-API resolver (TMDB, Filmweb,
  // MC, RT, IMDb) uses: decoration PLUS programme prefix / accessibility tag /
  // "+ <event>" suffix, so "Kino bez barier: Freak Show (AD + CC + PJM)" queries
  // upstream as just "Freak Show". It does NOT feed the merge key — `sanitize`
  // keeps the banner — so the DKF / accessibility / event row stays its own
  // cache card while resolving off the bare film. The display title likewise
  // keeps the banner; only its CASING is normalised (see `recase` below).

  import TitleNormalizer.{apiQuery, programmePrefix, recase}

  "apiQuery" should "leave 'Orwell: 2 + 2 = 5' alone (the '+ <event>' suffix must start with a letter)" in {
    apiQuery("Orwell: 2 + 2 = 5") shouldBe "Orwell: 2 + 2 = 5"
  }

  it should "strip the 'Kino bez barier:' programme prefix" in {
    apiQuery("Kino bez barier: Freak Show") shouldBe "Freak Show"
  }

  it should "strip the trailing accessibility tag '(AD + CC + PJM)'" in {
    apiQuery("Freak Show (AD + CC + PJM)") shouldBe "Freak Show"
  }

  it should "strip a truncated accessibility tag '(AD' (display-clipping artefact)" in {
    apiQuery("Freak Show (AD") shouldBe "Freak Show"
  }

  it should "strip both prefix and suffix in one pass" in {
    apiQuery("Kino bez barier: Freak Show (AD + CC + PJM)") shouldBe "Freak Show"
  }

  it should "strip the 'Pokaz sensorycznie przyjazny:' prefix for sensory-friendly screenings" in {
    apiQuery("Pokaz sensorycznie przyjazny: Delfinek i ja 2") shouldBe "Delfinek i ja 2"
  }

  it should "strip the 'Filmowe Poranki:' prefix (Helios morning-screening programme)" in {
    apiQuery("Filmowe Poranki: Świnka Peppa, cz. 2") shouldBe "Świnka Peppa, cz. 2"
  }

  it should "strip the kids-morning programme in its other spellings (singular, seasonal, subtitled)" in {
    // Same programme, different banners cinemas actually ship. All strip down
    // to the film for upstream lookups; the display row keeps the banner.
    apiQuery("Filmowe Poranki - Miraculous: Biedronka i Czarny Kot") shouldBe "Biedronka i Czarny Kot"
    apiQuery("Filmowy Poranek: Tomek i Przyjaciele")                 shouldBe "Tomek i Przyjaciele"
    apiQuery("Filmowy Poranek - Tomek i Przyjaciele: Wielki wyścig") shouldBe "Wielki wyścig"
    apiQuery("Poranek dla dzieci: Pszczółka Maja")                   shouldBe "Pszczółka Maja"
    apiQuery("Zimowe Poranki z Bobem Budowniczym: Śnieżna przygoda") shouldBe "Śnieżna przygoda"
    // A real film whose own title carries a colon must NOT be eaten by the banner.
    apiQuery("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  it should "strip the 'Filmowe spotkania z psychoanalizą:' prefix (Rialto themed screening)" in {
    apiQuery("Filmowe spotkania z psychoanalizą: dobry chłopiec") shouldBe "dobry chłopiec"
  }

  it should "strip the 'Filmowy Klub Seniora:' prefix (Rialto senior-club screening)" in {
    // Kept its own cache row (a senior-club showing is listed separately) but
    // enriched off the clean base title upstream.
    apiQuery("Filmowy Klub Seniora: Ojczyzna") shouldBe "Ojczyzna"
  }

  it should "strip the 'Dyskusyjny Klub Filmowy:' prefix for ratings/enrichment but keep it on the row" in {
    // Film-club screenings are their own row but rate/enrich off the clean title.
    // Case-insensitive: cinemas report it upper-cased ("DYSKUSYJNY KLUB FILMOWY: ").
    apiQuery("Dyskusyjny Klub Filmowy: Vertigo") shouldBe "Vertigo"
    apiQuery("DYSKUSYJNY KLUB FILMOWY: Vertigo") shouldBe "Vertigo"
    programmePrefix("DYSKUSYJNY KLUB FILMOWY: Vertigo") shouldBe Some("DYSKUSYJNY KLUB FILMOWY: ")
  }

  // ── programmePrefix — split point for prefix-vs-film-title casing ──────────

  "programmePrefix" should "return the matched prefix including the ': ' delimiter" in {
    programmePrefix("Filmowy Klub Seniora: OJCZYZNA")              shouldBe Some("Filmowy Klub Seniora: ")
    programmePrefix("Filmowe spotkania z psychoanalizą: DOBRY CHŁOPIEC") shouldBe Some("Filmowe spotkania z psychoanalizą: ")
  }

  it should "return None for a title without a recognised programme prefix" in {
    programmePrefix("Mavka. Prawdziwy mit") shouldBe None
    programmePrefix("Top Gun: Maverick")    shouldBe None // a real colon title, not a programme
  }

  it should "strip the 'Plenerowe Pałacowe:' prefix (Kino Pałacowe outdoor screenings)" in {
    apiQuery("Plenerowe Pałacowe: La Grazia") shouldBe "La Grazia"
  }

  it should "strip the 'Cinema Italia Oggi:' prefix (Italian-film festival banner)" in {
    apiQuery("Cinema Italia Oggi: La Grande Bellezza") shouldBe "La Grande Bellezza"
  }

  it should "leave a title with a colon that isn't a programme prefix alone" in {
    apiQuery("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  it should "leave a title with parens that aren't an accessibility tag alone" in {
    apiQuery("Annie (2014)") shouldBe "Annie (2014)"
  }

  it should "still apply the decoration strippers (anniversary etc.)" in {
    apiQuery("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  it should "strip the ' + <event>' screening suffix so the event screening enriches off the base film" in {
    // The display row keeps "Ojczyzna + spotkanie…" as its own card (sanitize
    // keeps the suffix); the external resolvers must still query the bare
    // "Ojczyzna" so the event screening shares the plain film's tmdbId / URLs.
    apiQuery("Ojczyzna + spotkanie z producentką Ewą Puszczyńską") shouldBe "Ojczyzna"
  }

  it should "strip a '<sep> … spotkanie <…>' meeting suffix introduced by any separator" in {
    // One generalised rule for the 'spotkanie' (meeting) annotation cinemas append
    // after a separator — pipe, plus, or dash — generalised over the spotkani- stem
    // (spotkanie / spotkania / spotkaniem), an arbitrary leading annotation, and an
    // arbitrary trailing phrase. The screening keeps its own display card; only the
    // external resolvers query the bare film.
    apiQuery("Takie jest życie | spotkanie Beaty Kwiatkowskiej")              shouldBe "Takie jest życie"
    apiQuery("Ojczyzna | spotkanie z reżyserem")                             shouldBe "Ojczyzna"
    apiQuery("Ujście | SPOTKANIE z twórcami")                                shouldBe "Ujście"
    apiQuery("OJCZYZNA + spotkanie")                                         shouldBe "OJCZYZNA"
    apiQuery("Znaki Pana Śliwki + spotkanie z reżyserami")                   shouldBe "Znaki Pana Śliwki"
    // Dash-introduced, with annotation words between the separator and 'spotkanie'.
    apiQuery("Carmilla – pokaz z muzyką na żywo + spotkanie z Izabelą Trojanowską | Kino (nie)jawne: queerowe kody PRL-u") shouldBe "Carmilla"
    apiQuery("Wędrówka na północ – pokaz specjalny ze spotkaniem | gościni: podróżniczka Zuzanna Puchalska") shouldBe "Wędrówka na północ"
  }

  it should "not amputate a film whose own title contains 'i' or 'spotkania'" in {
    // 'Piłat i inni' (Pilatus und andere): the conjunction 'i' must NOT be read as a
    // separator — only punctuation introduces the meeting annotation, so the bare
    // film survives.
    apiQuery("Piłat i inni + spotkanie z prof. Anną Nacher") shouldBe "Piłat i inni"
    // 'Bliskie spotkania trzeciego stopnia' (Close Encounters) is a film: 'spotkania'
    // is followed by a lowercase ordinal, not a meeting tail (z / Capital / end), so
    // the lookahead leaves it intact even when a cycle banner precedes it.
    apiQuery("Kino cyrkularne | Bliskie spotkania trzeciego stopnia") shouldBe
      "Kino cyrkularne | Bliskie spotkania trzeciego stopnia"
  }

  it should "strip a 'Spotkanie/Spotkania …:' meeting-cycle banner prefix" in {
    // The mirror of the suffix rule for the colon-introduced banner form, where the
    // film sits AFTER the colon. Anchored at start so a film carrying its own colon
    // is safe (see the SMOK / 'Trzy kolory: Czerwony' guard below).
    apiQuery("SPOTKANIA FILOZOFICZNE: Wędrówka na północ")       shouldBe "Wędrówka na północ"
    apiQuery("Spotkanie filozoficzne: Wędrówka na północ")       shouldBe "Wędrówka na północ"
    apiQuery("FILMOWE SPOTKANIA Z PSYCHOLOGIĄ: Ojczyzna")        shouldBe "Ojczyzna"
    apiQuery("Spotkania Filmowe: Werdykt")                       shouldBe "Werdykt"
  }

  it should "leave a film whose own title carries a colon alone (no leading spotkani- banner)" in {
    // 'SMOK (Spotkania Młodych Odkrywców Kina). Trzy kolory: Czerwony' — the banner
    // does not START with a spotkani- word, so the colon prefix rule never fires and
    // 'Trzy kolory: Czerwony' (Three Colours: Red) keeps its own colon.
    apiQuery("Trzy kolory: Czerwony") shouldBe "Trzy kolory: Czerwony"
  }

  // ── recase — shared banner-aware display casing applied to every scraper ────

  "recase" should "sentence-case an all-UPPERCASE title" in {
    recase("OJCZYZNA")            shouldBe "Ojczyzna"
    recase("MAVKA. PRAWDZIWY MIT") shouldBe "Mavka. Prawdziwy mit" // caps after a 5-letter word + '. '
  }

  it should "sentence-case an all-lowercase title" in {
    recase("drzewo magii") shouldBe "Drzewo magii"
  }

  it should "leave an already-mixed-case title byte-identical (guardrail)" in {
    recase("Paris Saint-Germain") shouldBe "Paris Saint-Germain"
    recase("Moulin Rouge!")       shouldBe "Moulin Rouge!"
    // A LONE all-caps word inside an otherwise-cased title is almost always a
    // real acronym ("UEFA"), so it stays — only a run of 2+ shouted words is
    // re-cased (see below).
    recase("Liga Mistrzów UEFA - Finał 2026: Paris Saint-Germain") shouldBe
      "Liga Mistrzów UEFA - Finał 2026: Paris Saint-Germain"
  }

  it should "down-case a run of 2+ shouted words inside an otherwise-cased title" in {
    recase("FEDERICO FELLINI: Ciao a tutti! - Osiem i pół") shouldBe
      "Federico Fellini: Ciao a tutti! - Osiem i pół"
    // Two adjacent shouted words elsewhere in the title, too.
    recase("Rocky BALBOA RETURNS dzisiaj") shouldBe "Rocky Balboa Returns dzisiaj"
    // A pure roman numeral inside a shouted run keeps its caps.
    recase("Rocky BALBOA II powraca") shouldBe "Rocky Balboa II powraca"
  }

  it should "down-case a word a lowercase connective stranded out of the shout run" in {
    // "RYBKA ZWANA" is the run that flags the shout; "WANDA" is pulled in too
    // rather than left half-shouted, even though a lowercase "i" strands it out of
    // the run. (No identity-changing canonical strip here, so the guard adopts it.)
    recase("RYBKA ZWANA i WANDA dzisiaj") shouldBe "Rybka Zwana i Wanda dzisiaj"
  }

  it should "keep the original casing when re-casing would re-key the row's identity" in {
    // Down-casing "GWIEZDNE WOJNY:" → "Gwiezdne Wojny:" makes the case-sensitive
    // "Gwiezdne Wojny: " canonical strip fire (it doesn't match the all-caps form),
    // so the row would sanitize to a DIFFERENT key — scattering its merge and
    // spinning the staging fold. recase must leave such a title as scraped.
    val shout = "GWIEZDNE WOJNY: MANDALORIAN i GROGU"
    recase(shout) shouldBe shout
    TitleNormalizer.sanitize(recase(shout)) shouldBe TitleNormalizer.sanitize(shout)
  }

  it should "split a programme banner and case the banner and film independently" in {
    // The banner and the film each get the guardrail applied on their own, so an
    // all-caps film after an all-caps banner keeps its own leading capital.
    recase("FILMOWY KLUB SENIORA: OJCZYZNA") shouldBe "Filmowy klub seniora: Ojczyzna"
    recase("DYSKUSYJNY KLUB FILMOWY: VERTIGO") shouldBe "Dyskusyjny klub filmowy: Vertigo"
  }

  it should "keep a mixed-case banner as reported while casing only the all-caps film half" in {
    recase("Filmowy Klub Seniora: OJCZYZNA") shouldBe "Filmowy Klub Seniora: Ojczyzna"
  }

  it should "leave a digit-only title alone (no letters to case)" in {
    recase("2 + 2 = 5") shouldBe "2 + 2 = 5"
  }
}
