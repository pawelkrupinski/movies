# Rule ablation, 2026-10-21: which title and identity rules move takes

Proposal #5 (rule pruning). The tool is `scripts/identity-rule-ablation` / `scripts/identity_rule_ablation.py` with
`integration.IdentityRuleAblation`, on CI as `.github/workflows/identity-rule-ablation.yml`. It switches rules off in
batches over recorded corpora and bisects every batch that moves takes. Each move is judged against
`identity-judgements.tsv`. Identity rules are switched off through `services.identity.RuleAblation`, which is not on
main yet (see "State").

## Data

- **Local run**, base 96fb23c6c, recordings 38713274617 (5 countries), 573 (pl,uk,us) and 528 (de,pl). Rounds 0–2
  finished; round 2 was cut off. 418 rules: 366 title rules and 52 identity rules (14 accept, 27 veto families,
  6 join, 5 apart).
- **CI runs**: 805 and 850 were stopped because they held the ubuntu slots, 984 lost its runner when docker restarted,
  and 1015's rows hit the 110-minute cap (one round over 573+528 takes ~37 min). None reached the collect step. Every
  number below comes from the local run.

## Per rule: takes moved by class

**200 title rules move zero takes** in all three recordings (`title:` ids in round 1's cleared batches). No identity
rule has been settled to a single rule yet. The 29 batch pairs still in bisection moved the following when their parent
batch was switched off. Each row is one parent batch of 4–8 rules, split into its two halves for the next round.

| batch (both halves) | rules | moved | cure | gained-right | right-lost | right->wrong | new-wrong |
|---|---|---|---|---|---|---|---|
| b001 | `accept:dated-title`, `accept:directors-title`, `accept:directors-work`, `accept:exact-top-hit`, `accept:favoured-calibrated`, `accept:house-production`, `accept:imdb-suggested`, `accept:season-production` | 2244 | 10 | 10 | 116 | 1 | 1 |
| b002 | `accept:season-record`, `accept:segment-top-hit`, `accept:sole-result`, `accept:sole-work`, `accept:stage-production`, `accept:unrivalled-calibrated` | 3938 | 16 | 0 | 968 | 289 | 0 |
| b003 | `apart:copied-original-apart`, `apart:denies-film`, `apart:different-films`, `apart:reused-page-apart`, `apart:seasonsapart` | 84 | 0 | 1 | 3 | 0 | 17 |
| b004 | `join:original-title`, `join:same-catalogue-id`, `join:same-film`, `join:same-search-form`, `join:same-title`, `join:title-segment` | 1285 | 6 | 12 | 278 | 11 | 11 |
| b010 | `title:helios-suffix-5`, `title:helios-suffix-6`, `title:helios-suffix-7`, `title:helios-suffix-8`, `title:kinematograf-year`, `title:kino-alternatywy-trim`, `title:muza-najlepsze-z-najgorszych`, `title:search-accessibility-tag` | 6 | 0 | 0 | 0 | 0 | 0 |
| b011 | `title:search-meeting-suffix`, `title:search-plus-event-suffix`, `title:search-programme-prefix`, `title:search-quote-low9`, `title:search-quote-right`, `title:search-spotkanie-banner-prefix`, `title:search-with-event-suffix`, `title:structural-anniversary-suffix` | 38 | 0 | 0 | 12 | 0 | 0 |
| b012 | `title:structural-cykl-prefix`, `title:structural-restored-suffix`, `title:structural-slash-suffix`, `title:structural-wersja-suffix`, `title:wybrzeze-venue-suffix`, `title:xtra-10-10-klasyka`, `title:xtra-4k-restoration-suffix`, `title:xtra-4k-suffix` | 14 | 0 | 0 | 1 | 2 | 0 |
| b013 | `title:xtra-age-bo-pipe-suffix`, `title:xtra-angielskie-napisy-suffix`, `title:xtra-audience-suffix`, `title:xtra-bare-format-suffix`, `title:xtra-canonical-broadcast-season`, `title:xtra-canonical-bts-world-tour`, `title:xtra-canonical-fellini-prefix`, `title:xtra-canonical-festival-tag-prefix` | 381 | 0 | 5 | 139 | 0 | 0 |
| b014 | `title:xtra-canonical-festival-tag-suffix`, `title:xtra-canonical-festival-year-tag-prefix`, `title:xtra-canonical-kropka-film-suffix`, `title:xtra-canonical-met-opera`, `title:xtra-canonical-met-opera-suffix`, `title:xtra-canonical-nt-live`, `title:xtra-canonical-rocznica-suffix`, `title:xtra-canonical-royal-ballet-opera` | 414 | 0 | 2 | 63 | 0 | 0 |
| b015 | `title:xtra-canonical-trailing-paren-year`, `title:xtra-canonical-trailing-sep-year`, `title:xtra-certificate-suffix`, `title:xtra-cinema-experience-suffix`, `title:xtra-code-after-format-tag`, `title:xtra-cykl-prefix`, `title:xtra-czwartek-konesera-suffix`, `title:xtra-dialog-przez-film` | 794 | 2 | 16 | 723 | 0 | 2 |
| b016 | `title:xtra-dkf-bare-prefix`, `title:xtra-dkf-suffix-dash`, `title:xtra-dkf-suffix-period`, `title:xtra-dkf-suffix-pipe-underscore`, `title:xtra-dot-spotkanie-suffix`, `title:xtra-edukacja-mh-suffix`, `title:xtra-encore-suffix`, `title:xtra-english-subtitles-suffix` | 70 | 0 | 0 | 2 | 0 | 0 |
| b017 | `title:xtra-event-projekt-suffix`, `title:xtra-exhibition-on-screen`, `title:xtra-fan-event-suffix`, `title:xtra-fellini-prefix`, `title:xtra-festival-acronym-suffix`, `title:xtra-festival-tag-prefix`, `title:xtra-festival-tag-suffix`, `title:xtra-festival-year-tag-prefix` | 9 | 0 | 0 | 0 | 0 | 0 |
| b018 | `title:xtra-film-club-suffix`, `title:xtra-film-festival-prefix`, `title:xtra-film-pl-suffix`, `title:xtra-filmoterapia-w-kinie`, `title:xtra-filmowy-klub-seniora-dash`, `title:xtra-filmy-z-lektorem`, `title:xtra-fks-suffix`, `title:xtra-imax-experience-suffix` | 18 | 0 | 0 | 3 | 0 | 1 |
| b020 | `title:xtra-kino-przy-herbatce`, `title:xtra-kino-sensoryczne-suffix`, `title:xtra-kinowy-poranek`, `title:xtra-klasyka-cycles`, `title:xtra-klub-filmowy-quoted`, `title:xtra-klub-filmowy-suffix`, `title:xtra-kntj-suffix`, `title:xtra-kobiece-strands` | 5 | 0 | 0 | 3 | 0 | 0 |
| b022 | `title:xtra-najlepsze-suffix`, `title:xtra-napisy-dla-nieslyszacych`, `title:xtra-niedziela-z-dokumentem`, `title:xtra-oficjalna-premiera-prefix`, `title:xtra-paren-anniversary-note`, `title:xtra-paren-pokaz-seans`, `title:xtra-paren-screening-event`, `title:xtra-pipe-cycle-banner` | 4 | 0 | 0 | 0 | 0 | 0 |
| b023 | `title:xtra-pipe-dla-rodzica`, `title:xtra-pipe-festival-suffix`, `title:xtra-plakatowa-trasa-suffix`, `title:xtra-pokaz-filmu`, `title:xtra-pokaz-specjalny-prefix`, `title:xtra-pokaz-suffix`, `title:xtra-pokaz-z-muzyka-suffix`, `title:xtra-poranki-dzieciece-suffix` | 5 | 0 | 0 | 0 | 0 | 0 |
| b024 | `title:xtra-pp-4dx-rewind`, `title:xtra-pp-akademia-kina-polskiego`, `title:xtra-pp-akademia-kina-swiatowego`, `title:xtra-pp-audience-strand`, `title:xtra-pp-best-film-on-tour`, `title:xtra-pp-debiut`, `title:xtra-pp-dkf-named`, `title:xtra-pp-filmowe-popoludnie` | 29 | 0 | 0 | 8 | 0 | 0 |
| b025 | `title:xtra-pp-fks-seniorki`, `title:xtra-pp-fregata-seniorow`, `title:xtra-pp-horror-season`, `title:xtra-pp-kf-klub`, `title:xtra-pp-kino-dla-seniora`, `title:xtra-pp-kino-dostepne`, `title:xtra-pp-kino-kobiet-prefix`, `title:xtra-pp-kino-na-obcasach` | 1 | 0 | 0 | 0 | 0 | 0 |
| b027 | `title:xtra-pp-ladies-night`, `title:xtra-pp-modowy-klub`, `title:xtra-pp-najlepsze-z-najgorszych`, `title:xtra-pp-pora-dla-seniora`, `title:xtra-pp-portret-kobiety`, `title:xtra-pp-przyjazny-audience`, `title:xtra-pp-psychoanaliza`, `title:xtra-pp-relaxed-screening` | 2 | 0 | 0 | 0 | 0 | 0 |
| b030 | `title:xtra-preview-dated-tail`, `title:xtra-preview-pipe-tail`, `title:xtra-preview-prefix`, `title:xtra-preview-suffix`, `title:xtra-print-prefix`, `title:xtra-print-strand-tail`, `title:xtra-przed-filmem-suffix`, `title:xtra-przedpremiera-prefix` | 6 | 0 | 0 | 0 | 0 | 0 |
| b031 | `title:xtra-przedpremiera-suffix`, `title:xtra-przedpremierowy-bullet`, `title:xtra-przyblizenia-psychoanaliza`, `title:xtra-qa-clause-tail`, `title:xtra-qa-paren-note`, `title:xtra-qa-plus-tail`, `title:xtra-quoted-cycle-dash`, `title:xtra-quoted-film-banner-tail` | 4 | 0 | 0 | 2 | 0 | 0 |
| b032 | `title:xtra-rerelease-paren-suffix`, `title:xtra-rocznica-premiery-suffix`, `title:xtra-runtime-suffix`, `title:xtra-screen-language-suffix`, `title:xtra-screening-note-segment`, `title:xtra-seans-suffix`, `title:xtra-senior-w-kinie-suffix`, `title:xtra-smok-prefix` | 10 | 0 | 0 | 3 | 0 | 0 |
| b033 | `title:xtra-strand-pipe-screen-prefix`, `title:xtra-subtitle-note-bracket`, `title:xtra-subtitle-paren-tag`, `title:xtra-swieto-kina-suffix`, `title:xtra-sztuka-na-ekranie`, `title:xtra-tani-dzien-suffix`, `title:xtra-teleskop-suffix`, `title:xtra-ukrainski-lang-suffix` | 11 | 0 | 0 | 10 | 0 | 0 |
| b034 | `title:xtra-unclosed-bracket-tail`, `title:xtra-venue-colon-strand-suffix`, `title:xtra-venue-non-fiction-suffix`, `title:xtra-version-original-suffix`, `title:xtra-version-strand-tail`, `title:xtra-vorpremiere-prefix`, `title:xtra-vorpremiere-suffix`, `title:xtra-wajda-dot-prefix` | 28 | 0 | 0 | 17 | 0 | 0 |
| b035 | `title:xtra-wajda-dziedzictwo`, `title:xtra-wajda-rewizje-suffix`, `title:xtra-wielka-sztuka-w-kinoteatrze`, `title:xtra-with-live-qa-suffix`, `title:xtra-wtorek-seniora-suffix`, `title:xtra-wymiary-kina-na-prefix` | 21 | 0 | 0 | 0 | 0 | 0 |
| b038 | `veto:a-catalogue-namesake-of-a-bare-billing-under-a-film-festival-s-banner`, `veto:a-film-of-the-venue-s-own-language-its-title-shares-no-word-with`, `veto:a-namesake-of-the-film-whose-director-its-billing-names`, `veto:a-part-its-title-gives-a-subtitle-none-of-the-film-s-titles-holds`, `veto:a-record-of-another-work-than-its-stage-house-s-billing-names`, `veto:a-screen-namesake-of-the-work-its-stage-house-s-billing-names`, `veto:a-short-its-feed-*`, `veto:an-instalment-its-title-numbers-whose-subtitle-none-of-the-film-s-titles-holds` | 161 | 0 | 4 | 27 | 2 | 20 |
| b039 | `veto:another-house-s-season-production`, `veto:another-house-s-staging-of-its-work-beside-its-own-house-s-record`, `veto:another-instalment-than-the-one-the-title-numbers`, `veto:another-production-of-its-work-than-the-season-its-screenings-fall-in`, `veto:billed-as-one-piece`, `veto:bills-both-works`, `veto:it-bills-a-silent-film-and-this-one-is-of-the-sound-era`, `veto:its-director-s-other-film-which-the-title-does-not-name` | 188 | 0 | 114 | 1 | 0 | 44 |
| b040 | `veto:its-title-bills-a-film-festival-by-its-name-alone-the-programme-no-film`, `veto:its-title-names-it-only-by-a-festival-s-programme-slot`, `veto:its-title-names-it-only-by-a-programme-tag-billed-beside-many-titles`, `veto:its-title-names-it-only-by-the-venue-s-own-name`, `veto:its-venue-s-own-cast-field-names-none-of-its-top-billed-cast-and-no-fact-the-venue-states-agrees-with-it`, `veto:named-only-by-the-title-its-feed-*`, `veto:seasonsapart`, `veto:uncorroborated-namesake` | 247 | 0 | 26 | 26 | 18 | 139 |
| b041 | `veto:vetoed-not-released-in-de-as-namesake-*`, `veto:vetoed-not-released-in-gb-as-namesake-*`, `veto:vetoed-not-released-in-pl-as-namesake-*` | 55 | 0 | 0 | 21 | 4 | 5 |

How to read it:
- Switching off any acceptance batch loses hundreds of right takes (b001, b002), and so does any join batch (b004).
  None of them is dead weight.
- The veto families cut wrong takes: switching off b039 or b040 adds 44 and 139 new wrong takes. b039 also gains 114
  right takes, which is worth bisecting, because one veto in it may be refusing right films.
- The title batches that move anything mostly lose right takes when switched off (b013, b015: 139 and 723 right-lost),
  so they are load-bearing.

## Harm-only rules

A rule is harm-only when switching it off cures wrong takes and costs nothing right. **None found so far at batch
resolution:** every batch with cures (b001, b002, b004, b015) also loses right takes. b039 is the only batch whose gain
(114 gained-right) beats its harm (44 new-wrong, 1 right-lost). Bisect it first; a harm-only veto, if there is one, is
in it. No removal is proposed without a single-rule measurement.

## Deletion candidates (ExtraTitleRules), not landed

The filter: zero takes moved, zero firings in the recordings (the run's `fired` counts), absent from the prod trace
(`ws5-prod-title-fired.tsv`), not a relay rule (met-opera, nt-live, royal-ballet-opera, bts, encore-suffix,
exhibition-on-screen), and not in WS1's subsumed-search set (`xtra-canonical-*`, `cine`, `bts`). **125 ExtraTitleRules**
pass it: `2026-10-21-rule-ablation-deletion-candidates.txt`. The TitleRules baseline is kept (ProdTitlesNormalizationSpec).

**The CI unit run of the deletion says it is not safe as it stands** (identity-run 1000, commit 256f608a9, never
landed):
- `FilmScheduleEndToEndSpec`: 13 checked-in rows project differently, with changed titles (e.g. "Sztuka W Centrum.
  Nowości 2026 | Caravaggio w Rzymie…", "Prawdziwa historia Tamary Łempickiej"). These rules still shape display titles
  and merge keys in recorded data. The ablation's `fired` count only sees the identity path.
- `ExtraTitleRulesSpec`, `TitleNormalizerSpec`, `DedicatedCinemaTitleCleanSpec`, `MovieServiceSpec` and
  `FilmwebDiffTitleNormalizerSpec` hold real venue titles that these rules exist for.
- Many candidates are **seasonal** (summer cycles "Lato w/z…", "Wakacyjne…", "KINO LETNIE", "Bezpieczne wakacje",
  Dzień Dziecka). An October corpus and the current prod trace cannot see them; they come back every summer.

Next step: prune the candidate list to rules with no spec example, no FilmScheduleEndToEnd row and no seasonal banner.
Then confirm the remainder with one CI measure on 573+528 plus the unit layer.

## Relay rules (listed, not touched)

`xtra-canonical-met-opera-suffix`, `xtra-canonical-nt-live`, `xtra-canonical-nt-live-suffix`,
`xtra-canonical-royal-ballet-opera`, `xtra-canonical-bts-world-tour`, `xtra-encore-suffix`, `xtra-exhibition-on-screen`
(WS1's relay matcher owns them).

## State

- Landed: the ablation tool (46d39b41e); recordings the convergence release no longer holds are read from the
  `identity-recordings` archive release (d2f25bde3), which now holds 38713274617's five trees and corpora; 2 shard
  rows (2476378ff); 110-minute rows (84c87b62e).
- `services.identity.RuleAblation` is measured inert (912: pinned pair, 5 countries, 0 moves; 913: 528 pl+de,
  0 moves). It waits for main to be green; main was red on MatchedTakesLabelSpec (Island 16 Met Opera encores), which
  is not this change.
- The CI sweep needs a cheaper shape before it can finish inside a row: one recording per row, and the baseline
  resolved once per recording instead of once per round.

## Veto batch b039 resolved (2026-10-22)

I bisected b039 on recording 573 in short resumed runs (1122, 1153, 1163, 1169). Single rules, each switched off alone:

| rule | moved | gained-right | new-wrong | unjudged |
|---|---|---|---|---|
| `veto:another-house-s-season-production` | 29 | 29 | 0 | 0 |
| `veto:another-house-s-staging-of-its-work-beside-its-own-house-s-record` | 13 | 0 | 0 | 13 |
| `veto:another-production-of-its-work-than-the-season-its-screenings-fall-in` | 24 | 0 | **22** | 0 |
| `veto:it-bills-a-silent-film-and-this-one-is-of-the-sound-era` | 2 | 0 | **1** | 0 |
| `veto:billed-as-one-piece`, `veto:bills-both-works`, `veto:another-instalment-than-the-one-the-title-numbers`, `veto:its-director-s-other-film-which-the-title-does-not-name` | 0 | 0 | 0 | 0 |

- **another-production** is load-bearing. Without it, UK "Opéra de Paris Ballet - La Bayadère / Notre-Dame de Paris"
  takes La Scala's 2006/2013 records, and the bare "Notre-Dame de Paris" takes the 1956 Hunchback.
- **another-house's season production** costs 29 right takes, all one listing: Flicks UK's "RBO Cinema Season 2026-27:
  La Fanciulla Del West", which the user's review says is the Met's 2026/27 record. Removing it was measured on CI
  (1195 on 573, 1196 on 528). Both are LANDABLE: +29 gained-right at the **model** stage, and **0 moves at the final
  stage**. The final take was already right: the Broadcast stage takes the Met record through the venue's
  Richard Jones credit (`Broadcast.credits`). The veto also guards seven unit cases, and removing it fails all
  seven: "A season listing's banner…", "A banner spelling its house…", `IncrementalResolverSpec`'s house re-learning,
  and four more. In those cases a Met-banner listing must not take RBO's Carmen or Così. The two shapes mirror each
  other. Only a production credit tells them apart, and that credit is the Broadcast stage's, not the veto layer's.
- **Decision: no change.** The veto stays. Its only cost is at the model stage, and production does not see it. No
  narrower veto is warranted until a recording shows a final-stage take the veto loses.
