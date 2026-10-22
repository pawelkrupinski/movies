# Rule consolidation, 2026-10-22: the title rules become one table

The user's decision after the rule ablation (`2026-10-21-rule-ablation.md`): consolidate and simplify the title rules
instead of pruning them. Deleting 125 "dead" rules broke 13 FilmScheduleEndToEnd rows (seasonal banners), and no rule
was harm-only, so the aim here is the same behaviour from less code.

## What changed

| | before (045d2fcff) | after |
|---|---|---|
| where the rules live | `TitleRules.scala` (199 lines) + `ExtraTitleRules.scala` (1,237 lines) | `common/src/main/resources/title-rules.tsv` (1,088 lines, 363 rows, the rest comments) read by `TitleRuleTable.scala` (74 lines); `TitleRules` / `ExtraTitleRules` are 13 / 19 lines that select its rows |
| title rules | 366 | 363 (3 DKF tails → 1, 2 meeting tails → 1) |
| ways a rule was built | 12: `TitleRule(...)` by hand, the helpers `prog`, `searchStrip`, `searchReplace`, `canon`, `canonProperName`, `perCinema`, `perCinemaReplace`, the Helios-suffix and trim loops, Scala-interpolated pattern pieces (8 constants), and 3 order-stamping blocks (`beforePlusEvent` = 13, `beforeSlashBanners` = 25, `100 + index`) | 1: a table row, applied by one of 5 kinds (`banner`, `query`, `key`, `name`, `venue`) with 2 flags (`all`, `seed`) |
| fold order | explicit `order` numbers, stamped per block, ties broken by id | the row's place in the table |
| dead code removed | `TitleRule.last` (no rule set it), the order stamping, the helpers, the duplicated audience/note/Fellini/festival-tag constants (now placeholders in `TitleRulePlaceholders`: `AUDIENCE`, `AUDIENCE_STRAND`, `NOTE_SEGMENT`, `FELLINI_PREFIX`, `FELLINI_SUFFIX`, `FESTIVAL_TAG_NUMBERED`, `FESTIVAL_TAG_YEAR`, `FESTIVAL_TAG_AFTER_PIPE`) | |

The table's columns: `id kind cinema countries flags effect season pattern replacement note [examples]`. `effect` and
`season` describe a row and change nothing; a seasonal row stays in force all year. The weekly decoration-discovery
bot now appends rows to the table (above its marker line), and the identity capture counts the table as a decision
input.

## Follow-ups landed (2026-10-22, the user's approval)

The user approved three follow-ups: the candidate merges below, removing the rows that never fired (keeping any a spec
or fixture exercises, or that is seasonal and fires on a listing recorded at another time of year), and retiring the
rows BillingParse reads in full where the parser provably covers them. Three commits, each checked on its own:

| commit | rows before → after | what |
|---|---:|---|
| `title rules: remove the 6 rows that fire on nothing …` | 363 → 357 | never-fired rows that fire nowhere |
| `title rules: retire 6 of the 106 rows BillingParse reads in full …` | 357 → 351 | covered rows whose removal no consumer sees |
| `title rules: merge 152 rows into 24 alternations …` | 351 → 223 | the candidate merges |

Each: `TitleRulesGoldenSpec` unchanged (no golden diff), the title-rule specs green (common `services.titlerules.*`,
`services.movies.*`, `services.billing.*` 602 tests; worker `services.movies.*`, `scripts.*`, `TitleTraceSpec`,
`MovieServiceSpec`, `FilmwebDiffTitleNormalizerSpec` 558; scrapers 2,924; `BillingParseEquivalenceSpec`).
CI, on the three together (head a495f8282, base b00dd72ea): `FilmScheduleEndToEndSpec` green (Forgejo run 1336);
identity measure 0 takes moved in all five countries on recording 573 (run 1335) and on 528 (run 1361): no new-wrong,
no right→wrong, no right lost, nothing for the user to judge. LANDABLE.

### Never-fired rows: 6 removed, 78 kept

The 84 rows were "never fired" on the October recordings, the prod trace and the 17.8k corpus. That is not all the
repository holds, and it is what the 125-row deletion of 2026-10-21 tripped over. `TitleRuleFiring` (new, test scope)
folds each row over **5.3M strings**: every checked-in fixture's text (HTML text and attributes, JSON strings; the June
e2e fixture `08-06-2026` among them), the specs' 10,926 literals, and the billings of all 13 cached recordings
(`~/.cache/kinowo-identity/recordings`: 38363428661 … 38713274617, 528, 563, 573, 647). It reports whether a row fires
inside any country's real fold or any venue's cleanup, only alone, or only unanchored.

- **Kept, 78** (18 of them seasonal): every one fires inside a fold on a checked-in fixture or a spec literal (a second
  pass over the fixtures and specs alone, 1.8M strings, finds all 78). Most are the June fixture's summer and
  Children's-Day banners ("Bezpieczne Wakacje: Fleak. Futrzak i ja", "KINO LETNIE 2026: Amadeusz (1984)", "DZIEŃ
  DZIECKA W APOLLO - Drzewo Magii", "LATO W LUNIE | Elio") and the rows' own spec examples; the Helios tags fire on the
  Helios fixtures ("Avengers: Doomsday - napisy", "… - seanse z konkursami HDD").
- **Removed, 6**: `xtra-canonical-trailing-paren-lang`, `xtra-pp-psychologia` (no string at all), `helios-suffix-12`
  ("- Dubbing", capitalised: no Helios title bills it) and `helios-suffix-14` ("- NAP": only Kino Bajka Kluczbork's "-
  NAPISY" contains it, and the row is anchored at the end), `xtra-klasyka-w-kinie-prefix` and
  `xtra-wakacje-z-klasyka-prefix` (each changes its spec titles alone, but in the fold an earlier row has already taken
  the banner off). `TitleRulePipelineDiff --together` on the 6: 10 strings touched, 0 differences.

### BillingParse-covered rows: 6 retired, 100 kept

"BillingParse reads it in full" (the inventory below) means one of the parser's readings equals the rule's output, most
often a `piece` of the billing. The pipeline does not read pieces for the lookup: `apiQuery` is the rules' fold alone,
and `ProjectionDrafting` (the stored `searchTitle`), `IdentityModel`, `FilmEvidence` and `FilmwebRatings` read it
without BillingParse; only `searchQuery` adds the format peel. So "provably covered" is checked where it counts:
`TitleRulePipelineDiff` (new, test scope) removes one row and reads every string it could touch (of the 5.3M) the ways
the consumers do — each venue's `cinemaClean` (the scrapers store it), the listing title after `BillingParse.peeled`,
`apiQuery` (also of the recased title), `searchQuery`, `sanitize`, the display pick, the proper name, the programme
banner and the recasing — in every country holding the row.

- **99 rows change a reading**, nearly all the lookup query: "Kino Konesera: Drugie życie" searches as "Drugie życie"
  only because `xtra-pp-konesera` strips it. They stay; retiring them needs the lookup to read `BillingParse.pieces`
  (WS2's next phase), which is a behaviour change to measure, not a byte-identical batch.
- **7 change nothing** (alone, and the 7 together: 447 strings, 0 differences). Other rows already fold their titles the same
  way (the generic `xtra-pipe-cycle-banner` takes the pipe tails of `xtra-10-10-klasyka`, `xtra-pnkf-suffix-pipe`,
  `xtra-kino-przy-herbatce` and `xtra-klasyczne-srody-suffix`); `xtra-dyskusyjny-suffix` and
  `xtra-pokazy-specjalne-suffix` are covered the same way. **6 retired.** `canonical-gwiezdne-wojny` stays: it is a seed row, and `ProdTitlesNormalizationSpec` pins
  the seed set alone ("Gwiezdne Wojny: Mandalorian i Grogu" keys differently without it), where the extra row
  `xtra-canonical-gwiezdne-wojny-ci` that covers it in production is absent.

### The merges: 152 rows → 24

The 24 candidates above (`xtra-dkf-suffix` landed earlier), less their removed or retired members (`xtra-pp-psychologia`,
`xtra-klasyka-w-kinie-prefix`, `xtra-wakacje-z-klasyka-prefix`, the five pipe/dash tails retired above), each merged at
the member the earlier check showed the fewer differences for. `xtra-restored-version-suffix` goes at its LAST member:
there "Absolwent - odrestaurowana wersja 4K" (`ExtraTitleRulesSpec`) keeps its fold; at the first it did not. The
user's figure was 23 merges / 138 rows; the doc's table held 24 / 160 besides `xtra-dkf-suffix`, all of which pass.

`TitleRuleTableDiff` (corpus, spec titles, and every decoration stacked two at a time around a bare film), per merge:
no corpus or spec title changes; the differences are synthetic stacks only.

| merged row | members | placed at | titles beside another row fold differently | stacked members fold differently |
|---|---:|---|---:|---:|
| `xtra-pp-senior-banners` | 9 | first | 1006 | 196 |
| `xtra-pp-summer-banners` | 6 | first | 504 | 70 |
| `xtra-pp-family-banners` | 8 | first | 624 | 52 |
| `xtra-pp-audience-banners` | 5 | first | 420 | 18 |
| `xtra-pp-women-banners` | 7 | last | 234 | 28 |
| `xtra-pp-classics-banners` | 6 | last | 670 | 42 |
| `xtra-pp-club-banners` | 6 | last | 762 | 82 |
| `xtra-pp-psychology-banners` | 2 | first | 0 | 2 |
| `xtra-pp-festival-banners` | 9 | last | 506 | 78 |
| `xtra-pp-retrospective-banners` | 7 | last | 642 | 52 |
| `xtra-pp-cycle-banners` | 10 | last | 984 | 106 |
| `xtra-pp-us-uk-strands` | 6 | last | 432 | 40 |
| `xtra-director-dot-prefix` | 4 | first | 48 | 18 |
| `xtra-pipe-strand-suffix` | 8 | last | 338 | 42 |
| `xtra-dash-strand-suffix` | 12 | first | 1462 | 218 |
| `xtra-summer-prefix` | 7 | first | 0 | 0 |
| `xtra-strand-prefix` | 16 | last | 204 | 40 |
| `xtra-art-strand-prefix` | 4 | first | 266 | 10 |
| `xtra-retrospective-prefix` | 2 | first | 0 | 0 |
| `xtra-festival-prefix` | 4 | first | 208 | 6 |
| `xtra-premiere-prefix` | 5 | first | 652 | 46 |
| `xtra-named-cycle-prefix` | 5 | first | 0 | 0 |
| `xtra-english-subtitles-suffix` | 2 | first | 0 | 4 |
| `xtra-restored-version-suffix` | 2 | last | 122 | 2 |
| **all 24 together** | 152 | | 2346 | 6508 |

What a stack does now: "Filmowe popołudnie dla dzieci: SKOCZ Z BAJTLEM DO KINA: Lalka" lost both banners when the
two rows folded one after the other; the one alternation takes the first and leaves "SKOCZ Z BAJTLEM DO KINA: Lalka"
(SearchTitles' after-colon candidate still searches "Lalka").

## Byte-identity, as checked

- **Rule level** (batches 1–3, one per tier): every rule compiles to the same pattern, replacement, flags, countries
  and note, and each tier and venue folds them in the same sequence. Checked by dumping the rule set before and after
  (`TitleRulesGoldenSpec --rules`).
- **Corpus level**: `TitleRulesGoldenSpec` (new) pins what each country's rule set makes of 17,819 titles (every
  billing of the identity recordings and the prod-title snapshot): the lookup query, the merge-key fold, the display
  spelling, the proper-name spelling, the programme banner, the banner boundary and each venue's cleanup. Unchanged by
  every commit here.
- **Specs**: `ProdTitlesNormalizationSpec`, `ExtraTitleRulesSpec`, `TitleNormalizerSpec`, `DedicatedCinemaTitleCleanSpec`,
  `DiscoveredDecorationsSpec`, `PolishRewriteScopingSpec`, `TitleRuleSetSpec`, `BillingParseEquivalenceSpec` (reads no
  title rule) and `FilmScheduleEndToEndSpec`: green locally (common 1,704 tests, worker `services.movies.*`/`scripts.*` 523, scrapers 2,924) and `FilmScheduleEndToEndSpec` on CI (Forgejo run 1151, head 4f19c0375).
- **Identity measure**: 0 takes moved in all five countries of the pinned pair (573), for the migration alone (run 1143, head 820f90395) and for the whole change with the merges (run 1150, head 4f19c0375), both against 045d2fcff: LANDABLE.

## Merging rows: what is byte-identical and what is not

The plan's step 3 was to merge rows that say the same thing (overlapping banners, one family's phrases) into one
alternation each. 25 candidate merges covering 163 rows (`xtra-pp-senior-banners`, `xtra-pp-summer-banners`,
`xtra-dash-strand-suffix`, …) were built and checked two ways:

1. the golden corpus and the specs: **every** candidate passed, all 163 rows merged into 25;
2. `TitleRuleTableDiff` (new, test scope): the corpus plus the specs' own 10,926 string literals, plus synthetic titles
   stacking two decorations around a bare film (each decoration a rule strips off a corpus title), at least one from a
   merged row.

The second check is what a merge cannot pass in general. A merge changes two things the corpus barely exercises:

- **where a member folds.** The merged row sits at one place; a member that used to fold after, say, the 4K strip now
  folds before it. `ExtraTitleRulesSpec` caught one: "Absolwent - odrestaurowana wersja 4K" kept its "- odrestaurowana
  wersja" when the two re-release rows folded together above the 4K strip.
- **how many members apply.** N rows fold one after another, so "Kino Konesera: DKF Kropka: Lalka" loses both banners;
  one alternation applies once and leaves the second. Which banners stacked titles lose is itself an accident of the
  old order (the reverse stacking already kept one), but it is a change.

Per candidate (pl and uk rule sets; "beside" = a member's decoration beside another row's, "stacked" = two members'):

| merged row | members | titles folding differently (corpus + stacks), placed at the first member | … at the last member | rows |
|---|---:|---:|---:|---|
| `xtra-pp-senior-banners` | 9 | 650 | 1102 | `xtra-pp-pora-dla-seniora`, `xtra-pp-wtorki-seniora`, `xtra-pp-kino-dla-seniora`, `xtra-pp-spotkania-seniora`, `xtra-pp-fks-seniorki`, `xtra-pp-kino-seniora-pipe`, `xtra-pp-kino-seniora-colon`, `xtra-pp-kino-seniora-dash`, `xtra-pp-fregata-seniorow` |
| `xtra-pp-summer-banners` | 6 | 0 | 0 | `xtra-pp-lato-cycle`, `xtra-pp-wakacyjne-cycle`, `xtra-pp-wakacje-cycle`, `xtra-pp-kino-plenerowe`, `xtra-pp-bezpieczne-wakacje`, `xtra-pp-kino-letnie` |
| `xtra-pp-family-banners` | 8 | 92 | 692 | `xtra-pp-filmowe-popoludnie`, `xtra-pp-bajkowe-poranki`, `xtra-pp-rodzina-w-kinie`, `xtra-pp-baltycki-deser`, `xtra-pp-skocz-z-bajtlem`, `xtra-pp-janosik-kids`, `xtra-pp-seans-dla-rodzicow`, `xtra-pp-venue-junior` |
| `xtra-pp-audience-banners` | 5 | 280 | 786 | `xtra-pp-przyjazny-audience`, `xtra-pp-toddler-club`, `xtra-pp-kino-dostepne`, `xtra-pp-relaxed-screening`, `xtra-pp-audience-strand` |
| `xtra-pp-women-banners` | 7 | 292 | 104 | `xtra-pp-portret-kobiety`, `xtra-pp-kobieta-pelna-zycia`, `xtra-pp-kino-kobiet-prefix`, `xtra-pp-mikrofeminizacje`, `xtra-pp-czule-kino`, `xtra-pp-kino-na-obcasach`, `xtra-pp-ladies-night` |
| `xtra-pp-classics-banners` | 6 | 454 | 196 | `xtra-pp-klasyka-cycle`, `xtra-pp-klasyka-na-topie`, `xtra-pp-klasyka-w-kinie`, `xtra-pp-klasyk-w-kinie`, `xtra-pp-wtorek-z-klasyka`, `xtra-pp-z-klasa-do-kina` |
| `xtra-pp-club-banners` | 6 | 654 | 630 | `xtra-pp-konesera`, `xtra-pp-dkf-named`, `xtra-pp-klub-filmowy`, `xtra-pp-kf-klub`, `xtra-pp-modowy-klub`, `xtra-pp-klub-dlr` |
| `xtra-pp-psychology-banners` | 3 | 2 | 2 | `xtra-pp-psychoanaliza`, `xtra-pp-psychologia`, `xtra-pp-filmoterapia` |
| `xtra-pp-festival-banners` | 9 | 518 | 192 | `xtra-pp-big-festivalowski`, `xtra-pp-pnkf-prefix`, `xtra-pp-numbered-festival`, `xtra-pp-pnkf-bare`, `xtra-pp-fiesta-hiszpanskiego`, `xtra-pp-avant-art-festival`, `xtra-pp-replika`, `xtra-pp-tnkf`, `xtra-pp-kmw` |
| `xtra-pp-retrospective-banners` | 7 | 576 | 236 | `xtra-pp-bunuel-retro`, `xtra-pp-wajda-rewizje`, `xtra-pp-wajda-rewizje-bare`, `xtra-pp-mamoru-hosody`, `xtra-pp-rok-z-marilyn`, `xtra-pp-radu-jude-retro`, `xtra-3-wieczory-kieslowski` |
| `xtra-pp-cycle-banners` | 10 | 896 | 310 | `xtra-pp-ciekawi-swiata`, `xtra-pp-filmy-gorskie`, `xtra-pp-debiut`, `xtra-pp-najlepsze-z-najgorszych`, `xtra-pp-czlowiek-na-planie`, `xtra-pp-meskie-kino`, `xtra-pp-tani-wtorek`, `xtra-pp-akademia-kina-polskiego`, `xtra-pp-50-na-51`, `xtra-pp-akademia-kina-swiatowego` |
| `xtra-pp-us-uk-strands` | 6 | 602 | 304 | `xtra-pp-best-film-on-tour`, `xtra-pp-terror-tuesday`, `xtra-pp-special-event-alamo`, `xtra-pp-showtune-sundays`, `xtra-pp-4dx-rewind`, `xtra-pp-horror-season` |
| `xtra-dkf-suffix` | 3 | 0 | 0 | `xtra-dkf-suffix-pipe-underscore`, `xtra-dkf-suffix-dash`, `xtra-dkf-suffix-period` |
| `xtra-director-dot-prefix` | 4 | 48 | 56 | `xtra-wajda-dot-prefix`, `xtra-konwicki-prefix`, `xtra-hosoda-dot-prefix`, `xtra-konwicki-dot-prefix` |
| `xtra-pipe-strand-suffix` | 12 | 1116 | 948 | `xtra-10-10-klasyka`, `xtra-pnkf-suffix-pipe`, `xtra-wtorek-seniora-suffix`, `xtra-kino-przy-herbatce`, `xtra-teleskop-suffix`, `xtra-amondo-grindhouse`, `xtra-klasyczne-srody-suffix`, `xtra-wakacje-z-dokumentem-suffix`, `xtra-kf-klub-suffix`, `xtra-poniedzialki-konwicki`, `xtra-lato-robin-williams`, `xtra-age-bo-pipe-suffix` |
| `xtra-dash-strand-suffix` | 13 | 1446 | 2024 | `xtra-kino-dla-suffix`, `xtra-kino-kobiet-suffix`, `xtra-kntj-suffix`, `xtra-kino-konesera-suffix`, `xtra-dyskusyjny-suffix`, `xtra-koncert-suffix`, `xtra-plakatowa-trasa-suffix`, `xtra-on-35mm-suffix`, `xtra-wajda-o-filmie-suffix`, `xtra-tadeusz-konwicki-suffix`, `xtra-wong-kar-wai-suffix`, `xtra-premiera-ksiazki-suffix`, `xtra-przeglad-filmow-suffix` |
| `xtra-summer-prefix` | 8 | 0 | 0 | `xtra-lato-wakacje-klasyka`, `xtra-wakacje-dla-dzieci`, `xtra-lato-w-kinie`, `xtra-filmowe-wakacje-za-rogiem`, `xtra-filmowe-lato`, `xtra-wakacje-w-kinie`, `xtra-filmowe-lato-named`, `xtra-wakacje-z-klasyka-prefix` |
| `xtra-strand-prefix` | 16 | 476 | 166 | `xtra-filmy-z-lektorem`, `xtra-smiech-przez-lzy`, `xtra-niedziela-z-dokumentem`, `xtra-kinowy-poranek`, `xtra-przyblizenia-psychoanaliza`, `xtra-kino-bez-barier-dzieci`, `xtra-classy-monday-prefix`, `xtra-maraton-prefix`, `xtra-dialog-przez-film`, `xtra-mistrzowska-kreska`, `xtra-lekcje-filmowe`, `xtra-najlepsze-dash-prefix`, `xtra-kino-dla-ducha-prefix`, `xtra-smok-prefix`, `xtra-wymiary-kina-na-prefix`, `xtra-poniedzialki-konwicki-prefix` |
| `xtra-art-strand-prefix` | 4 | 102 | 194 | `xtra-wielka-sztuka-w-kinoteatrze`, `xtra-sztuka-w-centrum`, `xtra-sztuka-na-ekranie`, `xtra-art-beats` |
| `xtra-retrospective-prefix` | 2 | 0 | 0 | `xtra-pedro-almodovar-kolory`, `xtra-wajda-dziedzictwo` |
| `xtra-festival-prefix` | 4 | 0 | 96 | `xtra-wtf-fest-prefix`, `xtra-bkf-prefix`, `xtra-pp-ffs-festival`, `xtra-konkurs-pelnometrazowych` |
| `xtra-premiere-prefix` | 5 | 350 | 368 | `xtra-przedpremiera-prefix`, `xtra-pokaz-przedpremierowy-prefix`, `xtra-oficjalna-premiera-prefix`, `xtra-premiera-prefix`, `xtra-vorpremiere-prefix` |
| `xtra-named-cycle-prefix` | 6 | 0 | 0 | `xtra-cykl-prefix`, `xtra-przeglad-colon-prefix`, `xtra-przeglad-filmow-prefix`, `xtra-klasyka-w-kinie-prefix`, `xtra-klasyka-cycles`, `xtra-nlecie-org-prefix` |
| `xtra-english-subtitles-suffix` | 2 | 4 | 4 | `xtra-english-subtitles-suffix`, `xtra-angielskie-napisy-suffix` |
| `xtra-restored-version-suffix` | 2 | 0 | 44 | `xtra-rerelease-suffix`, `xtra-restored-adjective-first-suffix` |

With the specs' titles as decorations too (`--extra`), the candidates that showed 0 above were not 0 either:
`xtra-pp-summer-banners` 580 (510 beside, 70 stacked), `xtra-festival-prefix` 214, `xtra-named-cycle-prefix` 24,
`xtra-english-subtitles-suffix` 4, `xtra-pp-psychology-banners` 2: their members had no corpus title to stack before.
`xtra-restored-version-suffix` is the Absolwent case above. No candidate changed a corpus title or a spec title alone.

The two merges that landed are of another kind: each member cuts the title from its marker to the end
(`…DKF\b.*$`, `…przed filmem\b.*$`, `…Spotkani… z\b.*$`), and the members are neighbours in the fold. Run one after
another they cut at the first marker of any of them; one alternation's leftmost match cuts at the same place.
`TitleRuleTableDiff` with every corpus, spec and stacked title: 0 differences.

Landed at first: only those two (`xtra-dkf-suffix` from the pipe/underscore, dash and full-stop DKF tails; `xtra-meeting-tail`
from `xtra-przed-filmem-suffix` and `xtra-dot-spotkanie-suffix`). The other 23 change behaviour on stacked titles only
(none in the corpus, the recordings or the specs) and wait for the user's call: each group's rows are in the table above, and `TitleRuleTableDiff` re-runs the check
(`scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRuleTableDiff --extra=<spec titles> <before> <after>"`).

## Same pattern in two tiers

Three patterns are folded both into the lookup query and into the merge key: the Fellini retrospective's prefix and
suffix, and the festival acronym after a pipe. They stay two rows each (each tier folds at its own place, and an id
names one rule — `ExtraTitleRulesSpec`), but the pattern is now written once, as a placeholder both rows use. No pattern
is repeated across countries: only 3 rows name countries at all (all `pl`).

## BillingParse overlap

For each rule alone, on the corpus: how many titles it changes, and on how many of those `BillingParse` already reads
the same title out of the raw text (its format peel, certificate, version tag, years, a delimited piece, a bill's piece,
a slash piece or the one billed title). 107 rows are read in full by BillingParse on every corpus title they change
(mostly banners, through `pieces`), 89 in part. None can be deleted byte-identically yet: the lookup query is the
rules' fold, which does not consult BillingParse. They are the rows a lookup that read `BillingParse.pieces` would make
redundant (WS2's next phase), listed per row in the inventory below. The follow-up above checked each at the pipeline's
consumers and retired the 6 whose removal none of them sees.

## Rows that never fired (as listed for the user's review)

The follow-up above removed 6 of these and kept 78 (each fires on a fixture or spec title).

84 rows fired nowhere: not in the prod trace (`ws5-prod-title-fired.tsv`, 2026-10-21), not in the ablation's
recordings (38713274617, 573, 528), and not on the 17.8k-title corpus. 19 of them are seasonal (summer cycles,
Dzień Dziecka): an October corpus cannot see them, and they are kept.

| row | kind | effect | season | note |
|---|---|---|---|---|
| `xtra-pp-big-festivalowski` | banner | festival |  | Big Festivalowski festival cycle |
| `xtra-pp-lato-cycle` | banner | banner | jun-sep | 'Lato w/z <cycle>:' summer cycles |
| `xtra-pp-wakacyjne-cycle` | banner | banner | jun-sep | 'Wakacyjne <cycle>:' holiday cycles |
| `xtra-pp-wakacje-cycle` | banner | banner | jun-sep | 'Wakacje w/z <cycle>:' holiday cycles |
| `xtra-pp-filmy-gorskie` | banner | banner |  | Filmy górskie mountain-film cycle |
| `xtra-pp-kino-plenerowe` | banner | banner | jun-sep | Kino plenerowe open-air cycle |
| `xtra-pp-bunuel-retro` | banner | retrospective |  | Buñuel retrospective banner |
| `xtra-pp-ciekawi-swiata` | banner | banner |  | Ciekawi Świata documentary cycle |
| `xtra-pp-baltycki-deser` | banner | banner |  | Bałtycki Deser children's cycle |
| `xtra-pp-psychologia` | banner | banner |  | Filmowe spotkania z psychologią |
| `xtra-pp-klasyka-na-topie` | banner | banner |  | 'Klasyka na TOPie [na …]:' classics strand |
| `xtra-pp-numbered-festival` | banner | festival |  | '12. Przeźrocza Festiwal Filmowy: <film>' numbered festival-edition prefix, generic in the festival name — sib |
| `xtra-pp-bezpieczne-wakacje` | banner | banner | jun-sep | 'Bezpieczne wakacje:' kids-summer strand (Fleak, Hitpig, Tom i Jerry, …) |
| `xtra-pp-janosik-kids` | banner | audience |  | 'Janosik Dzieciom/Szkrabom:' kids strands (Minionki i straszydła) |
| `xtra-pp-meskie-kino` | banner | banner |  | 'Męskie Kino [na Dzień Ojca]:' cycle (Czas Apokalipsy) |
| `xtra-pp-kobieta-pelna-zycia` | banner | audience |  | 'Kobieta Pełna Życia:' Fregata cycle (Książę) |
| `xtra-pp-kino-letnie` | banner | banner | jun-sep | 'KINO LETNIE 2026:' summer-cinema strand (Amadeusz, Requiem dla snu, Rambo, Norymberga, Dom Dobry, Przekleństw |
| `xtra-pp-mamoru-hosody` | banner | retrospective |  | 'Kolekcja Mamoru Hosody: <film>' anime-retrospective banner (Cinema City + Multikino) — sibling of the 'Hosoda |
| `xtra-pp-dzien-dziecka-w-kinie` | banner | audience | may-jun | 'DZIEŃ DZIECKA W <venue> - <film>' Children's-Day banner, any venue's name (Kino Apollo's 'DZIEŃ DZIECKA W APO |
| `xtra-pp-radu-jude-retro` | banner | banner |  | 'Radu Jude. Retrospektywa: <film> (YYYY)' director-retrospective prefix — 5 films, each year-scoped-unique on  |
| `xtra-pp-replika` | banner | banner |  | 'Replika KFF: / Replika Młodzi i Film: <film>' festival-replay prefix (KFF = Krakowski Festiwal Filmowy, 'Młod |
| `xtra-pp-avant-art-festival` | banner | festival |  | 'Avant Art. Festival/Festiwal YYYY: <film>' festival prefix — the year follows the NAME, so the ordinal-first  |
| `xtra-pp-czule-kino` | banner | banner |  | 'czułe kino: <film>' Kino Orzeł discussion strand — the inner banner the seed 'Spotkania …:' rule leaves behin |
| `xtra-3-wieczory-kieslowski` | banner | banner |  | '3 Wieczory: KIEŚLOWSKI. <film>' Kino za Rogiem retrospective prefix |
| `xtra-pp-z-klasa-do-kina` | banner | banner |  | 'Z klasą do kina: <film>' school-screening strand (Wielicka Mediateka: Lalka) |
| `xtra-pp-venue-junior` | banner | venue |  | '<venue> JUNIOR: <film>' children's strand, any venue's name (Kino w Kadrze's 'KADR JUNIOR: Toy Story 5') |
| `xtra-ss-amerykanska-klasyka` | query | banner |  | '… Amerykańska klasyka od <distributor>' re-release strapline |
| `xtra-jim-jarmusch-suffix` | query | retrospective |  | '// jim jarmusch' director-cycle suffix (Kino za Rogiem) |
| `xtra-pedro-almodovar-kolory` | query | retrospective |  | 'Pedro Almodóvar: Kolory emocji - <film>' retrospective (Matador, Matki równoległe, …) |
| `xtra-pokaz-przedpremierowy-prefix` | query | event |  | 'Pokaz przedpremierowy: <film>' premiere prefix |
| `xtra-jarocinski-festiwal-suffix` | query | festival |  | '<film>: II Jarociński Festiwal Filmowy dla Dzieci i Młodzieży' suffix (~6 films) |
| `xtra-przedpremierowo-paren` | query | event |  | '<film> (przedpremierowo)' parenthesised pre-premiere tag (Kino Mikro: Róża) |
| `xtra-premiera-comma-suffix` | query | event |  | '<film>, PREMIERA' comma-separated premiere tail (Kino Apollo: Big Shark) |
| `xtra-wstep-wolny-suffix` | query | event |  | '<film> - LEKTOR - WSTĘP WOLNY' free-entry tail, the version tag before it re-peeled by searchQuery (Kino Bajk |
| `xtra-letnie-kino-dzieciom-suffix` | query | audience | jun-sep | '<film> - 2D Dubbing - Letnie Kino Dzieciom' children's-summer strand tail (Kino Hel) |
| `xtra-rekonstrukcja-pipe-suffix` | query | edition |  | '<film> / Rekonstrukcja 4K / PRELEKCJA PRZED SEANSEM' restoration + talk pipe tail (GCF: Do utraty tchu) |
| `xtra-premiera-ksiazki-suffix` | query | event |  | '<film> - premiera książki, pokaz filmu + dyskusja' book-launch suffix (Kino Kultura: Pasażerka) |
| `xtra-audiodeskrypcja-suffix` | query | audience |  | '<film> - audiodeskrypcją (na słuchawki), napisy dla osób nie(do)słyszących i PJM' accessible-screening suffix |
| `xtra-pokazy-specjalne-prefix` | query | event |  | 'Pokazy specjalne. [35 lat od premiery!] <film>' anniversary-screening prefix (Zacisze: Terminator 2) |
| `xtra-wajda-o-filmie-suffix` | query | retrospective |  | '<film> - Andrzej Wajda o filmie' suffix (Brzezina) |
| `xtra-wajda-rewizje-bare-year` | query | retrospective |  | 'Wajda Re wizje <film> <year>' separator-less retrospective banner with a bare trailing year (Kino Fenix: Kron |
| `xtra-wakacje-z-dokumentem-suffix` | query | banner | jun-sep | '<film> / Wakacje z dokumentem' documentary-strand suffix (Silver) |
| `xtra-wong-kar-wai-suffix` | query | retrospective |  | '<film> - Wong Kar Wai 4K' restoration-print suffix (Chungking Express → TMDB 11104) |
| `xtra-lato-wakacje-klasyka` | query | banner | jun-sep | 'LATO'26. Wakacje z Klasyką Kina - <film>' summer-classics strand (Casablanca, Amadeusz, Mulholland Drive, 200 |
| `xtra-wakacje-dla-dzieci` | query | audience | jun-sep | 'Wakacje dla dzieci: <film>' kids-summer strand (Arco, Pucio, Basia mam swój świat, Kicia Kocia w podróży, Fle |
| `xtra-lato-w-kinie` | query | banner | jun-sep | 'LATO w LUNIE / <film>' Kino Luna summer strand (Drzewo magii, Chłopiec na krańcach świata, Willow i tajemnicz |
| `xtra-smiech-przez-lzy` | query | banner |  | 'ŚMIECH PRZEZ ŁZY: <film>' comedy strand (Chłopaki nie płaczą, Wesele) |
| `xtra-konkurs-pelnometrazowych` | query | festival |  | 'MIĘDZYNARODOWY KONKURS FILMÓW PEŁNOMETRAŻOWYCH - <film>' festival full-length competition (Allah Is Not Oblig |
| `xtra-bkf-prefix` | query | festival |  | 'BKF #53 <film>' film-club numbered prefix (Chronologia wody) |
| `xtra-nlecie-org-prefix` | query | edition |  | '70-lecie Wydawnictwa Poznańskiego: <film>' anniversary-of-an-organisation prefix; the name guard is {{NSEP}}  |
| `xtra-capital-i-format-wrapper` | query | format |  | '<film> I 2D I' — a cinema that wraps the screen-format tag in literal capital-'I' separators instead of pipes |
| `xtra-filmoteka-dojrzalego` | query | banner |  | '<film> - Filmoteka Dojrzałego Człowieka' mature-viewers'-strand suffix (Sprawiedliwość owiec) |
| `xtra-przeglad-filmow-prefix` | query | banner |  | 'Przegląd filmów <reżyser> - <film>' retrospective PREFIX — sibling of the existing '<film> - przegląd filmów  |
| `xtra-filmowe-wakacje-za-rogiem` | query | banner | jun-sep | 'Filmowe wakacje za Rogiem: <film>' Kino za Rogiem kids-summer strand (Koszmarek, Pies i robot, Pan Zabawka, L |
| `xtra-filmowe-lato` | query | banner | jun-sep | 'Filmowe lato: <film>' summer strand (Toy Story 5) |
| `xtra-wakacje-w-kinie` | query | banner | jun-sep | 'Wakacje w Kinie Orzeł: / Wakacje w kinie: <film>' kids-summer strand (Drzewo magii, Chłopiec na krańcach świa |
| `xtra-pp-ffs-festival` | query | festival |  | '26 FFS <film>' film-festival edition prefix (Być kochaną, Ostatni wiking, Wartość sentymentalna, Świat po pra |
| `xtra-premiera-prefix` | query | event |  | 'Premiera: <film>' / 'PREMIERA!!! <film>' release-announcement prefix (Kumotry, Straszny film) — distinct from |
| `xtra-kino-bez-barier-dzieci` | query | audience |  | 'Kino bez barier dla dzieci: <film>' accessibility kids-strand — the seed 'Kino bez barier:' wants the colon r |
| `xtra-poniedzialki-konwicki-prefix` | query | retrospective |  | 'Poniedziałki z Konwickim: <film>' Mondays-with-<author> cycle PREFIX — the existing 'xtra-poniedzialki-konwic |
| `xtra-klasyka-w-kinie-prefix` | query | banner |  | 'Klasyka w kinie: <film>' classics strand — the bare colon form the venue-specific 'Klasyka w kinie Atlantic:' |
| `xtra-filmowe-lato-named` | query | banner | jun-sep | 'Filmowe Lato <name>: <film>' named summer strand — the bare 'Filmowe lato:' rule needs the colon right after  |
| `xtra-wakacje-z-klasyka-prefix` | query | banner | jun-sep | 'Wakacje z klasyką [kina]: <film>' summer-classics strand — the bare form the LATO-prefixed 'Wakacje z Klasyką |
| `xtra-restored-adjective-first-suffix` | query | edition |  | '<film> - odrestaurowana wersja [4K]' restoration suffix with the ADJECTIVE first — the mirror of 'xtra-rerele |
| `xtra-glm-guest-prefix` | query | banner |  | 'GŁM:<film>:<Guest Name>' Kino Millenium director/guest Q&A banner (GŁM:ODZYSKANY:MARCIN KWAŚNY → Odzyskany) |
| `xtra-koncert-z-rok-suffix` | query | event |  | '<film> (koncert z YYYY)' concert-recording-year note (André Rieu. Niech żyje Maastricht!) |
| `xtra-wakacyjne-poranki-quoted` | query | junk | jun-sep | 'WAKACYJNE PORANKI FILMOWE “<film>”' Kino Sokół Sokółka holiday-mornings strand — banner first, film QUOTED af |
| `xtra-premiera-lowercase-suffix` | query | event |  | '<film> premiera' bare lower-case premiere note (Centrum 3D Kalisz: Mity greckie i Odyseja) |
| `xtra-canonical-trailing-paren-lang` | key | format |  | Trailing PARENTHESISED 'wersja oryginalna' (original-language, no-dub) tag ('(wersja oryginalna)', '[wersja or |
| `cc-powrot-do-kin` | venue (cinema-city) | venue |  | Cinema City re-release suffix |
| `xtra-cyfrowe-premiera` | venue (cyfrowe-kino) | event |  | Cyfrowe Kino 'Premiera! <film>' prefix |
| `helios-suffix-2` | venue (helios) | format |  | Helios 'w Helios Anime' tag |
| `helios-suffix-4` | venue (helios) | format |  | Helios 'w HnS' tag |
| `helios-suffix-9` | venue (helios) | format |  | Helios '- seanse z konkursami HDD' tag |
| `helios-suffix-10` | venue (helios) | format |  | Helios '- Event projekt' tag |
| `helios-suffix-11` | venue (helios) | format |  | Helios '- dubbing' tag |
| `helios-suffix-12` | venue (helios) | format |  | Helios '- Dubbing' tag |
| `helios-suffix-13` | venue (helios) | format |  | Helios '- napisy' tag |
| `helios-suffix-14` | venue (helios) | format |  | Helios '- NAP' tag |
| `helios-suffix-15` | venue (helios) | format |  | Helios '- DUB' tag |
| `xtra-bajka-kluczbork-klaps` | venue (kino-bajka-kluczbork) | venue |  | Kino Bajka Kluczbork 'KLAPS / <cycle>: <film>' house-brand prefix (Jak żyć żeby nie zwariować, Przepis na świę |
| `xtra-starowce-akcja-lato` | venue (kino-na-starowce) | venue | jun-sep | Na Starówce '… [- film] akcja lato w kinie' campaign suffix (Toy Story 5, Vaiana) |
| `palacowe-wajda` | venue (kino-palacowe) | retrospective |  | Kino Pałacowe Wajda retrospective prefix |
| `xtra-starymlyn-sensoryczny` | venue (kino-stary-mlyn) | audience |  | Kino Stary Młyn '… sensoryczny' sensory-screening suffix (Toy Story 5) |

## Inventory

Every row before the merges, grouped by effect. `words` is the language the pattern's words are in (`any` for
punctuation and format shapes; a row's `countries` column is set only for the 3 Polish rewrites). Prod fires: rows of
the identity trace that fired it (ws5, 2026-10-21). Recordings: the ablation's base `fired` counts by country. Corpus:
TitleRuleInventory on the golden corpus. BillingParse: corpus titles it reads the same as the rule / titles the rule
changes, and the part that reads most of them.

| effect | banner | query | key | name | venue | rows | fired in prod | never fired anywhere | BillingParse reads every corpus firing | seasonal |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| banner | 48 | 56 | 0 | 0 | 0 | 104 | 49 | 31 | 37 | 15 |
| audience | 22 | 21 | 0 | 0 | 1 | 44 | 31 | 8 | 26 | 3 |
| event | 3 | 53 | 0 | 0 | 3 | 59 | 40 | 10 | 12 | 0 |
| edition | 0 | 14 | 2 | 0 | 0 | 16 | 12 | 3 | 4 | 0 |
| format | 1 | 24 | 1 | 0 | 18 | 44 | 25 | 11 | 8 | 0 |
| festival | 5 | 13 | 3 | 0 | 0 | 21 | 7 | 7 | 4 | 0 |
| retrospective | 5 | 19 | 2 | 0 | 1 | 27 | 7 | 9 | 5 | 1 |
| relay | 0 | 2 | 1 | 6 | 0 | 9 | 8 | 0 | 0 | 0 |
| rename | 0 | 0 | 5 | 0 | 0 | 5 | 2 | 0 | 2 | 0 |
| venue | 1 | 3 | 0 | 0 | 12 | 16 | 8 | 4 | 3 | 1 |
| junk | 0 | 10 | 3 | 0 | 8 | 21 | 14 | 1 | 6 | 1 |
| **all** | 85 | 215 | 17 | 6 | 43 | 366 | 203 | 84 | 107 | 21 |

The relay rows (Met Opera, NT Live, Royal Ballet & Opera, BTS, encore, Exhibition on Screen, the broadcast season)
are RelayStage's houses and were left as they were.

| row (before) | kind | effect | words | season | prod fires | recordings (by country) | corpus fires | BillingParse reads | after |
|---|---|---|---|---|---:|---|---:|---|---|
| `search-programme-prefix` | banner | banner | pl |  | 19 | pl:92 | 61 | 58/61 piece |  |
| `structural-cykl-prefix` | query | banner | pl |  | 3 | pl:9 | 6 | 1/6 piece |  |
| `xtra-pp-konesera` | banner | banner | pl |  | 4 | pl:10 | 6 | 6/6 piece |  |
| `xtra-pp-lato-cycle` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-wakacyjne-cycle` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-wakacje-cycle` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-filmowe-popoludnie` | banner | banner | pl |  | 1 | pl:4 | 5 | 4/5 piece |  |
| `xtra-pp-filmy-gorskie` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-kino-plenerowe` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-klasyka-cycle` | banner | banner | any |  | 2 | pl:7 | 3 | 3/3 piece |  |
| `xtra-pp-ciekawi-swiata` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-baltycki-deser` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-dkf-named` | banner | banner | any |  | 14 | pl:43 | 28 | 22/28 piece |  |
| `xtra-pp-klub-filmowy` | banner | banner | pl |  | 5 | pl:15 | 8 | 7/8 piece |  |
| `xtra-pp-debiut` | banner | banner | pl |  | 10 | pl:20 | 10 | — |  |
| `xtra-swieto-kina-suffix` | query | banner | pl |  | 6 | pl:12 | 6 | — |  |
| `xtra-edukacja-mh-suffix` | query | banner | pl |  | 11 | pl:36 | 12 | — |  |
| `xtra-pp-modowy-klub` | banner | banner | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-pp-klub-dlr` | banner | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-psychoanaliza` | banner | banner | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-pp-psychologia` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-filmoterapia` | banner | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-best-film-on-tour` | banner | banner | any |  | 3 | pl:6 | 6 | 2/6 piece |  |
| `xtra-pp-klasyka-na-topie` | banner | banner | any |  | 0 |  | 0 |  |  |
| `xtra-pp-klasyka-w-kinie` | banner | banner | pl |  | 0 |  | 5 | 5/5 piece |  |
| `xtra-pp-bezpieczne-wakacje` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-skocz-z-bajtlem` | banner | banner | pl |  | 0 |  | 2 | 1/2 piece |  |
| `xtra-pp-kmw` | banner | banner | pl |  | 4 | pl:13 | 5 | 4/5 piece |  |
| `xtra-pp-kf-klub` | banner | banner | pl |  | 1 | pl:6 | 4 | 2/4 piece |  |
| `xtra-pp-najlepsze-z-najgorszych` | banner | banner | pl |  | 5 | pl:15 | 7 | 6/7 piece |  |
| `xtra-pp-czlowiek-na-planie` | banner | banner | pl |  | 0 |  | 1 | — |  |
| `xtra-pp-meskie-kino` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-kino-letnie` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pp-tnkf` | banner | banner | pl |  | 0 |  | 11 | 11/11 piece |  |
| `xtra-pp-rok-z-marilyn` | banner | banner | pl |  | 2 | pl:7 | 5 | 5/5 piece |  |
| `xtra-pp-klasyk-w-kinie` | banner | banner | pl |  | 1 | pl:3 | 3 | 2/3 piece |  |
| `xtra-pp-fiesta-hiszpanskiego` | banner | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-radu-jude-retro` | banner | banner | any |  | 0 |  | 0 |  |  |
| `xtra-pp-akademia-kina-polskiego` | banner | banner | pl |  | 9 | pl:30 | 10 | — |  |
| `xtra-pp-50-na-51` | banner | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-mikrofeminizacje` | banner | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-replika` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-czule-kino` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-akademia-kina-swiatowego` | banner | banner | pl |  | 12 | pl:36 | 13 | 13/13 piece |  |
| `xtra-3-wieczory-kieslowski` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-terror-tuesday` | banner | banner | any |  | 4 | us:4 | 3 | 1/3 piece |  |
| `xtra-pp-showtune-sundays` | banner | banner | any |  | 1 | us:1 | 1 | 1/1 piece |  |
| `xtra-pp-z-klasa-do-kina` | banner | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-wtorek-z-klasyka` | banner | banner | pl |  | 2 | pl:7 | 3 | 2/3 piece |  |
| `xtra-pp-4dx-rewind` | banner | banner | any |  | 0 | uk:79 | 5 | — |  |
| `xtra-pp-horror-season` | banner | banner | any | oct | 245 | uk:734 | 6 | — |  |
| `xtra-ss-amerykanska-klasyka` | query | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-dkf-suffix-pipe-underscore` | query | banner | any |  | 3 | pl:12 | 5 | 4/5 piece | `xtra-dkf-suffix` |
| `xtra-dkf-suffix-dash` | query | banner | any |  | 4 | pl:10 | 9 | 6/9 piece | `xtra-dkf-suffix` |
| `xtra-dkf-suffix-period` | query | banner | pl |  | 0 | pl:1 | 1 | — | `xtra-dkf-suffix` |
| `xtra-dyskusyjny-suffix` | query | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-10-10-klasyka` | query | banner | pl |  | 6 | pl:18 | 8 | 8/8 piece |  |
| `xtra-amerykanska-klasyka` | query | banner | pl |  | 0 |  | 6 | 6/6 piece |  |
| `xtra-wielka-sztuka-w-kinoteatrze` | query | banner | pl |  | 4 | pl:12 | 6 | 3/6 piece |  |
| `xtra-sztuka-w-centrum` | query | banner | pl |  | 0 |  | 6 | 3/6 piece |  |
| `xtra-art-beats` | query | banner | pl |  | 0 |  | 3 | 1/3 piece |  |
| `xtra-przeglad-filmow-suffix` | query | banner | pl |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-kntj-suffix` | query | banner | any |  | 1 | pl:3 | 11 | 11/11 piece |  |
| `xtra-film-pl-suffix` | query | banner | pl |  | 2 | pl:7 | 3 | — |  |
| `xtra-age-bo-pipe-suffix` | query | banner | pl |  | 1 | pl:3 | 1 | — |  |
| `xtra-film-club-suffix` | query | banner | en |  | 5 | uk:10 | 5 | 5/5 piece |  |
| `xtra-kf-klub-suffix` | query | banner | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-kino-konesera-suffix` | query | banner | pl |  | 1 | pl:1 | 4 | 3/4 piece |  |
| `xtra-fks-suffix` | query | banner | pl |  | 5 | pl:15 | 8 | 8/8 piece |  |
| `xtra-wakacje-z-dokumentem-suffix` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-dkf-bare-prefix` | query | banner | pl |  | 22 | pl:65 | 26 | 22/26 piece |  |
| `xtra-fiesta-hiszpanskiego-suffix` | query | banner | pl |  | 0 |  | 6 | 6/6 piece |  |
| `xtra-amondo-grindhouse` | query | banner | any |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-filmoterapia-w-kinie` | query | banner | pl |  | 1 | pl:4 | 4 | 4/4 piece |  |
| `xtra-lato-wakacje-klasyka` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-lato-w-kinie` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-klasyka-cycles` | query | banner | pl |  | 4 | pl:11 | 8 | 7/8 piece |  |
| `xtra-smiech-przez-lzy` | query | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-classy-monday-prefix` | query | banner | any |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-niedziela-z-dokumentem` | query | banner | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-klub-filmowy-suffix` | query | banner | pl |  | 2 | pl:8 | 5 | 5/5 piece |  |
| `xtra-czwartek-konesera-suffix` | query | banner | any |  | 3 | pl:8 | 3 | — |  |
| `xtra-filmoteka-dojrzalego` | query | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-przeglad-filmow-prefix` | query | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-filmowe-wakacje-za-rogiem` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-filmowe-lato` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-wakacje-w-kinie` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-mistrzowska-kreska` | query | banner | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-najlepsze-dash-prefix` | query | banner | any |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-najlepsze-suffix` | query | banner | pl |  | 3 | pl:9 | 5 | 4/5 piece |  |
| `xtra-kino-dyskomfortu-suffix` | query | banner | pl |  | 3 | pl:9 | 3 | 3/3 piece |  |
| `xtra-sztuka-na-ekranie` | query | banner | pl |  | 2 | pl:4 | 3 | 3/3 piece |  |
| `xtra-przyblizenia-psychoanaliza` | query | banner | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-klasyka-w-kinie-prefix` | query | banner | any |  | 0 |  | 0 |  |  |
| `xtra-cykl-prefix` | query | banner | pl |  | 1 | pl:3 | 5 | — |  |
| `xtra-przeglad-colon-prefix` | query | banner | pl |  | 0 |  | 8 | 8/8 piece |  |
| `xtra-filmowe-lato-named` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-wakacje-z-klasyka-prefix` | query | banner | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-pipe-cycle-banner` | query | banner | pl |  | 25 | pl:76 | 101 | 92/101 piece |  |
| `xtra-glm-guest-prefix` | query | banner | pl |  | 0 |  | 0 |  |  |
| `xtra-plakatowa-trasa-suffix` | query | banner | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-klasyczne-srody-suffix` | query | banner | pl |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-smok-prefix` | query | banner | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-wymiary-kina-na-prefix` | query | banner | pl |  | 0 | pl:1 | 1 | — |  |
| `search-accessibility-tag` | query | audience | any |  | 11 | pl:25 | 15 | — |  |
| `xtra-pp-kino-seniora-pipe` | banner | audience | pl |  | 1 | pl:4 | 5 | 5/5 piece |  |
| `xtra-pp-pora-dla-seniora` | banner | audience | pl |  | 2 | pl:12 | 9 | 7/9 piece |  |
| `xtra-pp-wtorki-seniora` | banner | audience | pl |  | 3 | pl:8 | 8 | 7/8 piece |  |
| `xtra-pp-kino-dla-seniora` | banner | audience | pl |  | 2 | pl:7 | 4 | 4/4 piece |  |
| `xtra-pp-przyjazny-audience` | banner | audience | pl |  | 5 | pl:16 | 9 | 7/9 piece |  |
| `xtra-pp-toddler-club` | banner | audience | en |  | 206 | uk:315 | 7 | 5/7 piece |  |
| `xtra-pp-bajkowe-poranki` | banner | audience | any |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-pp-rodzina-w-kinie` | banner | audience | pl |  | 3 | pl:10 | 10 | 10/10 piece |  |
| `xtra-pp-fks-seniorki` | banner | audience | pl |  | 1 | pl:6 | 5 | 5/5 piece |  |
| `xtra-pp-kino-dostepne` | banner | audience | pl |  | 2 | pl:6 | 3 | 3/3 piece |  |
| `xtra-pp-portret-kobiety` | banner | audience | any |  | 0 | pl:3 | 2 | 2/2 piece |  |
| `xtra-pp-kino-seniora-colon` | banner | audience | pl |  | 6 | pl:15 | 5 | 5/5 piece |  |
| `xtra-pp-kino-seniora-dash` | banner | audience | pl |  | 0 | pl:1 | 1 | 1/1 piece |  |
| `xtra-pp-fregata-seniorow` | banner | audience | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-pp-janosik-kids` | banner | audience | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-kobieta-pelna-zycia` | banner | audience | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-kino-na-obcasach` | banner | audience | pl |  | 52 | pl:155 | 2 | 2/2 piece |  |
| `xtra-pp-ladies-night` | banner | audience | any |  | 25 | de:4 pl:69 | 2 | 2/2 piece |  |
| `xtra-pp-dzien-dziecka-w-kinie` | banner | audience | pl | may-jun | 0 |  | 0 |  |  |
| `xtra-pp-kino-kobiet-prefix` | banner | audience | pl |  | 3 | pl:9 | 3 | 3/3 piece |  |
| `xtra-pp-relaxed-screening` | banner | audience | en |  | 13 | uk:23 | 1 | 1/1 piece |  |
| `xtra-pp-audience-strand` | banner | audience | en |  | 27 | uk:58 | 32 | 17/32 piece |  |
| `xtra-napisy-dla-nieslyszacych` | query | audience | pl |  | 0 | pl:1 | 1 | 1/1 piece |  |
| `xtra-kino-sensoryczne-suffix` | query | audience | pl |  | 1 | pl:3 | 3 | 3/3 piece |  |
| `xtra-accessibility-star-ad` | query | audience | any |  | 0 |  | 2 | — |  |
| `xtra-kino-dla-suffix` | query | audience | pl |  | 5 | pl:15 | 5 | 5/5 piece |  |
| `xtra-kino-kobiet-suffix` | query | audience | pl |  | 53 | pl:153 | 2 | 2/2 piece |  |
| `xtra-letnie-kino-dzieciom-suffix` | query | audience | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-audiodeskrypcja-suffix` | query | audience | pl |  | 0 |  | 0 |  |  |
| `xtra-audience-suffix` | query | audience | en |  | 33 | pl:4 uk:82 | 17 | 13/17 piece |  |
| `xtra-wtorek-seniora-suffix` | query | audience | en |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-senior-w-kinie-suffix` | query | audience | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-filmowy-klub-seniora-dash` | query | audience | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-kino-przy-herbatce` | query | audience | pl |  | 6 | pl:21 | 10 | 10/10 piece |  |
| `xtra-pipe-dla-rodzica` | query | audience | pl |  | 2 | pl:7 | 6 | 6/6 piece |  |
| `xtra-wakacje-dla-dzieci` | query | audience | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-kobiece-strands` | query | audience | pl |  | 8 | pl:20 | 11 | 8/11 piece |  |
| `xtra-filmy-z-lektorem` | query | audience | pl |  | 1 | pl:5 | 4 | 4/4 piece |  |
| `xtra-poranki-dzieciece-suffix` | query | audience | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-kinowy-poranek` | query | audience | pl |  | 2 | pl:6 | 2 | 2/2 piece |  |
| `xtra-kino-bez-barier-dzieci` | query | audience | pl |  | 0 |  | 0 |  |  |
| `xtra-kino-dla-ducha-prefix` | query | audience | pl |  | 1 | pl:3 | 1 | — |  |
| `xtra-starymlyn-sensoryczny` | venue (kino-stary-mlyn) | audience | pl |  | 0 |  | 0 |  |  |
| `search-spotkanie-banner-prefix` | query | event | pl |  | 6 | pl:16 | 10 | 10/10 piece |  |
| `xtra-pokaz-filmu` | query | event | pl |  | 2 | pl:6 | 4 | — |  |
| `xtra-qa-clause-tail` | query | event | en |  | 7 | uk:14 | 7 | 2/7 piece |  |
| `search-plus-event-suffix` | query | event | pl |  | 553 | de:143 pl:212 uk:938 us:2 | 376 | 374/376 bill |  |
| `search-meeting-suffix` | query | event | pl |  | 6 | pl:18 | 55 | 43/55 bill |  |
| `search-with-event-suffix` | query | event | pl |  | 15 | pl:38 | 28 | 1/28 piece |  |
| `xtra-pp-spotkania-seniora` | banner | event | pl |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-pp-seans-dla-rodzicow` | banner | event | pl |  | 1 | pl:2 | 0 |  |  |
| `xtra-pp-special-event-alamo` | banner | event | en |  | 3 | us:6 | 3 | — |  |
| `xtra-premiera-bracketed` | query | event | pl |  | 1 | pl:3 | 1 | — |  |
| `xtra-przedpremiera-suffix` | query | event | pl |  | 11 | pl:30 | 26 | 13/26 piece |  |
| `xtra-przedpremiera-prefix` | query | event | pl |  | 5 | pl:13 | 9 | 5/9 piece |  |
| `xtra-rocznica-premiery-suffix` | query | event | pl |  | 2 | pl:7 | 4 | 2/4 piece |  |
| `xtra-lekcje-filmowe` | query | event | pl |  | 1 | pl:3 | 4 | 4/4 piece |  |
| `xtra-pokaz-przedpremierowy-prefix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-pokaz-suffix` | query | event | pl |  | 47 | pl:124 | 54 | 42/54 piece |  |
| `xtra-pokaz-colon-suffix` | query | event | pl |  | 0 |  | 1 | — |  |
| `xtra-seans-suffix` | query | event | pl |  | 3 | pl:10 | 9 | 3/9 piece |  |
| `xtra-pokaz-z-muzyka-suffix` | query | event | pl |  | 1 | pl:3 | 2 | 1/2 piece |  |
| `xtra-przedpremierowy-bullet` | query | event | pl |  | 2 | pl:6 | 3 | 1/3 piece |  |
| `xtra-przedpremierowo-paren` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-premiera-exclaimed-suffix` | query | event | pl |  | 4 | pl:12 | 5 | — |  |
| `xtra-premiera-comma-suffix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-wstep-wolny-suffix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-premiera-ksiazki-suffix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-koncert-suffix` | query | event | pl |  | 0 | pl:1 | 1 | — |  |
| `xtra-introduction-clause-tail` | query | event | en |  | 3 | uk:4 us:3 | 17 | 13/17 bill |  |
| `xtra-qa-paren-note` | query | event | any |  | 5 | de:2 uk:8 | 5 | — |  |
| `xtra-qa-plus-tail` | query | event | en |  | 6 | uk:14 | 106 | 92/106 bill |  |
| `xtra-fan-event-suffix` | query | event | en |  | 12 | us:32 | 2 | — |  |
| `xtra-live-qa-prefix` | query | event | en |  | 2 | us:4 | 2 | 2/2 piece |  |
| `xtra-with-live-qa-suffix` | query | event | en |  | 0 | uk:18 | 1 | — |  |
| `xtra-pokaz-specjalny-prefix` | query | event | pl |  | 3 | pl:12 | 8 | 5/8 piece |  |
| `xtra-pokazy-specjalne-prefix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-oficjalna-premiera-prefix` | query | event | pl |  | 1 | pl:3 | 1 | — |  |
| `xtra-pokazy-specjalne-suffix` | query | event | pl |  | 0 |  | 3 | 3/3 piece |  |
| `xtra-paren-pokaz-seans` | query | event | pl |  | 9 | pl:24 | 27 | 2/27 format |  |
| `xtra-maraton-prefix` | query | event | pl |  | 0 |  | 1 | 1/1 piece |  |
| `xtra-spotkania-filmowe-banner` | query | event | pl |  | 0 |  | 4 | 4/4 piece |  |
| `xtra-ostatni-seans-suffix` | query | event | pl |  | 0 |  | 1 | — |  |
| `xtra-przed-filmem-suffix` | query | event | pl |  | 3 | pl:9 | 5 | 5/5 piece | `xtra-meeting-tail` |
| `xtra-dot-spotkanie-suffix` | query | event | pl |  | 1 | pl:3 | 5 | 3/5 piece | `xtra-meeting-tail` |
| `xtra-premiera-prefix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-dialog-przez-film` | query | event | any |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-koncert-z-rok-suffix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-przedpremiera-bare-space-suffix` | query | event | pl |  | 0 |  | 20 | — |  |
| `xtra-premiera-announcement-suffix` | query | event | pl |  | 10 | pl:21 | 16 | 16/16 piece |  |
| `xtra-preview-prefix` | query | event | pl |  | 22 | de:33 uk:14 | 20 | 15/20 piece |  |
| `xtra-preview-suffix` | query | event | pl |  | 38 | de:46 uk:25 | 34 | 8/34 piece |  |
| `xtra-preview-pipe-tail` | query | event | en |  | 1 | de:2 | 3 | 2/3 piece |  |
| `xtra-preview-dated-tail` | query | event | de |  | 2 | de:4 | 2 | — |  |
| `xtra-vorpremiere-prefix` | query | event | de |  | 6 | de:11 | 5 | 2/5 piece |  |
| `xtra-vorpremiere-suffix` | query | event | de |  | 6 | de:10 | 5 | 1/5 piece |  |
| `xtra-event-projekt-suffix` | query | event | any |  | 67 | pl:201 | 4 | 4/4 piece |  |
| `xtra-paren-screening-event` | query | event | pl |  | 2 | pl:6 | 27 | 2/27 format |  |
| `xtra-premiera-lowercase-suffix` | query | event | pl |  | 0 |  | 0 |  |  |
| `xtra-cyfrowe-premiera` | venue (cyfrowe-kino) | event | pl |  | 0 |  | 0 |  |  |
| `apollo-przedpremierowy` | venue (kino-apollo) | event | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-mok-miedzyrzecz-premiera` | venue (kino-mok-miedzyrzecz) | event | pl |  | 2 | pl:6 | 25 | — |  |
| `structural-anniversary-suffix` | query | edition | en |  | 225 | de:18 pl:495 uk:74 us:76 | 39 | 8/39 piece |  |
| `structural-restored-suffix` | query | edition | any |  | 1 | de:2 | 1 | 1/1 version |  |
| `structural-wersja-suffix` | query | edition | pl |  | 54 | pl:177 | 8 | 7/8 version |  |
| `xtra-rekonstrukcja-pipe-suffix` | query | edition | pl |  | 0 |  | 0 |  |  |
| `xtra-print-strand-tail` | query | edition | any |  | 2 | uk:5 | 4 | — |  |
| `xtra-print-prefix` | query | edition | en |  | 2 | uk:4 | 2 | 1/2 piece |  |
| `xtra-paren-anniversary-note` | query | edition | en |  | 799 | de:37 uk:1790 | 43 | — |  |
| `xtra-4k-restoration-suffix` | query | edition | any |  | 96 | uk:252 us:22 | 20 | 19/20 version |  |
| `xtra-rerelease-suffix` | query | edition | pl |  | 1 |  | 1 | — |  |
| `xtra-nlecie-org-prefix` | query | edition | pl |  | 0 |  | 0 |  |  |
| `xtra-4k-suffix` | query | edition | pl |  | 18 | pl:1 uk:36 | 5 | 3/5 format |  |
| `xtra-restored-adjective-first-suffix` | query | edition | pl |  | 0 |  | 0 |  |  |
| `xtra-rerelease-paren-suffix` | query | edition | pl |  | 26 | pl:28 uk:13 | 6 | 6/6 version |  |
| `xtra-on-35mm-suffix` | query | edition | any |  | 0 |  | 1 | 1/1 format |  |
| `xtra-canonical-rocznica-suffix` | key | edition | pl |  | 167 | pl:509 | 22 | 4/22 piece |  |
| `xtra-canonical-trailing-org-wersja` | key | edition | pl |  | 1 |  | 1 | 1/1 piece |  |
| `xtra-pp-tani-wtorek` | banner | format | pl |  | 2 | pl:15 | 10 | 10/10 piece |  |
| `xtra-screening-note-segment` | query | format | en |  | 36 | uk:96 | 33 | 17/33 version |  |
| `xtra-certificate-year-suffix` | query | format | en |  | 0 |  | 11 | 11/11 format |  |
| `xtra-asterisk-note-suffix` | query | format | en |  | 0 |  | 6 | — |  |
| `xtra-runtime-suffix` | query | format | en |  | 2 | uk:4 | 32 | — |  |
| `xtra-certificate-suffix` | query | format | en |  | 8 | uk:18 | 380 | 380/380 format |  |
| `xtra-imax-experience-suffix` | query | format | de |  | 57 | de:2 es:6 uk:99 us:1 | 4 | 3/4 piece |  |
| `xtra-cinema-experience-suffix` | query | format | en |  | 719 | de:2 pl:108 uk:722 us:722 | 4 | 1/4 piece |  |
| `xtra-screen-language-suffix` | query | format | any |  | 5 | uk:6 | 3 | — |  |
| `xtra-version-original-suffix` | query | format | es |  | 0 | es:1 | 1 | — |  |
| `xtra-ukrainski-lang-suffix` | query | format | pl |  | 18 | pl:72 | 12 | — |  |
| `xtra-tani-dzien-suffix` | query | format | pl |  | 3 | pl:11 | 5 | — |  |
| `xtra-bare-format-suffix` | query | format | pl |  | 0 | pl:1 | 14 | 13/14 format |  |
| `xtra-capital-i-format-wrapper` | query | format | pl |  | 0 |  | 0 |  |  |
| `xtra-format-pl-suffix` | query | format | pl |  | 0 |  | 1 | 1/1 format |  |
| `xtra-subtitle-paren-tag` | query | format | pl |  | 3 | pl:9 | 34 | 34/34 format |  |
| `xtra-english-subtitles-suffix` | query | format | de |  | 5 | de:3 pl:12 | 6 | 2/6 piece |  |
| `xtra-angielskie-napisy-suffix` | query | format | pl |  | 0 | pl:1 | 1 | — |  |
| `xtra-subtitle-note-bracket` | query | format | de |  | 8 | de:13 | 10 | — |  |
| `xtra-inner-version-bracket` | query | format | de |  | 1 | de:2 | 1 | — |  |
| `xtra-inner-version-word` | query | format | de |  | 13 | de:26 | 3 | — |  |
| `xtra-version-strand-tail` | query | format | de |  | 8 | de:31 | 11 | — |  |
| `xtra-strand-pipe-screen-prefix` | query | format | pl |  | 14 | de:29 | 19 | — |  |
| `xtra-leading-bare-screen` | query | format | any |  | 2 | de:2 uk:2 | 2 | — |  |
| `xtra-code-after-format-tag` | query | format | pl |  | 2 | pl:8 | 4 | — |  |
| `xtra-canonical-trailing-paren-lang` | key | format | pl |  | 0 |  | 0 |  |  |
| `helios-suffix-1` | venue (helios) | format | any |  | 138 | pl:413 | 8 | — |  |
| `helios-suffix-2` | venue (helios) | format | any |  | 0 |  | 0 |  |  |
| `helios-suffix-3` | venue (helios) | format | any |  | 210 | pl:624 | 9 | — |  |
| `helios-suffix-4` | venue (helios) | format | any |  | 0 |  | 0 |  |  |
| `helios-suffix-5` | venue (helios) | format | any |  | 32 | pl:152 | 1 | 1/1 piece |  |
| `helios-suffix-6` | venue (helios) | format | any |  | 35 | pl:62 | 10 | 10/10 piece |  |
| `helios-suffix-7` | venue (helios) | format | any |  | 134 | pl:329 | 24 | 18/24 piece |  |
| `helios-suffix-8` | venue (helios) | format | pl |  | 48 | pl:191 | 3 | 3/3 piece |  |
| `helios-suffix-9` | venue (helios) | format | pl |  | 0 |  | 0 |  |  |
| `helios-suffix-10` | venue (helios) | format | any |  | 0 |  | 0 |  |  |
| `helios-suffix-11` | venue (helios) | format | pl |  | 0 |  | 0 |  |  |
| `helios-suffix-12` | venue (helios) | format | pl |  | 0 |  | 0 |  |  |
| `helios-suffix-13` | venue (helios) | format | pl |  | 0 |  | 0 |  |  |
| `helios-suffix-14` | venue (helios) | format | any |  | 0 |  | 0 |  |  |
| `helios-suffix-15` | venue (helios) | format | any |  | 0 |  | 0 |  |  |
| `helios-suffix-16` | venue (helios) | format | any |  | 11 | pl:35 | 9 | 8/9 piece |  |
| `xtra-bajka-sps` | venue (kino-bajka) | format | pl |  | 0 |  | 3 | — |  |
| `xtra-kijow-napisy-pl` | venue (kino-kijow) | format | pl |  | 0 |  | 4 | 2/4 format |  |
| `xtra-pp-big-festivalowski` | banner | festival | en |  | 0 |  | 0 |  |  |
| `xtra-pp-pnkf-prefix` | banner | festival | pl |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-pp-numbered-festival` | banner | festival | pl |  | 0 |  | 0 |  |  |
| `xtra-pp-pnkf-bare` | banner | festival | pl |  | 0 |  | 8 | 8/8 piece |  |
| `xtra-pp-avant-art-festival` | banner | festival | en |  | 0 |  | 0 |  |  |
| `xtra-wtf-fest-prefix` | query | festival | any |  | 0 |  | 14 | 14/14 piece |  |
| `xtra-pipe-festival-suffix` | query | festival | pl |  | 1 | pl:3 | 8 | 7/8 piece |  |
| `xtra-pnkf-suffix-pipe` | query | festival | pl |  | 0 |  | 10 | 10/10 piece |  |
| `xtra-pnkf-suffix-edycja` | query | festival | pl |  | 0 |  | 9 | — |  |
| `xtra-jarocinski-festiwal-suffix` | query | festival | pl |  | 0 |  | 0 |  |  |
| `xtra-film-festival-prefix` | query | festival | en |  | 27 | de:6 uk:71 | 48 | 45/48 piece |  |
| `xtra-festival-acronym-suffix` | query | festival | any |  | 33 | uk:64 | 43 | 4/43 piece |  |
| `xtra-konkurs-pelnometrazowych` | query | festival | pl |  | 0 |  | 0 |  |  |
| `xtra-festival-tag-prefix` | query | festival | pl |  | 44 | de:4 pl:121 | 44 | 31/44 piece |  |
| `xtra-festival-year-tag-prefix` | query | festival | en |  | 0 | us:7 | 7 | — |  |
| `xtra-festival-tag-suffix` | query | festival | en |  | 8 | de:3 pl:15 | 18 | 16/18 piece |  |
| `xtra-bkf-prefix` | query | festival | any |  | 0 |  | 0 |  |  |
| `xtra-pp-ffs-festival` | query | festival | pl |  | 0 |  | 0 |  |  |
| `xtra-canonical-festival-tag-prefix` | key | festival | en |  | 44 | de:4 pl:121 | 44 | 31/44 piece |  |
| `xtra-canonical-festival-year-tag-prefix` | key | festival | en |  | 0 | us:7 | 7 | — |  |
| `xtra-canonical-festival-tag-suffix` | key | festival | en |  | 15 | de:1 pl:42 | 18 | 16/18 piece |  |
| `xtra-pp-wajda-rewizje-slash` | banner | retrospective | pl |  | 1 | pl:3 | 1 | — |  |
| `xtra-pp-bunuel-retro` | banner | retrospective | any |  | 0 |  | 0 |  |  |
| `xtra-pp-wajda-rewizje` | banner | retrospective | pl |  | 7 | pl:15 | 12 | 10/12 piece |  |
| `xtra-pp-mamoru-hosody` | banner | retrospective | any |  | 0 |  | 0 |  |  |
| `xtra-pp-wajda-rewizje-bare` | banner | retrospective | pl |  | 6 | pl:20 | 9 | 9/9 piece |  |
| `xtra-fellini-prefix` | query | retrospective | any |  | 0 | pl:1 | 35 | 22/35 piece |  |
| `xtra-fellini-suffix` | query | retrospective | pl |  | 0 |  | 9 | 7/9 piece |  |
| `xtra-wajda-rewizje-suffix` | query | retrospective | any |  | 1 | pl:4 | 5 | 4/5 piece |  |
| `xtra-zulawski-kino-ekstazy` | query | retrospective | pl |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-poniedzialki-konwicki` | query | retrospective | pl |  | 0 |  | 5 | 4/5 piece |  |
| `xtra-jim-jarmusch-suffix` | query | retrospective | pl |  | 0 |  | 0 |  |  |
| `xtra-pedro-almodovar-kolory` | query | retrospective | pl |  | 0 |  | 0 |  |  |
| `xtra-wajda-dot-prefix` | query | retrospective | pl |  | 2 | pl:6 | 4 | — |  |
| `xtra-konwicki-prefix` | query | retrospective | any |  | 0 |  | 1 | — |  |
| `xtra-tadeusz-konwicki-suffix` | query | retrospective | any |  | 0 |  | 2 | 2/2 piece |  |
| `xtra-wajda-o-filmie-suffix` | query | retrospective | any |  | 0 |  | 0 |  |  |
| `xtra-hosoda-dot-prefix` | query | retrospective | any |  | 0 |  | 6 | — |  |
| `xtra-konwicki-dot-prefix` | query | retrospective | pl |  | 3 | pl:9 | 3 | — |  |
| `xtra-wajda-rewizje-bare-year` | query | retrospective | pl |  | 0 |  | 0 |  |  |
| `xtra-zywot-briana-monty-suffix` | query | retrospective | pl |  | 0 |  | 2 | — |  |
| `xtra-wong-kar-wai-suffix` | query | retrospective | any |  | 0 |  | 0 |  |  |
| `xtra-lato-robin-williams` | query | retrospective | pl | jun-sep | 0 |  | 3 | 3/3 piece |  |
| `xtra-wajda-dziedzictwo` | query | retrospective | pl |  | 1 | pl:3 | 2 | 2/2 piece |  |
| `xtra-poniedzialki-konwicki-prefix` | query | retrospective | pl |  | 0 |  | 0 |  |  |
| `xtra-canonical-fellini-prefix` | key | retrospective | pl |  | 0 | pl:1 | 35 | 22/35 piece |  |
| `xtra-canonical-fellini-suffix` | key | retrospective | pl |  | 0 |  | 9 | 7/9 piece |  |
| `palacowe-wajda` | venue (kino-palacowe) | retrospective | pl |  | 0 |  | 0 |  |  |
| `xtra-exhibition-on-screen` | query | relay | pl |  | 313 | de:65 uk:477 us:71 | 47 | 38/47 piece |  |
| `xtra-encore-suffix` | query | relay | en |  | 138 | es:46 uk:268 us:139 | 14 | — |  |
| `xtra-canonical-broadcast-season` | key | relay | pl |  | 9747 | de:2589 pl:474 uk:8869 us:8273 | 204 | — |  |
| `xtra-canonical-royal-ballet-opera` | name | relay | en |  | 5241 | de:2189 pl:285 uk:7626 us:166 | 127 | — |  |
| `xtra-canonical-met-opera-suffix` | name | relay | en |  | 2 | pl:7 | 3 | — |  |
| `xtra-canonical-met-opera` | name | relay | en |  | 5463 | de:1196 uk:2651 us:8036 | 59 | — |  |
| `xtra-canonical-nt-live-suffix` | name | relay | en |  | 0 |  | 7 | — |  |
| `xtra-canonical-nt-live` | name | relay | en |  | 821 | de:12 pl:96 uk:1363 us:180 | 70 | 4/70 format |  |
| `xtra-canonical-bts-world-tour` | name | relay | en |  | 136 | uk:272 | 2 | — |  |
| `canonical-gwiezdne-wojny` | key | rename | any |  | 0 |  | 1 | 1/1 piece |  |
| `canonical-ampersand-to-i` | key | rename | pl |  | 78 | pl:177 | 581 | — |  |
| `xtra-canonical-gwiezdne-wojny-ci` | key | rename | any |  | 0 |  | 8 | 8/8 piece |  |
| `xtra-canonical-mandalorian-grogu-en` | key | rename | pl |  | 0 |  | 1 | — |  |
| `xtra-canonical-skarpetek-3` | key | rename | pl |  | 2 | pl:6 | 8 | 4/8 format |  |
| `xtra-pp-venue-junior` | banner | venue | pl |  | 0 |  | 0 |  |  |
| `xtra-venue-non-fiction-suffix` | query | venue | pl |  | 1 | pl:3 | 1 | 1/1 piece |  |
| `xtra-venue-colon-strand-suffix` | query | venue | pl |  | 1 | pl:5 | 3 | 3/3 piece |  |
| `xtra-teleskop-suffix` | query | venue | pl |  | 1 | pl:3 | 4 | 3/4 piece |  |
| `bok-promo` | venue (bok) | venue | pl |  | 2 | pl:7 | 64 | 50/64 piece |  |
| `bok-pipe-to-colon` | venue (bok) | venue | any |  | 4 | pl:11 | 581 | — |  |
| `cc-powrot-do-kin` | venue (cinema-city) | venue | pl |  | 0 |  | 0 |  |  |
| `xtra-helios-na-scenie` | venue (helios) | venue | any |  | 0 |  | 9 | — |  |
| `xtra-bajka-kluczbork-klaps` | venue (kino-bajka-kluczbork) | venue | pl |  | 0 |  | 0 |  |  |
| `xtra-farys-tot-story` | venue (kino-farys) | venue | pl |  | 0 |  | 1 | — |  |
| `kinematograf-director` | venue (kino-kinematograf) | venue | any |  | 0 |  | 93 | — |  |
| `kinematograf-year` | venue (kino-kinematograf) | venue | any |  | 9 | pl:28 | 685 | 682/685 year |  |
| `muza-najlepsze-z-najgorszych` | venue (kino-muza) | venue | pl |  | 1 | pl:3 | 4 | 4/4 piece |  |
| `xtra-starowce-akcja-lato` | venue (kino-na-starowce) | venue | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-oskard-kino-cafe` | venue (kino-oskard) | venue | pl |  | 0 |  | 10 | — |  |
| `wybrzeze-venue-suffix` | venue (wybrzeze) | venue | pl |  | 4 | pl:16 | 8 | — |  |
| `structural-slash-suffix` | query | junk | any |  | 97 | de:106 es:1 pl:113 uk:7 | 122 | 104/122 piece |  |
| `xtra-klub-filmowy-quoted` | query | junk | pl |  | 2 | pl:9 | 3 | — |  |
| `xtra-korelacje-quoted` | query | junk | pl |  | 6 | pl:18 | 6 | — |  |
| `xtra-unclosed-bracket-tail` | query | junk | any |  | 20 | de:15 pl:16 uk:12 | 12 | — |  |
| `search-quote-right` | query | junk | any |  | 83 | de:7 pl:245 us:3 | 102 | — |  |
| `search-quote-low9` | query | junk | any |  | 114 | de:70 pl:243 | 137 | — |  |
| `xtra-quoted-film-banner-tail` | query | junk | pl |  | 4 | pl:11 | 6 | — |  |
| `xtra-quoted-cycle-dash` | query | junk | pl |  | 11 | de:20 pl:5 | 8 | 8/8 piece |  |
| `xtra-quoted-film-w-ramach-cyklu` | query | junk | pl |  | 0 |  | 5 | — |  |
| `xtra-wakacyjne-poranki-quoted` | query | junk | pl | jun-sep | 0 |  | 0 |  |  |
| `xtra-canonical-kropka-film-suffix` | key | junk | any |  | 4 | pl:34 | 2 | — |  |
| `xtra-canonical-trailing-paren-year` | key | junk | any |  | 16387 | de:1725 es:29 pl:318 uk:2752 us:28380 | 685 | 682/685 year |  |
| `xtra-canonical-trailing-sep-year` | key | junk | any |  | 9 | de:11 pl:3 uk:3 | 11 | — |  |
| `bok-ws` | venue (bok) | junk | any |  | 0 |  | 50 | 36/50 format |  |
| `bok-trim` | venue (bok) | junk | any |  | 0 |  | 3 | 3/3 format |  |
| `alt-okladka` | venue (kino-alternatywy) | junk | pl |  | 16 | pl:49 | 34 | — |  |
| `alt-quotes` | venue (kino-alternatywy) | junk | pl |  | 16 | pl:48 | 268 | — |  |
| `alt-collapse-ws` | venue (kino-alternatywy) | junk | pl |  | 0 |  | 36 | 36/36 format |  |
| `kino-alternatywy-trim` | venue (kino-alternatywy) | junk | any |  | 16 | pl:48 | 3 | 3/3 format |  |
| `kino-kinematograf-trim` | venue (kino-kinematograf) | junk | any |  | 0 |  | 3 | 3/3 format |  |
| `kino-muza-trim` | venue (kino-muza) | junk | any |  | 0 |  | 3 | 3/3 format |  |

