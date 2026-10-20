# 2026-10-20: DE lost TMDB matches at the 8b75b70 → aa334f31d release

## Summary

Deploying aa334f31d (it replaced 8b75b704b, the 2026-10-15 image, at about 06:58 UTC) cost Germany about 28 films in
Prometheus `with_tmdb_id`, which fell from 3123–3125 to 3095 by 07:18. The take-change log has **113 listings still
lost and 22 changed** under aa334f31d. Another 10 were lost and then filled again by the agreement stage within
minutes.

**The hypothesis is refuted.** The cast-mismatch guard (784d6da76), with original_language (f48ab8001), genres
(4c3f3d604) and the cast tie-break (fd96c63ce), refused **none** of these listings:

- At 07:02 the guard could not fire. It needs the candidate's TMDB genres and original language. The DE `tmdb_films`
  store had 7 records holding them at 06:xx, and the backfill only started in bulk at 09:xx.
- No lost or changed log line names a cast reason.
- The prod mirror's `identity_traces` hold no cast rule at all.

The losses fall into two groups:

| Bucket | Listings | Verdict |
|---|---|---|
| Event and wrong-namesake takes refused by the 10-17…10-19 rules (film-ness V3f, part/subtitle denial, piece-beside, copied-original-apart) | 31 lost + 3 changed (cures) | **correct refusals**: every one of the 29 the old recording holds is judged WRONG→none or WRONG→RIGHT |
| LOTR Extended Versions (65), Über Unterbiberger (7), De Gaulle (4), Ben'Imana, Kikis kleiner Lieferservice, Das Versteck, Vorspiel, Roya | 81 lost + 7 changed right→wrong (6 LOTR 2 → Bakshi's 1978 tmdb:123, King Kong 244 → 254) | **false refusals (regressions)**, but **not reproduced by the code**: aa334f31d takes the right film for every one of them on recording 38713274617 |
| Heimsuchung ×9 | changed 16699 → 635775 | wrong → wrong (the feed links 1089789; the slice takes 1089789 at both refs) |
| Tagträumer | 1 lost | undecided |

**Most likely cause of the regressions:** the boot's whole-corpus re-resolve. The first periodic projection after a
boot (07:02:49, 242 s, 67 take changes) is the only whole-corpus resolve. 8b75b704b last ran one at its own boot on
10-15 23:50 and then moved only incrementally for 4½ days. The 10-20 boot therefore re-resolved, whole, data the old
code had only ever taken incrementally. LOTR has flipped the same way before:

- 10-14 08:42: lost at a deploy (2ef867e3a).
- 10-17 01:00: gained incrementally under 8b75b704b, with no deploy.
- 10-20: lost again.

That points to arrival order or input that changed after 10-15, not to a rule in 8b75b70..aa334f31d. It is not proven
yet, because no recording newer than 10-15 can be replayed (see "Limits").

## Timeline (UTC, 2026-10-20)

| Time | Event |
|---|---|
| 10-15 23:50 | 8b75b704b boots; its whole-corpus projection is the last until today |
| 10-17 01:00 | under 8b75b704b, no deploy: 107 LOTR listings gain 120/121/122 incrementally |
| 06:58:13 | last 8b75b704b projection |
| 06:58 | aa334f31d deployed |
| 07:02:49 → 07:06:51 | boot's periodic (whole-corpus) projection under aa334f31d: 242 s, 87 written, 67 take changes (63 lost or changed) |
| 07:08 – 09:48 | changed projections: the rest of the losses (LOTR at the CineStars at 09:07), 10 listings re-filled by the agreement (Kiki ×2, Chinatown, Blade ×2, Manmadhan, Dancing Queen, Regio Tales, Shinjuku Boys, Drei Männer im Schnee) |
| 07:18 | `with_tmdb_id` 3095; steady at 3096–3100 since |
| 09:xx → | TMDB backfill (genres, original_language, cast) runs: 7 records at 06:xx, 13.6k at 09:5x, 25.4k of 82k at 10:06 (~1.3k/min, done ≈ 10:50) |

For scale, DE lost takes per day under 8b75b704b were 57, 10, 13 and 19 (10-16 to 10-19). On 10-20 there were 183.

## Method

All of it was read-only: VictoriaLogs, the local prod mirror and a recording already in the cache.

1. **Take-change log** (VictoriaLogs, `identity-investigation` skill):
   `_time:[2026-10-20T06:00Z, 14:00Z] country:de kind:*`. This gave 188 lines: 125 lost, 22 changed and 27 gained
   under aa334f31d, plus 8 lost under 8b75b704b before the deploy. I netted each (venue, listing) to its last event,
   giving 113 lost, 22 changed and 10 lost-then-regained.
2. **Prod now** (`mongodb://127.0.0.1:28017/kinowo_de_prod_mirror`, after `scripts/local-mirror/freshness de`, 6 s
   lag). `identity_traces` confirms that all 113 are still filmless and all 22 still on the new film. `tmdb_films` gave
   the old and new films' records and the backfill's progress. `cinema_scrapes` gave the billings' feed ids,
   original titles and directors.
3. **Code vs data**: `scripts/identity-slice de --recording 38713274617 --batch <93 listings, one per billing and
   venue chain> --ref 8b75b704b --ref aa334f31d`. The batch decided 84 listings, the ones the 10-15 recording holds:
   - **29 moved, all CURED** (old take judged WRONG in `identity-judgements.tsv`, new take none or RIGHT). Tally:
     cured 29, lost 0, new-wrong 0.
   - The other 55 take the same film at both refs. That includes 120/121/122 for LOTR at Kinopolis, Kinorama, Lichtburg,
     Capitol, Cineplex, Traumpalast, Metropol and Cinema München; 1559083 for Über Unterbiberger; 1165366 for De Gaulle;
     16859 for Kiki; 36647 for Blade; and 1089789 for Heimsuchung.
   - The CineStar "(ext.)" billings are refused at both refs on that recording too (learned listing-film veto:
     `title=overlap -3.95`, `originalTitle=overlap -3.11`).
4. Judged each sample against the TMDB record (title, year, director, runtime) and the venue's own facts (feed id,
   original title, director), and against `identity-judgements.tsv` where it has a row (LOTR Extended: 120/121/122
   RIGHT).

## Sample (42 billings, 135 listings)

"Rule" is the refusal named in the take-change line's `reason_first`. "Code?" says whether the slice moves the
listing between the two refs on recording 38713274617: **yes** means it moves, **same** means both refs take the
same film, **—** means the recording does not hold the listing.

| Listing (venue) | n | Old TMDB | Now | Rule that refused | Code? | Verdict |
|---|---|---|---|---|---|---|
| Herr der Ringe 1/2/3 Extended (CineStar ×8, Traumpalast ×7, Kinopolis ×6, Cineplex ×3, Lichtburg, Capitol, Kinorama, Metropol, Cinema München…) | 65 | 120 / 121 / 122 (2001–03, Jackson) | none (Vetoed) | `apart:denies-film` + `veto:learned-listing-film-probability-below-the-cannot-link-cut`: "kept apart from 'DER HERR DER RINGE - … EXTENDED VERSION' [2002] (cluster 0): denies-film" | same (right at both, except the CineStars, refused at both) | **false**: judged RIGHT |
| Der Herr der Ringe 2 (Extended Version) (Kinopolis ×6) | 6 | 121 | **123** (Bakshi 1978) | favoured-calibrated at 40.8%, `originalTitle=decorated` | same (121 at both) | **false, right → wrong** |
| Über Unterbiberger – Vom Woid in die Welt (Cinecitta, Cineplex ×2, Roxy, Kinopolis Freiberg, Kurtheater Tutzing, KULTUR im Oberbräu) | 7 | 1559083 (2026, Ditscherlein) | none (Agreed lost) | piece-beside: "one piece of the billing names 'Über Unterbiberger' — 'vom woid in die welt' is billed beside" (`ListingShape`) | same (1559083 at both) | **false**: the subtitle is the film's |
| De Gaulle – Zeit des Widerstands (E-Werk, Weltspiegel, Zeise) | 3 | 1165366 (Baudry 2026) | none (Vetoed) | `joined by same-catalogue-id` to Bambi's billing, then the cluster vetoed | same (1165366 at both) | **false**: E-Werk's feed links cineamo 1165366m |
| De Gaulle Zeit des Widerstands (Bambi) | 1 | 1675748 (Schauder 2026) | none | as above | — | **false**: the venue bills Schauder's "Vater, Sohn und der Preis des Widerstands" page under the wrong title |
| AFRIKAMERA 2026: BEN'IMANA – LES ENFANTS DE DIEU (City Kino Wedding) | 1 | 1405200 Ben'Imana | none | piece-beside ('les enfants de dieu') | — | **false** |
| Kikis kleiner Lieferservice- Studio Ghibli (Andreasstadel) | 1 | 16859 (Miyazaki 1989) | none | piece-beside ('studio ghibli') | — | **false** (Leopold and Liliom regained at 07:12) |
| Das Versteck (Union Friedrichshagen) | 1 | 286391 (Beyer 1978) | none | BelowThreshold: best 1697977 (2023) at 16.7% | — | **false**: feed links cineamo 286391m |
| Vorspiel (Union Friedrichshagen) | 1 | 286393 (Kahane 1987) | none | BelowThreshold 3.3% | — | **false**: feed links cineamo 286393m |
| ROYA – Eintritt frei mit Seebrücke Würzburg (Central) | 1 | 1616163 Roya (2026) | none | BelowThreshold 29.7% | — | false (likely) |
| King Kong (Eulenspiegel) | 1 | 244 (1933) | 254 (2005) | pooled evidence → 254 at 39.9% | — | **false, right → wrong**: feed links cineamo 244m |
| HEIMSUCHUNG / Heimsuchung (Anker, Burgtheater, Filmklubb, Harsefeld, KoKi ×3, Lamm, seenema) | 9 | 16699 (Smother 2008) | 635775 (Andrä 2019) | "feed cineamo 1089789m links 1089789, overruled: 2 other sources back 635775" | same (1089789 at both) | wrong → wrong |
| Titans (CinemaxX ×3) | 3 | 10637 Remember the Titans | 869152 Titans (2026, ml) | own match 88.6% | — | cure (likely) |
| WHEN PIGS FLY (Neue Post, Neues Maxim) | 2 | 149933 (1993) | 1599204 (2026, de) | release veto on the 1993 film | yes | **cure** (judged) |
| Ecrire la vie – Writing Life (Kamino) | 1 | 1579323 Giv os en chance | 1518673 (Claire Simon) | sole-work | yes | **cure** (judged) |
| KILMINISTER – A Tribute to Motörhead (Casablanca, Kaminwerk) | 2 | 1502991 (3-min short) | none (Event) | film-ness V3f, "event, not a film: live event" | yes | correct |
| Sunday Night Jazz (Forum 2) | 1 | 1177528 Sunday Night (10 min) | none (Event) | film-ness V3f | yes | correct |
| Andreas Kümmert & The Electric Circus – live 2.0 | 1 | 506869 Allen Anders: Live… | none | film-ness V3f (BelowThreshold) | yes | correct |
| Helmut Schleich / Martin Schmitt (Landsberg) | 2 | 865169 Helmut (4 min) / 43429 Martín (Hache) | none | film-ness V3f | yes / — | correct |
| One Stage, Five Bands; 3-D-Show: Süd-Afrika; Teenie Fasching; Dark New Wave Night; kurz & BANDig; Three for Silver; In a Rocking Mood; Regio Tales* | 8 | Three Worlds One Stage; VAPE SHOW; Fasching 1939; New Wave; KURZ; Tea For Three; In a sentimental mood; Heads and Tales | none | film-ness V3f (BelowThreshold) | yes | correct |
| Zöllners blinde Passagiere; VOODOO LOUNGE; Tom Cruise Digger; KNEIPEN Festival Montabaur 2026 | 4 | Blinde Passagiere 1936; VooDoo; Tom Cruise (1 min); The Cure – Maia North Festival | none | film-ness V3f / title-segment | yes (2) / — | correct |
| DONNERSTAGS(-)KNEIPE (AJZ ×2); Kurzfilmprogramm (B-Movie, Filmhaus) | 4 | 760104 X (Ti West); 703131 Endlich Tacheles | none | lost the pooled same-film join | yes | correct |
| Weihnachten mit den Drei Herren 2026 (Studio) | 1 | 93189 La glace à trois faces (1927) | none | agreement refused | yes | correct |
| DAS INDISCHE GRABMAL (TEIL 1 / TEIL 2) (Union Friedrichshagen) | 2 | 70555 (Lang 1959) | none | part denial, a1cb04588: "a part its title gives a subtitle none of the film's titles holds" | yes | correct (Joe May's 1921 two-parter) |
| The Stranger (Filmmuseum, ALFILM) | 1 | 1429348 Ozon's L'Étranger | none | `copied-original-apart`, 5ab4254a8 | yes | correct (judged; Manibusan's film) |
| Patagonien (Kinopolis Freiberg) | 1 | 1010170 Patagonia 2023 | none | 0ab79600b (alternative-title IMDb suggestion) | yes | correct (judged: a travel lecture) |
| Gegen das Vergessen – Weilheim in der Zeit des NS (Trifthof) | 1 | 599503 (Toscano 2015) | none | piece-beside | yes | correct (judged) |
| Französische Filmwoche: Das goldene Zeitalter – L'Âge d'or (Thalia), L'ÂGE D'OR (Metropolis) | 2 | 5729 (Buñuel 1930) | none | `joined by original-title` | — | correct: the feed links cineamo 1049439m, the page says "FSK folgt" (a new film) |
| Tabaluga und Leo – Die Weihnachtsgeschichte (Kinorama Unna) | 1 | 68030 Tabaluga – Der Weihnachtsfilm (2003) | none | learned listing-listing cannot-link | — | correct: the feed links another cineamo id (a new film) |
| Stille (Kinemathek Karlsruhe) | 1 | 306928 (2013) | none | BelowThreshold | — | correct: the feed links cineamo 1646983m |
| Shinjuku Boys // The Fish with One Sleeve (Filmhaus Köln)* | 1 | 797140 | none | slash double bill | yes | correct by policy (fa99272a2) |
| Tagträumer (Brotfabrik) | 1 | 83486 (Balayan 1983) | none | BelowThreshold 7.5% | — | undecided |

\* Regio Tales and Shinjuku Boys were re-filled by the agreement at 07:14, so they are not among the 113.

## Per-rule breakdown (113 lost and 22 changed, net)

| Rule (from the log's reason) | Lost | Changed | Correct | False | Reproduced by the code on 10-15 input? |
|---|---|---|---|---|---|
| cannot-link `denies-film` / learned listing-film veto (LOTR) | 65 | 6 (→123) | 0 | 71 | **no**: right at both refs |
| piece-beside (`ListingShape`, "… is billed beside …") | 10 (Unterbiberger 7, Ben'Imana, Kiki, Gegen das Vergessen) | – | 1 | 9 | no (Unterbiberger right at both refs); Gegen das Vergessen yes |
| `same-catalogue-id` join, then veto (De Gaulle) | 4 | – | 0 | 4 | no |
| BelowThreshold, feed-linked film not honoured (Versteck, Vorspiel, Roya) | 3 | – | 0 | 3 | — |
| film-ness V3f / Event (c33c887dc, f11255486, landed 1cb0e0a81) | 19 | – | 19 | 0 | **yes**, judged cures |
| pooled same-film join lost (Donnerstagskneipe, Kurzfilmprogramm) | 4 | – | 4 | 0 | yes |
| part denial (a1cb04588), copied-original-apart (5ab4254a8), IMDb alt-title (0ab79600b), agreement refusals | 5 | – | 5 | 0 | yes |
| cannot-link on another feed's film (L'Âge d'or ×2, Tabaluga, Stille) | 4 | – | 4 | 0 | — |
| changed: own match or pooled (Heimsuchung 9 wrong→wrong; Titans 3, When Pigs Fly 2, Ecrire la vie 1 cures; King Kong 1 right→wrong) | – | 16 | 6 | 1 (+9 wrong→wrong) | When Pigs Fly / Ecrire yes; Heimsuchung no |
| cast mismatch / cast tie-break / original_language / genres | **0** | **0** | – | – | – |
| undecided (Tagträumer) | 1 | – | – | – | – |

The listings add up as follows:

- **Correct refusals:** 31 lost, plus 6 changed that are cures. Every one the recording holds is a judged cure, as
  `identity-measure` intended for those commits.
- **Regressions:** 81 lost and 7 right→wrong, about 10 distinct films.
- **Wrong → wrong:** 9 (Heimsuchung).

None of the regressions moves between 8b75b704b and aa334f31d on the 10-15 recording.

## What the TMDB backfill will change

- **It will not restore any of these losses.** No loss was refused for want of genres, language or cast. The
  regressions (LOTR, Unterbiberger, De Gaulle) are cluster and piece decisions that read none of those fields.
- **The cast-mismatch guard (784d6da76) has been inert in prod until now.** It needs genres, so 190a1a441 recorded it
  as "inert in prod until the backfill". It switches on as the records fill (≈10:50 UTC) and can only **refuse** more.
  Its scope is a venue cast field listing two or more names, a German-language candidate that is neither Animation nor
  Documentary, and no agreeing year or director. Measured on recording 38713274617 it cured ES "La maleta" ×14 and moved
  nothing in DE. Expect few DE moves, but watch for losses after the backfill.
- **The cast tie-break (fd96c63ce) can gain.** Where the venue names the cast, it can choose between namesakes. It might
  settle Heimsuchung (1089789 vs 635775) only if the venues bill a cast; the scraped rows bill none, so expect nothing.
- So re-measuring after the backfill isolates the guard's effect. It does not bring back the 81.

## Limits

- No recording newer than 10-15 can be replayed. GitHub is suspended, and the first Forgejo "Record scrape fixtures"
  runs (ids 211…469) failed or were cancelled. Run 508 was still running at 10:00. Without newer input, "regression
  from the code" vs "the whole re-resolve of input that changed since 10-15" is decided by the 10-15 replay alone. On
  that replay the new code keeps every regressed film, which points to the latter.
- `identity-slice --prod-mirror` found no prod trace under the slice's key for Kinopolis Hanau LOTR 3 ("prod
  missing"), so it gave no third view.

## Verdict

- **Accept** the 31 event and namesake refusals and the 6 changed cures. They are the intended effect of the 10-17…10-19
  rules, and the slice and the judgements confirm them.
- **Do not accept** the ~88 regressions (LOTR Extended ×71, Über Unterbiberger ×7, De Gaulle ×4, King Kong, Ben'Imana,
  Kiki, DEFA ×2, Roya). They are real right-take losses on the site. They are not explained by the cast or language
  rules, and on the input they replay, they are not explained by any rule in 8b75b70..aa334f31d.

## Recommended action

1. **Do not wait for the backfill to fix this.** It cannot restore these losses. Re-measure `with_tmdb_id` and the
   take-change log after it finishes (≈10:50 UTC) only to isolate the cast guard, which now switches on.
2. **As soon as a Forgejo recording newer than 10-20 06:58 is green**, replay the regressed listings on it:
   - `scripts/identity-slice de --batch <regressed listings> --ref 8b75b704b --ref aa334f31d --recording <new run>`
     answers whether the code regresses on today's input.
   - Add `--orders 12` on one LOTR CineStar listing and on Über Unterbiberger to see whether the take depends on arrival
     order (incremental vs boot whole resolve).
   - `scripts/listing-history de --title "Rückkehr des Königs" --venue CineStar --replay` checks the 10-14 / 10-17 /
     10-20 flips.
3. Then fix the rule that the new recording names. Until then, the leading suspects are:
   - the LOTR cluster: the CineStar "(ext.)" family's learned listing-film veto, spread over the whole
     LOTR Extended cluster as `denies-film`;
   - the piece-beside rule, which reads a film's own subtitle ("Vom Woid in die Welt", "Les enfants de dieu") as a second
     piece;
   - the `same-catalogue-id` join, which lets Bambi's mis-titled kinoheld page veto every De Gaulle billing.

   Pin each with `identity-slice --emit-case` once it reproduces. Add `identity-judgements.tsv` rows for Über
   Unterbiberger → 1559083, De Gaulle → 1165366 and King Kong (Eulenspiegel, feed 244m) → 244 so the gates see them.
4. If the site's LOTR Extended screenings matter before then, pins (`identity_pins`, empty in DE) are the documented
   stop-gap (`docs/design/identity-resolver.md` §13).
