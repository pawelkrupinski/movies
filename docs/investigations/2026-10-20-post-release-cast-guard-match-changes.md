# 2026-10-20: match changes after the TMDB backfill (cast guard on) and the a744eff38 release, all countries

## Summary

Two production events followed the 06:5x aa334f31d rollout (covered in `2026-10-20-de-release-match-losses.md`):

- **(b) TMDB record backfill + worker restart.** Stored `tmdb_films` gained `genres`, `original_language` and
  `credits.cast`. That switched on the cast-mismatch guard (784d6da76), along with the other readers of those fields:
  the subtitle-language check (f48ab8001) and the cast tie-break and `cast.venue` evidence (fd96c63ce).
- **(a) a744eff38 rollout (~10:50).** It carries aa6d8e682, which keys a venue's rows as its listings are keyed,
  including credits in the title.

**The cast guard refused no take in any country.** Of the 529 take-change lines logged 09:00–11:30, none names the
cast guard. The prod traces do show that it runs: ES, UK and US hold 4,860 listings with a candidate
`DENIED (its venue's own cast field names none of its top-billed cast …)`. Every one of those denials falls on a
candidate the listing did not take, or would not have taken. No listing went filmless because of one.

**aa6d8e682 cost nothing either.** After a744eff38 the logs show 15 lost and 1 changed take:

- PL Lalka/Dolly ×4: re-filled by the agreement after 11 s.
- DE ×11 + 1 changed: the boot's whole-corpus re-resolve applying rules that predate a744eff38 (piece-beside,
  CineStar LOTR veto, film-ness).

None has a credit in its title, so none comes from the keying change.

**No country is near the 2% flag.** Net wrong losses are at most about 0.3% of a country's matched films (PL, 2 of 713,
after the backfill). None comes from the cast guard or aa6d8e682.

## Timeline (UTC, 2026-10-20)

Pod and ReplicaSet start times come from `kubectl -n kinowo get rs,pods` and `process_start_time_seconds`. The backfill
window is the `changedAt` of the mirror's `tmdb_films` records that hold `genres`.

| Country | aa334f31d boot | Backfill window | Restart after backfill | a744eff38 boot |
|---|---|---|---|---|
| pl | 06:54 | 09:10–09:2x (15.9k records) | 09:21 (+09:25) | 10:50:53 |
| uk | 06:58 | 09:20–09:4x (35.7k) | 09:45 (+09:53) | 10:55:50 |
| de | 06:58 | 09:40–10:2x (48.3k) | 10:20 (+10:23) | 10:55:48 |
| es | 06:54 | 10:40–10:4x (6.7k) | 10:45 | 10:49:51 (both events together) |
| us | 06:54 | 10:40–11:14 (40.5k) | 11:14:27 | 10:51:45 (a744eff38 before the backfill restart) |

The brief expected ES at ~11:3x and US at ~12:0x, but both had already happened. ES's two events cannot be told apart.
In the US, the release came first, at 10:51. The guard-on restart followed at 11:14, and its periodic projection ran at
11:18:42 with 0 take changes.

## Per country: counts and take changes

The counts are `kinowo_worker_corpus_movies` (`total` / `with_tmdb_id` / `with_any_rating`), sampled every 10 minutes
just before and after each event. The take changes are the maximum of `kinowo_worker_identity_take_changes_total` in
each process lifetime, cross-checked against the VictoriaLogs take-change lines.

| Country | Before backfill (films / tmdb / rated) | After backfill | After a744eff38 (11:20) | Lost / changed / gained in the backfill lifetime | Lost / changed / gained under a744eff38 | Net wrong losses (films) | % of matched |
|---|---|---|---|---|---|---|---|
| pl | 1123 / 713 / 684 (09:20) | 1148 / 733 / 702 (10:40) | 1158 / 738 / 707 | 4 / 1 / 4 | 4 / 0 / 5 (the 4 lost re-filled in 11 s) | 2 (Cicha powódź ×2 listings, Binti right→wrong) | 0.3% |
| uk | 2724 / 1752 / 1638 (09:40) | 2722 / 1754 / 1639 (10:50) | 2722 / 1754 / 1639 | 8 / 0 / 12 (3 lost re-filled in 1 min) | 0 / 0 / 1 | ≤3 (Così fan tutte, La Fanciulla, Last Train Home?) | ≤0.2% |
| de | 4832 / 3100 / 2765 (10:20) | 4821 / 3122 / 2788 (10:50) | 4833 / 3131 / 2795 | 26 / 21 / 64 | 11 / 1 / 4 | ≤3 new (What Will I Become, The Killer?, Newborn?), plus the known LOTR / Kiki family | ≤0.1% |
| es | 256 / 236 / 215 (10:40) | 256 / 236 / 215 (11:20; both events) | same | 0 / 0 / 0 | 0 / 0 / 0 | 0 | 0 |
| us | 2059 / 2014 / 1945 (10:30) | 2069 / 2025 / 1956 (11:20) | 2064 / 2019 / 1950 (11:00) | 0 / 0 / 0 | 0 / 0 / 0 | 0 | 0 |

The count drifts (PL +35 films, US +10) are scrape intake. Neither lifetime logged a take change that matches them.

For comparison, the aa334f31d lifetimes before the backfill logged 12/4/10 (PL), 264/5/582 (UK) and 133/22/28 (DE)
lost/changed/gained, and 0 in ES and US.

### The cast guard in the prod traces (mirror `identity_traces`, read 11:23)

| Country | Listings with a cast-DENIED candidate | …and no film taken | Highest-scored denial (taken instead) | Verdict |
|---|---|---|---|---|
| es | 83 | 0 | all <10% (e.g. "La maleta" → 1494231, Therón 2025) | correct |
| uk | 1435 | 18, every denied candidate ≤4.4% and none the right film ("It (1990)" → Misery, "Silent Night (MET)" → Silent Night 2023) | Beats (UK), GFT: 603519 (US Beats) 31.1% denied → 493141 (Brian Welsh) | correct |
| us | 3342 | 8, all ≤2.5% ("It (1990)") | Atonement, Landmark Opera Plaza: 4347 (Wright 2007) 41.5% denied → 1484085 (Van Dyk 2026) | correct |
| de | 0 | – | – | guard never fires |
| pl | 0 | – | – | guard never fires |

Other high-scored or rank-1 denials are also correct: "Wuthering Heights" at Cineworld Dalton Park (the venue's director
is Arnold, so it took 9364 and denied Fennell's 1316092), "Beast (2026)" (took Atkins, denied Ayer's "Heart of the
Beast"), Home Alone (denied Home Alone 3), "Agbara Nla: The Return" (denied LOTR 3), "Deadline (Mandarin)" and
"The Great Gatsby (1974)".

The groups the brief asked about are exempt from the guard by its own terms:

| Group | Why the guard skips it |
|---|---|
| Sparse cast | It needs two or more entries of two words or more, and no one-word entry. |
| Foreign cast | The candidate must be in the country's language and have a Latin-script cast. |
| Documentaries and animation | Genres 99 and 16 are excluded. |
| Opera and theatre relays | The billing names a stage work or relay. |
| Re-releases | A stated year within ±1 or a stated director clears it. |

No denial in the three countries where it fires falls on such a listing.

## Sample of lost and changed takes after each event

"Attribution" uses three labels:

- **G**: cast guard.
- **K**: aa6d8e682 keying.
- **O**: other, meaning the other backfill-fed rules (subtitle language, genres, cast evidence) or the boot's
  whole-corpus re-resolve under rules that predate the event.

| Event | Country | Listing (venue) | n | Before → after | Logged reason | Attribution | Verdict |
|---|---|---|---|---|---|---|---|
| backfill | pl | „Zemsta” z autorską narracją (CK Nowy Pafawag) | 1 | 19721 (Wajda 2002) → none | BelowThreshold 29.7% | O | correct (a narrated festival event) |
| backfill | pl | Okładka „Sanatorium pod Klepsydrą” (Alternatywy) | 1 | 423881 → none | BelowThreshold 36.2% | O | correct (a talk/event) |
| backfill | pl | Cicha powódź / 11. UFF (Muza, Atlantic) | 2 | 951038 (UA doc 2026) → none | same-title join, BelowThreshold | O | **wrong loss** (the festival's own film) |
| backfill | pl | Binti Edukacja Młode Horyzonty (Terminal Kultury) | 1 | 577285 (Migom 2019) → 855156 (Shamte 2021) | pooled 39.1% | O | **right → wrong** (kids' education strand) |
| backfill | uk | RBO: Così fan tutte 2027 (Palace Gorleston) | 1 | 1702775 → none | `denies-film` cannot-link | O | **wrong loss** |
| backfill | uk | The Met: La Fanciulla del West 27 (Beehive Honiton) | 1 | 1703629 → none | NoCandidate | O | **wrong loss** |
| backfill | uk | Last Train Home (Garden Cinema) | 1 | 45576 (Lixin Fan 2009 doc) → none | release veto | O | likely wrong loss (arthouse doc) |
| backfill | uk | The Model + Director Q&A (Phoenix) | 1 | 1375200 (1915) → none | BelowThreshold | O | correct |
| backfill | uk | Hocus Pocus (Tower Skegness) | 1 | 1549037 (Slovak 2025) → none | learned cannot-link | O | correct |
| backfill | uk | Caravaggio; Scottish Ballet; Resident Evil (Arzner) | 3 | lost → re-filled within 1 min | agreement | O | net zero |
| backfill | de | Rental Family (10 venues) | 10 | 1499673 (JP 2026 namesake) → none | BelowThreshold (Hikari's 1208348 at 64.3%) | O | correct refusal (wrong → none; right film not yet taken) |
| backfill | de | Saint-Saens: Samson et Dalila (MET live) (15 Kinopolis/Gloria/Mathäser…) | 15 | 1501054 (Egyptian comedy) → 1703624 (Met relay) | own match 47.1% | O | **cure** |
| backfill | de | Fallen Angels by Noël Coward (5 Cineplex/Rundkino) | 5 | 11220 (Wong Kar-wai) → 1702350 (Coward relay) | own match, director same person | O | **cure** |
| backfill | de | Newborn (CITY 46, KoKi) | 2 | 787844 (Parker 2026) → none | BelowThreshold 35.2% | O | undecided |
| backfill | de | WHAT WILL I BECOME (Traum-Kino) | 1 | 1557013 (doc 2026) → none | BelowThreshold 46.3% | O | likely wrong loss |
| backfill | de | TANZRAUSCHEN: Rotzgöre / Desvelado (Rex) | 1 | 1799912 (34-min short) → none | BelowThreshold 6.2% | O | correct (dance event) |
| backfill | de | Ein Neues Leben (Stadtkino Trostberg) | 1 | 1398460 → 898354 | own match 62.5% | O | undecided (billing says 2024, neither film is) |
| backfill | de | The Killer (Brotfabrik) | 1 | 10835 (Woo 1989) → 970347 (Woo 2024) | own match, feed overruled | O | likely right → wrong |
| backfill / a744eff | de | Buddies (Pollux, Lichtblick, Lichtwerk) | 2 → +3 | 259433 / 1629957 → none → 1629957 | same-title join | O | undecided |
| backfill / a744eff | de | Herr der Ringe 1–3 (ext.) (CineStar Oberhausen, Lübeck, Neubrandenburg; Kinopalast; LichtBlick; Vilsbiburg) | 15 | 120/121/122 → none | `denies-film` (CineStar LOTR veto) | O | wrong, the known family (see the DE doc) |
| a744eff | pl | Lalka / Dolly ×4 (ChDK, Stary Młyn, ŚDK, Światowid) | 4 | 1309083 → none → 1309083 (11 s) | same-title join, then agreement fill | O | net zero |
| a744eff | de | Kikis kleiner Lieferservice – Studio Ghibli (Andreasstadel) | 1 | 16859 → none | piece-beside | O | wrong (known, see the DE doc) |
| a744eff | de | Jens Wawrczeck – Immer Ärger mit Harry (Savoy); Spieleabend \| Command Tower; Die Welt Hautnah – Rückenwind; Gitarrenfestival Eröffnung | 4 | Hitchcock 1955 / 1172648 / 165027 / 122769 → none | piece-beside, short-untaken | O | correct (readings, game night, travel talk, concert) |
| a744eff | de | Tabaluga und Leo – Die Weihnachtsgeschichte (Tichelpark) | 1 | 68030 → none | learned cannot-link | O | correct (a new film) |
| a744eff | de | Das doppelte Lottchen (1950) (Schlosstheater) | 1 | 69289 (2007 animation) → 94228 (von Báky 1950) | dated title | O | **cure** |
| a744eff | de, uk | Der geteilte Himmel (Thalia); Nosferatu (ArtHouse Crouch End) | 2 | gained 201429 / 426063 | agreement / namesake | O | gains |

None of the rows is G or K. None of the titles a744eff38 moved carries a credit ("reż.", "dir.", "Regie"), the only
listings whose key aa6d8e682 changes. The visible trace of aa6d8e682 is in the first a744eff38 periodic projection,
which wrote rows while taking no film change: US 84 written with 0 take changes, DE 147 with 10, PL 15 with 4. That
pattern matches drafted films regaining their venue slots, the fix it was made for, but it is not proven.

## Verdict per event

- **(b) Backfill, cast-mismatch guard (784d6da76): keep.**
  - It refused 0 takes in all five countries. It fires on 4,860 listings in ES, UK and US, always against a candidate
    the listing did not take.
  - Its highest-scored denials (Atonement 2007 vs 2026, Beats US vs UK) are correct namesake splits.
  - Its exemptions (language, Latin script, genres 99/16, stage works, a year or director) cover the documentaries,
    foreign films, relays and re-releases where it could have gone wrong, and none is affected.
  - It never fires in DE and PL, so it can be neither credited nor blamed there.
- **(b) Backfill, the other rules it switched on: accept, with three follow-ups.**
  - Net PL, UK and DE effect: about 20 cured listings (Samson et Dalila ×15, Fallen Angels ×5) and 10 correct wrong →
    none (Rental Family).
  - Against that, ≤8 films lost or moved wrongly. Two are relays: RBO Così fan tutte ×1 and Met Fanciulla ×1. The others
    are Cicha powódź ×2, Binti ×1, What Will I Become ×1, The Killer ×1 and possibly Last Train Home ×1.
  - That is ≤0.3% per country, so it is not flagged.
- **(a) a744eff38 / aa6d8e682: keep.**
  - 0 take losses come from it.
  - The DE losses at its boot are the whole-corpus re-resolve applying earlier rules (piece-beside, the CineStar LOTR
    veto), as in the 06:58 boot.
  - PL's 4 were re-filled in 11 s, and ES and US moved nothing.

## Recommended action

1. **Keep the cast guard as is.** No threshold change is warranted: 0 wrong refusals across 4,860 denials. If DE or PL
   ever show a cast refusal, these are the knobs to look at first:
   - `ListedCast.Names` and `VenueNames.Names`: raise from 2 to 3 names before a field counts.
   - Widen the "fact agrees" year tolerance from ±1 to ±2 for re-releases.

   Re-check after the next DE/PL scrape cycle with
   `_time:1d kind:* reason_first:~"cast field"` and the trace sweep in Method step 4.
2. **Keep a744eff38.** Revert nothing.
3. Judge and fix the backfill-side regressions:
   - Add `identity-judgements.tsv` rows:
     - PL Cicha powódź → 951038
     - PL Binti (Młode Horyzonty) → 577285
     - UK RBO Così fan tutte 2027 → 1702775
     - UK Met La Fanciulla (Beehive) → 1703629
     - DE What Will I Become → 1557013
   - Replay each with `scripts/identity-slice <cc> --key '<venue>|<title>' --prod-mirror` (the log line's `repro`) to
     find the rule. The likely suspects are the subtitle-language check on non-country-language documentaries and the
     relay `denies-film` cannot-link.
4. The LOTR (ext.) and piece-beside regressions remain open as in `2026-10-20-de-release-match-losses.md`.

## Method

All of it was read-only.

1. Pods: `kubectl -n kinowo get rs,pods` on monitoring-1, plus `process_start_time_seconds{job="kinowo-worker"}`, which
   showed the extra restarts.
2. Prometheus (`10.20.0.11:9090`): `kinowo_worker_corpus_movies` and `kinowo_worker_identity_take_changes_total`, the
   latter's maximum taken per process lifetime.
3. VictoriaLogs (`10.20.0.11:9428`, the source of the `kinowo-logs.json` dashboard):
   - `_time:[2026-10-20T09:00Z, 12:30Z] kind:*` gave 529 lines.
   - These were grouped by country, build, process lifetime and kind.
   - Each lost or changed line was read with its `reason_first`.
   - None contains "cast field".
4. Local prod mirror (`:28017`, fresh per `scripts/local-mirror/freshness`, lag ≤5 min):
   - `tmdb_films`: the `changedAt` of records that hold `local.genres`, for the backfill windows, and the TMDB facts for
     every id above.
   - `identity_traces`: every candidate line containing "cast field", grouped by the listing's taken film.

## Limits

- DE and PL traces hold no cast denial at all. The guard's scope is a candidate in the country's language with a
  Latin-script cast and a venue cast field of two or more names, and it is apparently never met there. Its effect on
  those countries is therefore unmeasured, not shown to be safe.
- The verdicts marked "likely" or "undecided" were not replayed with `identity-slice`. No recording newer than 10-15
  can be fetched yet (see the DE doc).
- ES's two events coincide, so their effects cannot be told apart. Both are 0.
