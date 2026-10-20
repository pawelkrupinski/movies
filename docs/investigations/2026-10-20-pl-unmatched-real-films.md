# Poland's unmatched real films, and the strand prefixes they hide behind

Investigated 2026-10-20 (data at about 11:25 UTC), read-only. All times are UTC. This follows row 1 of
[2026-10-20-corpus-drops-all-countries.md](2026-10-20-corpus-drops-all-countries.md) (Poland's films with a TMDB id
fell from 70.8% to 63.8%). No identity code was changed. Another agent is working on Poland's convergence suite, so
the rule-side findings here are proposals for that work.

Method: the local prod mirror (`mongodb://127.0.0.1:28017/kinowo_prod_mirror`, fresh to the second per
`scripts/local-mirror/freshness pl`). From it:

- every `movies` row with `tmdbId: null` that has a showtime on or after 2026-10-20: **420 films**;
- their screenings, and each listing's `identity_traces` row (basis, blocker, refusals, candidates and the searches
  actually run);
- the `tmdb_films` cache (25.5k records), to check whether TMDB has the film at all.

Lost takes come from VictoriaLogs (`_time:14d country:pl kind:lost`, 50 lines; the fields exist since 10-14).
Prefix use comes from every `identity_traces` listing in the five country mirrors (185k listings). "First seen" is
the earlier of a film's first showtime on record and its oldest screening write. The mirror keeps no creation date,
so treat it as approximate.

## Summary

- **Of the 420 unmatched films, 86 are real films.** The earlier estimate of about 20 was low. Most of the other 334
  are events: festival blocks, short-film sets, marathons, ballet, opera and concert relays, theatre plays (every
  Kino PCKul title is a stage play), children's sets, talks, e-sports and K-pop. 16 more could not be judged from
  the data (list at the end).
- The five films the earlier doc named as new releases have all **gained ids since**, through the existing 24 h
  TMDB retry: Przepis na święta 1601776, Jedziemy do domu 1710638, TEDE. Forever Rap 1768341, Lachtara. Tęsknota
  1705490, and Jak żyć, żeby nie zwariować 1489938. The "Kino na obcasach:" copy is now *the* film: the record keyed
  `kinonaobcasachjakzyczebyniezwariowac|2026` holds tmdb 1489938 and all 291 of its slots. "WSP:" no longer appears in
  Poland's corpus.
- Why the 86 have no id:

| Cause | Films | Fix bucket |
|---|---:|---|
| Prefix or suffix not stripped from the search | 30 | (a) |
| Refused by an identity rule; the right candidate was found | 25 | rules |
| Had an id and lost it to a rule change since 10-14 | 7 | rules |
| Title mismatch: a typo, an abbreviation, or a Polish title TMDB doesn't carry | 17 | (b) |
| New release not on TMDB yet | 6 | (c) |
| Search miss, cause unknown | 1 | – |

- **Rules are the largest single cause (32), not prefixes (30).** Three of the losses landed today, with the 10-20
  06:58 rollout of the 10-16 → 10-20 rule batch: Cicha powódź, Gabinet doktora Caligari, and Gradiva.
- **Estimated recovery:** (a) 19 to 25 films, (b) 8 to 14, (c) 0 now, rule fixes about 10 to 15. All of them together
  would add about 40 to 50 ids, which lifts coverage only to about 67–68%. The rest of the drop from 70.8% is the event
  mix, not matching quality.

## The table

"Target" is the TMDB record the listing should take, where one is known. A number is its TMDB id, and a percentage is
the resolver's own score for it. "not in TMDB cache" means our cache has no such record, not that TMDB lacks one.

| # | Title as listed | Venue(s) | ≈ First seen | Why no TMDB id | Target | Fix |
|---|---|---|---|---|---|---|
| 1 | Debiut Klarnet | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead (no separator) | Klarnet 1513079 | a |
| 2 | Debiut Kobieta stanu wolnego | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | not in TMDB cache | a |
| 3 | Debiut Mistyczka | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | Mistyczka 1731866 | a |
| 4 | Debiut Miłe kobiety | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | not in TMDB cache | a |
| 5 | Debiut Mroki | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | Mroki 1710676 | a |
| 6 | Debiut Odzyskany | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | Odzyskany 1731683 | a |
| 7 | Debiut Psoty | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | Psoty 1584452 | a |
| 8 | Debiut Violetta Villas | Kino Studyjne Centrum | 2026-10-19 | Prefix/suffix not stripped: `Debiut ` lead | Violetta Villas 1046771 | a |
| 9 | Drogi Panie Dyktatorze Edukacja Młoda Horyzonty | Terminal Kultury Gocław | 2026-10-19 | Prefix/suffix not stripped: trailing `Edukacja Młoda/Młode Horyzonty` (no pipe) | Dear Dictator (2018) | a, b |
| 10 | Elf. Świąteczne życzenie / dubbing - Nasze Kino | Nasze Kino | 2026-10-14 | Prefix/suffix not stripped: venue suffix `/ dubbing - Nasze Kino`: `dubbing` searched → 'Synchronisation' | Elf. Świąteczne życzenie (not in cache) | a |
| 11 | Festiwal Pięć Smaków: Idę z mnichem (1984) | Kino NCKF EC1 | 2026-10-20 | Prefix/suffix not stripped: `Festiwal Pięć Smaków:` not stripped; `(1984)` searched as the title `1984 1984` | Idę z mnichem (1984) | a |
| 12 | Halloween '26: Inni (re- release) | Kino Światowid | 2026-10-16 | Prefix/suffix not stripped: `(re- release)` with a space: `re[-\s]?release` misses it, `release)` searched → TMDB 'Release' wins | The Others 1933 | a |
| 13 | Halloweenowy mini-maraton  (syn przeznaczenia) | Kino Zbyszek | 2026-10-15 | Prefix/suffix not stripped: `Halloweenowy mini-maraton (X)`: one film in parentheses | Syn przeznaczenia | a |
| 14 | Halloweenowy mini-maraton (małpa) | Kino Zbyszek | 2026-10-15 | Prefix/suffix not stripped: `Halloweenowy mini-maraton (X)`: one film in parentheses | Małpa / The Monkey (2025) | a |
| 15 | Jak zyć, żeby nie zwariować czas kobiet | Na Starówce | 2026-10-19 | Prefix/suffix not stripped: trailing `czas kobiet` (Na Starówce) + typo `zyć` | Jak żyć, żeby nie zwariować 1489938 | a |
| 16 | Jak zyć, żeby nie zwariować święto kina | Na Starówce | 2026-10-20 | Prefix/suffix not stripped: trailing `święto kina` + typo `zyć` | Jak żyć, żeby nie zwariować 1489938 | a |
| 17 | KF Ambasada: Faust z muzyką na żywo \| 200. spotkanie klubu & 100. rocznica filmu | Kino Kosmos | 2026-09-23 | Prefix/suffix not stripped: `KF Ambasada:` stripped, but ` z muzyką na żywo \| 200. spotkanie…` kept | Faust 10728 | a |
| 18 | KLAPS + BABSKI WIECZÓR Czas, który nie nadszedł | Świt Zwoleń | 2026-10-09 | Prefix/suffix not stripped: `KLAPS + BABSKI WIECZÓR` strand: search kept `Klaps`, dropped the film | Czas, który nie nadszedł | a |
| 19 | Klub Filmowy Perspektywy film „Hardkor Disko” | Centrum Kultury Nowy Pafawag | 2026-10-12 | Prefix/suffix not stripped: `Klub Filmowy <name> film „X”` (rule needs a colon) | Hardkor Disko (2014), not in cache | a |
| 20 | Klub Filmowy Perspektywy film „Oczy szeroko zamknięte” | Centrum Kultury Nowy Pafawag | 2026-10-12 | Prefix/suffix not stripped: `Klub Filmowy <name> film „X”` (rule needs a colon) | Oczy szeroko zamknięte 345 | a |
| 21 | Klub Filmowy WCK "Odlot" | Wawerska Strefa Kultury | 2026-09-23 | Prefix/suffix not stripped: `Klub Filmowy WCK "X"` (no colon) | Odlot 14160 | a |
| 22 | Lalka święto kina | Na Starówce | 2026-10-20 | Prefix/suffix not stripped: trailing `święto kina` | Lalka (2026) 1321666/1309396 | a |
| 23 | Lydia i Władca Burz Edukacja Młode Horyzonty | Terminal Kultury Gocław | 2026-10-19 | Prefix/suffix not stripped: trailing `Edukacja Młode Horyzonty`; TMDB has only the French title | Lydia et le vaisseau des tempêtes 1594694 | a, b |
| 24 | Primetime święto kina | Na Starówce | 2026-10-20 | Prefix/suffix not stripped: trailing `święto kina` | Primetime (2026) 1375441/1787048 | a |
| 25 | Replika 51. FPFF w Gdyni: Nagrodzone Złotymi Lwami "Przez ścianę" + spotkanie z reżyserem | Kino Kultura | 2026-10-05 | Prefix/suffix not stripped: `Replika 51. FPFF w Gdyni: Nagrodzone Złotymi Lwami` (rule knows only KFF / Młodzi i Film) + `+ spotkanie` | Przez ścianę 1325963 | a |
| 26 | SONIA Edukacja Młode Horyzonty, cykl "Filmowe sekrety", Warszawa | Terminal Kultury Gocław | 2026-09-24 | Prefix/suffix not stripped: trailing `Edukacja Młode Horyzonty, cykl "…", Warszawa` | Sonia (unknown year) | a |
| 27 | Supermocni święto kina | Na Starówce | 2026-10-20 | Prefix/suffix not stripped: trailing `święto kina` | Supermocni 1571101 | a |
| 28 | Witajcie w hope 30.10 piątek - 20:40 (wieczór horrorów) | Kino Grunwald | 2026-10-03 | Prefix/suffix not stripped: date/time suffix `30.10 piątek - 20:40 (wieczór horrorów)` | Witajcie w Hope | a |
| 29 | Znachor. Pokaz w ramach Akademii Filmu Polskiego | Kino Centrum CSW Toruń | 2026-10-19 | Prefix/suffix not stripped: trailing `. Pokaz w ramach Akademii Filmu Polskiego` | Znachor (1937 or 1982) | a |
| 30 | Środy z kinem niezależnym-Najlepsze z Najgorszych | Kino Oskard | 2026-10-02 | Prefix/suffix not stripped: `Środy z kinem niezależnym-` (unspaced dash): the strand half was searched, the film half dropped | Best Worst Movie (2009) | a |
| 31 | 11. UFF - Cicha powódź | Kino Atlantic, Kino Muza | 2026-10-07 | Lost its id to a rule since 10-14: lost tmdb 951038 today (aa334f31d, 09:23): 'another film of its director stands beside it'; the bare `Cicha powódź` at Kino Muza lost it too | Тиха повінь 951038 (85.8%) | rule |
| 32 | Homo sapiens? | Kino MDK Wągrowiec | 2026-09-29 | Lost its id to a rule since 10-14: lost 1471345 on 10-19 (8b75b704b): exact top hit vetoed 'not released in PL' | Homo Sapiens? 1471345 | rule |
| 33 | NADZIEJA | Kino Kultura Gołdap | 2026-09-28 | Lost its id to a rule since 10-14: lost 277730 on 10-14 (ca4059b55, Filmweb-fill drop); now below the rating cut | Nadzieja 277730? | rule |
| 34 | Okładka „Kłopot” | Kino Alternatywy | 2026-10-18 | Lost its id to a rule since 10-14: lost 206820 today (aa334f31d); the old take ('Soup for One') was probably wrong | Kłopot ? | rule |
| 35 | Okładka „Sanatorium pod Klepsydrą” | Kino Alternatywy | 2026-10-17 | Lost its id to a rule since 10-14: lost 423881 today (aa334f31d): the old take was the wrong film (Quay 2025); the right one (Has 1973, 45698) ranks 3.5% | Sanatorium pod Klepsydrą 45698 | rule |
| 36 | PANTEON - Gabinet Doktora Caligari \| muzyka na żywo | Kino Wisła | 2026-10-20 | Lost its id to a rule since 10-14: lost tmdb 234 today (aa334f31d): 'another piece of its billing is a performance' | Gabinet doktora Caligari 234 (87.4%) | rule |
| 37 | Przedpremiera: Gradiva \| Teleskop | Kino Kosmos | 2026-10-20 | Lost its id to a rule since 10-14: lost tmdb 1154730 today (aa334f31d): pooled to no film | Gradiva 1154730 | rule |
| 38 | 11. UFF - Teatr weteranów | Kino Atlantic | 2026-10-07 | Refused by a rule (candidate found): below the rating cut | Театр ветеранів 1755398 (28.6%) | rule |
| 39 | 19. FGA: Dora - film konkursowy | Janosik | 2026-10-07 | Refused by a rule (candidate found): learned veto: listing-film probability below the cannot-link cut | Dora (2026) 1750294 | rule |
| 40 | Akademia Polskiego Filmu: Znachor (1937) \| Gatunki międzywojnia II: melodramat | Kino Spektrum | 2026-10-08 | Refused by a rule (candidate found): double-billed strand text; `(1937)` not used to pick the year | Znachor (1937) 293920 | rule |
| 41 | Czarna Owca \| UŚMIECH O PORANKU | Gdyńskie Centrum Filmowe | 2026-10-15 | Refused by a rule (candidate found): below the rating cut; strand suffix `\| UŚMIECH O PORANKU` | Czarna owca 850489 (10.5%) | rule |
| 42 | Człowiek znikąd \| uroczysty pokaz odrestaurowanego filmu | Kino Kosmos | 2026-10-09 | Refused by a rule (candidate found): below the rating cut; restored film, year unknown | Człowiek znikąd ? | rule |
| 43 | Dni Kina Niemego - Quo vadis? | Kinoteatr Rialto | 2026-09-14 | Refused by a rule (candidate found): below the rating cut; silent-era rule correctly denies 1951 | Quo Vadis? 114166 (1913) or 1924 | rule |
| 44 | Dziedzictwi Krwi (Napisy) | Kino ŚDK | 2026-10-14 | Refused by a rule (candidate found): typo `Dziedzictwi`; 37.2% < 37.5% cut | Dziedzictwo krwi 309886 | b, rule |
| 45 | edukacja MH: Kos | Janosik | 2026-10-15 | Refused by a rule (candidate found): below the rating cut (prefix stripped) | Kos 1047917 (10.5%) | rule |
| 46 | Ice Cream Man 2D napisy | Kino MOK Międzyrzecz, Kino Ton +1 | 2026-09-23 | Refused by a rule (candidate found): 'its own facts do not favour it over the runner-up' (Lodziarz 1995) | Ice Cream Man (2026) 1477712 (46.7%) | rule |
| 47 | KINO KONESERA + „KINO, KTÓRE MÓWI” - BEZ KOŃCA | Kino Gwiazda Kętrzyn | 2026-10-07 | Refused by a rule (candidate found): `bills-several-works` (the `+` reads as a double bill) despite a 90.1% candidate | Bez końca 1596319? (listing's director is Marczak) | rule |
| 48 | Kobiety Bauhausu | Kino Paradox | 2026-10-20 | Refused by a rule (candidate found): below the rating cut although the director matches | Bauhausfrauen 832273 | rule |
| 49 | KOSMICZNY PORANEK \| Dawid \| warsztaty magiczne | Kino Luna | 2026-10-14 | Refused by a rule (candidate found): `KOSMICZNY PORANEK \| X \| warsztaty` strand; 'Dawid' search ranked 'Dawid i elfy' first | David (2025) | rule |
| 50 | Krew za krew \| Splat!FilmFest | Kinoteka | 2026-09-25 | Refused by a rule (candidate found): learned veto: country mismatch + runtime gap 16 | Ikatan Darah 1421155 (86.4%) | rule |
| 51 | Look Back (pokaz przedpremierowy) | Kino NCKF EC1 | 2026-10-20 | Refused by a rule (candidate found): below the rating cut; Oasis doc ranks above | Look Back 1244492 (15.6%) | rule |
| 52 | LOT NAD KUKUŁCZYM GNIAZDEM | Kino Tatry | 2026-10-20 | Refused by a rule (candidate found): learned veto; the listing's year (1984) contradicts the film (1975) | Lot nad kukułczym gniazdem 510 | rule |
| 53 | Maraton Horrorów: Halloween | Planet Cinema Zawiercie | 2026-10-12 | Refused by a rule (candidate found): `bills-several-works` on `Maraton Horrorów: <one film>` | Halloween 948 (54.8%) | rule |
| 54 | Maraton Strachu: Dźwięk śmierci | Janosik | 2026-10-18 | Refused by a rule (candidate found): `bills-several-works` on `Maraton Strachu: <one film>` | Dźwięk śmierci 1193501 (50.7%) | rule |
| 55 | Maraton Strachu: Martwe zło 2 | Janosik | 2026-10-18 | Refused by a rule (candidate found): `bills-several-works` on `Maraton Strachu: <one film>` | Martwe zło 2 765 (50.7%) | rule |
| 56 | Okładka „199 małych bohaterów” | Kino Alternatywy | 2026-10-18 | Refused by a rule (candidate found): below the rating cut: only the sequels '199 kleine Helden 2/3' surfaced | 199 kleine Helden (2015) | rule |
| 57 | Okładka „Eroica” | Kino Alternatywy | 2026-10-19 | Refused by a rule (candidate found): 'a rival fits its facts better' | Eroica (1958) 117767 | rule |
| 58 | Okładka „Wielka przygoda” | Kino Alternatywy | 2026-10-18 | Refused by a rule (candidate found): below the rating cut, ranked 2 | Wielka przygoda 119555 (1953) | rule |
| 59 | Ostatni Cygan w Oświęcimiu | Kino Cytadela | 2026-10-18 | Refused by a rule (candidate found): veto: 'a film of the venue's own language its title shares no word with' (Polish doc, English TMDB title) | The Last Romani in Auschwitz 550591 (59.2%) | rule |
| 60 | Wiedźmin. 25 lat | Kino Pionier 1907 | 2026-10-18 | Refused by a rule (candidate found): numeral rule: `25 lat` read as an instalment number | Wiedźmin 57278 (87.5%) | rule |
| 61 | Wielka Sztuka w Kinoteatrze Rialto - Frida: Viva la Vida | Kinoteatr Rialto | 2026-09-11 | Refused by a rule (candidate found): 'the title names another film' (¡Viva la vida! 1969 at 37.0%) | Frida. Viva la Vida 649244 | rule |
| 62 | „WAJDA: re-wizje” 2026 / "BEZ ZNIECZULENIA" (1985) reż. Andrzej Wajda | Kino Sokolnia Kępno | 2026-10-20 | Refused by a rule (candidate found): below the rating cut; the listing dates it 1985 (film is 1978) | Bez znieczulenia 42199 | rule |
| 63 | 24. WJFF: BUDAPESZT 1956 | Kino Luna | 2026-10-15 | Title mismatch: WJFF Polish title | Budapest 1956 doc? | b |
| 64 | 24. WJFF: UPADEK | Kino Luna | 2026-10-15 | Title mismatch: generic Polish title `Upadek` (WJFF) | ? | b |
| 65 | Akademia Kina Polskiego: Epitrafium dla Barbary Radziwiłłówny (1982) | Kino NCKF EC1 | 2026-09-03 | Title mismatch: typo `Epitrafium` (prefix stripped) | Epitafium dla Barbary Radziwiłłówny (1983) | b |
| 66 | Alim \| UFF | Kino Amondo | 2026-10-07 | Title mismatch: `\| UFF` suffix; Ukrainian original title | Алім 1048075 (1926) | b |
| 67 | Avengers: Koniec gry. Dogrywka (re-release) | Kino KDK | 2026-10-14 | Title mismatch: `Dogrywka` (the re-release's subtitle) is in no TMDB title | Avengers: Koniec gry 299534 | b |
| 68 | Avengres: Koniec Gry - Wersja Rozszerzona DUB | Max Kino | 2026-10-19 | Title mismatch: typo `Avengres` + ` - Wersja Rozszerzona DUB` | Avengers: Koniec gry 299534 | b |
| 69 | Dialog przez film: Sprawa Dmitriewa | Kino Kultura | 2026-10-20 | Title mismatch: Polish title of a Russian doc | Дело Дмитриева? | b |
| 70 | DZIEŃ DZIECKA KS. JANA KACZKOWSKIEGO | Kino Sokolnia Kępno | 2026-10-18 | Title mismatch: abbreviation `KS.` for `księdza` | Dzień Dziecka księdza Jana Kaczkowskiego 1707378 | b |
| 71 | Grzechy i cnoty. od Giotta do Papieża Franciszka | Kino Muranów | 2026-09-30 | Title mismatch: original title searched too, still 0 hits | Vizi e Virtù: Da Giotto a Papa Francesco (2025) | b |
| 72 | Kusama. Nieskończoność (napisy PL) \| TYLKO SZTUKA CIĘ NIE OSZUKA | Kino Spektrum | 2026-09-02 | Title mismatch: Polish title absent on TMDB | Kusama: Infinity (2018) | b |
| 73 | Najstarsza osoba na świecie | Kino Żak | 2026-10-15 | Title mismatch: Polish title absent on TMDB | The Oldest Person in the World (2025) | b |
| 74 | Największy lęk hitlera: proces przeciwko sztuce zdegenerowanej | Kino 1410, Kino Zamek | 2026-09-26 | Title mismatch: TMDB has only English/Italian titles | Hitler's Big Fear 1683499 / 1566116 | b |
| 75 | NIEZWYKŁE PRZYGODY SKARPETEK 4. DO ROBOTY! | Zacisze | 2026-10-19 | Title mismatch: listing says `Niezwykłe`, TMDB says `Niesamowite` | Niesamowite przygody skarpetek 4 1735319 | b |
| 76 | Odblask \| UFF | Kino Amondo | 2026-10-13 | Title mismatch: `\| UFF` suffix; Ukrainian original title | Odblask (Відблиск, 2025) | b |
| 77 | Pociąg Czerwona Ruta | Kino Kijów | 2026-10-13 | Title mismatch: only the IMDb suggestion finds it (3.0%) | Потяг «Червона Рута» 1479369 | b |
| 78 | The Taxidermist \| Splat!FilmFest | Kinoteka | 2026-09-25 | Title mismatch: IMDb id known (tt40381362); TMDB search finds '13 Souls' | The Taxidermist (2026) | b |
| 79 | Za dwoma zającami \| UFF | Kino Amondo | 2026-10-09 | Title mismatch: `\| UFF` suffix; Ukrainian original title | За двома зайцями (1961) | b |
| 80 | 19. FGA: Mam rzekę we krwi | Janosik | 2026-10-07 | New release, not on TMDB: festival doc, not on TMDB | Mam rzekę we krwi | c |
| 81 | OPOWIEŚCI O WILKU | Kino Agrafka | 2026-10-19 | New release, not on TMDB: 2026 Polish film, not on TMDB | Opowieści o wilku | c |
| 82 | Pasja. Początek świętości | Multikino Rzeszów | 2026-10-19 | New release, not on TMDB: 2026 Polish doc, not on TMDB | Pasja. Początek świętości | c |
| 83 | Pucio 2 | KinoPort | 2026-10-08 | New release, not on TMDB: not on TMDB as 'Pucio 2' (TMDB: Pucio 1645035, Pucio kocha zwierzaki 1731848, Pucio 3 1744707) | ? | c |
| 84 | Recepta na szczęście 2 | Kino Iskra, Kino Wars | 2026-09-27 | New release, not on TMDB: sequel not on TMDB yet | Recepta na szczęście 2 | c |
| 85 | Za drzwiami Atlantica: Morderstwo w Filharmonii | Kino Atlantic | 2026-10-17 | New release, not on TMDB: 2026 local production, not on TMDB | Za drzwiami Atlantica | c |
| 86 | Ekstremalna Pogoda | Cinema City Arkadia | 2026-10-20 | Search miss: no TMDB hit; Cinema City listing dated 2019 | Extreme Weather? | – |

Not counted above:

- **16 unclear titles.** These are bare titles that might be local films, shorts or kids' sets: Kostucha, Polski,
  Papużka, Prawdziwi ludzie, Wielkie pranie, Zamki na piasku, Zorza w ciemno, ŻYWIA…, Poświęceni (Napisy), Fantastyczne
  historie, Podwodny świat, Baczne Oczka, Idzie Nowe – Ty i Ja, Mity greckie i Odyseja na bis, (UN)SUPPORTED RACE and
  Reel Rock 20. Most have only a 3% IMDb suggestion as a candidate. A person needs to look at each.
- **Six art-on-screen documentaries** that probably have a TMDB record under an English or Italian title: FRIDA KAHLO.
  IKONICZNA ARTYSTKA, Malując duszę XX wieku: Pellizza, Pocałunek Klimta, Słoneczniki. Tajemnica Van Gogha, VAN GOGH w
  The National Gallery London, and Muzeum Prado: kolekcja cudów. They behave like the (b) rows.

## How titles are normalised today, and why these escape

- **Generic splitting (query only).** `SearchTitles.candidates`
  (`common/src/main/scala/services/resolution/SearchTitles.scala`) adds three kinds of split as extra search
  candidates: the halves around ` | `, the halves around a spaced dash, and the half after the first `: `. This is
  why most colon strands match without a rule of their own. In Poland, Akademia Kina Polskiego matches 9/10, DKF
  10/11, Dzień Kina Polskiego 11/11 and Kino Seniora 6/6.
- **Named strand rules.**
  - `ExtraTitleRules` (`common/src/main/scala/services/titlerules/ExtraTitleRules.scala`) has `prog(...)` and
    `searchStrip(...)` rules. Both are `GlobalStructural`, so they strip the search query and leave the display row
    and merge key alone. Examples: `xtra-pp-kino-na-obcasach`, `xtra-pp-klub-filmowy` (`^Klub Filmowy [^:]+:`),
    `xtra-pp-kf-klub` (`^KF [^:]+:`), `xtra-pp-replika` (`^Replika (KFF|Młodzi i Film):`),
    `xtra-pipe-cycle-banner` and `xtra-rerelease-paren-suffix` (`\(re[-\s]?release\)`).
  - `TitleRules.scala` has `ProgrammePrefixPattern` and the per-cinema rules, for example `alt-okladka` for Kino
    Alternatywy.
  - `FormatTags.scala` removes 2D, napisy and dubbing.
  - "WSP:" was dropped on purpose (ExtraTitleRules around lines 797 and 872): splitting the all-caps banner made
    `caseSegment` re-case "WSP" to "Wsp" on display.
  - "KLAPS" is handled only as `^KLAPS\s*/` for Kino Bajka Kluczbork.
- **Why these 30 escape:** every escaping strand lacks the separator that the generic split and the named rules key
  on:
  - a leading word with no separator: "Debiut Klarnet";
  - a trailing strand with no pipe: "Lalka święto kina", "Binti Edukacja Młode Horyzonty";
  - the film in quotes inside a banner with no colon: "Klub Filmowy Perspektywy film „Oczy szeroko zamknięte”";
  - the film in parentheses after a banner: "Halloweenowy mini-maraton (małpa)";
  - an unspaced dash: "Środy z kinem niezależnym-Najlepsze z Najgorszych";
  - a spaced variant of a handled marker: "(re- release)";
  - a date or time tail: "Witajcie w hope 30.10 piątek - 20:40 (wieczór horrorów)".

  In two cases a split ran but kept the wrong half: "Środy z kinem niezależnym" was searched instead of the film, and
  "KLAPS + BABSKI WIECZÓR …" was searched as "Klaps".
- **Strand copies that stay separate films.** Query-only stripping can find the id, but the strand copy keeps its own
  display row. It folds into the plain film only if both take the same id. When the copy is refused, the copy and the
  plain film stay apart. Cicha powódź shows this today: tmdb 951038 sits on the film "Silent Flood | Ukraina!
  Festiwal Filmowy", while the Polish listings "11. UFF - Cicha powódź" and the bare "Cicha powódź" (Kino Muza) lost
  it at 09:23. Only `Canonical` rules (for example the anniversary fold near ExtraTitleRules:1062) change the merge
  key.

## Prefix catalogue

The counts are distinct listings in each country's mirror today. "Matched" is how many of them have a film.

### Poland: strands with a separator (mostly handled by the generic split)

| Prefix | Form | Listings | Matched | Venues | Status |
|---|---|---:|---:|---|---|
| Festiwal Pięć Smaków | `X: film` | 32 | 28 | NCKF EC1, Pałacowe | Split works; "(1984)" year tails confuse it (#11) |
| N. UFF | `N. UFF - film`, `film \| UFF` | 27 | 19 | Atlantic, Amondo | Split works; Ukrainian titles and rule refusals |
| Akademia Polskiego Filmu | `X: a + b \| theme` | 21 | 1 | Spektrum, Pałacowe | Double bills (events), except Znachor (#40) |
| CINEMAFORUM | `X - …` | 19 | 0 | Atlantic | Events (short blocks) |
| Akademia Kina Światowego / Polskiego | `X: film (year)` | 22 | 21 | NCKF EC1 | OK |
| DKF / DKF Zamek | `X: film`, `X - film` | 29 | 28 | several | OK |
| Dzień Kina Polskiego | `X: film` | 11 | 11 | Kosmos, Pałacowe | OK |
| N. FGA | `N. FGA: film - film konkursowy` | 11 | 4 | Janosik | Split works; festival docs not on TMDB |
| Misiowe Poranki, Bajeczne soboty, Astra Dzieci, Poranek dla dzieci | `X: …` | 23 | 6 | Wisła, NCKF EC1, Astra, Pałacowe | Kids' events |
| Splat!FilmFest (on tour) | `X: film`, `film \| X` | 80 | 71 | Kinoteka, DCF, Kosmos | OK; two refusals (#50, #78) |
| N. WJFF | `N. WJFF: film` | 5 | 3 | Luna | Split works; generic Polish titles (#63, #64) |
| Kino Seniora, Pora dla Seniora, Kino Kobiet, Kino bez barier, Tani wtorek, Wtorek z klasyką, Klasyka kina, Seans przyjazny sensorycznie | `X: film` | 31 | 26 | many | OK |
| Kino na obcasach | `X: film` | 1 film, 291 slots | 1 | many | OK (`xtra-pp-kino-na-obcasach`) |
| Przedpremiera | `X: film \| strand` | 4 | 3 | Kosmos, Marzenie | Gradiva lost today (#37) |
| Maraton Strachu, Maraton Horrorów | `X: one film` | 4 | 0 | Janosik, Planet Zawiercie | Split works; refused by `bills-several-works` (#53–55) |
| Replika N. FPFF w Gdyni | `X: … "film" + spotkanie` | 3 | 1 | Kultura | Not in `xtra-pp-replika` (#25) |
| KF \<klub\> | `KF X: film …` | – | – | Kosmos | Prefix stripped; suffix kept (#17) |
| Wielka Sztuka w Kinoteatrze Rialto, Dni Kina Niemego | `X - film` | 8 | 3 | Rialto | Split works; art docs and silent films refused |
| Festiwal Korelacje | `X: „film” z narracją …` | 5 | 0 | Nowy Pafawag | Narrated-screening events |
| Balet z Opery Paryskiej N-N, Royal Ballet and Opera, OPERA N/N, ReTransmisje Met | `X: work` | 175 | 7 | chains | Relays (events) |
| Halloween 'N | `X: film (re- release)` | 4 | 2 | Światowid | Spaced "(re- release)" escapes (#12) |

### Poland: strands without a separator (escape everything; all are (a) rows)

| Strand | Form | Listings | Matched | Venue | Example |
|---|---|---:|---:|---|---|
| Debiut | `Debiut <film>` | 10 | 1 | Kino Studyjne Centrum | Debiut Klarnet |
| święto kina / czas kobiet | `<film> święto kina` | 5+ | 1 | Na Starówce | Lalka święto kina |
| Edukacja Młode / Młoda Horyzonty | `<film> Edukacja Młode Horyzonty[, cykl …]` | 9 | 7 | Terminal Kultury Gocław | Lydia i Władca Burz Edukacja Młode Horyzonty |
| Klub Filmowy \<name\> film „X” | quoted film, no colon | 3 | 0 | CK Nowy Pafawag, Wawerska Strefa Kultury | Klub Filmowy Perspektywy film „Hardkor Disko” |
| Okładka „X” | quoted film | 16 | 4 | Kino Alternatywy | Has a per-cinema rule (`alt-okladka`); the misses are rule refusals (#34, #35, #56–58) |
| Halloweenowy mini-maraton (X) | film in parentheses | 2 | 0 | Kino Zbyszek | (małpa) |
| … Pokaz w ramach Akademii Filmu Polskiego | `<film>. Pokaz w ramach …` | 1 | 0 | CSW Toruń | Znachor |
| / dubbing - Nasze Kino | venue tail | 15 | 14 | Nasze Kino | Elf. Świąteczne życzenie |
| w Helios RePlay, w Helios na Scenie, - KNT | chain tails | 21+ | 20 | Helios | Handled |
| (Napisy) | format tail | 7 | 4 | Kino ŚDK | Handled (FormatTags); the misses are typos |

### Other countries (strands with 3 or more distinct films and under 70% matched)

- **de:** `Pornfilmfestival:` (66 listings, 24 matched), `Exhibition on Screen:` (35/24, art docs), `Opéra de
  Paris:`, `Kurz Film Tour N -`, `LT Kino N:`, `Special:`, `Tour du Cinéma -`, `Jazz am Donnerstag -`,
  `Afrikamera N:`, `Jüdische Filmwoche:`.
- **uk:** `Toddler Club:` (176/101), `Secret Screening -` (64/0, correctly unmatched), `Royal Ballet:`, `+ Q&A`
  (36/19), `Purbeck Film Festival -`, `FBF:`, `MetOpera:`, `Opéra de Paris Ballet -`, `TVIFF:`, `LIAF N:`, `London
  Breeze:`, `+ Q&A FFFL`.
- **us:** `MetOpera:` (1,023/507), `Dismember the Alamo N -`, `Terror Tuesday:`.
- **es:** none with 3 or more distinct films.

All of these have a separator. The separator-less forms that hurt Poland did not show up as strands elsewhere.

## Quantified fixes

- **(a) Prefix and suffix stripping: 19 to 25 films.**
  - Rows 1–30 are the candidates. 19 have a TMDB record we already know of: six of the eight Debiut films, Oczy
    szeroko zamknięte, Odlot, Lydia, Jak żyć ×2, Lalka, Primetime, Supermocni, Znachor, Faust, Przez ścianę, Inni/The
    Others, and Czas, który nie nadszedł, which is matched elsewhere.
  - About six more are likely: Hardkor Disko, Dear Dictator, The Monkey, Best Worst Movie, Syn przeznaczenia, and Elf.
  - The fixes by strand:
    - "Debiut " recovers 6 to 8.
    - Trailing "święto kina" / "czas kobiet" recovers 5.
    - Trailing "Edukacja Młode Horyzonty" recovers 3.
    - Quoted film inside a banner with no colon recovers 3, plus KINO KONESERA and Replika.
    - Spaced "(re- release)" recovers 1.
    - "mini-maraton (X)" recovers 2.
    - An unspaced dash recovers 1.
    - A date or time tail recovers 1.
  - Several of these fold into films that already have ids: Jak żyć ×2, and Lalka and Primetime at other venues.
    That shrinks the no-id count rather than adding ids, and the coverage share improves either way.
- **(b) A Polish-title search fallback recovers 0 as such.** TMDB search already runs with `language=pl-PL`
  (`TmdbClient.searchUrl`), and TMDB matches every translation it holds. None of the 86 failed because the Polish
  title went unsearched. Two different fallbacks would work:
  - **(b1) Typo and abbreviation tolerance, 6 films:** Avengres, Avengers … Dogrywka, Niezwykłe → Niesamowite, "KS."
    → księdza, Epitrafium, and Dziedzictwi.
  - **(b2) A bridge to the English or original title, 5 to 10 films.** It applies where TMDB lacks the Polish title:
    Hitler's Big Fear, Kusama: Infinity, The Oldest Person in the World, The Taxidermist (the listing already carries
    IMDb tt40381362), Lydia, Pociąg Czerwona Ruta, and three Ukrainian UFF titles. The art docs above would add up
    to 6 more. Inputs could be the listing's IMDb id or Filmweb link, then TMDB `/find`, or the original-language
    title on Filmweb.
- **(c) New releases: 0 now, 6 later.** Rows 80–85 are not on TMDB yet. The existing retry (`TmdbAttempt.RetryAfter`,
  24 h, plus `TmdbGapMemory`) already picks them up once TMDB adds them, as it did for all five films the earlier
  doc named. No code is needed.
- **Rule refusals (not asked, but the largest bucket): about 10 to 15 films.**
  - `bills-several-works` on single-film strands (Maraton Strachu/Horrorów: X, KINO KONESERA + …): 4.
  - Today's losses: Cicha powódź, Caligari and Gradiva, 3. These came with the 10-16 → 10-20 batch. Likely
    commits: `76a4ac679` "Refuse the older namesake of a work a billing credits to its author", the film-ness V3f
    experiments `c33c887dc` and `f11255486`, and `784d6da76` (the cast deny).
  - Near-misses on the rating cut (Dziedzictwo krwi at 37.2%, Frida. Viva la Vida, Sanatorium pod Klepsydrą): about 3.
  - The Polish-language veto on a Polish doc whose TMDB record is English-titled (Ostatni Cygan w Oświęcimiu, 59.2%):
    1.
  - Ikatan Darah at 86.4%, Ice Cream Man (2026) and Wiedźmin. 25 lat: 3.

## Ranked proposals

1. **Separator-less strand stripping (a): about 20 films, title rules only, no identity code.** This needs these
   `searchStrip` rules or extensions in `ExtraTitleRules`:
   - a lead `^Debiut\s+` (scoped to Kino Studyjne Centrum if a global rule is too wide);
   - a trail `\s+(?:święto kina|czas kobiet)$` (Na Starówce);
   - a trail `\s+Edukacja\s+Młod[ae]\s+Horyzonty\b.*$`;
   - `xtra-pp-klub-filmowy` extended to `^Klub Filmowy \S+(?: film)?\s+[„"](.+)[”"]$` (keep group 1);
   - `xtra-pp-replika` extended to `Replika N. FPFF w Gdyni[^:]*:`;
   - `xtra-rerelease-paren-suffix` widened to `\(re\s*-\s*release\)`;
   - `mini-maraton \((.+)\)$` (keep group 1);
   - a date or time tail, `\s+\d{1,2}\.\d{1,2}\s+\p{L}+\s*-\s*\d{1,2}:\d{2}.*$`;
   - an unspaced-dash split for `Środy z kinem niezależnym-`.

   A broader alternative is a generic rule: "a billing whose only quoted segment is a title searches that segment".
   It covers Klub Filmowy, KINO KONESERA, Replika and Okładka in one go.

   *Test first:* add each raw title from rows 1–30 to `ExtraTitleRulesSpec` as raw → expected search query, and watch
   them fail (`scripts/fail-before`). Then replay the listings with `scripts/identity-slice pl --key '<venue>|<raw>'
   --ref HEAD~1 --ref HEAD` and confirm each takes its target id. Add a negative case for each rule: a real film whose
   title begins with "Debiut" or ends in "Horyzonty". Regenerate the snapshots (`regenerate-snapshots`) and run
   `scripts/identity-measure-local --countries pl` for the WRONG/LOST ratchet.
2. **Rule regressions and over-refusals: about 10 to 15 films. Hand these to the Poland convergence agent; nothing
   here was changed.**
   - Judge the today-lost takes (Cicha powódź → 951038, Caligari → 234, Gradiva → 1154730) and the four
     `bills-several-works` single-film strands into `identity-judgements.tsv` as RIGHT (`scripts/label-lookup pl --title
     …`).
   - Then make the slices `scripts/identity-slice pl --key 'Kino Atlantic|11. UFF - Cicha powódź' --ref 8b75b704b
     --ref aa334f31d` (and the same for the others) fail before the fix and pass after.
   - For `bills-several-works`: a strand prefix that names no second work ("Maraton Strachu: <one title>") should not
     count as a double bill.

   *Test first:* the labelled takes must show as LOST in the ratchet on today's base.
3. **Title-tolerance and bridge fallback (b): 8 to 14 films.**
   - (b1) When all searches return nothing or only sub-cut candidates, retry with a TMDB candidate within edit
     distance 2 of the listing title, or with abbreviations expanded (`ks.` → `księdza`, `św.` → `święty`). Accept it
     only through the normal resolver rules.
   - (b2) When the listing carries an IMDb id or Filmweb link, resolve through TMDB `/find/{imdb_id}`. This is
     already partly available: `imdb-suggested` exists but sits at a 3% prior.

   *Test first:* a recorded TMDB response (`record-client-fixtures`) for "Avengres: Koniec Gry" and "Dzień dziecka
   ks. Jana Kaczkowskiego". A resolver spec asserts the take 299534 / 1707378 and fails before the change. Add a
   negative case: "Lalka" must not reach "Lalka (ale to horror)" through edit distance.
4. **New releases (c): no code.** If anything, add a gauge for unmatched films with a Filmweb or IMDb link and a
   first showtime within 30 days, so "not on TMDB yet" separates from "we failed". *Test first:* a census spec in
   the `CinemaContentCensus` style over an in-memory corpus.
5. **Measure coverage over films only.** Exclude event, relay and festival-block titles (the classifier
   `cross_country.EVENT_RULES` already exists), so the ratio tracks matching quality and not the event mix. This
   is also row 1's recommendation in the parent doc.
