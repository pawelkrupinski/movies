# 2026-10-23 — kinoheld's copied credits: a short taking a feature's billing — PARKED, no signal cures the class (pinned 573, base `43718c02b`)

- **Cases:** DE Mal seh'n Kino "THE THING WITH FEATHERS" (prod serves 1278923, Anja Oelofse's 24-minute short whose
  credits kinoheld copied; right 1127628, both judged 10-22), DE Augustinum Diessen "Island" (1192466, a 3-minute
  short's year, runtime and director copied), Zum Dorfkrug and Heilige Cnaipe (already vetoed by the copied-title veto).
- **Reproduced:** on both recordings (573, 528) The Thing With Feathers is already RIGHT: `identity-slice de --title
  "THE THING WITH FEATHERS"` takes 1127628 by OwnMatch, the short 1278923 refused by `withdrawn:sibling-denies-film`
  (the cineamo-linked "Das Ding mit den Federn" denies it). Prod's wrong take is its incremental state, which no
  recording holds: nothing measurable to cure. Island is still WRONG (OwnMatch 93.1%: director, runtime and year all
  kinoheld's copy) and its decision made **no search at all** (`searched` empty, the only candidate a denied film): the
  take comes from the feed link alone.
- **Signals weighed:**
  - *the matched film's runtime against the screening's slot or the billing's stated runtime*: no listing field
    carries the slot length; kinoheld's stated runtime IS the copied short's (3′), so it agrees with the wrong film.
  - *a short (<40′) taking a billing when a same-titled feature exists* (S: taken runtime < 40, no short-programme word
    in the billing, a same-titled film ≥ 60′ among the candidates or the recorded searches): surveyed over every take
    of 573 (TMDB runtimes from the recording's enrichment fixtures). 162 short takes (pl 2, de 75, uk 73, us 12; es has
    no runtimes); 3 judged (2 right, 1 wrong = Island). S holds **21 takes, all unjudged**, and **not Island** (no
    search, so no same-titled feature is known to the decision; the recording holds no "Island" ≥ 60′ at all).
  - *the credited director's filmography lacking a feature of the title*: the copied director's filmography holds
    exactly the copied short, so it agrees with the wrong film by construction.
- **Why parked:** no signal reaches Island (its decision has no rival to prefer) and The Thing With Feathers is right on
  the recordings, so no rule here cures the class; S alone would move 21 unjudged takes, some plainly right shorts at
  repertory venues. Island stays known-wrong. Still needs the slot length, or a search the feed-linked take does not
  make today (search the billing's bare title when the linked film is a short, and prefer a same-titled feature).
- **For the user (not judged here):** S's 21 takes, several of which look like the same class (a short's record taking
  a famous feature's billing) and may be wrong takes: DE Raststätte "Latcho Drom" → 1247239 (19′; Gatlif's 1993 film
  47105 exists), Filmmuseum München "Huis clos" → 832791 (4′; 130650), Kino im Druckluft "Snake Eyes" → 1730437 (21′;
  8688), Forum 2 Olympiadorf "Keep The Change" → 1677543 (11′), Thalia-Lichtspiele "FEINKOST" → 1031289 (14′),
  Kulturfabrik Meda "The Invite - Die Einladung" → 708520 (24′), Cinecitta "Triple Feature" → 1656210 (4′), and US "The
  Dads" → 1073659 (12′; 1629358) at three venues. The rest (Sylvia, Kismet, Camouflage, Unlocked, BLUTGELD, BROTHERS
  IN ARMS, Das perfekte Geschenk, A Quiet House, The Exhibition, The Ties That Bind Us, Dandelion) have many
  namesakes. Survey script and data: scratchpad `s.py` / `shorts.pkl` (session 87a4c1bd); runtimes in the recording's
  `enrichment-<cc>/api.themoviedb.org/3/movie/<id>.*`.
