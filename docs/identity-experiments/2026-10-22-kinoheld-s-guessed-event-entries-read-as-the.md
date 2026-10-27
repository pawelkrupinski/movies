# 2026-10-22 — kinoheld's guessed event entries read as the venue's name alone (WS4 phase 3, the "Next angle" of the copied-share entry below) — NOT LANDED (local tags `experiment/kinoheld-guessed-entry`, `-strip` in ~/projects/movies, recording 528 DE and the pinned pair DE, base `a37f2c6f7`, CI runs 994/995)

- **Signal:** a venue's event gets a kinoheld catalogue entry of its own name, which kinoheld fills from a film database
  by that name: an original title, credit, cast and genres, and never a year, runtime or production country ("Ping Pong
  Session" carries Ping Pong (2025)'s original title and Yu Tianji, "Helmut Schleich" Helmut's, "ICONIC MOVIE"
  Iconic's). No API field tells these from a festival film's own yearless entry (checked: `__typename`, `castCurated`,
  images, language, rating, distributor, release dates, show flags and groups over all 1,083 venues' live programmes;
  `DynamicMovie` entries are events but carry no facts). What does: the entry's original title is mostly inside its
  title, beside a word of the title's own.
- **Rule** (`KinoheldClient.entryGuessed`): no year, runtime or production country; at least half of the original
  title's words in the title, and a title word the original lacks; not when one part of a multi-part billing (`:`, `|`,
  `/`, a spaced dash, brackets) holds the whole original ("FRAUENWELTEN 2026: Killing Anna", "Viva a Vida - Auf das
  Leben"). Such a show keeps only the venue's name, as `entryRenamed`'s does.
- **Precision, hand-checked:** DE prod mirror 2026-10-22: 46 entries (47 of 3,081 kinoheld listings), 45 an event on
  a guessed film's facts, 1 a film's own ("30 Jahre mit Kardeş Türküler"). Live, all venues: 103 entries, 3 a film's own
  (Kardeş Türküler, "Bolshoi: Don Quichotte", "Çakallarla Dans 8"), ~5 unsure. The plain "no year and no runtime"
  split (the copied-share entry's suggestion) is not precise: 159 mirror entries carry facts, 42 of them a festival
  film's correct ones ("Cloud", "Falcon Lake", "Red Rocks", "Nuisance Bear").
- **Measured** (CI identity-measure, the recorded DE corpora stripped by the same rule in `CorpusFixture`, as
  `entryRenamed` was measured):

  | recording | cure | right lost | right→wrong | new wrong | gained right | unjudged moves |
  |---|---|---|---|---|---|---|
  | 528 DE (994) | 30 (all "ICONIC MOVIE" ×27 / "Iconic Movie" ×3 → Iconic (2024)) | 0 | 0 | 1 | 0 | 27 (21 events → none, Appunti per un film sull'India → Pasolini's 1968 film, Kardeş Türküler ×2 lost, …) |
  | pinned DE (995) | 31 | 0 | 0 | 1 | 0 | 28 |

- **Why stopped:** the new wrong, Filmforum Landsberg's "Helmut Schleich" (the Kabarettist live) → "Helmut Schleich:
  Kauf, du Sau" (2018, a recording of an old programme). The guessed Kuzmic credit was all that vetoed it; with it gone the
  resolver takes a performer's recorded programme for his live evening by the name alone. That is a resolver cluster of
  its own (a performer's name billed bare takes a TV recording titled "<name>: <programme>"), not the scraper's.
- **Next angle (done above, landed):** land this together with a fix for that cluster (a bare person-name billing never takes a recording
  titled "<name>: …" without a date or runtime agreeing), or with that listing held known-wrong if the bar allows it.
  The 30 "ICONIC MOVIE" cures alone are worth it.
