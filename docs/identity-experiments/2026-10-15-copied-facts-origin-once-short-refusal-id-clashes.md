# 2026-10-15/18 — copied facts, origin once, short refusal, id clashes (recording 38713274617, base `f2f27813d` unless noted)

Measures by `identity-measure` (DE unless noted; the other countries moved nothing or only unjudged takes). Reports and
slice outputs: `~/.cache/kinowo-identity/handoff/2026-10-15-wrong-match-round2/item-{d-origin-once,e-copied-facts}/`.

## Refuse a short whose every fact is a feed's copy ("Island")
- **Tried:** DE "Island" (Augustinum, kinoheld) takes a 3-min short on kinoheld's copied director, runtime and year,
  and no feature titled "Island" is in the pool for the short rule to prefer. Refuse a short when every fact backing it
  is a feed's copy; a second commit also refused shorts named by a piece of a longer billing.
- **Result (run 38730798889):** Island cured; 57 unjudged takes lost, several clearly right (IMAX Speyer's 40-min
  documentaries, "Donald und der Vogel Strauß", "Dr. Dolittle und seine Tiere").
- **Why stopped:** a refusal rule is too blunt; led to weighing a copied fact less (next entry).
- **Code:** `experiment/identity-short-copied-facts` (`c04b18ac7`, its last two commits).
- **Next angle:** the losses are all shorts with no rival at all; refuse only where the billing's runtime (or the
  screening's slot length) is a feature's, so a short has to explain a 90-min slot.

## A copied fact weighs a share of its own (`ScopeModel.copied`, signal `<signal>@copied`)
- **Tried:** each copied year, director and runtime scores at a share of its weight; the refit gained a step for the
  share. Variants: both directions at 0.5 (A) and 0.25 (B); support only at 0.5 (C5) and 0.25 (C25).
- **Result:**

  | Variant | Run | Cured | Right lost | Right→wrong | Unjudged moves |
  |---|---|---|---|---|---|
  | A, 0.5 | 38807878048 | 0 | 2 | 5 | 18 |
  | B, 0.25 | 38807898963 | 2 | 3 | 5 | 36 |
  | C5, support 0.5 | 38812134524 | 1 | 6 | 5 | 21 |
  | C25, support 0.25 | 38812145695 | 4 (Alpha, local heroes ×2, ATMOS: Michael) | 9 | 5 | 36 |

  The refit kept nothing; its copied-share step moved 1 unjudged take (it measures only the unmatched-cluster captures,
  which hold no own-match take, so it never sees these cures or losses).
- **Why stopped:** every variant flips "Fallen Angels by Noël Coward" ×5 (Cineplex) to Wong Kar-wai's 1995 film:
  Cineamo's copied facts were the only thing picking the 2026 stage capture over the famous namesake. And no target
  was cured: Island rides an exact title at search rank 1 with no rival; The Killer has 6 corroborating venues and
  rank 1 on both sides; Black Hole did not move.
- **Code:** `experiment/identity-copied-share-a` (`17929b7aa`), `-b` (`b1070e660`), `-c5` (`437819a1e`), `-c25`
  (`a2e03d3c5`); combined with origin once: `-ad` (`6ada52193`), `-c25d` (`ed289fe5b`).
- **Next angle:** discount a copied fact only when a namesake competes and nothing the venue states itself agrees; or
  keep its weight but count copies from one feed once (next entry). Fallen Angels needs its "by Noël Coward" credit
  read as a stage-play fact at intake (`TitleFacts` now does such peels) before any discount is safe. Leads it
  surfaced, still unjudged: "Teenie Fasching"→Fasching (1939), "Dark New Wave Night"→New Wave, "Three for Silver"→Tea
  For Three, "VOODOO LOUNGE"→VooDoo (2017), "Kurzfilmprogramm"→Endlich Tacheles ×2.

## Count each origin of a listing's facts once
- **Tried:** a feed entry backs a film once however many venues it is copied onto (corroborating venues counted by
  origin, not by venue).
- **Result:** on base `1d6fba3ea`: 0 cured, DE 2 + US 2 right lost, 28 unjudged moves. Re-measured on main as run
  38807915210 with the 28 judged: ~22 right lost (Der kleine Prinz ×15, Do You Love Me ×3, Mittagsstunde — the venues'
  own Cineamo ids name those films) for 7 cures (local heroes „Preisverleihung" ×2; Royal Ballet: La Bayadère ×5, the
  2027 relay, not the 2018 recording; both since known-wrong `served`). Its final stage showed 0 moves only because of
  the caveat above; slices show those takes going to none. With copied shares: A+D (run 38808011724) loses 4 right,
  C25+D (run 38812172262) 41, both keeping the 5 right→wrong. Two refits (`item-d-origin-once/refit{1,2}`) kept nothing.
  Re-measured locally 10-17 on base `36b83b72d` (identity-measure-local, hermetic): DE 2 + UK 5 cured, DE 23 + US 2
  right lost — Der kleine Prinz (1972) ×23, Volcanoes 3D ×2.
- **Why stopped:** ~3 right lost per cure, and its targets (The Killer, Black Hole) not cured.
- **Code:** `experiment/identity-origin-once` (`d9249b8dd`, ported onto main; first built as `52af5ce06`).
- **Next angle:** the losses are venues whose own feed id names the film; let an own id stand outside the origin
  count, and count origins once only among venues with no id of their own. A full re-calibration on a prod snapshot
  was never run.
- **Retried 2026-10-18, LANDED** (`origin-pages`): only the venues reading one listings site's ENTRY PAGE (Flicks',
  a `p:` claim) count once; a feed's catalogue id stays each venue's own. Local measure on recording 38713274617,
  base `807e7f287`: UK 5 cured (Royal Ballet: La Bayadère → the 2018 recording, known-wrong `served`, struck), US 2
  right lost (Volcanoes 3D at two IMAX venues reading one Flicks page, its copied director the only fact: recorded
  `accepted-right-loss`), DE Der kleine Prinz untouched. The Killer and NT Live Hamlet were not among the moves.

## Hold apart a join made only by a copied original title whose link names another film ("The Black Hole")
- **Tried (10-18, branch `kw-copied-facts`):** a cannot-link (`copied-original-apart`) between two nodes that meet
  only through feed-copied original titles (no billed title or search form alike) when one node's TMDB-keyed link
  names a film the other does not hold: either another film the other already holds, or one the other's own facts
  deny.
- **Result (identity-measure-local, base `807e7f287`, recording 38713274617):** DE model 2 cured (CITY 46 "THE BLACK
  HOLE", Filmmuseum München "The Stranger"), 2 right lost (DFF "CONFIDENTE", Traum-Kino "TOGETHER APART") and 3
  unjudged lost that look right (Andreasstadel "Departures" → Nokan, Filmhauskino "ENOUGH IS ENOUGH" → Trop c'est trop,
  Kurbelkiste "Felix" → Felix - Ein Hase auf Weltreise). Every other country moved nothing.
- **Why stopped:** each loss is the "holds another film" half: a venue bills a film's English title and its operator
  linked a namesake record. Black Hole is the same pattern; only the recording's gap (508578m is absent) sets it apart.
  The "denies" half landed alone (`ConstraintEdges.copiedOriginalJoinsAnotherFilm`): measured on base `f1571ea7e`,
  all five countries, it cures The Stranger and moves nothing else (LANDABLE).
- **Code:** `experiment/identity-copied-original-apart-wide`.
- **Next angle:** Black Hole needs a signal that its link is right, such as a festival banner or the 2024 record being
  in the recording, not a rule against the copy.

## Hold two family records apart when they name another id in one database ("Romeo and Juliet") — narrowed form LANDED 10-18
- **Tried:** UK Empire Theatre Halstead "Romeo and Juliet" (Royal Ballet relay): Wikidata's undated record of the play
  (IMDb tt2428892) joins IMDb's 1968 Zeffirelli film by title alone. A record naming another id in the same database
  is another entry; the vote join reads the same `Agreement.joins`. A follow-up reads only IMDb and TMDB ids so, as
  Wikidata spells some sites' ids its own way.
- **Result (slices and the ratchet; no whole-corpus measure):** Romeo and Juliet cured; lost DE "Der Film 444 …"
  (accepted) and UK "Thanks for coming FFFL", and asked a UK capture fill.
- **Why stopped:** the UK right loss with no evidenced trade; the take stays known-wrong.
- **Code:** `experiment/identity-id-clash` (`4e406da84`; the unlanded commits are `a11b0633d` and `4e406da84`).
- **Retried 10-18 (branch kw-id-clash):** the clash in `apart` only where one record is bare (undated, uncredited):
  identity-measure-local on 38713274617 (base `807e7f287`) still lost UK FFFL and newly UK "NFTS Collective Visions of
  Pleasure: I.K.U". Traced: FFFL stood on the PICK join (Wikidata's bare tt36481185 beside IMDb's and RT's dated 2026
  Cavalier), I.K.U on the VOTE join (the model's bare vote for TMDB's Brazilian "IKÚ" beside Wikidata's dated 2001
  film), Romeo on the vote join the other way round (the bare TAKER, Wikidata's play, beside a dated vote). The record's
  Wikidata type is not in the capture (families are filed parsed records), so the type angle needs a WikiFamily change
  and a recapture. LANDED instead: `Agreement.votesAnotherEntry` — the model's vote does not join takers that ALL date
  and credit nothing when it names another IMDb or TMDB id. Measure (base `4d43b4341`, every country): 1 cure, 0 lost,
  0 new wrong, nothing else moved.
- **Next angle (if the pick-join form is wanted):** carry Wikidata's non-film classes (play, literary work) into its
  record so a pick can be held apart by type, not by ids every right-by-luck agreement like FFFL's also clashes on.
