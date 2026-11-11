# 2026-11-11 — a CLIP poster-embedding vote beside the pHash vote (stopped at validation)

**Aim.** Add a second poster vote beside `PosterEvidence.vote`. It would use CLIP ViT-B/32 (laion2b) image embeddings
computed out of process, stored as int8 in their own collection, and read only for clusters the model and the pHash
vote leave unmatched. The rule is a MARGIN: the best candidate's max cosine over its posters is >= 0.75 and >= 0.05
above the runner-up candidate's. On a research sample (12.7k venue x candidate pairs, the right film always among the
candidates) it had 0 FP and 52.8% judged recall, against pHash <= 8's 35.9%.

**Validation on prod's REAL candidate sets** (the gate before building anything; bar: precision on judged labels
>= 99.5%). The local prod mirror was fresh (2026-11-11 ~12:40Z). The population was every model decision whose listings
show a venue poster: 6,360 clusters over the five countries. Kept for measurement: all judged and multi-candidate
clusters, plus 600 single-candidate matched ones, so 2,621 clusters and 5,043 candidate films. A cluster's candidates
are its members' `identity_traces` candidates that are not DENIED, plus the decision's film. A film's posters are prod's
filed `poster|film` paths, else `PosterHashing.chosen`'s 8. Labels come from `identity-judgements.tsv`, by member
listing (venue row, else `*`). A vote counts as FP when its film is judged wrong, when another film is judged right, or
when a non-TMDB film is judged right. pHash was recomputed locally for EVERY candidate (a PIL emulation of the worker's
slot), so both signals see the same candidates. Prod files film hashes only for the candidates of unmatched clusters.

| rule (judged clusters: 371, right film among candidates: 283) | TP | FP | precision | recall of reachable |
|---|---|---|---|---|
| CLIP margin (0.75 / 0.05) | 171 | 16 | 91.4% | 60.4% |
| pHash <= 4, one candidate | 146 | 13 | 91.8% | 51.6% |
| CLIP where the pHash vote does not fire | 30 | 3 | 90.9% | +10.6 pts |
| same, on clusters the model left UNMATCHED (where it would act) | 3 | 3 | 50% | |
| CLIP 0.85 / 0.10 | 139 | 9 | 93.9% | 49.1% |
| CLIP 0.90 / 0.10 | 94 | 3 | 96.9% | 33.2% |

int8 quantisation (x127) changes nothing measurable: the same TP/FP everywhere, and 4 of 1,782 weak agreements.
Among the 580 unmatched clusters that have candidates, the margin rule votes 82 times. Only 33 of those votes are
clusters the pHash vote does not already take, and 11 at 0.85 / 0.10.

**The false positives are mostly not CLIP's.** In most of them the venue really shows another film's poster, and a
pHash over the same candidates matches it too:
- Kaminwerk's "Teenie Fasching" uses the 1939 "Fasching" poster.
- Its "Dark New Wave Night" uses the poster of "New Wave" (2024).
- "Queen Rock Montreal" matches TMDB's 1982 "We Will Rock You" record.
- "Das fliegende Klassenzimmer" is judged to be the 2023 film but shows the 2003 poster.

The rest are other editions and remakes with near-identical art: DE "Buddies" (the 2026 film over the 1985 one), US
"NT Live: Hamlet" (2015 over 2026), ES "El Cascanueces" (the 2019 ROH over the 2026), DE "Billie". Then come events
billed with a film's artwork ("Maraton Igrzysk śmierci", "Volcanoes 3D", "THE COMIC STRIP") and a double bill
("We're Going on a Bear Hunt + The Tiger Who Came to Tea"). The stage's guards would drop some of these:
`PosterEvidence.shows` (relays and double bills) and `editionsApart` (Hamlet's billed year). The rest stay:
"Buddies", "THE COMIC STRIP", "Fliegende Klassenzimmer" and "Billie" bill no year or number to tell the editions
apart.

**Why it stopped.** Judged precision is 91.4%, far under the 99.5% bar, and no threshold reaches the bar with
useful recall: 0.90 / 0.10 is 96.9% at a third of the recall. On prod's real candidate sets the right film is often
missing or has a near-twin, which the research sample (right film always present) could not show. CLIP's margin
measures "same artwork", and on these clusters "same artwork" is not "same film" often enough. Its marginal
gain over pHash on the population it would act on is 33 votes in 580 unmatched clusters at 50% judged precision. So
nothing was built: no embedder, store or vote.

**Angles worth trying next.**
1. A guarded CLIP vote: `shows`, `editionsApart`, and a year/number check on every candidate pair with similar art.
   It would take a film only when no other film with near-identical art (cos >= 0.7) exists in TMDB's search for the
   title, not only among the candidates. That asks for wider poster sets, the cost the top-3 film-hashing change is
   cutting.
2. CLIP as a VETO helper rather than a vote. Many pHash misses on the right film are localised art (pHash 18-34
   bits, CLIP >= 0.8), so a CLIP match on the taken film could stop a pHash veto. That spends no new wrong take.
   Measure it on `Vetoed` decisions (222 in this snapshot).
3. Re-judge the clusters behind the FPs above. Several labels look like the venue showing the other film's poster
   ("Fliegende Klassenzimmer", "Queen Rock Montreal"), and that hurts the pHash vote equally.

**Where.** The scripts and data are kept outside the repository, in `~/.cache/movies-posters-research/prodval/`:
`extract.py` (mirror -> clusters.json), `fetch.py`, `embed.py`, `phash.py`, `evaluate.py [--int8] [--top T]
[--margin M]`, `sweep.py`. The earlier research sample is in the parent directory (`clip-eval.py`, `clip-comb.py`).
