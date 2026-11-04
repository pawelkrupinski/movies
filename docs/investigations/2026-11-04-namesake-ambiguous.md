# Namesake-ambiguous listings decide by their own facts only (2026-11-04)

Root cause 3 of the convergence reds (claude docs, convergence-reds-root-causes-2026-11-04, fix A; owner-approved
2026-11-04 as step 3, measured first). Every repeat offender of the week's identity reds -- UK "Hen" (Mowlem, Lighthouse
Poole, Keswick), Torrington "Dracula", CinemaxX "Titans", Lichtblick "Buddies", Keswick "These Violent Delights" -- is a
title TMDB holds two or more plausible works for, whose own facts do not tell them apart. Every rule that then decided
it reads other listings (a family majority's bound, solver tiers and lowest-id joins, the venues.current fill, sibling
denials, shared posters, learned vetoes), so its take moved as listings arrived.

## The state

A listing is NAMESAKE-AMBIGUOUS (`NamesakeAmbiguity`) when, read as a node of its own (its own evidence, text, venue and
cast; its own queries' reach; no sibling's reached candidates, no feed link of another listing of its node):

- two works or more are plausible: named by its title (or IMDb-titled), not untaken, denied by nothing but the cut;
- none is singled out by its own facts, read in turn: its house or relay season; the facts `Corroboration.facts` reads
  (year, director credited or named by its text, own original title, discriminating runtime incl. the "longer runtime
  tells nothing" rule, season, cast); a stated original title "A (B)"; a release in its country (older works only);
- and it has no pin and no TMDB-keyed feed link of its own (those decide it through the cluster as before).

Classification is per LISTING: an evidence node holds every venue's listing of an identical evidence, and reading it
per node let Chichester's director credit decide Pavilions Teignmouth's "De Gaulle: Résistance" only while Chichester
was present.

## The rule

A namesake-ambiguous listing is decided apart from its cluster, as no film, and no stage after the model (fills,
posters, families, catalogue ids, corrections) reads it again (`DecisionTrace.namesake`). The rest of its cluster is
decided as before.

## Properties

`NamesakeTakeSpec` walks every committed slice along 8 arrival chains of 8 steps: (a) a whole-resolve namesake-ambiguous
listing's take never moves; (b) every prefix classifies each listing as the whole resolve does. Rule off (main): 27 moved
takes (Titans x4, Camouflage x8, The Killer x4, Dracula, Hen x2, These Violent Delights x3, CBeebies x2, Ladykillers,
Fallen Angels, Fishguard Alice and R&J, Obsession) and, before the per-listing read, 8 reclassed listings. Rule on: 0 and 0.

Measurements: see the commit that turns the rule on.
