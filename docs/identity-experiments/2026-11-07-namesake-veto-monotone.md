# A namesake veto that only grows (parked, 2026-11-07)

**Aim.** Cure the arrival flips the uncorroborated-namesake veto decides (`veto:uncorroborated-namesake`, Corroboration.refused;
explorer 2026-11-07, run 42379263954: DE "Momo" x4, UK "The Grinch (Pyjama Party)" x6, "Miracle on 34th Street" x3, DE
Versmold "Nosferatu") by making it monotone in the population (structural fix 2).

**Why it flips.** A bare node is disputed when a namesake is taken on its own facts by >= 2 nodes AND by more nodes than the
cluster's film (`rivalBacking > backing`). Both counts grow as listings arrive, so a dispute starts and lifts: film -> none
-> film (Momo: Roxy Heinsberg's arrival gives 1175842 back its lead). The film's count also depended on the cluster's
decided film.

**Tried** (`experiment/namesake-contested-none`): support counted from each node's own facts alone (`ownlyNamed`, no
cluster film), and any namesake taken on its own facts by >= 2 nodes disputes, whatever the film's own count -- the only
form of the rule that moves a take one way ("contested -> none", flip-unification §5 stage 5).

**Measured** (identity-what-if, base fca77a5472, snapshot 2026-11-07T20:47Z):

| | model | final |
|---|---|---|
| DE | right-lost 11, new-wrong 1, unjudged 15 (every bare "Momo" -> none, "Karla", "In the Mood for Love", "Emil und die Detektive") | right->wrong 9 (bare "Karla" -> 9695 by the agreement fill once the model took none), unjudged 12 |
| UK | right-lost 219, cure 1, unjudged 169 (every RBO / Met relay family: Tosca, Swan Lake, Nutcracker, Parsifal, Romeo & Juliet) | right-lost 176, cure 1, unjudged 161 |

**Verdict.** Not landable: ~190 served right takes lost for ~14 flips, and the agreement fill turns the model's none into
a wrong film (Karla). Any rule that compares two growing counts (the film's backing and its namesake's) is two-way in the
population; the one-way form disputes every bare billing beside an evidenced namesake, and relay seasons (one season record
beside last season's, both evidenced) are exactly that. An owner trade, not a fix: keep the veto as is (its flips are
model-only for Momo / Miracle / Nosferatu; the final take stays put) unless a population-independent tie-break (TMDB's,
or the listing's screening season) can stand in for the count comparison.
