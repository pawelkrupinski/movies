# 2026-11-06 — a runtime billed beside a joined event says nothing against the film when it is longer

**The flip class (backlog, kind `population`, UK):** DCA's "These Violent Delights + Recorded Q&A", Gala Durham's "It's
Nice Up North (U) (20th Anniversary) + Q+A", Watershed's and Quad Derby's "Oxide Ghosts: The Brass Eye Tapes + Q&A (18)":
none -> film -> none as their families arrive (`identity-flip-backlog.tsv`; flip-unification §7.6 and §7.8, both
backed out).

## The mechanism

Each family has a sibling whose OWN running time is the screening's: the film plus the Q&A. A learned cannot-link reads
that time as a fact against the film and, read from the sibling, withdraws the other members' credited matches
(`Families.siblingDenying`) or keeps two listings apart (listing-listing):

| family | sibling | its minutes | the film's | the learned rule that fired |
|---|---|---|---|---|
| It's Nice Up North | Forum Hexham "+ Q&A" {Fellows, 2006} | 120 | 80 | `runtime.gap >= 36 AND title in {none,overlap}` |
| Oxide Ghosts | Cultplex "+ Q&A" {Cumming, 1996} | 90 | 60 | `runtime.gap >= 6 AND year.distance >= 2` |
| These Violent Delights | DCA "+ Recorded Q&A" vs Kendal's ballet | +20 | | listing-listing runtime cut |

So the members take the film while that sibling is absent and lose it when it arrives: a rule over the present
listings that is not monotone in them, through a fact that is not about the film.

## The rule

`runtime.delta` is MISSING (states nothing) when the listing's billing joins an event to its film
(`TitleShapeMeasures.joinedEvents`: "+ Q&A", "+ Recorded Q&A", "+ Interval"; never a second feature, "1+2") and its
minutes are LONGER than every runtime TMDB states for the film by more than `Corroboration.RuntimeBand` (10): the extra minutes are the event's (within the band the slot still agrees: ICA's 92-minute "... A Jason Molina Story + Q&A" is the 91-minute film). Between two listings,
likewise when the longer one bills the event. A shorter time still counts (an event never makes a film shorter). It is
the edition rule's twin (`YearRuntimeMeasures.runtimeDelta`: an "Extended Edition" running longer is neutral) and the
slot doctrine of 2405daba00 (`Corroboration.fits`, `Agreement.runsShort`).

A function of each listing's own title and runtime and the film's record: population-independent (R4), so it adds no
A -> B -> A of its own. It removes a fact; it gives no film evidence it lacked, so it can only let the listing's other
facts (director, year, title) decide.

Not taken: §7.8's director-credit split (a learned denial whose own scoring credits the film's director withdraws
nobody). It also exempted DE Wittenberg's double bill "Panem-Mockingjay 1+2", whose runtime is a second feature's; a
director credit cannot tell those apart, a joined EVENT can.

## Measured

See the commit: `scripts/identity-what-if` in every country, then `scripts/identity-measure` (all countries, both
stages), right -> wrong 0. PopulationMonotonicity on the four slices: 0 flips (was 4); at full population every member
of the three families takes its film (197993, 475376, 1462368), the takes §7.6 and §7.8 judged right.
