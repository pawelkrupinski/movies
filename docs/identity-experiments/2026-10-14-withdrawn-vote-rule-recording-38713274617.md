# 2026-10-14/18 — withdrawn-vote rule (recording 38713274617)

## Withdraw a pooled vote that a billing of the title, taking another film, denies — superseded
- **Tried:** in `ResolverDecisions`, a film only the cluster's pooled vote reaches (no member's own match, no pin) is no
  take when a listing kept apart from the cluster by a `denies-film` edge takes another film itself, is billed as a
  whole as a piece of a member's billing, and its family denies the film (IL KINO "Ukrainian Film Festival Berlin:
  CAMOUFLAGE" pooling to the 2001 film).
- **Measured:** scoped DE selectors: cured 1, lost 0. Landed as 26e48897d; convergence 38656624268 lost PL "Lalka (ale
  to horror)" ×9 and UK "Sense and Sensibility" ×3 / "Home Alone (Pyjama Party)" ×6. Reverted in 22361d8bb.
- **Why it stopped:** whole-corpus losses the scoped selectors missed; then e40e9a4af (`Corroboration.refused`)
  cured Camouflage another way (IL KINO now takes 1735950 via Filled), and the slice case f772906e9 pinned turned real.
  Re-applied on main 10-18, the rule moves none of the title's 12 listings: nothing left for it to cure.
- **Next angle, if a pooled-vote namesake comes back:** gate the withdrawal on the listing carrying a fact of its own
  against the take, and never across a decorated programme edition ("(ale to horror)", "(Pyjama Party)") — the losses'
  shape. Measure the whole corpus first (`scripts/identity-measure-local`, full).
- **Code:** `experiment/withdrawn-vote` (8c666e431).
