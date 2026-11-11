# Evaluating a poster signal: on prod's real candidate sets, never on a sample

**The rule.** Measure any identity signal (a poster hash, an embedding, a new vote or veto) on the candidate sets prod
really has: every model decision, its members' trace candidates (not DENIED) plus its film. Never measure it on a sample
built so that the right film is always among the candidates. `scripts/poster-eval` does this for the poster vote and veto.

**Why (2026-11-11).** Seven agents asked whether a venue poster that matches a candidate's TMDB poster is a strong
signal, and how to improve it. On their research sample, where every listing's right film was a candidate, the pHash
vote scored about 99.4% precision and a CLIP margin vote 0 FP. On prod's real candidate sets the same rules scored 91.8%
(pHash <= 4) and 91.4% (CLIP). The difference is in how real sets look. Many lack the right film altogether, so the
poster picks the only lookalike there is: an older edition, the 1939 "Fasching" poster on a "Teenie Fasching" party, a
recording's artwork for a live show. Others hold a near-twin, such as a remake or a re-release with the same art. A
sample that always holds the right film has neither case, and so it measures a different question ("which candidate is
the right one?") from the one prod asks ("is this candidate the right one?"). The CLIP vote was stopped at that
validation: `docs/identity-experiments/2026-11-11-clip-poster-margin.md`.

**How to apply.**
- Start from `scripts/poster-eval` (the dataset is prod's decisions over the local mirror) and report judged labels
  (`identity-judgements.tsv`) separately from prod's own takes (weak labels). A weak label agrees with prod by
  construction wherever prod is wrong.
- List every new FP and every lost TP: a precision figure hides which films moved.
- Reproduce production's hash bit for bit (`scripts/poster_hash.py`: vips as the worker runs it). A pure PIL resize
  matches only ~82% of prod's hashes, enough to move a 4-bit vote.
- For a new signal with no tool yet, extract the candidate sets the way `poster_eval.build` does, rather than sampling
  pairs from the judgements.

**The 2026-11-11 baseline**, reproduced by `scripts/poster-eval`: pHash <= 4 at 135 TP / 14 FP on prod's real sets (the
prototype found 142 / 13 three hours earlier), and 150 / 14 with 16 posters a film. The figures and why they differ are in
the `poster-eval` section of `.claude/skills/identity-investigation/SKILL.md`.
