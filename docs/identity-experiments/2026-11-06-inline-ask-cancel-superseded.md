# The inline asks' cancel at the deadline (step 2b): superseded by the model's input gate

**Tried:** built, never landed (branch `identity/inline-ask-cancel`, `18d6c1857392`, kept as tag `experiment/inline-ask-cancel`: `InlineFamilyAsks` cancels every inline ask still unstarted when the 5 s wait ends, failing-first test in `InlineFamilyAsksSpec`). Step 2b of the await-answers work (2026-11-05, `AWAIT-ANSWERS-STATE.md`, "Part 2") proposed
that a projection's inline asks (`AgreementStage.askedInline`, at most `AskNowWithin` = 5 s, the 16-thread
`InlineFamilyAsks` pool) cancel a cluster's unstarted asks at the deadline, so the next cluster's asks would not queue
behind them. It was parked behind its instrumentation (the `kinowo_worker_identity_agreement_inline_ask_seconds`
histogram, `part=queued|asking`, d3364b838c1e) until 2026-11-09, to confirm the pool wait was the gap.

**Why it stopped:** fix 1 of `docs/design/identity-structural-fixes.md` removes the gap it shortened. A listing's version
now enters the model only once the questions its own facts put to the families are answered or concluded
(`IdentityModelService.gate`, `AgreementReadiness`), asked through the queue ahead of the agreement's own. The
projection that meets a new listing's cluster finds its family answers filed, so the inline asks find nothing to ask
for it; cancelling them early buys nothing. The two-step write the cancel was to shorten (a "no film" written before the
answers, restored 5-7 s later: 67 of DE's 134 reversals in the 7 days to 11-06, Geister's 56 among them) is removed at
its cause instead of made rarer.

**What is left of the inline asks:** the cluster-level questions only a population leads to (a candidate's poster, the
find of an agreed IMDb id) and the corrections' asks. Deleting `askNow` with them is fix 1's step 1c, together with the
drift harness's model of it (`StageAnswers.inline`), once the gate's prod numbers show how often they still fire.

**Angle worth trying next:** none for the cancel. If 1c's numbers show the remaining inline asks still write a "no film"
first, the same gate, widened to the cluster's own questions once the cluster is known, is the next step.
