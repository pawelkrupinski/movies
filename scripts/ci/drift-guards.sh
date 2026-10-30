#!/usr/bin/env bash
# THE DRIFT GATE'S COMMIT RULES (pre-push's `driftguards` check; docs/landing-process.md), over the commits of A..B:
#
#  - A RED FIX IS NO RESOLVER RULE (the owner, 2026-10-29). A flip or drift is fixed in the incremental state that
#    diverges -- a lag, a budget, the queue's order, a cache, a restart's key -- never by a new identity resolver rule:
#    a rule is identity design, measured on the full corpus in every country, outside red fixing. A commit naming a red
#    (`Fixes-Red:`) that changes resolver or agreement RULE code (RuleFiles below) carries `Identity-Design: <doc>`,
#    the design doc that measured it. Mechanism files -- AgreementStage's state handling, the incremental resolver, the
#    stores, the keys -- need none.
#  - A CHANGE TO THE DRIFT HARNESS CARRIES ITS SELF-TEST (G6). A commit changing what replays production's incremental
#    sequence or judges it (HarnessFiles: IdentityDrift, IdentityLiveDrift, TakeFlips, the drift check script) also
#    changes a self-test that proves the replay behaves as the worker does (SelfTestFiles: IdentityDriftSpec, the drift
#    slices and their spec, CorrectionFlipReplaySpec, TakeFlipsSpec, IdentityLiveDriftSpec, the drift check's own test):
#    a third of the week's drift reds were the judge's, not the product's (91cfbe7e8, 7b586af0f, 5df3295bd). A pure
#    refactor says so: `Drift-Harness-Refactor: <why>`.
#
# usage: drift-guards.sh A..B
# Exit: 0 every rule holds, 1 a commit breaks one (each named, with the fix), 2 usage or an unreadable range.
set -uo pipefail

range=${1:?A..B}
# The identity resolver's and the agreement's RULES: what a take is decided by, not how state is kept between ticks.
id=identity/src/main/scala/services/identity
RuleFiles="^($id/(IdentityResolver|FeedLink|FamilyScope|Acceptance[A-Za-z]*|CandidateScoring|CandidateGeneration|ClusterVoting|ConstraintEdges|ConstraintSolver|Corroboration|EvidenceWeights|EvidenceRatingGate|ReleaseVeto|LearnedCannotLinks|IdentityCalibration|VenueBacking|SourceReliability|FilmNess|MultiFilmBill|CatalogueLinks|TitleLinks|TitleCredit|CreditRelation|FactRelations)\\.scala|$id/agreement/(Agreement|AgreementCandidates|Broadcast|Catalogue|Correction|FamilyVerdicts|NonFilmEvents|SynopsisEvidence)\\.scala|common/src/main/resources/title-(rules|words)\\.tsv|common/src/main/scala/services/titlerules/.*)$"
HarnessFiles='^(worker/src/test/scala/scripts/IdentityDrift\.scala|worker/src/test/scala/scripts/IdentityLiveDrift\.scala|identity/src/main/scala/services/identity/TakeFlips\.scala|scripts/ci/identity-drift-check\.sh)$'
SelfTestFiles='^(worker/src/test/scala/scripts/IdentityDrift(Spec|SlicesSpec)\.scala|worker/src/test/scala/scripts/IdentityLiveDriftSpec\.scala|identity/src/test/scala/services/identity/TakeFlipsSpec\.scala|identity/src/test/scala/services/identity/agreement/CorrectionFlipReplaySpec\.scala|scripts/ci/identity-drift-check-test\.sh|test/resources/fixtures/drift-slices/.*)$'

commits=$(git rev-list --reverse "$range") || { echo "drift-guards: cannot read $range" >&2; exit 2; }

failed=0
for c in $commits; do
    subject=$(git log -1 --format='%h %s' "$c")
    trailers=$(git log -1 --format='%(trailers:only,unfold)' "$c")
    rules=$(git diff-tree --no-commit-id --name-only -r "$c" | grep -E "$RuleFiles" || true)
    if grep -qiE '^Fixes-Red:' <<< "$trailers" && [ -n "$rules" ] \
        && ! grep -qiE '^Identity-Design:[[:space:]]*[^[:space:]]' <<< "$trailers"; then
        echo "drift-guards: $subject fixes a red by changing identity RULE code: $(tr '\n' ' ' <<< "$rules")" >&2
        echo "  Red fixers do not add resolver rules (the owner, 2026-10-29): fix the incremental state that diverges (a lag, a budget," >&2
        echo "  the queue's order, a cache, a restart key). A rule change is identity design, measured on the full corpus in every" >&2
        echo "  country: it lands outside red fixing, or with 'Identity-Design: <doc>' naming the design doc that measured it." >&2
        failed=1
    fi
    files=$(git diff-tree --no-commit-id --name-only -r "$c")
    harness=$(grep -E "$HarnessFiles" <<< "$files" || true)
    if [ -n "$harness" ] && ! grep -qE "$SelfTestFiles" <<< "$files" \
        && ! grep -qiE '^Drift-Harness-Refactor:[[:space:]]*[^[:space:]]' <<< "$trailers"; then
        echo "drift-guards: $subject changes the drift harness ($(tr '\n' ' ' <<< "$harness")) without its self-test." >&2
        echo "  A harness change carries a capture-replay case proving the replay behaves as the worker does: extend IdentityDriftSpec," >&2
        echo "  CorrectionFlipReplaySpec, TakeFlipsSpec or" >&2
        echo "  identity-drift-check-test.sh in the same commit. A pure refactor: the trailer 'Drift-Harness-Refactor: <why>'." >&2
        failed=1
    fi
done
exit "$failed"
