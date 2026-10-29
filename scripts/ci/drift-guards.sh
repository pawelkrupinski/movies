#!/usr/bin/env bash
# THE DRIFT GATE'S COMMIT RULES (pre-push's `driftguards` check; docs/landing-process.md), over the commits of A..B:
#
#  - A DRIFT OR FLIP RED IS NOT CLOSED UNTIL ITS SLICE IS COMMITTED. A commit naming a red (`Fixes-Red:`) whose message
#    speaks of drift or flips must come with a drift slice of the listing it fixes -- a change under
#    test/resources/fixtures/drift-slices/ (scripts/identity-slice <cc> SELECTOR --emit-drift-slice) in the same push --
#    so IdentityDriftSlicesSpec holds the fix from then on (convergence-failures 2026-10-29, G2: E3, E4, E8 and E9's
#    re-breaks were listings that had failed before). A fix whose red has no listing to slice (a CI script, a timeout)
#    says so: `Drift-Slice: none -- <why>`.
#  - A RED FIX IS NO RESOLVER RULE (the owner, 2026-10-29). A flip or drift is fixed in the incremental state that
#    diverges -- a lag, a budget, the queue's order, a cache, a restart's key -- never by a new identity resolver rule:
#    a rule is identity design, measured on the full corpus in every country, outside red fixing. A commit naming a red
#    (`Fixes-Red:`) that changes resolver or agreement RULE code (RuleFiles below) carries `Identity-Design: <doc>`,
#    the design doc that measured it. Mechanism files -- AgreementStage's state handling, the incremental resolver, the
#    stores, the keys -- need none. The flip gate stays hard: no baseline for known flips.
#
# usage: drift-guards.sh A..B
# Exit: 0 every rule holds, 1 a commit breaks one (each named, with the fix), 2 usage or an unreadable range.
set -uo pipefail

range=${1:?A..B}
SlicePaths='^test/resources/fixtures/drift-slices/'
# The identity resolver's and the agreement's RULES: what a take is decided by, not how state is kept between ticks.
id=identity/src/main/scala/services/identity
RuleFiles="^($id/(IdentityResolver|FeedLink|FamilyScope|Acceptance[A-Za-z]*|CandidateScoring|CandidateGeneration|ClusterVoting|ConstraintEdges|ConstraintSolver|Corroboration|EvidenceWeights|EvidenceRatingGate|ReleaseVeto|LearnedCannotLinks|IdentityCalibration|VenueBacking|SourceReliability|FilmNess|MultiFilmBill|CatalogueLinks|TitleLinks|TitleCredit|CreditRelation|FactRelations)\\.scala|$id/agreement/(Agreement|AgreementCandidates|Broadcast|Catalogue|Correction|FamilyVerdicts|NonFilmEvents|SynopsisEvidence)\\.scala|common/src/main/resources/title-(rules|words)\\.tsv|common/src/main/scala/services/titlerules/.*)$"

commits=$(git rev-list --reverse "$range") || { echo "drift-guards: cannot read $range" >&2; exit 2; }
changed=$(git diff --name-only "${range/../...}") || { echo "drift-guards: cannot diff $range" >&2; exit 2; }
sliced=0
grep -qE "$SlicePaths" <<< "$changed" && sliced=1

failed=0
for c in $commits; do
    message=$(git log -1 --format=%B "$c")
    subject=$(git log -1 --format='%h %s' "$c")
    trailers=$(git log -1 --format='%(trailers:only,unfold)' "$c")
    if grep -qiE '^Fixes-Red:' <<< "$trailers" && grep -qiE '(^|[^a-z])(drift|flip)' <<< "$message" && [ "$sliced" = 0 ] \
        && ! grep -qiE '^Drift-Slice:[[:space:]]*none[[:space:]]*(--|—|-)[[:space:]]*[^[:space:]]' <<< "$trailers"; then
        echo "drift-guards: $subject fixes a drift/flip red but the push commits no drift slice." >&2
        echo "  A drift or flip red is not closed until its slice is committed: scripts/identity-slice <cc> --venue V --title T --emit-drift-slice," >&2
        echo "  then commit test/resources/fixtures/drift-slices/<cc>/<name>/ in this push. No listing to slice (a CI script, a timeout):" >&2
        echo "  add the trailer 'Drift-Slice: none -- <why>'." >&2
        failed=1
    fi
    rules=$(git diff-tree --no-commit-id --name-only -r "$c" | grep -E "$RuleFiles" || true)
    if grep -qiE '^Fixes-Red:' <<< "$trailers" && [ -n "$rules" ] \
        && ! grep -qiE '^Identity-Design:[[:space:]]*[^[:space:]]' <<< "$trailers"; then
        echo "drift-guards: $subject fixes a red by changing identity RULE code: $(tr '\n' ' ' <<< "$rules")" >&2
        echo "  Red fixers do not add resolver rules (the owner, 2026-10-29): fix the incremental state that diverges (a lag, a budget," >&2
        echo "  the queue's order, a cache, a restart key). A rule change is identity design, measured on the full corpus in every" >&2
        echo "  country: it lands outside red fixing, or with 'Identity-Design: <doc>' naming the design doc that measured it." >&2
        failed=1
    fi
done
exit "$failed"
