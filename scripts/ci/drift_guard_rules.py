#!/usr/bin/env python3
# Purpose: The drift gate's `driftguards` rules: a drift fix carries its slice, a red fix adds no rule, gauges before behaviour
"""THE `driftguards` RULES of commit_rules.py (split out of it): the drift slice, the red fix that is no resolver rule, the
harness change that carries its self-test, and gauges before behaviour. The rules' story is commit_rules.py's header."""

from __future__ import annotations

import re
import subprocess

import tool_git
from commit_facts import SLICES, Commit, at
from retype_rules import retypes_only

_ID = "identity/src/main/scala/services/identity"
RULE_FILES = re.compile(
    rf"^({_ID}/(IdentityResolver|FeedLink|FamilyScope|Acceptance[A-Za-z]*|CandidateScoring|CandidateGeneration|ClusterVoting"
    r"|ConstraintEdges|ConstraintSolver|Corroboration|EvidenceWeights|EvidenceRatingGate|ReleaseVeto|LearnedCannotLinks"
    r"|IdentityCalibration|VenueBacking|SourceReliability|FilmNess|MultiFilmBill|CatalogueLinks|TitleLinks|TitleCredit"
    rf"|CreditRelation|FactRelations|NonFilmEvents)\.scala|{_ID}/agreement/(Agreement|AgreementCandidates|Broadcast|Catalogue|Correction"
    r"|FamilyVerdicts|SynopsisEvidence)\.scala|common/src/main/resources/title-(rules|words)\.tsv"
    r"|common/src/main/scala/services/titles/rules/.*)$")
HARNESS_FILES = re.compile(
    r"^(worker/src/test/scala/scripts/IdentityDrift\.scala|worker/src/test/scala/scripts/IdentityLiveDrift\.scala"
    rf"|{_ID}/TakeFlips\.scala|scripts/ci/identity-drift-check\.sh|scripts/ci/identity_drift_check(_[a-z]+)?\.py)$")
SELF_TEST_FILES = re.compile(
    r"^(worker/src/test/scala/scripts/IdentityDrift(Spec|SlicesSpec)\.scala|worker/src/test/scala/scripts/IdentityLiveDriftSpec\.scala"
    r"|identity/src/test/scala/services/identity/TakeFlipsSpec\.scala"
    r"|identity/src/test/scala/services/identity/agreement/CorrectionFlipReplaySpec\.scala"
    r"|scripts/ci/identity_drift_check_test\.py|test/resources/fixtures/drift-slices/.*)$")

# What the worker serves over time -- the model's input gate, holds, settle, asks -- and what sees it
SERVING_FILES = re.compile(
    r"^(worker/src/main/scala/services/(tmdb|agreementasks|identitystaging|lookupfill|identityvenuepages|identityprojection|identitymodel)/(IdentityModelService|ModelInputGate|AgreementReadiness|PendingListings"
    r"|AgreementQuestions|AgreementAsker|InlineFamilyAsks|QuestionMemory|QuestionPolicies|QuestionRenewals|OutstandingQuestions|VenuePageWait"
    r"|RetainedListings|ProjectionTriggers)\.scala|identity/src/main/scala/services/identity/(ListingReadiness|PageWait)\.scala"
    r"|worker/src/main/scala/services/tasks/SettleReaper\.scala)$")
# The series live beside what they measure (services.metrics keeps the shared plumbing): the metrics package, and each
# package's own series classes, named here one by one.
GAUGE_FILES = re.compile(
    r"^(worker/src/main/scala/services/metrics/.*\.scala"
    r"|worker/src/main/scala/services/tasks/(WorkerTaskMetrics|TaskPipelineSeries|MeteredTaskQueue|ParkedScrapeMetrics"
    r"|RatingRunCensus|CinemaScrapeCensus|CinemaContentCensus|ListingIncompleteMetrics)\.scala"
    r"|worker/src/main/scala/services/corpus/CorpusCensus\.scala"
    r"|worker/src/main/scala/services/(tmdb|agreementasks|identitystaging|lookupfill|identityvenuepages|identityprojection|identitymodel)/(IdentityAgreementMetrics|IdentityLookupFillMetrics|IdentityModelGauges"
    r"|IdentityProjectionGauges|IdentityTakeMetrics)\.scala"
    r"|infra/nix/files/monitoring/(rules/.*\.rules|grafana/.*)"
    r"|infra/test/app-metrics-catalogue/.*)$")


DRIFT_WORDS = re.compile(r"(^|[^a-z])(drift|flip)", re.I)
SLICE_NONE = re.compile(r"^none\s*(--|\u2014|-)\s*\S", re.I)


def drift_fix_carries_its_slice(c: Commit, opts: dict) -> list[str]:
    if not c.claims_fix() or not DRIFT_WORDS.search(c.message):
        return []
    declared = c.trailers.get("drift-slice", []) + [v for sha, v in opts.get("declared", ()) if c.sha.startswith(sha)]
    if any(p.startswith(SLICES) for p in opts.get("pushed", ())) or any(SLICE_NONE.match(v) for v in declared):
        return []
    return ["fixes a drift/flip red but the push commits no drift slice.\n"
            "  A drift or flip red is not closed until its slice is committed: scripts/identity-slice <cc> --venue V --title T\n"
            "  --emit-drift-slice, then commit test/resources/fixtures/drift-slices/<cc>/<name>/ in this push. No listing to slice\n"
            "  (a CI script, a timeout): add the trailer 'Drift-Slice: none -- <why>'; for a commit already on main, a later\n"
            "  commit says it for that one: 'Drift-Slice-For: <sha> none -- <why>'."]


REVERTS = re.compile(r"^This reverts commit ([0-9a-f]{40})\b", re.M)


def restores_its_reverted_rules(c: Commit, rules: list[str]) -> bool:
    """A revert (`This reverts commit <sha>`, as `git revert` writes it) whose every rule file is byte for byte the
    reverted commit's parent's: it takes the red's change back out and adds no rule (agentctl's red reverts, Main #2409)."""
    reverted = REVERTS.search(c.message)
    return bool(reverted) and all(at(c.sha, p) == at(f"{reverted.group(1)}^", p) for p in rules)


def red_fix_is_no_rule(c: Commit, _opts: dict) -> list[str]:
    rules = sorted(p for p in c.changed if RULE_FILES.match(p))
    if not c.claims_fix() or not rules or restores_its_reverted_rules(c, rules):
        return []
    docs = c.trailers.get("identity-design", [])
    if docs and all(at(c.sha, d.split()[0]) for d in docs):
        return []
    named = f" (Identity-Design: {', '.join(docs)} names no file of the commit)" if docs else ""
    return [f"fixes a red by changing identity RULE code: {' '.join(rules)}{named}\n"
            "  Red fixers do not add resolver rules (the owner, 2026-10-29): fix the incremental state that diverges (a lag, a\n"
            "  budget, the queue's order, a cache, a restart key). A rule change is identity design, measured on the full corpus\n"
            "  in every country: it lands outside red fixing, or with 'Identity-Design: <doc>' naming the design doc that measured it." + c.misplaced("Identity-Design")]


def harness_carries_self_test(c: Commit, _opts: dict) -> list[str]:
    harness = sorted(p for p in c.changed if HARNESS_FILES.match(p))
    if not harness or any(SELF_TEST_FILES.match(p) for p in c.changed) or c.has("Drift-Harness-Refactor"):
        return []
    return [f"changes the drift harness ({' '.join(harness)}) without its self-test.\n"
            "  A harness change carries a capture-replay case proving the replay behaves as the worker does: extend\n"
            "  IdentityDriftSpec, CorrectionFlipReplaySpec, TakeFlipsSpec or identity_drift_check_test.py in the same commit.\n"
            "  A pure refactor: the trailer 'Drift-Harness-Refactor: <why>'." + c.misplaced("Drift-Harness-Refactor")]


def gauges_before_behaviour(c: Commit, opts: dict) -> list[str]:
    behaviour = sorted(p for p in c.changed if SERVING_FILES.match(p) and not retypes_only(c, p))
    if not behaviour or any(GAUGE_FILES.match(p) for p in c.changed):
        return []
    why = []
    for value in c.trailers.get("gauges", []):
        if SLICE_NONE.match(value):
            return []
        why.append(f"'Gauges: {value}' gives no why (write 'Gauges: none -- <why>')")
    for value in c.trailers.get("gauges-shipped", []):
        sha = value.split()[0]
        try:
            full = tool_git.Git.here().text("rev-parse", "--verify", "-q", f"{sha}^{{commit}}").strip()
        except subprocess.CalledProcessError:
            why.append(f"Gauges-Shipped: {sha} names no commit")
            continue
        if full in opts["in_push"]:
            why.append(f"Gauges-Shipped: {sha} is in the same push, so the same deploy: land the gauges first")
        elif not tool_git.Git.here().ok("merge-base", "--is-ancestor", full, c.parent):
            why.append(f"Gauges-Shipped: {sha} is not an ancestor of this commit")
        elif not any(GAUGE_FILES.match(p) for p in Commit.read(full).changed):
            why.append(f"Gauges-Shipped: {sha} changes no gauge, alert or dashboard file")
        else:
            return []
    return [f"changes what the worker serves over time ({' '.join(behaviour)}) without its gauges.\n"
            + "".join(f"  {w}.\n" for w in why) +
            "  A gate, hold, settle or ask change ships the gauges, alert and dashboard that see it in the same commit\n"
            "  (worker/src/main/scala/services/metrics/ or the series beside what they measure, GAUGE_FILES;\n"
            "  infra/nix/files/monitoring/) or in an EARLIER push, named by\n"
            "  'Gauges-Shipped: <sha>' (review 2026-11-06 P4: the readiness gate shipped before its gauges, 4.5 h unseen).\n"
            "  The existing series already see it: 'Gauges: none -- <which series, why>'."]
