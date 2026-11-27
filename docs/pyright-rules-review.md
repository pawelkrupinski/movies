# Pyright rule set review (strict, Python 3.9)

Measured 2026-11-27 over the 100 frozen files of `test/resources/py-quality-baseline.tsv`: 15,663 errors before this
review's fixes, 15,319 after (the Part 1 landings). `pyrightconfig.json` is unchanged: the owner decides.

## What the non-"Unknown" rules found (274 diagnostics)

Classes: (a) a real bug, (b) a typing gap (behaviour fine, the types did not say so: fixed with a real type, narrowing
or deletion), (c) a false positive.

| rule | before | after | a | b | c / left |
|---|---|---|---|---|---|
| reportArgumentType | 81 | 43 | 0 | 38 (ModuleSpec, `str \| None` pairs, bool kwargs) | 43 left: `recording_diff.py` cascade, JSON rows |
| reportOptionalMemberAccess | 48 | 2 | 0 | 46 (34 are `__doc__.split`, None only under `python -OO`; Popen pipes; regex matches) | 2 left (`ci_wait.py:273` unreachable: c) |
| reportCallIssue | 35 | 31 | 1 (`process_group=` is 3.11+: the 3.9 floor raised TypeError, `identity_rule_ablation.py:227`) | 3 | 31 left, 29 of them `recording_diff.py` |
| reportAttributeAccessIssue | 29 | 10 | 0 | 19 (`urllib.request.quote`, heterogeneous dicts) | 10 left (`recording_diff.py`, `touched_methods.py`) |
| reportIndexIssue | 14 | 7 | 0 | 7 | 7 left |
| reportOperatorIssue | 12 | 5 | 0 | 7 | 5 left |
| reportOptionalSubscript | 7 | 0 | 0 | 7 | 0 |
| reportPossiblyUnboundVariable | 5 | 1 | 0 | 4 | 1 left (`deploy_watch.py:188`, c: loop guarantees it) |
| reportAssignmentType | 5 | 2 | 0 | 3 | 2 left |
| reportMissingImports | 4 | 4 | 0 | 0 | 4 (c: `sys.path` set at runtime: `backtest-alerts:99`, `discover.py:52`; optional `numpy`, `poster_hash.py:18`) |
| reportUnusedVariable / reportUnusedImport | 12 | 0 | 0 | 12, deleted | 0 |
| reportPrivateUsage | 5 | 0 | 0 | 5 (`recording_diff.py:288`, now public helpers) | 0 |
| reportReturnType | 4 | 0 | 0 | 4 (`runner_scheduler.py:1209` wrong annotation) | 0 |
| reportOptionalOperand | 3 | 0 | 1 (`job_metrics.py:294` subtracted a None PSI reading: TypeError, test added) | 2 | 0 |
| reportOptionalContextManager / Iterable | 6 | 0 | 0 | 6 | 0 |
| reportUnnecessaryComparison / IsInstance | 3 | 0 | 0 | 3 (a dead `isinstance(hook, dict)`, `owner-decisions:163`, deleted) | 0 |
| reportGeneralTypeIssues | 1 | 0 | 0 | 1 | 0 |

Totals: 2 real bugs in 274 diagnostics, about 165 typing gaps fixed, 3 false positives left, about 100 typing gaps
left (mostly `recording_diff.py`, whose 83 come from one untyped nested result dict). Another real bug class sits
under the `__doc__` findings: every tool with `__doc__.split(...)` crashes under `python -OO`.

## Verdict per rule

KEEP = error, WARN = shown, not counted by the ratchet (it counts severity error), DROP = off.

| rule | count | verdict | why, with an example |
|---|---|---|---|
| reportUnknownVariableType | 4,010 | WARN | Pure cascade. One untyped parameter or `json.load` makes every variable derived from it unknown; 0 real bugs. `infra/bin/nix-update:312` is unknown only because `drvs` is an untyped parameter. |
| reportUnknownArgumentType | 3,908 | WARN | Same cascade; fixing the source removes it (`scripts/claude-hooks/masked-exit-guard:214`). |
| reportUnknownMemberType | 3,096 | WARN | `x.append` unknown because `x = []` has no element type (`scripts/ci/fix_replay_exemptions.py:62`). Typing the root removes it. |
| reportUnknownParameterType | 2,238 | KEEP | Duplicates reportMissingParameterType on the same parameter, but also names unknown RETURN types (`scripts/adopt_ci_budgets.py:104`), which are roots. Counts are doubled; keep one of the pair. |
| reportMissingParameterType | 1,792 | KEEP | The root of the cascade: one annotation (`scripts/claude-hooks/gh-run-guard:39`) erases dozens of Unknown rows below it. This is the row to ratchet and to fix "when visiting". |
| reportUnknownLambdaType | 177 | WARN | Cascade from an untyped parameter of the enclosing function (`scripts/ci/commit_rules.py:430`). |
| reportMissingTypeArgument | 168 | KEEP | Cheap, root, never a cascade: a bare `set` / `dict` (`infra/bin/fleet_hosts.py:43`). |
| reportArgumentType, reportAttributeAccessIssue, reportCallIssue, reportIndexIssue, reportOperatorIssue, reportAssignmentType, reportReturnType | 195 | KEEP | Where the 2 real bugs came from (`identity_rule_ablation.py:227`), and every mis-annotation (`runner_scheduler.py:1209`). About 1 real bug per 100; still the best yield. |
| reportOptionalMemberAccess / Subscript / Operand / Iterable / ContextManager | 64 | KEEP | The `job_metrics.py:294` crash; but 34 of 48 are one idiom, `__doc__.split` (`scripts/ci/shard_tests.py:93`): keep, and the fix is a module-level `DOCSTRING = __doc__ or ""`. |
| reportPossiblyUnboundVariable | 5 | KEEP | Cheap; 4 were loops the types could not see (`stage-nixos-closures:135`), but a real unbound is a crash. |
| reportMissingImports | 4 | WARN | All false positives: the tool puts a directory on `sys.path` at runtime (`infra/bin/backtest-alerts:99`, `scripts/discover-cinemas/discover.py:52`). Better fix: `extraPaths` (`infra/test`, `data/uk/scripts`) and `numpy` in the tools venv. |
| reportUnusedVariable, reportUnusedImport | 12 | KEEP | Dead code found: `identity_stamp_carry.py:66`, `lint_registry_reads.py:78`. |
| reportPrivateUsage | 5 | KEEP | Cross-module private access, a coupling smell (`recording_diff.py:288`). |
| reportUnnecessaryComparison / IsInstance | 3 | KEEP | A guard that cannot fire, deleted (`owner-decisions:163`). |
| reportGeneralTypeIssues | 1 | KEEP | `runner_scheduler.py:1226`, a caller unpacking a wrongly annotated return. |

Strict rules that never fire in these files: reportUnusedClass, reportUnusedFunction, reportUnusedExpression,
reportUntypedFunctionDecorator, reportUntypedClassDecorator, reportUntypedBaseClass, reportUntypedNamedTuple,
reportConstantRedefinition, reportInconsistentConstructor, reportUnnecessaryCast, reportUnnecessaryContains,
reportMatchNotExhaustive, reportTypeCommentUsage, reportUnusedCoroutine, reportUnusedExcept,
reportSelfClsParameterName, reportOverlappingOverload, reportDeprecated, reportMissingTypeStubs,
reportTypedDictNotRequiredAccess, reportOptionalCall, reportAbstractUsage, reportNoOverloadImplementation,
reportInvalidStringEscapeSequence, reportUnsupportedDunderAll. KEEP all: zero cost today, and each guards a
mistake a new tool could make (an unawaited coroutine, a bare `except` that cannot match).

## Cost per real bug

About 13,400 of the 15,319 errors (87%) are the five Unknown rules: a cascade from roughly 1,800 untyped parameters and
a few untyped sources (`json.load`, `subprocess`, `argparse.Namespace`). They found 0 real bugs. The ~275 other rows
found 2 (and a -OO crash class), so about 140 diagnostics read per real bug, against thousands for the Unknown
cascade. Recommendation: keep the root rows (reportMissingParameterType, reportMissingTypeArgument, the non-Unknown
bug rules) as errors, move the four cascade rules to WARN, and let "types only when visiting" retire the roots.
Moving to WARN lowers the ratchet to about 4,300 rows (the two parameter rules, which double-count, and the real-bug rules), each one a thing to type, with no loss of bug-finding.
