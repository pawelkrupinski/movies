# The tools' end-to-end layer

The owner, 2026-11-27: "for the important Python scripts, also add end-to-end tests". Each tool has black-box CLI
tests (`scripts/lib/cli_harness.py`: one tool, fakes around it). Nothing ran the real tools together, through their
real chain. `scripts/e2e/` does that. It is modelled on agentctl's `~/bin/tests/e2e/`: real processes in one temp
root, stubs only at the machine's edge, deterministic polling and a mutation proof for every scenario.

## The harness: `scripts/e2e/e2e_world.py`

Each scenario gets one `World`, which is one temp root that is removed at the end. It holds:

- **HOME, TMPDIR and every cache.** XDG_CACHE_HOME, fail-before's worktrees, run-script-tests' durations and logs,
  and the hook's reuse store all live inside the root. No laptop cache, store or log is read or written.
- **Real git repositories.**
  - `toy_repo()` makes a bare "origin", a clone, and worktrees.
  - `movies_clone()` makes a clone of this checkout. It uses `--shared` and a sparse checkout without
    `test/resources/fixtures`. The tool trees are committed on top exactly as the working tree has them (or as a
    mutation leaves them), then pushed to a bare origin as its `main`.
- **Shims first on PATH.** Each one logs its argv (and its stdin) to one calls file:
  - `gh` (`gh_shim.py`) sends every call to the **stub GitHub**, `gh_stub.py`, a local HTTP server. A stub run moves
    one step through its states each time it is read, so there is no clock.
  - `ssh`, `nix`, `kubectl`, `sbt`, `sbt-warm` and `docker` refuse unless a scenario gives them a canned host.
    **No real host, k8s, prod Mongo or mirror is reachable.**
  - `agentctl` (on PATH and at `~/bin/agentctl`) agrees to `prepush-check` and refuses everything else. The live
    agentctl store is never reached.
- **Isolation.** It uses `cli_harness.isolated_env`:
  - no `GIT_*`, `GITHUB_*`, `AGENTCTL_*`, `PREPUSH_*` or `CI` from the surrounding run;
  - a fixed, unsigned git identity;
  - the governor and the JVM gate are off inside. The scenario itself runs under run-script-tests' ticket.

The harness's own effects go through the library's seams: processes through `cli_harness.run_bounded`, the
environment through `machine`, files through `tool_files`, and the gh shim's HTTP through `tool_http`. The one
exception is making and removing the temp root (one `test/resources/design-exemptions.tsv` row). `scripts/e2e` has
its own layer row (rank 2, over the library).

Every step is bounded (`TOOLS_E2E_STEP_BOUND`, default 240 s). Past the bound, the step's process group is killed
and the step fails. A scenario never runs inside another scenario's world (`TOOLS_E2E_INSIDE`). Otherwise the
pre-push scenario's own run-script-tests would push a clone of a clone.

**Running it.** run-script-tests finds the scenarios by name (`scripts/e2e/*_test.py`), so they run in CI's
`script-tests` job. In the pre-push hook's `scripttests` check, a scenario runs when one of the tools it covers
changes, through the derived reads and `pre_push_e2e_test.py`'s `# script-test-reads:` line. The whole layer takes
about 45 s on a laptop.

**The mutation proof** is `scripts/e2e/mutations.py [SCENARIO...]`. Each entry is one exact text replacement in one
tool file, and it must match exactly once or the entry reports itself stale. The scenario runs against a copy of the
tool trees with that replacement applied, and it must fail. The proof is slow (one scenario run per entry), so run
it when a scenario or a mutated file changes, not on every push.

## The ranking

Tools are ranked by what breaks, and for whom, when the tool is wrong. The landing path comes first because every
agent goes through it on every push. Tools that reach prod come next.

| # | Tool | Why it matters | e2e |
|---|---|---|---|
| 1 | `scripts/hooks/pre-push` | Every push to main goes through it. A false pass lands breakage on everyone; a false block stops every agent. Being migrated to Python, so it is driven only as a black box, by `git push`. | `pre_push_e2e_test.py` |
| 2 | `scripts/run-script-tests` | The hook's `scripttests` check and CI's `script-tests` job. A failure it misses, or a test it fails to select, lands untested. | the same, through the hook |
| 3 | `scripts/install-hooks.sh` | Without it, no hook runs at all and every push lands unchecked. | the same, first step |
| 4 | `scripts/pure-move-check` | A false PURE MOVE skips the identity what-if and carries the identity stamps, so changed code ships without its re-resolve. | the same, the `puremove` check |
| 5 | `infra/bin/deploy` | Switches prod NixOS hosts. A stale checkout rolls a host back (2026-10-23), and a forced-command reply read as the host's state is wrong data. | `deploy_e2e_test.py` (preview only) |
| 6 | `scripts/fail-before` | The commit gate's proof. A false "yes" admits a test that proves nothing. | `fail_before_e2e_test.py` |
| 7 | `scripts/ci-wait` | Every wait on CI. A misread verdict lands on red or waits forever; a cancelled run must be followed to the run that superseded it. | `ci_wait_e2e_test.py` |
| 8 | `infra/bin/rolling-reboot` | Reboots prod hosts. The web tier must not go down (`--accept-outage` for k3s-worker-1). | not here: the guards are fleetkit's, proved in fleetkit with `PROFILE=movies`, and the wrapper needs `npm ci` in `infra/version-dashboard` |
| 9 | `scripts/identity-stamp-carry` | A wrong carry skips re-resolving stored families on boot. | `identity_stamp_carry_e2e_test.py` (`propose` and `verify`; the stamps from a stub sbt) |
| 10 | `scripts/sbt-warm` | Every sbt call. A wedged server stalls every land. | `sbt_warm_e2e_test.py` (lifecycle, slot, stale-TASTy retry; stall detection stays in the black-box tests) |
| 11 | `scripts/land-precheck` | The cheap rejects before `agentctl land` queues a change. | `land_precheck_e2e_test.py` |
| 12 | `identity-what-if`, `identity-measure`, `identity-slice` | Identity verdicts that a landing relies on. | `identity_tools_e2e_test.py` (`identity-slice --explain`, `identity-what-if` over a pure move); the mirror and CI-dispatch paths stay in their black-box tests |
| 13 | `infra/bin/fleet-ssh`, `infra/bin/push-k8s-secrets` | Reach prod hosts and the cluster's Secrets. | `fleet_ssh_e2e_test.py` (`--ssh-config`), `push_k8s_secrets_e2e_test.py` |
| 14 | `scripts/mobile-ship.sh`, `scripts/source-refresh`, `scripts/local-mirror/*` | Releases, the source refresh and the mirror. | `mobile-ship.sh`: not here (below); the rest later |

## The scenarios and their mutations

| Scenario | The real flow | Mutation that must fail it |
|---|---|---|
| `pre_push_e2e_test` | `install-hooks.sh`, then a real `git push`. A new tool whose self-test fails is REFUSED (`FAIL … probe_adder_test.py`, `scripttests FAILED`, `blocked by: scripttests`) and origin's main does not move. Once the test is fixed, the same push lands. A `Pure-Move: verified` commit that also edits a line is REFUSED by `puremove` (`NOT PURE`); the real move lands. | run-script-tests exits 0 over a failure; pure-move-check ignores non-move hunks; install-hooks points git at an empty directory |
| `fail_before_e2e_test` | A fix and its test: `FAIL-BEFORE: yes`, `PASS-AFTER: yes`, and the caller's tree and worktree list are untouched. A test that already passes at the parent: `FAIL-BEFORE: no`, exit 1. | the default test-side globs widened to `*` |
| `deploy_e2e_test` | At origin main: the preview (running, candidate, start/stop/restart) and only the PROBE is sent to the host. Behind origin main: REFUSED before any ssh or nix call. A reply with no end marker: COULD NOT MEASURE. | staleness ignored; end marker not required |
| `ci_wait_e2e_test` | A run that goes queued, then running, then green exits 0. A red run exits 1. A cancelled run is followed, through the clone's real git ancestry, to its descendant's green run. | ancestry asked the wrong way round; failure read as success |
| `push_k8s_secrets_e2e_test` | A map, a stub vault (`secrets`) and a stub k3s server behind `ssh`. `--dry-run` says what would change and patches nothing; a real run patches only the keys whose sha256 differs (a key the vault lacks is skipped), through ssh's stdin; no vault value is in any argv; a rerun patches nothing. | the equal-sha check never equal; `--dry-run` patching; the patch given to kubectl as an argument |
| `fleet_ssh_e2e_test` | `fleet-ssh --ssh-config` over a stub flake (`nix`) and a temp `~/.ssh/config`: the block is appended once, the person's own text and mode are kept, a rerun changes nothing, a moved host is replaced in the same block, an empty address skipped. | the block appended on every run; the person's config overwritten |
| `land_precheck_e2e_test` | `land-precheck` in a worktree of a clone, the real hook run as a black box. A conflict with a moved main fails `rebase` naming the file and skips the lints; a clean change passes all three steps and touches no ref; a failing lint spec fails `lints`, sbt never reaching a server is UNCHECKED (exit 2); a row added to an allow-list fails `rules`. | a conflict read as clean; a rules refusal read as pass; an sbt that never ran read as a failure |
| `sbt_warm_e2e_test` | `sbt-warm` over a stub `sbt --client` (`sbt_stub.py`): one server and a keeper holding the JVM slot across calls, a failing command's exit, the stale-TASTy retry, `--stop` freeing server, keeper and slot. | `--stop` leaving the server running; no stale-TASTy retry |
| `identity_stamp_carry_e2e_test` | `identity-stamp-carry` in a worktree of a clone. The real `stamps` lays each commit out whole and runs the real sbt command line over the stub sbt, whose digest of a stamp is a hash of the paths and text under `identity/src/main/scala/e2e/<stamp>/`. A pure move of a rules file: `propose` writes the one row (the build's digest of the head, the base's as `equals`, proof pure-move) and the range, says the other two stamps are unchanged; committed, `verify` proves it again. A row with its digest or its `equals` edited, a rebase over identity code under the carry, and a what-if row over a hand-written verdict (no published asset) are each REFUSED by `verify` (exit 1). A move that also edits a line: `propose` exits 1 and writes nothing. | the pure-move proof read as always holding; the row's digest, or its `equals`, never compared; the cited range taken as the pushed one; the published asset not asked for |
| `identity_tools_e2e_test` | `identity-slice de --explain` over a two-listing capture and a stub `java` that decides each listing by the ref's own rules file: the selector reaches the JVM (the other listing never shown), the JVM runs in this checkout's tree or in each `--ref`'s laid-out tree, two refs print side by side then the diff naming the TAKE that moved, one ref twice is identical. `identity-what-if` over a pure move prints `PURE MOVE: what-if skipped`, starts no JVM, reaches no mirror, and records a `pure-move` verdict for each country under the move's head and base. | the selector never passed on; different traces called identical; the verdict recorded under the wrong head; only the first country recorded |

The pre-push scenario runs only the check under test (`PREPUSH_ONLY`, the landing check's own split), with the
floor bash and the `--linux` container skipped. Those depend on the machine, and the hook's own tests cover them.

Round 3 added the two rows above. `identity-slice` and `identity-what-if` are driven only where no mirror, recording
download or real JVM is reached: a non-pure `identity-what-if` goes on to the local prod mirror (and the vault), so it stays
in its black-box tests. `mobile-ship.sh` is left out: its flow starts by inspecting the App Store and Google Play through
their signed HTTP APIs (credentials from the vault), and a stub of both stores' APIs would be most of a second tool. It has no
working-tree check to prove (it builds from a detached worktree at main's sha and pushes only the version bump it cut);
that logic is held by the dashboard's own tests (`infra/version-dashboard/test`).
