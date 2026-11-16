# Deploying a NixOS host (monitoring-1, k3s-worker-1, mongo-1)

## `infra/bin/deploy <host>` -- the one command

`infra/bin/deploy monitoring-1` previews and changes nothing. `--apply` switches, then verifies.
Same tool, same file, as bitcashier's `bin/deploy`; `infra/test/test_deploy.py` covers its pure logic
and runs in `bin/check` and nix-stage-closures.yml.

1. **Refuses a checkout behind origin main** (it fetches first). Ahead or dirty is allowed and said;
   `--allow-stale` is the deliberate-rollback override.
2. **Picks the closure**: this checkout's toplevel (`nix eval`, the path CI stages for the same
   commit; the repo is shallow, so the flake ref is `git+file://...?dir=infra&shallow=1`); else CI's
   pin when it was built from a commit with an identical `infra/` tree (staging only runs for commits
   touching `infra/**`, yet every commit changes `self.rev` and so the closure); else `--build`
   copies the derivation to the host and builds it THERE. Without either it refuses: wait for
   `nix-stage-closures` (check it is green: run 1587 on 2026-10-23 failed, so main was not staged).
3. **ssh as the alias named like the host** (`monitoring-1`, `mongo-1`, `k3s-worker-1`, root) when
   `ssh -G` shows one. monitoring-1's bare IP binds the dashboard's restricted forced-command key, so
   `ssh <ip>` is not a shell; an alias pointing at an address other than `fleet.publicAddress` is
   refused, and every reply must end with a marker.
4. **Preview**: `switch-to-configuration dry-activate` sorted into units started / stopped /
   restarted / reloaded / changed-but-not-restarted, plus boot-chain components (kernel, initrd,
   kernel-modules, systemd) that differ from `/run/booted-system` -- a reboot is owed. (No host here runs
   CI runners: ci-runner is bitcashier infra's, and its runner-stop guard lives in that repository's
   `bin/deploy` and nixos-auto-apply.)
5. **`--apply`**: re-points `/var/lib/nixdeploy/staged-system` at the candidate first (otherwise
   nixos-auto-apply could switch back to an older pin), then `nix-env --profile
   /nix/var/nix/profiles/system --set` + `switch-to-configuration switch` -- the order the applier
   and the fleet dashboard's switch.sh use -- as a DETACHED transient unit
   (`systemd-run --collect --unit deploy-switch-<UTC stamp>`, no `--wait`/`--pipe`), and returns at
   once. It then polls the unit over fresh ssh sessions (every 5 s, printing its new journal lines and
   state; a host not answering is said and retried for 15 min, the switch bounded at 1 h) until the
   unit's status file, `/run/bin-deploy/<unit>.status`, holds switch-to-configuration's exit status.
   Non-zero (or a unit gone without one) prints the unit's journal tail and fails the deploy. Every
   ssh sets `ServerAliveInterval=15`/`ServerAliveCountMax=4`, so a dead session shows in ~1 min.
   Why (2026-11-10 ~07:00Z, monitoring-1): the switch used to stream through `--wait --pipe`; it
   restarted sshd and systemd-networkd, the session died silently mid "starting the following units:
   ...", ssh (no keepalives) waited ~22 min for TCP to give up, and with no end marker the tool exited
   2 -- while the host had switched fully. By hand: `journalctl -u deploy-switch-<stamp>` on the host.
6. **Verify**: `/run/current-system` is the candidate, its `configurationRevision` is the expected
   commit, no unit newly failed, every started/restarted unit is active (or a oneshot that
   succeeded). Exit 0 ok, 1 refused or verify failed, 2 could not measure.

## nixos-auto-apply forgives NEW and ROUTINE units (2026-10-23)

monitoring-1 refused a closure that only added units (`units_would_change`), and the switch was
then run by hand. Two rules now sit beside `restartableUnits` (empty on mongo-1 and k3s-worker-1):

- `fleet.autoApply.startNewUnits` (default on): units the running system does not have at all, and
  their `.wants` links, are started unattended. Never mounts or swaps.
- `fleet.autoApply.routineUnits` (default `*.timer`, `*-exporter.service`,
  `nixos-auto-apply.service`, `nixos-auto-apply.path`): their restarts and changed-but-skipped
  state are forgiven, so a `fleet.autoApply.*` change rolls itself out. A routine timer's `.service`
  is forgiven only while idle (mongodump mid-run is still refused).

Both are fenced by `neverDisturbUnits` and the applier's built-in `STATEFUL_UNITS` floor (mongodb,
k3s, sshd, prometheus, alertmanager, mounts...), which no option widens. Cases in
`infra/test/test_auto_apply_classify.py` (`Leniency`). The rollout changes the applier's own unit,
which the old applier refuses: each host needs one `infra/bin/deploy <host> --apply` to pick it up.

## A reboot already owed blocks nothing; a family is described once (2026-11-10)

Two faults kept routine switches with the owner after the nixpkgs c56a87d bump (kernel 6.18.60):

- **`classifier_disagreement` after a hand-switched kernel.** k3s-worker-1 and mongo-1 ran the new
  kernel's closure (pid 1 re-executed, kernel waiting for a reboot); the next files-only closure
  was refused because the boot chain, compared against `/run/booted-system`, still differed and was
  counted as "not inert" -- while dry-activate, which cannot see a kernel and saw pid 1 already on
  the candidate's systemd, said only "reload dbus-broker" (forgiven by `reloadableUnits`).
  `boot_chain` now splits the debt: what the switch INTRODUCES (candidate differs from
  `/run/current-system` too) is `reboot_owed`; what is already owed is reported
  (`nixos_auto_apply_reboot_owed`, `_reboot_component`) and blocks nothing. The cross-check
  (`refusal`) compares only what both halves see: unit changes, and a replaced systemd
  ("would restart systemd"). A kernel-only bump is now `reboot_owed`, never a disagreement.
- **monitoring-1 "not covered".** A `reboot_owed` verdict for four components wrote `# HELP
  nixos_auto_apply_reboot_component` four times; node_exporter refuses a file with a second HELP
  for a family, so EVERY auto-apply series vanished. `Metrics.gauge` describes a family once.

Cases: `RebootAlreadyOwed` and `PublishedMetrics` in `infra/test/test_auto_apply_classify.py`.

## A switch that stops midway is `switch_failed`; ssh accounts linger (2026-11-10)

monitoring-1, 08:29Z: the operator Mac's dashboard polls Prometheus as root (the `dashboards-readonly`
forced-command key) over a fresh ssh session every 15 s, so logind starts `user@0` and removes it ~10 s
after each. switch-to-configuration-ng lists logind's users, then reads each GID with `?`; root vanished
between the two calls (`Failed to get GID for root ... Unknown object '/org/freedesktop/login1/user/_0'`),
the switch exited 1 after `/run/current-system` had moved, and `local-fs.target`/`remote-fs.target`
stayed stopped (owed in `/run/nixos/start-list`).

- **Every account with an ssh key lingers** (`fleet/accounts.nix`, keyed on authorized keys, so a new
  role's account does too): a lingering user never leaves logind, so nothing listed can vanish.
- **The applier** retries a switch that failed on exactly that race once, when `/run/current-system`
  and the profile both name the candidate. Any switch exiting non-zero after "activating the
  configuration..." is `blocked (switch_failed)` with its output tail, never `could_not_measure`; a
  pass that finds `/run/nixos/{start,restart,reload}-list` left behind (and the switch lock free)
  reports `switch_failed` naming what is owed, before classifying. `AutoApplySwitchFailed` (critical,
  5 min) says so; finish it with `infra/bin/deploy <host> --apply`.

Cases: `SwitchFailedMidway` in `infra/test/test_auto_apply_classify.py`.

## A forgiven oneshot that runs for hours is never restarted by a switch (2026-11-15)

`roles/scheduled-dispatch.nix`'s `scheduled-dispatch-*` oneshots wait 3-14 h for GitHub's late run, and
were in `restartableUnits` with the default `restartIfChanged`. switch-to-configuration stops a changed
running unit, starts it again and waits on a oneshot's start job -- its whole run -- so any closure
touching them held the switch past the applier's 3600 s bound (`switch_failed`). They now carry
`restartIfChanged = false` (a running guard finishes on its old definition) and are `routineUnits`,
the one rule that forgives their changed-but-skipped state; `fleet-container-image-gc` (1 h) got the
same flag.

`auto-apply.nix` asserts it fleet-wide: a oneshot forgiven by `restartableUnits`/`routineUnits` whose
declared `TimeoutStartSec` exceeds 30 min (or is unbounded or unreadable) must set
`restartIfChanged = false`, or the host does not evaluate. `routineUnits`' four built-ins are now a
definition rather than the option default, so a module adding its own entries merges with them
instead of dropping them (`lib.mkForce` replaces the list). Case: `Leniency` in
`infra/test/test_auto_apply_classify.py`.
