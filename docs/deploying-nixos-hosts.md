# Deploying a NixOS host (monitoring-1, k3s-worker-1, mongo-1)

## `infra/bin/deploy <host>` -- the one command

`infra/bin/deploy monitoring-1` previews and changes nothing. `--apply` switches, then verifies.
Same tool, same file, as bitcashier's `bin/deploy`; `infra/test/test_deploy.py` covers its pure logic
and runs in `bin/check` and nix-stage-closures.yml.

1. **Refuses a checkout behind forgejo main** (it fetches first). Ahead or dirty is allowed and said;
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
   kernel-modules, systemd) that differ from `/run/booted-system` -- a reboot is owed.
5. **`--apply`**: re-points `/var/lib/nixdeploy/staged-system` at the candidate first (otherwise
   nixos-auto-apply could switch back to an older pin), then `nix-env --profile
   /nix/var/nix/profiles/system --set` + `switch-to-configuration switch` -- the order the applier
   and the fleet dashboard's switch.sh use -- under `systemd-run`, so an ssh drop cannot kill it.
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
