"""The shell scripts `bin/deploy` runs on the host, each over one ssh session (`remote` appends nothing: every reply
ends with the `@@ END` marker). Text only; deploy_parse.py reads what they print."""

from __future__ import annotations

PROBE = r"""
set -u
cand="${1:-}"
SUDO=""; [ "$(id -u)" -eq 0 ] || SUDO="sudo -n"
echo "user=$(id -un)"
echo "hostname=$(hostname)"
echo "running=$(readlink -f /run/current-system)"
echo "booted=$(readlink -f /run/booted-system)"
echo "pinned=$(readlink -f /var/lib/nixdeploy/staged-system 2>/dev/null || true)"
rev() { [ -x "$1/sw/bin/nixos-version" ] && "$1/sw/bin/nixos-version" --json 2>/dev/null \
  | sed -n 's/.*"configurationRevision":"\([^"]*\)".*/\1/p'; }
echo "running_rev=$(rev /run/current-system)"
pin=$(readlink -f /var/lib/nixdeploy/staged-system 2>/dev/null || true)
[ -n "$pin" ] && echo "pinned_rev=$(rev "$pin")"
echo "failed=$(systemctl list-units --state=failed --no-legend --plain --no-pager 2>/dev/null | awk '{print $1}' | tr '\n' ' ')"
if [ -n "$cand" ] && [ -x "$cand/bin/switch-to-configuration" ]; then
  echo "candidate_present=1"
  echo "candidate_rev=$(rev "$cand")"
  for c in kernel initrd kernel-modules systemd; do
    a=$(readlink -f "/run/booted-system/$c" 2>/dev/null); b=$(readlink -f "$cand/$c" 2>/dev/null)
    [ "$a" = "$b" ] && echo "boot_$c=same" || echo "boot_$c=differs"
  done
  if [ "$(readlink -f /run/current-system)" != "$cand" ]; then
    # Top-level unit files whose target differs: what dry-activate does NOT mention because the unit
    # is idle (it picks the new definition up at its next start).
    new=$cand/etc/systemd/system; old=/run/current-system/etc/systemd/system
    for f in $( (ls "$new"; ls "$old") 2>/dev/null | sort -u); do
      [ "$(readlink "$new/$f")" = "$(readlink "$old/$f")" ] || echo "unitfile $f"
    done
    out=$($SUDO "$cand/bin/switch-to-configuration" dry-activate 2>&1); rc=$?
    echo "@@ DRY rc=$rc"
    printf '%s\n' "$out"
    echo "@@ DRYEND"
    for u in $(printf '%s\n' "$out" | sed -n 's/^would [a-zA-Z ]*units: //p' | tr ',' ' '); do
      echo "state $u $(systemctl show -p ActiveState --value "$u" 2>/dev/null)"
    done
  fi
else
  echo "candidate_present=0"
fi
echo "@@ END"
"""

# The switch, as its own transient unit's script: $1 the candidate, $2 the file its exit status goes
# to. PROFILE BEFORE SWITCH, the order nixos-auto-apply and nixos-rebuild use: the bootloader menu is
# built from the profile, so the other order boots the old generation at the next reboot. The unit
# gets systemd's PATH, not the login shell's /run/current-system/sw/bin, so nix-env is named by the
# candidate's own path: a bare `nix-env` exited 127 and switched nothing (mongo-1, 2026-10-24).
# THE APPLIER MAY HOLD THE LOCK: the pin LAUNCH writes wakes nixos-auto-apply, which takes
# switch-to-configuration's lock while it judges the pin and lets go a second or so later. The switch
# then exits 11, "Could not acquire lock" (mongo-1, 2026-10-24): wait that out, for a minute at most.
# The status file is written last (renamed into place whole): its presence means the switch ENDED.
# EVERY COMMAND BUT THE SHELL'S BUILTINS IS NAMED BY THE CANDIDATE'S PATH, sleep and mv too: that PATH
# holds no coreutils, so a bare `mv` failed, no status file appeared, and a clean switch read as
# "ended without an exit status (killed?)" (monitoring-1, 2026-11-10).
SWITCH_UNIT = r"""set -u
cand="$1"; status="$2"
tries=0
while :; do
  "$cand/sw/bin/nix-env" --profile /nix/var/nix/profiles/system --set "$cand" \
    && "$cand/bin/switch-to-configuration" switch
  rc=$?
  [ "$rc" -eq 11 ] && [ "$tries" -lt 20 ] || break
  tries=$((tries + 1))
  echo "· switch lock held (another activation); retrying in 3 s"
  "$cand/sw/bin/sleep" 3
done
echo "$rc" > "$status.tmp" && "$cand/sw/bin/mv" -f "$status.tmp" "$status"
exit "$rc"
"""

# Starts the switch DETACHED and returns at once: $1 the candidate, $2 the transient unit's name.
# Nothing of the switch rides on this ssh session -- no --wait, no --pipe -- because the switch may
# restart sshd, systemd-networkd and the rest under it: on monitoring-1 (2026-11-10) the session
# streaming a `--wait --pipe` switch died mid "starting the following units: ...", the tool hung ~22
# minutes and exited 2, while the host had switched fully. follow_switch reads the outcome afterwards.
LAUNCH = r"""
set -u
# BIN_DEPLOY_DIR is the tests' override (ssh forwards no environment).
cand="$1"; unit="$2"; dir="${BIN_DEPLOY_DIR:-/run/bin-deploy}"
SUDO=""; [ "$(id -u)" -eq 0 ] || SUDO="sudo -n"
[ -x "$cand/bin/switch-to-configuration" ] || { echo "!! $cand is not on this host"; echo "@@ END"; exit 5; }
# THE PIN FIRST: nixos-auto-apply activates the pin whenever it is not running and is inert, so an
# OLDER pin left behind would switch this host back. Writing it also wakes the applier, which may
# hold the switch lock for a moment (SWITCH_UNIT's retry) and then finds the closure already running.
if [ "$(readlink -f /var/lib/nixdeploy/staged-system 2>/dev/null)" != "$cand" ] && [ -d /var/lib/nixdeploy ]; then
  $SUDO ln -sfn "$cand" /var/lib/nixdeploy/staged-system && echo "· pinned $cand"
fi
$SUDO mkdir -p "$dir"
$SUDO tee "$dir/$unit.sh" >/dev/null <<'SWITCH_UNIT_EOF'
""" + SWITCH_UNIT + r"""SWITCH_UNIT_EOF
$SUDO systemd-run --quiet --collect --unit "$unit" --description "bin/deploy switch to $cand" \
  -- /bin/sh "$dir/$unit.sh" "$cand" "$dir/$unit.status"
echo "@@ LAUNCHED rc=$?"
echo "@@ END"
"""

# Where the detached switch is: $1 the transient unit. Its status file once it ended, the unit's
# state while it runs, the running closure, and the unit's whole journal (follow_switch prints the
# lines it has not printed yet).
SWITCH_STATUS = r"""
set -u
unit="$1"; dir="${BIN_DEPLOY_DIR:-/run/bin-deploy}"
SUDO=""; [ "$(id -u)" -eq 0 ] || SUDO="sudo -n"
echo "state=$(systemctl show -p ActiveState --value "$unit" 2>/dev/null)"
[ -f "$dir/$unit.status" ] && echo "status=$(cat "$dir/$unit.status")"
echo "now=$(readlink -f /run/current-system)"
echo "@@ JOURNAL"
$SUDO journalctl -q -u "$unit" -o cat --no-pager 2>/dev/null
echo "@@ END"
"""

VERIFY_STATES = r"""
set -u
for u in "$@"; do
  # One property per call, in the order unit_ok reads them: with several, `--value` prints them in
  # systemd's own order (ActiveState, Type, Result), and a cleanly exited oneshot read as unhealthy.
  echo "unit $u $(systemctl show -p ActiveState --value "$u" 2>/dev/null)" \
    "$(systemctl show -p Result --value "$u" 2>/dev/null) $(systemctl show -p Type --value "$u" 2>/dev/null)"
done
echo "@@ END"
"""
