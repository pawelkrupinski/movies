"""What the host's scripts print, read back (infra/bin/deploy, deploy_scripts.py; unit-tested in
infra/test/test_deploy.py): the probe's facts, the dry-activate preview and its rendering, the detached switch's status."""

from __future__ import annotations

import re
from typing import Any, Dict, List, NamedTuple, Optional

from deploy_decide import boot_changes

Facts = Dict[str, Any]


def _probe_fact(facts: Facts, line: str) -> None:
    if line.startswith("unitfile "):
        facts.setdefault("unitfiles", []).append(line.split(" ", 1)[1])
    elif line.startswith("state "):
        parts = line.split()
        facts["states"][parts[1]] = parts[2] if len(parts) > 2 else ""
    elif "=" in line and not line.startswith("@@"):
        key, value = line.split("=", 1)
        facts[key] = value


def _probe_line(facts: Facts, dry: List[str], in_dry: bool, line: str) -> bool:
    """Read one line of the probe's reply; whether the next one is still inside the dry-activate text."""
    if line.startswith("@@ DRY rc="):
        facts["dry_rc"] = int(line.split("=", 1)[1])
        return True
    if line == "@@ DRYEND":
        return False
    if in_dry:
        dry.append(line)
        return True
    _probe_fact(facts, line)
    return False


def parse_probe(out: str) -> Facts:
    """key=value lines, plus `dry` (the dry-activate text), `dry_rc` and `states` {unit: state}."""
    facts: Facts = {"states": {}}
    in_dry, dry = False, []
    for line in out.splitlines():
        in_dry = _probe_line(facts, dry, in_dry, line)
    if "dry_rc" in facts:
        facts["dry"] = "\n".join(dry)
    return facts


# -------------------------------------------------------------------------------------------------
# The preview
# -------------------------------------------------------------------------------------------------

UNIT_LINE = re.compile(
    r"^would (NOT )?(start|stop|restart|reload) the following (changed )?(user )?units: (.+)$")
NOISE = (
    re.compile(r"^Not checking switch inhibitors"),
    re.compile(r"^would activate the configuration\.\.\.$"),
    re.compile(r"^sops-install-secrets: Imported "),
)


class Preview(NamedTuple):
    start: List[str]
    stop: List[str]
    restart: List[str]
    reload: List[str]
    skipped: List[str]      # changed but NOT restarted (restartIfChanged = false)
    user: List[str]         # "kind unit" for user-manager units
    other: List[str]        # every other line: secrets, systemd re-exec, swap, anything unrecognised


def _file_units(preview: Preview, negated: bool, kind: str, user: bool, names: List[str]) -> None:
    if user:
        preview.user.extend(f"{'skip' if negated else kind} {u}" for u in names)
    elif negated:
        preview.skipped.extend(names)
    else:
        getattr(preview, kind).extend(names)


def _classify(preview: Preview, line: str) -> None:
    match = UNIT_LINE.match(line)
    if not match:
        preview.other.append(line)
        return
    negated, kind, _, user, units = match.groups()
    _file_units(preview, bool(negated), kind, bool(user), [u.strip() for u in units.split(",") if u.strip()])


def parse_dry_activate(text: str) -> Preview:
    preview = Preview([], [], [], [], [], [], [])
    for raw in text.splitlines():
        line = raw.strip()
        if line and not any(n.match(line) for n in NOISE):
            _classify(preview, line)
    return preview


def watched_units(preview: Optional[Preview]) -> List[str]:
    """The units a switch started or restarted, whose health is verified afterwards."""
    return sorted(set(preview.start + preview.restart) if preview else set())


def _names(states: Dict[str, str], units: List[str]) -> str:
    return ", ".join(f"{u} ({states[u]})" if states.get(u) else u for u in units) or "-"


def _header(host: str, target: str, facts: Facts, candidate: str, expected_rev: str, checkout: str, via: str) -> List[str]:
    out = [f"== {host}  via ssh {target}  ({via})", f"   {checkout}",
           f"   running   {facts.get('running', '?')}  rev {facts.get('running_rev') or '?'}",
           f"   candidate {candidate}  rev {expected_rev or 'none (unstamped rsync build)'}"]
    pinned = facts.get("pinned")
    if pinned and pinned != candidate:
        out.append(f"   pinned    {pinned}  (CI's pin differs; --apply re-pins to the candidate)")
    return out


def _idle_unit_files(facts: Facts, preview: Preview) -> List[str]:
    acted = set(preview.start + preview.stop + preview.restart + preview.reload + preview.skipped)
    return [f for f in facts.get("unitfiles", []) if f not in acted and not f.endswith(
        (".wants", ".requires", ".upholds", ".d"))]


def _unit_lines(states: Dict[str, str], preview: Preview) -> List[str]:
    # switch-to-configuration restarts some units as stop + start; say so once, as a restart.
    both = [u for u in preview.stop if u in preview.start]
    return [
        f"   start     {_names(states, [u for u in preview.start if u not in both])}",
        f"   stop      {_names(states, [u for u in preview.stop if u not in both])}",
        f"   restart   {_names(states, preview.restart + [u + ' [stop+start]' for u in both])}",
        f"   reload    {_names(states, preview.reload)}",
        f"   changed, NOT restarted  {_names(states, preview.skipped)}",
    ]


def _actions(facts: Facts, preview: Preview) -> List[str]:
    out = _unit_lines(facts.get("states", {}), preview)
    idle = _idle_unit_files(facts, preview)
    if idle:
        out.append(f"   unit files changed, unit idle  {', '.join(idle)}  (used at its next start)")
    if preview.user:
        out.append(f"   user units {', '.join(preview.user)}")
    return out + [f"   note      {line}" for line in preview.other]


def _boot_lines(facts: Facts) -> List[str]:
    boot = boot_changes(facts)
    out = ["   boot chain " + (f"CHANGES {', '.join(boot)} -> a REBOOT is needed to use it "
                               "(bin/rolling-reboot where present)" if boot
                               else "unchanged (no reboot needed)")]
    if facts.get("failed", "").strip():
        out.append(f"   already failed before the switch: {facts['failed'].strip()}")
    return out


def render_preview(host: str, target: str, facts: Facts, preview: Optional[Preview],
                   candidate: str, expected_rev: str, checkout: str, via: str) -> str:
    out = _header(host, target, facts, candidate, expected_rev, checkout, via)
    if facts.get("running") == candidate:
        out.append("   NOTHING TO DO: the host already runs this closure.")
    elif preview is None:
        out.append("   (no dry-activate: the candidate is not on the host)")
    else:
        out += _actions(facts, preview) + _boot_lines(facts)
    return "\n".join(out)


def parse_switch_status(out: str) -> Facts:
    """SWITCH_STATUS's reply: `state`, `now`, `status` (an int, absent while the switch runs) and
    `journal` (its lines)."""
    facts: Facts = {"journal": []}
    in_journal = False
    for line in out.splitlines():
        if line == "@@ JOURNAL":
            in_journal = True
        elif line.startswith("@@"):
            in_journal = False
        elif in_journal:
            facts["journal"].append(line)
        elif "=" in line:
            key, value = line.split("=", 1)
            facts[key] = value
    if re.fullmatch(r"-?\d+", facts.get("status", "")):
        facts["status"] = int(facts["status"])
    else:
        facts.pop("status", None)
    return facts


def unit_fields(out: str) -> Dict[str, List[str]]:
    """VERIFY_STATES' reply: {unit: [ActiveState, Result, Type]}."""
    fields: Dict[str, List[str]] = {}
    for line in out.splitlines():
        if line.startswith("unit "):
            parts = line.split()
            fields[parts[1]] = parts[2:]
    return fields
