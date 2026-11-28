"""THE PULL REQUEST'S BODY (infra/bin/nix-update): what the weekly upgrade found -- the pinned packages, the flake inputs
that moved, the container image pins, the hosts that do not build, the package version changes -- as Markdown."""
import dataclasses
import datetime
import re
from typing import Any, Dict, List, Optional

import image_pins  # beside this script: the container image pins no other bumper owns

# GitHub rejects a pull request body past 65,536 characters; a nixpkgs bump's diff-closures can be longer.
BODY_LIMIT = 60000


@dataclasses.dataclass
class PinnedUpgrades:
    """The pinned packages' answer: what is bumped, what a newer major holds back, and what to say about both."""
    bumped: List[Any] = dataclasses.field(default_factory=list)
    held: List[Any] = dataclasses.field(default_factory=list)
    failures: List[str] = dataclasses.field(default_factory=list)
    notes: List[str] = dataclasses.field(default_factory=list)
    failed: bool = False


@dataclasses.dataclass
class Upgrade:
    """Everything the pull request's body reports."""
    pinned: PinnedUpgrades
    old_in: Dict[str, Dict[str, Any]]
    new_in: Dict[str, Dict[str, Any]]
    moved: List[str]
    image_rows: List[Dict[str, Any]]
    image_failures: List[str]
    names: List[str]
    changes: Dict[str, List[str]]
    failed: Dict[str, str]
    caught_up: str

    @property
    def images_bumped(self) -> List[Dict[str, Any]]:
        return [r for r in self.image_rows if r["action"] == "bumped" and not r["gitops"]]


def where_changed(hosts_changed: List[str], names: List[str]) -> str:
    if len(hosts_changed) == len(names):
        return f"all {len(names)} hosts"
    return f"{len(hosts_changed)}: {', '.join(sorted(hosts_changed))}"


def pinned_section(u: Upgrade) -> str:
    p = u.pinned
    if p.bumped:
        rows = [f"| `{f.path}` | {f.current} | **{f.take}** | {f.checked} |" for f in p.bumped]
        why = "".join(f"\n<details><summary>Why <code>{f.name}</code> is pinned (the file's comment)</summary>\n\n"
                      f"> {f.header}\n</details>\n" for f in p.bumped)
        table = ("| file | from | to | hash |\n|---|---|---|---|\n" + "\n".join(rows) + "\n" + why
                 + "\nA mongod bump restarts mongod on mongo-1, which auto-apply never does: deploy it by hand "
                   "(`colmena apply --on @database`).\n")
    else:
        table = "None.\n"
    held = "".join(f"- **{f.name} {f.later}** is a newer major than {f.current} -- not proposed.\n" for f in p.held)
    return (f"## Pinned packages ({len(p.bumped)})\nNewest release within each package's current major; a newer major is only listed.\n\n"
            + table + held + "".join(f"- {line}\n" for line in p.notes + p.failures) + u.caught_up)


def input_rows(u: Upgrade) -> List[str]:
    rows = []
    for n in u.moved:
        o, w = u.old_in.get(n, {}), u.new_in[n]
        link = compare_link(o, w)
        rows.append(f"| `{n}` | {o.get('rev', 'new')[:12]} ({date(o)}) | {w.get('rev', '?')[:12]} ({date(w)}) | "
                    + (f"[compare]({link})" if link else "") + " |")
    return rows


def flake_section(u: Upgrade) -> str:
    table = "| input | from | to | |\n|---|---|---|---|\n" + "\n".join(input_rows(u)) + "\n" if u.moved else "No input moved.\n"
    return f"## flake.lock ({len(u.moved)})\n" + table


def failures_section(u: Upgrade) -> str:
    failures = "".join(f"\n<details><summary><b>{n}</b></summary>\n\n```\n{e}\n```\n</details>\n"
                       for n, e in sorted(u.failed.items()))
    none = "None: every host evaluated and built." if u.names else "Not built: nothing Nix moved."
    return f"## Hosts that do not build ({len(u.failed)})\n{failures or none}\n"


def package_rows(u: Upgrade) -> List[str]:
    lines = sorted(u.changes.items(), key=lambda kv: (-len(kv[1]), kv[0]))
    return [f"- `{change}` ({where_changed(hs, u.names)})" for change, hs in lines]


BODY_INTRODUCTION = """\
Weekly `infra/bin/nix-update` (`.github/workflows/nix-update.yml`): every Nix dependency upgrade, and every container
image pin no other bumper owns, in one pull request.

**Merging this deploys it.** `nix-stage-closures.yml` stages the new closures on every host, and
`fleet.autoApply` activates them wherever every restart is one the host declared acceptable
(`neverDisturbUnits`: sshd, mongod and k3s are left for a person). To leave something out, push a
commit to this branch reverting it -- the weekly run will not overwrite a branch with commits by
anyone but ci-bot.
"""


def pull_request_body(u: Upgrade) -> str:
    rows = package_rows(u)
    images = (f"## Container image pins ({len(u.images_bumped)} bumped)\n"
              "`infra/bin/image_pins.py`. This repository's pins are bumped in this pull request (a `Dockerfile` bump ships with\n"
              "the next app image Main builds); movies-gitops' are only listed: their tag bumps are that repository's own PR\n"
              f"(`{image_pins.GITOPS_BRANCH}`), and Flux's regeneration is by hand.\n\n"
              + image_pins.markdown(u.image_rows, u.image_failures))
    changes = (f"## Package version changes, union over {len(u.names)} hosts ({len(rows)})\n"
               "From `nix store diff-closures` of each host's closure before and after; size-only changes are left out.\n\n"
               + ("\n".join(rows) or "None.") + "\n")
    body = "\n".join([BODY_INTRODUCTION, pinned_section(u), flake_section(u), images, failures_section(u), changes])
    if len(body) > BODY_LIMIT:
        body = body[:BODY_LIMIT] + "\n\n... cut at the pull request size limit; the job log has the rest.\n"
    return body


def compare_link(old: Dict[str, Any], new: Dict[str, Any]) -> str:
    if new.get("type") == "github" and old.get("rev") != new.get("rev"):
        return f"https://github.com/{new['owner']}/{new['repo']}/compare/{old.get('rev', '')[:12]}...{new['rev'][:12]}"
    return ""


def date(locked: Dict[str, Any]) -> str:
    t = locked.get("lastModified")
    return datetime.datetime.fromtimestamp(t, datetime.timezone.utc).strftime("%Y-%m-%d") if t else "?"


# ---- reading `nix store diff-closures` ----------------------------------------------------------

def changed(line: str) -> Optional[str]:
    """The `old → new` part of a diff-closures line, None for a line that is not a version change."""
    matched = DIFF_LINE.match(line)
    return matched["change"] if matched else None


DIFF_LINE = re.compile(r"^(?P<pkg>[^:]+): (?P<change>.*→.*?)(?:, [+-][\d.]+ [KMG]?i?B)?$")
# diff-closures colours the size even when its output is not a terminal.
ANSI = re.compile(r"\x1b\[[0-9;]*m")
