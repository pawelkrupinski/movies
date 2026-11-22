"""convergence_fill.py's `gaps` (the newest refetch list, less what an origin refused lately) and `fold` (a pair's fills
folded into one past CONVERGENCE_FILL_FOLD_AT), over the fill tool's own release reads and writes."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from typing import List, Mapping, Optional, Protocol, Set

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "scripts/lib"))
import convergence_release  # noqa: E402
import tool_process  # noqa: E402


class Release(Protocol):
    """What these steps need of convergence_fill.py's Fill."""
    env: Mapping[str, str]
    runner: tool_process.Runner

    def tag(self) -> str: ...

    def say(self, line: str) -> None: ...

    def named(self, prefix: str, code: str, corpus: str, ext: str) -> List[str]: ...

    def assets(self) -> Optional[List[str]]: ...

    def download(self, *args: str, quiet: bool = False, attempts: Optional[str] = None) -> bool: ...

    def unpack(self, stage: str, *fills: str) -> int: ...

    def pack(self, directory: str, archive: str) -> int: ...

    def quietly(self) -> "Release": ...


def last_fold(names: List[str]) -> Optional[int]:
    """Where the newest fold is among the pair's fills, None when it has none."""
    folds = [i for i, n in enumerate(names) if n.endswith("-folded.tar.zst")]
    return folds[-1] if folds else None


def from_fold(names: List[str]) -> List[str]:
    """The fills a leg lays: from the newest fold on (every fill when there is none)."""
    return [n for n in names[last_fold(names) or 0:] if n]


def tabbed(path: str) -> int:
    """`grep -c <tab>`: the lines holding a tab."""
    with open(path) as f:
        return sum(1 for line in f if "\t" in line)


def refused_keys(path: str) -> Set[str]:
    with open(path) as f:
        return {line.split("\t")[0] for line in f.read().splitlines() if "\t" in line}


def skip_refused(fill: Release, out: str, refused: List[str], kept: int) -> None:
    """A gap an origin refused is asked again only once `kept` later fills published lists without it -- never every
    run through paid egress."""
    skip = f"{out}.refused"
    open(skip, "w").close()
    for listed in refused:
        if fill.download("release", "download", fill.tag(), "--pattern", listed, "--output", f"{skip}.part", "--clobber",
                         quiet=True):
            with open(f"{skip}.part") as part, open(skip, "a") as into:
                into.write(part.read())
    if os.path.getsize(skip) == 0:
        return
    before = tabbed(out)
    keys = refused_keys(skip)
    with open(out) as f:
        kept_lines = [line for line in f if line.split("\t")[0].rstrip("\n") not in keys]
    with open(out, "w") as f:
        f.writelines(kept_lines)
    sys.stderr.write(f"::notice::skipping {before - tabbed(out)} gap(s) an origin refused in the last {kept} fill(s)\n")


def gaps(fill: Release, code: str, corpus: str, out: str) -> int:
    lists = fill.named("refetch", code, corpus, "tsv")
    newest = lists[-1] if lists else ""
    kept = int(fill.env.get("CONVERGENCE_FILL_REFUSED_KEPT") or 4)
    refused = fill.named("refused", code, corpus, "tsv")[-kept:] if kept else []
    open(out, "w").close()
    # a list that cannot be read is no gaps this time -- the next leg's fill reads it again
    if newest and fill.download("release", "download", fill.tag(), "--pattern", newest, "--output", out, "--clobber",
                                quiet=True):
        sys.stderr.write(f"::notice::{newest} lists {tabbed(out)} fetchable gap(s)\n")
        skip_refused(fill, out, refused, kept)
    fill.say(str(tabbed(out)))
    return 0


def fold_name(code: str, corpus: str, newest: str) -> str:
    """Under the newest fill's run id and attempt, so the fold sorts after every fill it holds."""
    run_id = re.sub(rf"^fill-{code}-{corpus}-([0-9]+).*", r"\1", newest)
    attempt = re.match(rf"^fill-{code}-{corpus}-[0-9]+-([0-9]+)([-.].*)?$", newest)
    return f"fill-{code}-{corpus}-{run_id}-{attempt.group(1) if attempt else 0}-folded.tar.zst"


def publish_fold(fill: Release, work: str, laid: List[str], fold: str, code: str, corpus: str) -> bool:
    quiet = fill.quietly()  # the unpack's and the pack's own lines set aside (`> /dev/null`)
    if quiet.unpack(os.path.join(work, "tree"), *laid) != 0:
        fill.say(f"::warning::could not restore pair {code}-{corpus}'s fills to fold them")
        return False
    if quiet.pack(os.path.join(work, "tree"), os.path.join(work, fold)) != 0:
        return False
    # A name is never replaced: an upload refused because a concurrent fold published this very name is that fold done
    upload = fill.runner.capture(tool_process.Command(
        [convergence_release.GH_RELEASE, "upload", fill.tag(), os.path.join(work, fold)], fill.env))
    if upload.code != 0 and fold not in (fill.assets() or []):
        fill.say(f"::warning::could not publish {fold}")
        return False
    return True


def fold(fill: Release, code: str, corpus: str) -> int:
    at = int(fill.env.get("CONVERGENCE_FILL_FOLD_AT") or 12)
    names = fill.named("fill", code, corpus, r"tar\.zst")
    laid = from_fold(names)
    if len(laid) <= at:
        fill.say(f"pair {code}-{corpus}: {len(laid)} fill(s) since its last fold, at most {at} -- not folded")
        return 0
    name = fold_name(code, corpus, laid[-1])
    work = tempfile.mkdtemp()
    try:
        if not publish_fold(fill, work, laid, name, code, corpus):
            return 1
    finally:
        shutil.rmtree(work, ignore_errors=True)
    fill.say(f"folded {len(laid)} fill(s) of pair {code}-{corpus} into {name}")
    # Everything before the PREVIOUS fold: that fold holds it, and stays for the legs that listed before this one.
    previous = last_fold(names)
    if previous is None:
        return 0
    for stale in names[:previous]:
        fill.runner.capture(tool_process.Command([convergence_release.GH_RELEASE, "delete-asset", fill.tag(), stale, "-y"],
                                                 fill.env))
    fill.say(f"deleted {previous} fill(s) the fold before it holds")
    return 0
