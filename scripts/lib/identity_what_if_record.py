"""THE WHAT-IF VERDICT RECORD, AND A CONVERGENCE RUN'S TAKES (scripts/lib; identity-tools.sh's what_if_* functions and
identity_takes_download).

THE WHAT-IF VERDICT RECORD (review 2026-11-06 P3): scripts/identity-what-if writes one line a country it finished --
`<cc> TAB <verdict> TAB <finished, UTC> TAB <takes diff> TAB <takes moved> TAB <mode> TAB <published asset>` -- under
(head, base); scripts/identity-measure refuses to dispatch a CI measure without one (its what-if gate). One file read: the
gate is a lookup, never a run. <takes moved> counts the model's and the final takes' diff rows; <mode> is `whole` (every
family re-resolved: --all, --verify) or `selected`, then `final` or `model-only` -- what scripts/identity-stamp-carry reads
to carry a stamp (only a whole, nothing-moved verdict of every country does, published as <published asset>:
scripts/identity_stamp_carry.py, THE PUBLISHED VERDICT).

  verdicts_file(cache, head, base)                      <cache>/what-if-verdicts/<head>-<base>.tsv
  record_verdict(files, cache, head, base, cc, verdict, takes_diff, moved, mode, asset)   one line appended
  verdicts(files, cache, head, base) -> `pl done, de done` (each country's newest, in the order first finished), or None
  TakesDownloads(ctx).download(run, cc, diag)           an "Identity model convergence" run's
        convergence-diagnostics-<cc>-convergence artifact's <cc>-takes, as <diag>/<cc>-takes (identity-slice
        --faithfulness RUN), downloaded beside it and moved in whole"""
from __future__ import annotations

import os
import shutil
import time
from typing import Dict, List, Optional

import gh_download
import machine
from identity_context import Context, Failed
from tool_files import Files


def verdicts_file(cache: str, head: str, base: str) -> str:
    return os.path.join(cache, "what-if-verdicts", f"{head}-{base}.tsv")


def record_verdict(files: Files, cache: str, head: str, base: str, cc: str, verdict: str, takes_diff: str,
                   moved: str = "", mode: str = "", asset: str = "") -> None:
    path = verdicts_file(cache, head, base)
    files.make_directories(os.path.dirname(path))
    finished = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(machine.wall()))
    line = "\t".join([cc, verdict, finished, takes_diff, moved or "-", mode or "-", asset or "-"])
    with open(path, "a", encoding="utf-8") as record:
        record.write(line + "\n")


def verdicts(files: Files, cache: str, head: str, base: str) -> Optional[str]:
    path = verdicts_file(cache, head, base)
    text = files.read_text_or(path, "")
    if not text:
        return None
    newest: Dict[str, str] = {}
    order: List[str] = []
    for line in text.splitlines():
        fields = line.split("\t")
        if fields[0] not in newest:
            order.append(fields[0])
        newest[fields[0]] = fields[1] if len(fields) > 1 else ""
    return ", ".join(f"{cc} {newest[cc]}" for cc in order)


class TakesDownloads:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx

    def download(self, run: str, cc: str, diag: str) -> None:
        if os.path.isfile(os.path.join(diag, f"{cc}-takes", f"{cc}.json.gz")):
            return
        self.ctx.files.make_directories(diag)
        with self.ctx.files.scratch(diag + ".dl.") as fetched:
            got = gh_download.download(self.ctx.env, self.ctx.gh_download, [
                "run", "download", run, "-R", self.ctx.repository, "--name", f"convergence-diagnostics-{cc}-convergence",
                "--dir", fetched]) == 0
            if not (got and os.path.isfile(os.path.join(fetched, f"{cc}-takes", f"{cc}.json.gz"))):
                self.ctx.log(f"could not download run {run}'s convergence-diagnostics-{cc}-convergence (gh failed or "
                             f"timed out, or it holds no {cc}-takes)")
                raise Failed(1)
            shutil.rmtree(os.path.join(diag, f"{cc}-takes"), ignore_errors=True)
            os.rename(os.path.join(fetched, f"{cc}-takes"), os.path.join(diag, f"{cc}-takes"))
