#!/usr/bin/env python3
"""Lists the lint-registry rows that miss a path their spec reads.

Each registered spec's source is scanned for string literals naming an existing file or directory of the repo (a spec
reading tools/devpanel/scripts/devpanel.py with a row that never selected on tools/ turned Main red, 2026-11-06). A
push carrying that path is put to the hook itself (`pre-push --lint-plan`, the GitOps manifests in place), so `*`,
`@repo`, `@gitops` and globs are judged by the one implementation. A directory is probed by one file of each extension
under it (a spec scanning it reads the kinds it scans): one selecting probe is enough.
Prints `<spec>TAB<literal path>` for each miss; nothing when every row covers what its spec reads. Run from the root.
Usage: lint-registry-reads.py [registry]
"""
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

LITERAL = re.compile(r'"([A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)+/?)"')
EXTENSION = re.compile(r"\.[A-Za-z]{1,6}")  # a file kind; a dated or numbered suffix is no kind
HOOK = "scripts/hooks/pre-push"


def probes(literal):
    """The pushed paths standing for `literal`: itself, or one file per extension below a directory."""
    path = os.path.normpath(literal)
    if os.path.isfile(path):
        return [path]
    found = {}
    if os.path.isdir(path):
        for root, dirs, files in os.walk(path):
            dirs.sort()
            for name in sorted(files):
                extension = os.path.splitext(name)[1]
                found.setdefault(extension if EXTENSION.fullmatch(extension) else "", os.path.join(root, name))
    return list(found.values())


def selected_specs(path):
    run = subprocess.run([HOOK, "--lint-plan"], input=path + "\n", capture_output=True, text=True,
                         env={**os.environ, "PREPUSH_GITOPS": "1"})
    return set(run.stdout.split())


def main():
    registry = sys.argv[1] if len(sys.argv) > 1 else "scripts/hooks/lint-registry.tsv"
    with open(registry) as rows:
        table = [line.rstrip("\n").split("\t") for line in rows if line.strip() and not line.startswith("#")]
    wanted = {}  # spec -> {literal: [probe, ...]}
    for columns in table:
        spec, reads = columns[0], columns[1] if len(columns) > 1 else ""
        module, name = spec.split(":", 1)
        source = f"{module}/src/test/scala/{name.replace('.', '/')}.scala"
        if reads.split() == ["-"] or not os.path.isfile(source):
            continue
        with open(source) as text:
            for literal in sorted(set(LITERAL.findall(text.read()))):
                found = [p for p in probes(literal) if not p.startswith(("target/", ".git/"))]
                if found:
                    wanted.setdefault(spec, {})[literal] = found
    unique = sorted({p for per in wanted.values() for found in per.values() for p in found})
    with ThreadPoolExecutor(max_workers=8) as pool:
        selection = dict(zip(unique, pool.map(selected_specs, unique)))
    for spec, per in wanted.items():
        for literal, found in per.items():
            if not any(spec in selection[p] for p in found):
                print(f"{spec}\t{literal}")


main()
