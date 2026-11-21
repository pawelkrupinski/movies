#!/usr/bin/env python3
"""Whether a lint spec JUDGES EACH FILE ALONE -- the proof behind a lint-registry row's `@per-file` mark
(scripts/hooks/lint-registry.tsv).

WHY (the owner, 2026-11-21, landing-speed proposal 1, "make rechecks cheaper"): agentctl keeps a change's verdict when
main moved only outside what its checks read. A spec reading every script reads both sides of any two script changes,
but when it judges each file alone (NoSettingFallbackSpec: no file defaults a credential) the two sides meet in its
verdict only through a file both change -- which agentctl rechecks in full anyway -- or through the spec's own code.
The mark lets agentctl leave such a spec out of the overlap unless one side changes its code (pre-push
--per-file-lints).

HOW IT PROVES IT, by mutation: the spec is run once to see what it opens (scripts/ci/lint_reads_trace.py's access
times), then, for pairs (A, B) of files it read of one kind, run with A changed and every other file as it is:
  copy   A holds B's text: every file but A is judged as before, so the spec passes, or fails naming A alone
  empty  A is empty: the same
Every file of a kind it reads at most LONE_AT of (an allowlist, a config its rule reads) is emptied in turn too.
A spec reading across files (MetricRegistrationsUniqueSpec: B's name registered twice; a wiring spec: the script no
workflow calls any more) then fails naming B, or names no file at all: it is NOT per-file. A failure naming only A is
A's own verdict -- the spec judging A by its path (an allowlist) is still judging A alone. Every changed file is put
back as it was (a classpath copy under */target/ too, which a spec reading a test resource reads).

  lint_per_file_check.py [--classpaths FILE] [--pairs N] (--marked | SPEC ...)
      SPEC: `<module>:<package.Spec>`; --marked: every registry row with `@per-file`. Prints `<spec>TAB<verdict>TAB<why>`,
      PER-FILE or CROSS-FILE; exit 1 when a spec checked is CROSS-FILE (for --marked: its mark is wrong), 2 when one did
      not run green unchanged. --pairs: the pairs tried per spec (default 3).
Run from the root, after `scripts/sbt-warm "<module>/Test/compile"` for every module checked.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lint_reads_trace as trace  # noqa: E402

REGISTRY = trace.REGISTRY
LONE_AT = 3  # files of one kind a spec reads at most for each to be emptied in turn
BY_NAME_MAX = 40  # files opened by name emptied one at a time, at most


def marked_specs():
    with open(REGISTRY) as f:
        rows = [l.rstrip("\n").split("\t") for l in f if l.strip() and not l.startswith("#")]
    return [r[0] for r in rows if len(r) > 1 and "@per-file" in r[1].split()]


def pairs(files, n):
    """Up to n (A, B) pairs of files of one kind, A and B in different directories where it can: spread over the reads."""
    by_kind = {}
    for f in sorted(files):
        by_kind.setdefault(trace.extension(f), []).append(f)
    groups = sorted((g for g in by_kind.values() if len(g) > 1), key=len, reverse=True)
    out = []
    for i in range(n):
        if not groups:
            break
        g = groups[i % len(groups)]
        a = g[(i * 7) % len(g)]
        b = next((x for x in g[::-1] if os.path.dirname(x) != os.path.dirname(a)), next(x for x in g if x != a))
        if (a, b) not in out:
            out.append((a, b))
    return out


def mentions(output, path):
    return path in output or (len(os.path.basename(path)) > 3 and os.path.basename(path) in output)


def verdict_of(run, changed, reads):
    """None when the run passed or named only files it changed (their own verdicts); else why it is cross-file."""
    if trace.green(run):
        return None
    out = run.stdout + run.stderr
    others = [p for p in reads if p not in changed and mentions(out, p)]
    if others:
        return f"names {others[0]}, which it did not change"
    if not any(mentions(out, p) for p in changed):
        return "fails naming no file it changed"
    return None


def mutated(texts, created, action):
    """action() with each path of texts holding its text and each of created written, all put back afterwards."""
    saved = {}
    try:
        for p, text in texts.items():
            with open(p, "rb") as f:
                saved[p] = f.read()
            with open(p, "wb") as f:
                f.write(text)
        for p, text in created.items():
            with open(p, "wb") as f:
                f.write(text)
        return action()
    finally:
        for p, data in saved.items():
            with open(p, "wb") as f:
                f.write(data)
        for p in created:
            if os.path.exists(p):
                os.remove(p)


def twin(path):
    """A new path beside path, of its kind: `a/b.sh` -> `a/b-perfiletwin.sh`."""
    stem, ext = os.path.splitext(path)
    return f"{stem}-perfiletwin{ext}"


def check(spec, cp, watched, copies, n):
    module = spec.split(":", 1)[0]
    return check_command(trace.spec_command(spec, cp), trace.SPEC_CWD.get(module), watched, copies, n)


def check_command(command, cwd, watched, copies, n):
    """(verdict, why) of the lint command (run from the root, or cwd) over the watched files: PER-FILE, CROSS-FILE or
    UNRUN (it fails unchanged)."""
    run = lambda: trace.observe(command, watched, copies, cwd)[1]
    reads, first = trace.observe(command, watched, copies, cwd)
    if not trace.green(first):
        return "UNRUN", "it does not run green unchanged"
    files = sorted(p for p in reads if os.path.isfile(p))
    listed = {p.rstrip("/") for p in reads if os.path.isdir(p)}
    if len(files) < 2:
        return "CROSS-FILE", "it reads fewer than two files: nothing to prove"
    copy_of = {}
    for c, src in copies.items():
        copy_of.setdefault(src, []).append(c)
    content = {}
    def text(p):
        if p not in content:
            with open(p, "rb") as f:
                content[p] = f.read()
        return content[p]
    def with_copies(texts):  # a test resource is read through its classpath copy too
        return {q: x for p, x in texts.items() for q in [p, *copy_of.get(p, [])]}
    trials = []
    # TWINS: every file it lists gets a new twin holding its text -- a spec judging files alone judges each twin as its
    # original; one reading across them (a name registered twice, the exact set of a workflow's dispatchers) names the
    # original
    walked = [p for p in files if os.path.dirname(p) in listed]
    if walked:
        trials.append(("twins", {}, {twin(p): text(p) for p in walked}))
    # QUARTERS: each quarter of what it read emptied at once -- one reading across files (the script no workflow calls
    # any more) names a file of another quarter
    for q in range(4):
        part = files[q::4]
        if part:
            trials.append((f"quarter {q + 1} emptied", {p: b"" for p in part}, {}))
    # BY NAME: each file it opens by name (outside what it lists), and each of a kind it reads few of, emptied alone
    by_kind = {}
    for f in files:
        by_kind.setdefault(trace.extension(f), []).append(f)
    alone = sorted({p for p in files if os.path.dirname(p) not in listed}
                   | {p for g in by_kind.values() if len(g) <= LONE_AT for p in g})
    trials += [(f"{p} emptied", {p: b""}, {}) for p in alone[:BY_NAME_MAX]]
    # PAIRS: A holding B's text
    trials += [(f"{a} holding {b}", {a: text(b)}, {}) for a, b in pairs(files, n)]
    for name, texts, created in trials:
        why = verdict_of(mutated(with_copies(texts), created, run), set(texts) | set(created), files)
        if why:
            return "CROSS-FILE", f"{name}: {why}"
    return "PER-FILE", f"{len(trials)} mutations (twins, quarters, files by name, pairs): no other file's verdict moved"


def main(args):
    given, n = None, 3
    while args[:1] in (["--classpaths"], ["--pairs"]) and len(args) > 1:
        given, n = (args[1], n) if args[0] == "--classpaths" else (given, int(args[1]))
        args = args[2:]
    specs = marked_specs() if args == ["--marked"] else args
    if not specs or any(":" not in s for s in specs):
        print(__doc__, file=sys.stderr)
        return 2
    cps = trace.classpaths(sorted({s.split(":", 1)[0] for s in specs}), given)
    files, dirs = trace.tracked()
    copies = trace.resource_copies(files)
    watched = files + dirs + list(copies)
    worst = 0
    for spec in specs:
        verdict, why = check(spec, cps[spec.split(":", 1)[0]], watched, copies, n)
        print(f"{spec}\t{verdict}\t{why}", flush=True)
        worst = max(worst, {"PER-FILE": 0, "CROSS-FILE": 1, "UNRUN": 2}[verdict])
    return worst


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
