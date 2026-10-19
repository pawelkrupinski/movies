#!/usr/bin/env python3
"""Delete the class files in a checkout's sbt output that its own Zinc analysis never produced.

Usage: scripts/zinc_orphans.py [--dry-run] CHECKOUT...

Zinc deletes only the products it recorded: a .class written into */target/scala-*/classes by anything else lives
there forever, and shadows the real class wherever that directory comes first on a classpath. On 2026-10-18 23:16
IntelliJ's JPS build (whose worker main module counts src/fixtures/scala as a source root, with sbt's classes/ as its
output) wrote ~400 fixtures classes into the root checkout's worker/target/scala-3.9.0/classes; every checkout seeded
from that build (scripts/sbt-warm, scripts/identity-tools.sh) got them too, and a measure died on
NoClassDefFoundError: modules/wiring/IdentityCutoverWiring$ -- a class renamed away the day before, named by an
orphan UnmatchedClusters$ that shadowed fixtures-classes' current one.

For every analysis <prefix>zinc/inc_compile*.zip under CHECKOUT's */target/scala-* and project/**/target, the
sibling <prefix>classes directory keeps only the .class files the analysis lists as products, and the .tasty files
beside a kept class; everything else ending .class/.tasty is deleted (resources are left alone). An analysis that
cannot be read, or lists no product in its directory, leaves that directory untouched: never delete on a failed read.

Run it only where no sbt compile is live (a seed's fresh destination): a compile writes its classes before it saves
the analysis that lists them. Prints one line per directory pruned; exits 0 unless a CHECKOUT is missing.
"""
import os
import pathlib
import sys
import zipfile

BASE = b"${BASE}/"
SUFFIXES = (".class", ".tasty")


def products(analysis: pathlib.Path) -> "set[str]":
    """Every ${BASE}-relative product path an analysis zip lists, read from its protobuf strings by their lengths."""
    found = set()
    with zipfile.ZipFile(analysis) as z:
        for name in z.namelist():
            if not name.endswith(".bin"):
                continue
            data = z.read(name)
            i = data.find(BASE)
            while i >= 0:
                path = _class_path_at(data, i)
                if path is not None:
                    found.add(path[len(BASE):].decode("utf-8", "replace"))
                i = data.find(BASE, i + 1)
    return found


def _class_path_at(data: bytes, start: int):
    """The protobuf string beginning at <start>, when it names a .class: its varint length ends right before it (1-3
    bytes; a longer length's last byte alone also reads as a 1-byte one, so each width is tried), or None."""
    for width in (1, 2, 3):
        if start - width < 0:
            break
        varint = data[start - width:start]
        if varint[-1] & 0x80 or any(b & 0x80 == 0 for b in varint[:-1]):
            continue
        length = 0
        for shift, b in enumerate(varint):
            length |= (b & 0x7F) << (7 * shift)
        if start + length <= len(data):
            s = data[start:start + length]
            if s.endswith(b".class") and all(0x20 <= b < 0x7F for b in s):
                return s
    return None


def analyses(checkout: pathlib.Path):
    for target in [*checkout.glob("*/target/scala-*"), *checkout.glob("project/**/target/scala-*/sbt-*")]:
        for zinc in target.glob("*zinc"):
            for analysis in sorted(zinc.glob("inc_compile*.zip")):
                yield analysis, target / (zinc.name[: -len("zinc")] + "classes")


def prune(checkout: pathlib.Path, dry_run: bool) -> int:
    removed = 0
    for analysis, classes in analyses(checkout):
        if not classes.is_dir():
            continue
        try:
            listed = products(analysis)
        except (OSError, zipfile.BadZipFile) as e:
            print(f"zinc-orphans: {analysis} unreadable ({e}): {classes} left as it is", file=sys.stderr)
            continue
        here = {os.path.normpath(checkout / p) for p in listed}
        rel = os.path.relpath(classes, checkout)
        if not any(p.startswith(str(classes) + os.sep) for p in here):
            continue   # lists nothing in this directory: an empty build, or a format this cannot read
        gone = []
        for root, _, files in os.walk(classes):
            for f in files:
                if not f.endswith(SUFFIXES):
                    continue
                path = os.path.join(root, f)
                cls = path if f.endswith(".class") else path[: -len(".tasty")] + ".class"
                if os.path.normpath(cls) not in here:
                    gone.append(path)
        if gone:
            if not dry_run:
                for path in gone:
                    os.remove(path)
            print(f"zinc-orphans: {'would remove' if dry_run else 'removed'} {len(gone)} class file(s) "
                  f"no build of {rel} produced", file=sys.stderr)
            removed += len(gone)
    return removed


def main(argv) -> int:
    dry_run = "--dry-run" in argv
    checkouts = [a for a in argv if a != "--dry-run"]
    if not checkouts:
        print(__doc__, file=sys.stderr)
        return 2
    status = 0
    for c in checkouts:
        p = pathlib.Path(c).resolve()
        if not p.is_dir():
            print(f"zinc-orphans: no checkout at {c}", file=sys.stderr)
            status = 2
            continue
        prune(p, dry_run)
    return status


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
