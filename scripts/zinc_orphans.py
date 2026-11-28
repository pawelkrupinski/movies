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
A <prefix>classes directory with NO analysis of its own (no <prefix>zinc/inc_compile*.zip) is one sbt never compiled
into -- a configuration without sources, like repochecks' Compile -- so every .class/.tasty in it is an orphan: on
2026-11-21 18:45 JPS wrote repochecks' fixtures into its classes/, ahead of fixtures-classes on Test's classpath, and
every checkout seeded from that build ran the old deploy.RepoFile (RepoFileSpec failed until `repochecks/clean`).

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


def _varint_before(data: bytes, start: int, width: int) -> "int | None":
    """The length a protobuf varint of <width> bytes ending right at <start> encodes, or None when those bytes are no
    varint (a continuation bit missing inside it, or set on its last byte)."""
    if start - width < 0:
        return None
    varint = data[start - width:start]
    if varint[-1] & 0x80 or any(b & 0x80 == 0 for b in varint[:-1]):
        return None
    length = 0
    for shift, b in enumerate(varint):
        length |= (b & 0x7F) << (7 * shift)
    return length


def _class_path_at(data: bytes, start: int):
    """The protobuf string beginning at <start>, when it names a .class: its varint length ends right before it (1-3
    bytes; a longer length's last byte alone also reads as a 1-byte one, so each width is tried), or None."""
    for width in (1, 2, 3):
        length = _varint_before(data, start, width)
        if length is not None and start + length <= len(data):
            s = data[start:start + length]
            if s.endswith(b".class") and all(0x20 <= b < 0x7F for b in s):
                return s
    return None


def targets(checkout: pathlib.Path):
    return [*checkout.glob("*/target/scala-*"), *checkout.glob("project/**/target/scala-*/sbt-*")]


def analyses(checkout: pathlib.Path):
    for target in targets(checkout):
        for zinc in target.glob("*zinc"):
            for analysis in sorted(zinc.glob("inc_compile*.zip")):
                yield analysis, target / (zinc.name[: -len("zinc")] + "classes")


def unowned(checkout: pathlib.Path):
    """Every <prefix>classes directory whose <prefix>zinc holds no analysis: no sbt compile ever wrote into it."""
    for target in targets(checkout):
        for classes in target.glob("*classes"):
            zinc = target / (classes.name[: -len("classes")] + "zinc")
            if classes.is_dir() and not any(zinc.glob("inc_compile*.zip")):
                yield classes


def stray_path(root: str, name: str, here: "set[str]") -> "str | None":
    """The path of a .class/.tasty file named <name> in <root> whose class is not in <here>, else None."""
    if not name.endswith(SUFFIXES):
        return None
    path = os.path.join(root, name)
    cls = path if name.endswith(".class") else path[: -len(".tasty")] + ".class"
    return None if os.path.normpath(cls) in here else path


def stray_paths(walked: "list[tuple[str, list[str], list[str]]]", here: "set[str]") -> "list[str]":
    """The .class/.tasty files of an `os.walk` listing whose class is not in <here>."""
    found = (stray_path(root, f, here) for root, _, files in walked for f in files)
    return [path for path in found if path is not None]


def strays(classes: pathlib.Path, here: "set[str]") -> "list[str]":
    """The .class/.tasty files under <classes> whose class is not in <here>."""
    return stray_paths(list(os.walk(classes)), here)


def remove_files(paths: "list[str]") -> None:
    for path in paths:
        os.remove(path)


def readable_products(analysis: pathlib.Path, classes: pathlib.Path) -> "set[str] | None":
    """What <analysis> lists, or None when it cannot be read: never delete on a failed read."""
    try:
        return products(analysis)
    except (OSError, zipfile.BadZipFile) as e:
        print(f"zinc-orphans: {analysis} unreadable ({e}): {classes} left as it is", file=sys.stderr)
        return None


def products_here(listed: "set[str]", classes: pathlib.Path, checkout: pathlib.Path) -> "set[str] | None":
    """The listed products as paths under <checkout>, or None when none lies in <classes>: an empty build, or a
    format this cannot read."""
    here = {os.path.normpath(checkout / p) for p in listed}
    return here if any(p.startswith(str(classes) + os.sep) for p in here) else None


def doomed_directories(checkout: pathlib.Path) -> "list[tuple[pathlib.Path, list[str]]]":
    """Every classes directory of <checkout> with the files no build of it produced."""
    doomed = [(classes, strays(classes, set())) for classes in unowned(checkout)]
    for analysis, classes in analyses(checkout):
        listed = readable_products(analysis, classes) if classes.is_dir() else None
        here = None if listed is None else products_here(listed, classes, checkout)
        if here is not None:
            doomed.append((classes, strays(classes, here)))
    return doomed


def prune(checkout: pathlib.Path, dry_run: bool) -> int:
    removed = 0
    for classes, gone in doomed_directories(checkout):
        if gone:
            if not dry_run:
                remove_files(gone)
            print(f"zinc-orphans: {'would remove' if dry_run else 'removed'} {len(gone)} class file(s) "
                  f"no build of {os.path.relpath(classes, checkout)} produced", file=sys.stderr)
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
