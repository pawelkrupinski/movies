#!/usr/bin/env bash
# sbt-invalidate-exporters.sh: delete the compiled classes of every Scala source holding a WILDCARD export
# (`export X.*`, `export x.{a as _, *}`), so a build restored from an OLDER build cache recompiles them.
#
# Why: zinc invalidates by name. Adding a member to an object that another object `export X.*`s changes X's API by a
# name the exporter never spells out, so zinc leaves the exporter's class -- and its forwarders -- as they were. A build
# cache restored from an older commit then lacks the new member's forwarder, and a caller of it fails with "value ... is
# not a member of object ...", while a clean build compiles (Main #226: IdentityMeasures' `export TitleShapeMeasures.*`
# missing 7b3897ca6's numbersAnotherSubtitle). A missing product makes zinc recompile its source, and a recompiled
# exporter whose API grew invalidates its callers as any API change does. It costs those few sources a recompile.
#
# Run from the repository root, after the restore and before sbt. Prints each source it invalidated.
set -euo pipefail

wildcard_export='^[[:space:]]*export[[:space:]].*(\.\*|[{,][[:space:]]*\*[[:space:]]*\})'

{ find */src -path '*/scala/*' -name '*.scala' 2>/dev/null || true; } | LC_ALL=C sort | while IFS= read -r src; do
  grep -qE "$wildcard_export" "$src" || continue
  module=${src%%/*}
  # The package clauses, chained (`package a` + `package b` is a.b), as a class-file directory.
  pkg=$(sed -nE 's/^package[[:space:]]+([A-Za-z0-9_.]+).*/\1/p' "$src" | paste -sd. - | tr . /)
  base=$(basename "$src" .scala)
  # The file's top-level definitions, whose products carry the exporters' forwarders, plus the file's own name and its
  # `$package` object (a top-level export's forwarders).
  names=$( { sed -nE 's/^([a-z]+(\[[A-Za-z0-9_.]+\])?[[:space:]]+)*(object|class|trait|enum)[[:space:]]+([A-Za-z0-9_]+).*/\4/p' "$src"
             echo "$base"; echo "$base\$package"; } | sort -u)
  deleted=0
  for dir in "$module"/target/scala-*/*/"$pkg"; do
    [ -d "$dir" ] || continue
    while IFS= read -r name; do
      for product in "$dir/$name.class" "$dir/$name.tasty" "$dir/$name\$"*.class; do
        [ -e "$product" ] || continue
        rm -f "$product"
        deleted=$((deleted + 1))
      done
    done <<< "$names"
  done
  [ "$deleted" -eq 0 ] || echo "invalidated $src ($deleted class files)"
done
