#!/usr/bin/env bash
# Drop the GitHub-hosted Ubuntu runner's packages.microsoft.com apt sources before an
# `apt-get update`. Usage: scripts/ci/drop-microsoft-apt-sources.sh
#
# The runner image ships them for azure-cli, powershell, ...; nothing this repo installs
# comes from there. One 403 from that repository fails the whole `apt-get update` and the
# job with it (run 36104133560, a page-test shard, 2026-09-25: "packages.microsoft.com ...
# InRelease 403 Forbidden"). Dropping them leaves only the Ubuntu mirrors apt needs.
#
# Only Microsoft's own entries go: a line of a one-line `.list`, a stanza of a deb822 `.sources`. A file is removed
# only when nothing else is left in it. Removing every file that MENTIONED Microsoft took Ubuntu's archive with it on
# runner image 20261101.64 (Main #2149): `apt-get update` then fetched nothing, and every package Playwright installs
# was "Unable to locate". So the script also fails, by name, when no apt source is left at all.
#
# Env: APT_SOURCES_DIR (default /etc/apt/sources.list.d), APT_SOURCES_LIST (default /etc/apt/sources.list),
# APT_SOURCES_SUDO (default sudo; empty runs without). Tested by scripts/ci/drop-microsoft-apt-sources-test.sh.
set -euo pipefail

dir="${APT_SOURCES_DIR:-/etc/apt/sources.list.d}"
main_list="${APT_SOURCES_LIST:-/etc/apt/sources.list}"
as_root=${APT_SOURCES_SUDO-sudo}
microsoft='packages\.microsoft\.com'

# What a sources file still says once comments and blank lines are set aside.
entries() { grep -v -E '^[[:space:]]*(#|$)' "$1" || true; }

# A second call in the same job, or an image without Microsoft's sources, finds nothing to drop: not a failure.
shopt -s nullglob
for file in "$dir"/*.list "$dir"/*.sources; do
  grep -q -E "$microsoft" "$file" || continue
  kept="$(mktemp)"
  case "$file" in
    *.list)    grep -v -E "$microsoft" "$file" > "$kept" || true ;;
    # deb822: a stanza is a paragraph; drop the ones naming Microsoft, keep the rest as they were.
    *.sources) awk -v RS= -v ORS='\n\n' -v pattern="$microsoft" '$0 !~ pattern' "$file" > "$kept" ;;
  esac
  if [ -n "$(entries "$kept")" ]; then
    $as_root cp "$kept" "$file"
    echo "drop-microsoft-apt-sources: dropped Microsoft's entries from $file, kept the rest"
  else
    $as_root rm -f "$file"
    echo "drop-microsoft-apt-sources: removed $file (Microsoft's only)"
  fi
  rm -f "$kept"
done

left=""
for file in "$main_list" "$dir"/*.list "$dir"/*.sources; do
  [ -f "$file" ] && [ -n "$(entries "$file")" ] && left="$left $file"
done
if [ -z "$left" ]; then
  echo "::error::no apt source is left in $main_list or $dir: apt-get update would fetch nothing and every install fail" \
       "\"Unable to locate package\" (the runner image's sources: $(find "$dir" -mindepth 1 -maxdepth 1 -exec basename {} \; 2>/dev/null | tr '\n' ' '))" >&2
  exit 1
fi
