#!/usr/bin/env bash
# Delete the superseded per-commit entries of main's GitHub Actions caches, keeping the
# newest entry of each family.
#
#   scripts/ci/prune-actions-caches.sh --dry-run   # print what it would delete, delete nothing
#   scripts/ci/prune-actions-caches.sh             # delete it (needs `actions: write`)
#
# WHY. The repo's caches sat at 10.8 GB against GitHub's 10 GB cap, so GitHub evicted the
# least recently used — which were the Android jobs' Gradle homes, saved only on the rare
# pushes that touch android/. With its own entry gone, Android's `build` job restored ci.yml's
# `mobile-local-server` home, which holds no dex or R8 outputs, so every run re-dexed and
# re-ran R8 (~4-6 min). Most of the 10.8 GB was dead weight: sbt-target-e2e-Linux-<hash>,
# sbt-target-page-Linux-<hash> and friends are saved on every push under a new hashFiles key,
# and a restore-keys prefix only ever restores the NEWEST of them.
#
# WHAT COUNTS AS A FAMILY: the key with a trailing `-<40 to 64 hex digits>` stripped (a commit
# SHA, a hashFiles() sha256, setup-gradle's `-<sha>` on its gradle-home key). Left alone:
#   - keys without such a suffix (the AVD snapshot, the sbt runner): one entry, nothing to prune;
#   - setup-gradle's content-addressed extracted entries (gradle-dependencies-v1-<32 hex>, …):
#     different jobs resolve different sets, so two of them are valid at once, and each
#     gradle-home entry names the ones it needs;
#   - families matching PRUNE_EXCLUDE (default: setup-node's `node-cache-`, which is restored by
#     EXACT key and keyed on whichever lockfile a job uses, so two coexist legitimately);
#   - anything not on refs/heads/main: branch caches are scoped to their branch and expire.
#
# "Newest" is by createdAt, which is what a restore-keys prefix match picks.
set -euo pipefail

dry_run=false
case "${1:-}" in
  --dry-run) dry_run=true ;;
  "") ;;
  *) echo "usage: $0 [--dry-run]" >&2; exit 2 ;;
esac

exclude="${PRUNE_EXCLUDE:-^node-cache-}"

# id <TAB> size <TAB> key, one line per entry to delete.
doomed="$(gh cache list --ref refs/heads/main --limit 1000 --json id,key,ref,sizeInBytes,createdAt |
  jq -r --arg exclude "$exclude" '
    [ .[]
      | select(.ref == "refs/heads/main")
      | (.key | capture("^(?<family>.+)-[0-9a-f]{40,64}$")?) as $m
      | select($m != null and ($m.family | test($exclude) | not))
      | . + {family: $m.family} ]
    | group_by(.family)[]
    | sort_by(.createdAt) | reverse | .[1:][]
    | "\(.id)\t\(.sizeInBytes)\t\(.key)"')"

count=0 bytes=0
while IFS=$'\t' read -r id size key; do
  [ -n "$id" ] || continue
  count=$((count + 1)); bytes=$((bytes + size))
  if $dry_run; then
    printf 'would delete %s (%s MB) %s\n' "$id" "$((size / 1000000))" "$key"
  else
    # A cache already gone (another prune, GitHub's own eviction) is the outcome we wanted.
    gh cache delete "$id" >/dev/null 2>&1 || echo "::notice::cache $id ($key) was already gone"
    printf 'deleted %s (%s MB) %s\n' "$id" "$((size / 1000000))" "$key"
  fi
done <<< "$doomed"

verb=$($dry_run && echo "would free" || echo "freed")
printf '%s entries, %s %s MB\n' "$count" "$verb" "$((bytes / 1000000))"
