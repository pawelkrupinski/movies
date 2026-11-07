#!/usr/bin/env bash
# Prune registry.kinowo.net's images, run daily by modules/roles/registry.nix before the garbage collector.
#
#   registry-prune.sh <registry API base, e.g. http://127.0.0.1:5000/v2> <keep> <storage root> <fresh minutes>
#
# KEEP THE NEWEST <keep> `main-<14-digit UTC timestamp>-<sha7>` TAGS PER REPOSITORY, by digest, plus
# whatever `latest` points at; delete every other manifest. Those tags are what Flux's image
# policies order on (movies-gitops image-automation/automation.yaml), so whatever is deployed is
# always among them. The fixed-width timestamp is why a plain reverse sort is a time sort.
#
# A REPOSITORY WITH NO main-* TAG IS LEFT ALONE: no ordering means no notion of "newest", and
# guessing is how a prune deletes the one image something runs.
#
# A COMMIT-SHA TAG PUSHED WITHIN <fresh minutes> IS KEPT TOO: it is ci's hand-off. ci pushes `<sha>`, and main.yml's
# publish jobs re-tag those same bytes `main-*` once ci is green, minutes later. A prune in between deleted the image
# the re-tag was about to name, and that commit never shipped (Main #2257: pushed 03:49, pruned 04:00, "not found"
# 04:01). Its age is its tag link's mtime under <storage root>, which the registry writes at the push. A sha tag never
# published (its ci red) goes once it is older than that, as before.
#
# Tested by registry-prune-test.sh, against a fake registry.
set -euo pipefail

api=$1
keep=$2
storage=$3
fresh_minutes=$4
accept='application/vnd.oci.image.index.v1+json,application/vnd.oci.image.manifest.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.docker.distribution.manifest.v2+json'
# fresh <repo> <tag>: a commit-sha tag pushed within the last <fresh minutes>
fresh() {
  [[ $2 =~ ^[0-9a-f]{40}$ ]] || return 1
  [ -n "$(find "$storage/docker/registry/v2/repositories/$1/_manifests/tags/$2/current/link" -mmin "-$fresh_minutes" 2>/dev/null)" ]
}
digest() {
  curl -fsSI -H "Accept: $accept" "$api/$1/manifests/$2" |
    tr -d '\r' | grep -i '^docker-content-digest:' | cut -d' ' -f2
}

for repo in $(curl -fsS "$api/_catalog?n=1000" | jq -r '.repositories[]?'); do
  tags=$(curl -fsS "$api/$repo/tags/list" | jq -r '.tags[]?')
  newest=$(grep -E '^main-[0-9]{14}-[0-9a-f]{7}$' <<<"$tags" | sort -r | head -n "$keep" || true)
  if [ -z "$newest" ]; then
    echo "$repo: no main-* tags, left alone"
    continue
  fi
  handoffs=$(for t in $tags; do if fresh "$repo" "$t"; then echo "$t"; fi; done)
  keepers=$(for t in $newest $(grep -x latest <<<"$tags" || true) $handoffs; do digest "$repo" "$t"; done | sort -u)
  deleted=0
  for t in $tags; do
    d=$(digest "$repo" "$t") || continue
    if [ -z "$d" ] || grep -qx "$d" <<<"$keepers"; then continue; fi
    # A digest shared by several tags goes with the first; the rest then 404, harmlessly.
    if curl -fsS -o /dev/null -X DELETE "$api/$repo/manifests/$d"; then deleted=$((deleted + 1)); fi
  done
  echo "$repo: kept $(wc -l <<<"$keepers") image(s), deleted $deleted"
done
