#!/usr/bin/env bash
# A recorded fixture tree from ci-runner's RECORDING CACHE instead of unpacked by this job, and the overlay
# that makes it writable.
#
#   recording-cache.sh tree <release tag> <asset name or glob>
#       Prints the directory holding the asset unpacked (tar's own paths under it). Exit 0, or non-zero
#       with the reason on stderr: the caller then unpacks the asset itself, as it always did.
#   recording-cache.sh overlay <unpacked fixtures dir> <workspace fixtures dir> <scratch dir>
#       Mounts each top-level entry of the first over the same entry of the second: an overlayfs whose
#       lower layer is the cache's (read-only) tree, over whatever the checkout already has there, and
#       whose upper layer is a tmpfs at <scratch>. Every write the job makes into it -- the fills laid over
#       the tree -- lands in RAM, never in the shared tree and never on disk. A plain file is copied.
#
# WHY. Every convergence, order-independence, next-day and takes row of a leg replays its country's tree:
# 45-330 MB of zstd that unpacks to 1-5 GB in 130-220k files. Unpacked by every row -- into $RUNNER_TEMP, then
# "moved" into the workspace, which on ci-runner is another filesystem (a docker volume), so a second full copy
# -- that was ~10 GB of writes per row and most of the ~300 MB/s ci-runner's NVMes took around the clock
# (2026-10-22: 24 TB in two days). The host unpacks each asset ONCE, under the asset's sha256, and every
# container job mounts the result read-only (bitcashier infra: nix/files/forgejo-recording-cache).
#
# HERMETIC ALL THE SAME: the tree is the asset's bytes as tar lays them out, keyed by their digest, so a row
# replays exactly what it would have unpacked; the fills land over it in the same order; and the lower layer is
# read-only, so no row can change what another replays.
#
# ONLY WHERE THE HOST OFFERS IT: $FORGEJO_RECORDING_CACHE (the service) and $FORGEJO_RECORDING_CACHE_DIR (the
# mount) are set by ci-runner's container runners alone. On github.com, on a host runner, with the service down,
# or with RECORDING_CACHE=off, `tree` fails and the job unpacks the asset itself.
set -uo pipefail

die() { echo "recording cache: $*" >&2; exit 1; }

tree() {
  local tag="${1:?release tag}" pattern="${2:?asset name or glob}" url dir answer digest
  [ "${RECORDING_CACHE:-on}" != off ] || die "off (RECORDING_CACHE=off)"
  url="${FORGEJO_RECORDING_CACHE:-}"
  dir="${FORGEJO_RECORDING_CACHE_DIR:-}"
  [ -n "$url" ] && [ -n "$dir" ] || die "not offered here (no FORGEJO_RECORDING_CACHE / _DIR)"
  [ -d "$dir" ] || die "$dir is not mounted"
  [ -n "${GH_TOKEN:-}" ] && [ -n "${GITHUB_REPOSITORY:-}" ] || die "no GH_TOKEN / GITHUB_REPOSITORY to ask with"
  # The first row to ask for a tree waits while the host unpacks it (~30-60 s for the largest); later ones are
  # answered at once. Bounded: a hung service costs this row its own unpack, never its job.
  answer=$(curl -sS --fail-with-body --max-time "${RECORDING_CACHE_TIMEOUT:-900}" --get \
             -H "Authorization: token $GH_TOKEN" \
             --data-urlencode "repo=$GITHUB_REPOSITORY" --data-urlencode "tag=$tag" --data-urlencode "asset=$pattern" \
             "$url/tree" 2>&1) || die "the service refused $pattern: $answer"
  digest=$(jq -r '.digest // empty' <<< "$answer" 2>/dev/null)
  [[ "$digest" =~ ^[0-9a-f]{64}$ ]] || die "the service answered without a digest: $answer"
  [ -d "$dir/$digest" ] || die "the service unpacked $digest, but $dir/$digest is not there"
  echo "recording cache: $(jq -r '.asset' <<< "$answer") is $digest ($(jq -r 'if .hit then "already unpacked" else "unpacked now" end' <<< "$answer"))" >&2
  echo "$dir/$digest"
}

overlay() {
  local lower="${1:?unpacked fixtures dir}" target="${2:?workspace fixtures dir}" scratch="${3:?scratch dir}"
  local entry name lowers
  [ -d "$lower" ] || die "$lower is not a directory"
  mkdir -p "$target" "$scratch"
  # RAM, not disk, for whatever the job writes over the tree; owned by the job's user, so the overlay's root
  # (which takes the upper layer's owner) lets it write. Its pages count against the job's memory only as used.
  if ! mountpoint -q "$scratch"; then
    sudo mount -t tmpfs -o "size=${RECORDING_CACHE_UPPER_SIZE:-2g},mode=0755,uid=$(id -u),gid=$(id -g)" \
      recording-upper "$scratch" || die "could not mount a tmpfs at $scratch"
  fi
  for entry in "$lower"/* "$lower"/.[!.]*; do
    [ -e "$entry" ] || continue
    name=$(basename "$entry")
    if [ ! -d "$entry" ] || [ -L "$entry" ]; then
      cp -a "$entry" "$target/$name" || die "could not copy $name"
      continue
    fi
    mkdir -p "$scratch/$name/upper" "$scratch/$name/work" "$target/$name"
    lowers="$entry"
    # What the checkout already holds there stays visible beneath the tree, as the move it replaces merged them.
    [ -z "$(ls -A "$target/$name" 2>/dev/null)" ] || lowers="$entry:$target/$name"
    sudo mount -t overlay recording-overlay \
      -o "lowerdir=$lowers,upperdir=$scratch/$name/upper,workdir=$scratch/$name/work" "$target/$name" ||
      die "could not mount the overlay at $target/$name"
    echo "recording cache: $target/$name is $entry, read-only, under a RAM upper layer" >&2
  done
}

command="${1:-}"
[ "$#" -gt 0 ] && shift
case "$command" in
  tree)    [ "$#" -eq 2 ] || { echo "usage: $0 tree <release tag> <asset name or glob>" >&2; exit 64; }; tree "$@" ;;
  overlay) [ "$#" -eq 3 ] || { echo "usage: $0 overlay <unpacked fixtures dir> <workspace fixtures dir> <scratch dir>" >&2; exit 64; }; overlay "$@" ;;
  *) echo "usage: $0 <tree|overlay> ..." >&2; exit 64 ;;
esac
