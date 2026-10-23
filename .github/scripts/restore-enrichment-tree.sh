#!/usr/bin/env bash
# Fetch a convergence leg's enrichment tree and unpack it into a STAGING directory, for
# convergence-setup to move into the workspace once its build-cache steps are done.
#
#   restore-enrichment-tree.sh <code> <mode> <stage dir>
#
# Reads FIXTURE_RELEASE_TAG, KINOWO_CONVERGENCE_TREE_ASSET ("Resolve the recorded pair") and
# GH_TOKEN from the environment.
#
# WHY A STAGE, AND WHY IN THE BACKGROUND. The tree is 200-300 MB of zstd and ~130k files: a
# download and an unpack that were 13-19 s of every leg's setup (run 37105119296), queued behind
# the JDK, sbt and build-cache restores they share nothing with. Those restores have to run
# BEFORE the tree is in the workspace, because their cache keys glob the workspace and a restored
# tree is 130k files every glob would walk (~50 s of the US setup once). Unpacked outside it —
# into $RUNNER_TEMP, on the same filesystem, so moving it in afterwards is a rename — the two
# overlap: convergence-setup starts this first (scripts/ci/in-background.sh) and waits for it
# where it used to unpack.
#
# Exit status: 0 with the tree staged, or with nothing to stage on a RECORDING (which fetches
# live and publishes what it learns); 3 when a replay leg's pinned tree is missing (a hermetic
# leg replays nothing else, so its setup fails); 4 when the release could not be read at all;
# anything else is an unpack that failed — the tree's, or a fill's the pair names.
set -uo pipefail

code="${1:?usage: restore-enrichment-tree.sh <code> <mode> <stage dir>}"
mode="${2:?usage: restore-enrichment-tree.sh <code> <mode> <stage dir>}"
stage="${3:?usage: restore-enrichment-tree.sh <code> <mode> <stage dir>}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/tool-conventions.sh
. "$here/../../scripts/tool-conventions.sh" # gh_download

TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"
# The pinned pair's tree for a hermetic leg, the working tree for a recording one. Unpacked by
# whatever name it downloads under — the tarball's paths are what place it, and its magic what
# inflates it.
ASSET="${KINOWO_CONVERGENCE_TREE_ASSET:-enrichment-$code.tar.*}"
archives="$stage.archive"
mkdir -p "$archives" "$stage"

# ci-runner's RECORDING CACHE first, for a replay (scripts/ci/recording-cache.sh): the host unpacked this asset once,
# read-only, for every job, so this row neither downloads nor writes it -- convergence-setup's unpack lays an overlay over
# it instead (the path it leaves in $stage.cached). A RECORDING extends its working tree, so it always unpacks its own.
# Anything short of a tree in hand (github.com, a host runner, the service down) unpacks it here, as before.
rm -f "$stage.cached"
if [ "$mode" != "record" ] && cached=$("$here/../../scripts/ci/recording-cache.sh" tree "$TAG" "$ASSET"); then
    echo "$cached" > "$stage.cached"
    echo "enrichment tree $ASSET from release $TAG, unpacked once on the host at $cached"
    if [ -n "${KINOWO_CONVERGENCE_FILL_ASSETS:-}" ]; then
        read -ra fills <<< "$KINOWO_CONVERGENCE_FILL_ASSETS"
        "$here/convergence-fill.sh" unpack "$stage" "${fills[@]}"
    fi
    exit $?
fi

# Only gh's own "no assets match" means the release lacks the tree (gh_download's 3). Any other failure — a
# network blip, a 5xx, an expired token — is a read that did not happen, not an empty release: a recording
# that took it for one would fetch every lookup live and publish that over the tree it never read. gh_download
# bounds each attempt (RESTORE_TREE_DOWNLOAD_TIMEOUT seconds, 1200) and asks a failed read again: one GitHub 5xx
# used to fail the leg's setup.
gh_download "${RESTORE_TREE_DOWNLOAD_TIMEOUT:-1200}" release download "$TAG" --pattern "$ASSET" --dir "$archives" --clobber
download_status=$?
# A replay's pinned tree the rolling release no longer holds (it keeps the newest five): the ARCHIVE release
# (RECORDING_ARCHIVE_TAG, default identity-recordings) keeps the landing recordings' trees (528, 573) under the same name.
if [ "$download_status" -eq 3 ] && [ "$mode" != "record" ]; then
    ARCHIVE_TAG="${RECORDING_ARCHIVE_TAG:-identity-recordings}"
    gh_download "${RESTORE_TREE_DOWNLOAD_TIMEOUT:-1200}" release download "$ARCHIVE_TAG" --pattern "$ASSET" --dir "$archives" --clobber
    download_status=$?
    [ "$download_status" -ne 0 ] || TAG="$ARCHIVE_TAG"
fi
if [ "$download_status" -eq 0 ]; then
    # Both spellings of the working tree only in the moment between a first zstd publish and its
    # deleting the gzip it replaced: the zstd one is the newer.
    if compgen -G "$archives/*.tar.zst" >/dev/null; then rm -f "$archives"/*.tar.gz; fi
    echo "enrichment tree $ASSET from release $TAG"
elif [ "$download_status" -ne 3 ]; then
    echo "::error::could not read release $TAG for $ASSET (gh's error above)"
    exit 4
elif [ "$mode" != "record" ]; then
    echo "::error::the pinned tree $ASSET is not in release $TAG (it keeps the newest five) — a hermetic leg replays nothing else"
    exit 3
fi

tree=$(compgen -G "$archives/*" | head -1 || true)
if [ -z "$tree" ]; then
    echo "no enrichment capture found for $code — this leg fetches live and publishes the result"
    exit 0
fi
ls -l "$archives/"
"$here/unpack-fixture-archive.sh" "$tree" "$stage" || exit $?
echo "restored $tree ($(du -h "$tree" | cut -f1))"

# A HERMETIC leg's pair is the pinned tree AND the fills earlier legs published over it, oldest first
# ("Resolve the recorded pair" names them in KINOWO_CONVERGENCE_FILL_ASSETS; convergence-fill.sh). One it cannot
# restore fails the restore, and so the leg's setup: the pair names it, so its verdict and its bisect would not agree.
if [ "$mode" != "record" ] && [ -n "${KINOWO_CONVERGENCE_FILL_ASSETS:-}" ]; then
    read -ra fills <<< "$KINOWO_CONVERGENCE_FILL_ASSETS"
    "$here/convergence-fill.sh" unpack "$stage" "${fills[@]}"
fi
