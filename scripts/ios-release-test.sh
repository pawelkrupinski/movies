#!/usr/bin/env bash
#
# ios-release.sh drives xcodebuild and altool, so it is exercised here against fakes
# of both (and of swift, xcode-select and the vault), in a throwaway copy of the repo.
# What it pins: the IPA export runs the SYSTEM rsync. Xcode's packaging step spawns
# `rsync` off PATH, and a Homebrew rsync 3.x ahead of /usr/bin made the 2.0.12
# export die with nothing but "exportArchive Copy failed".
#
#   scripts/ios-release-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/shell-spec.sh"
. "$HERE/scratch-git.sh"

printf '\033[36m▸\033[0m ios-release.sh\n'

SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

# A clean repo holding just what the script sources: it refuses a dirty tree.
REPO="$SCRATCH/repo"
mkdir -p "$REPO/scripts"
cp "$HERE/ios-release.sh" "$HERE/log.sh" "$HERE/app-store-credentials.sh" "$REPO/scripts/"
printf '#!/usr/bin/env bash\n' > "$REPO/scripts/tag-mobile-release.sh"
chmod +x "$REPO/scripts/tag-mobile-release.sh"
echo 2.0.99 > "$REPO/mobile-version.txt"
scratch_repo "$REPO"
scratch_git "$REPO" add -A
scratch_git "$REPO" commit -qm init

BIN="$SCRATCH/bin"
mkdir -p "$BIN" "$SCRATCH/brew" "$SCRATCH/xcode/usr/bin"
fake() { # $1 path, stdin body
  { printf '#!/usr/bin/env bash\n'; cat; } > "$1"
  chmod +x "$1"
}
fake "$BIN/swift" <<<'exit 0'
fake "$SCRATCH/xcode/usr/bin/altool" <<<'exit 0'
fake "$BIN/xcode-select" <<<"echo '$SCRATCH/xcode'"
# The iOS SDK version the release checks for; the real xcrun is absent on Linux.
fake "$BIN/xcrun" <<<'echo 27.1'
# The rsync a Homebrew install puts ahead of /usr/bin.
fake "$SCRATCH/brew/rsync" <<<'exit 1'
# xcodebuild: archive succeeds; export records which rsync it would spawn, then lays
# down the ipa the script looks for.
fake "$BIN/xcodebuild" <<SH
if [ "\$1" = -exportArchive ]; then
  r=\$(command -v rsync); printf '%s\n' "\$(readlink "\$r" || echo "\$r")" > "$SCRATCH/export-rsync"
  while [ \$# -gt 0 ]; do [ "\$1" = -exportPath ] && { mkdir -p "\$2"; echo ipa > "\$2/Kinowo.ipa"; }; shift; done
fi
exit 0
SH
# shellcheck disable=SC2016 # the fake expands $3 when it runs
fake "$SCRATCH/secrets" <<<'case "$3" in APP_STORE_KEY_ID) printf K1 ;; APP_STORE_ISSUER_ID) printf I1 ;; *) printf p8 ;; esac'

out=$(PATH="$SCRATCH/brew:$BIN:$PATH" SECRETS_BIN="$SCRATCH/secrets" \
  "$REPO/scripts/ios-release.sh" --no-upload 2>&1)
check "runs through to the --no-upload stop" "0" "$?"
check "exports with the system rsync, not one earlier on PATH" "/usr/bin/rsync" "$(cat "$SCRATCH/export-rsync" 2>/dev/null)"
# shellcheck disable=SC2154 # fails is shell-spec.sh's tally
[ "$fails" -eq 0 ] || printf '%s\n' "$out"

# An SDK older than 27.1 would ship an app letterboxed on the unfolded iPhone Duo:
# refused before the archive (sort -V, so 27.10 counts as newer than 27.9). The build
# dir moves outside the scratch repo, and the first run's output there is cleared, so
# the tree stays clean.
rm -rf "$REPO/ios/build"
fake "$BIN/xcodebuild" <<SH
[ "\$1" = archive ] && touch "$SCRATCH/archived"
exit 0
SH
release() {
  PATH="$SCRATCH/brew:$BIN:$PATH" SECRETS_BIN="$SCRATCH/secrets" IOS_RELEASE_BUILD_DIR="$SCRATCH/build" \
    "$REPO/scripts/ios-release.sh" --no-upload 2>&1
}
archived() { [ -e "$SCRATCH/archived" ] && echo yes || echo no; }
for sdk in 26.4 27.0; do
  fake "$BIN/xcrun" <<<"echo $sdk"
  out=$(release); code=$?
  check "iOS SDK $sdk is refused before archiving" "1 no yes" \
    "$code $(archived) $(grep -q 'older than 27.1' <<<"$out" && echo yes || echo no)"
done
fake "$BIN/xcrun" <<<'echo 27.10'
release > /dev/null
check "iOS SDK 27.10 passes the check and archives" "yes" "$(archived)"

spec_summary
