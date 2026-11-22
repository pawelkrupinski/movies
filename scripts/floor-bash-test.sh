#!/usr/bin/env bash
# scripts/floor-bash: which bash is the floor -- a system bash 3 (macOS's /bin/bash), else the kept 3.2.57 build --
# and a host with neither (Linux: ci-runner's landing checks) answering exit 1 with the fix, never another bash. The
# system bash and the build are stubs reporting the version given; --install's real build (~1 min) is not run here:
# the stub `cc` it finds missing, or a tarball failing its checksum, stops it before anything is built.
# Run: bash scripts/floor-bash-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m floor-bash\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
fake_bash() { # <path> <major.minor.patch>: a "bash" whose BASH_VERSINFO reads as given
  mkdir -p "$(dirname "$1")"
  printf '#!/bin/sh\necho %s\n' "$2" > "$1"
  chmod +x "$1"
}
# [env <assignments> <bash> <floor-bash> <args>] -- with nothing given, scripts/floor-bash itself
floor() {
  [ $# -gt 0 ] || set -- env bash "$REPO_ROOT/scripts/floor-bash"
  FLOOR_BASH_SYSTEM="$work/system-bash" FLOOR_BASH_DIR="$work/build" "$@" 2> "$work/err"
}

fake_bash "$work/system-bash" 3.2.57
check "a system bash 3 (macOS's /bin/bash) is the floor" "$work/system-bash" "$(floor)"

fake_bash "$work/system-bash" 5.2.21
check "a bash 5 (Linux) is never the floor: exit 1, naming --install" "1 1" \
  "$(floor > /dev/null; echo $?) $(grep -c 'scripts/floor-bash --install' "$work/err")"
fake_bash "$work/build/bin/bash" 3.2.57
check "...the kept 3.2.57 build is" "$work/build/bin/bash" "$(floor)"
fake_bash "$work/build/bin/bash" 3.2.48
check "...a build of another 3.2 is not" "1" "$(floor > /dev/null; echo $?)"
rm -rf "$work/build"

# --install stops before building when a tool is missing: a PATH with no cc
mkdir -p "$work/nocc"
for tool in make curl mktemp tar sha256sum dirname getconf rm; do
  p="$(command -v "$tool")" && ln -sf "$p" "$work/nocc/$tool"
done
check "--install with no cc fails, naming it" "1 1" \
  "$(floor env PATH="$work/nocc" "$BASH" "$REPO_ROOT/scripts/floor-bash" --install > /dev/null; echo $?) $(grep -c 'needs cc' "$work/err")"
check "an unknown argument is a usage error" "64" "$(floor env "$BASH" "$REPO_ROOT/scripts/floor-bash" --nope > /dev/null; echo $?)"

# --install's build, over stubs: a tarball whose configure does nothing, a `make` that records how it was asked and
# installs a bash reporting 3.2.57 where its prefix says (FLOOR_MAKE_FAIL: the build step fails as 3.2's -j race did)
src="$work/src/bash-3.2.57"; mkdir -p "$src/support" "$work/tools"
printf '#!/bin/sh\nexit 0\n' > "$src/configure"; printf '#!/bin/sh\necho "$1"\n' > "$src/support/config.sub"; chmod +x "$src/configure"
tar -C "$work/src" -czf "$work/bash.tar.gz" bash-3.2.57
tarball_sha="$(shasum -a 256 "$work/bash.tar.gz" 2> /dev/null | cut -d' ' -f1 || sha256sum "$work/bash.tar.gz" | cut -d' ' -f1)"
printf '#!/bin/sh\necho x86_64-linux-gnu\n' > "$work/tools/cc"; printf '#!/bin/sh\nexit 0\n' > "$work/tools/bison"
cat > "$work/tools/make" <<'MAKE'
#!/bin/sh
echo "make $*" >> "$MAKE_CALLS"
case " $* " in
  *" install "*) for a in "$@"; do case "$a" in prefix=*) p="${a#prefix=}" ;; esac; done
                 mkdir -p "$p/bin"; printf '#!/bin/sh\necho 3.2.57\n' > "$p/bin/bash"; chmod +x "$p/bin/bash" ;;
  *) sleep 1; [ -z "${FLOOR_MAKE_FAIL:-}" ] || { echo "mv: cannot stat 'newversion.h': No such file or directory"; exit 2; } ;;
esac
MAKE
chmod +x "$work/tools/"*
install_floor() { # [env assignments...]: --install over the stubs into $work/cache
  env PATH="$work/tools:$PATH" MAKE_CALLS="$work/make-calls" FLOOR_BASH_SYSTEM="$work/system-bash" \
    FLOOR_BASH_DIR="$work/cache/bash-3.2.57" FLOOR_BASH_TARBALL_URL="file://$work/bash.tar.gz" \
    FLOOR_BASH_TARBALL_SHA256="$tarball_sha" "$@" bash "$REPO_ROOT/scripts/floor-bash" --install
}
: > "$work/make-calls"
check "--install builds serially (-j1: 3.2's makefile races itself) and installs into a staging dir renamed into place" \
  "$work/cache/bash-3.2.57/bin/bash|make -s -j1|1|0" \
  "$(install_floor 2> "$work/err")|$(grep -c -- '-j1' "$work/make-calls" | sed 's/^1$/make -s -j1/')|$(grep -c "install prefix=$work/cache/bash-3.2.57.build." "$work/make-calls")|$(find "$work/cache" -maxdepth 1 -name '*.build.*' | grep -c .)"
rm -rf "$work/cache"; : > "$work/make-calls"
install_floor > "$work/one" 2> /dev/null & first=$!
install_floor > "$work/two" 2> /dev/null & second=$!
wait "$first"; s1=$?; wait "$second"; s2=$?
check "two builders at once: one builds, the other waits and finds its build" "0 0 1 same" \
  "$s1 $s2 $(grep -vc install "$work/make-calls") $([ "$(cat "$work/one")" = "$(cat "$work/two")" ] && echo same)"
rm -rf "$work/cache"
check "a failing build names its step, exit and last line, leaves no floor and no staging dir behind" \
  "1|floor-bash: building bash 3.2.57 failed at make (exit 2): mv: cannot stat 'newversion.h': No such file or directory|absent 0" \
  "$(install_floor FLOOR_MAKE_FAIL=1 > /dev/null 2> "$work/err"; echo $?)|$(grep '^floor-bash: building' "$work/err")|$([ -e "$work/cache/bash-3.2.57" ] && echo present || echo absent) $(find "$work/cache" -maxdepth 1 -name '*.build.*' | grep -c .)"
check "...on GitHub Actions also as an annotation" "1" \
  "$(install_floor FLOOR_MAKE_FAIL=1 GITHUB_ACTIONS=true > /dev/null 2> "$work/err"; grep -c '^::error title=floor bash::building bash 3.2.57 failed at make (exit 2)' "$work/err")"
check "...and the landing check's step says which hook check it failed (pre-push floor FAILED), never a nameless job" "1" \
  "$(grep -c 'pre-push floor FAILED' "$REPO_ROOT/.github/actions/install-floor-bash/action.yml")"

# The floor is one thing everywhere: the hook asks scripts/floor-bash, and so does the nightly macOS step.
# shellcheck disable=SC2016 # the workflow's text, not expanded
check "tool-self-tests.yml's macOS step runs the self-tests under scripts/floor-bash's bash" "1" \
  "$(grep -c 'scripts/run-script-tests --bash "$(scripts/floor-bash)"' "$REPO_ROOT/.github/workflows/tool-self-tests.yml")"
check "the landing check's tools build it (.github/actions/install-floor-bash, from hook-tools)" "1" \
  "$(grep -c 'uses: ./.github/actions/install-floor-bash' "$REPO_ROOT/.github/actions/hook-tools/action.yml")"

spec_summary
