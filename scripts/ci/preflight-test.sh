#!/usr/bin/env bash
# preflight.sh: secrets present or not (never printed), and which unaccepted Android SDK licences
# fail the build's preflight. sdkmanager is a stub replaying the shapes a real one prints.
# Run: bash scripts/ci/preflight-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m preflight.sh\n'

stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT

# sdkmanager_says <output>: the stub sdkmanager prints <output> for `--licenses`.
sdkmanager_says() {
  printf '#!/usr/bin/env bash\ncat >/dev/null\ncat <<'"'"'OUT'"'"'\n%s\nOUT\n' "$1" > "$stub/sdkmanager"
  chmod +x "$stub/sdkmanager"
}

# run <env assignments...> -- <args...>  -> "<exit status>"
run() {
  local envs=()
  while [ "$1" != "--" ]; do envs+=("$1"); shift; done
  shift
  env -i PATH="$stub:/usr/bin:/bin" "${envs[@]+"${envs[@]}"}" bash "$REPO_ROOT/scripts/ci/preflight.sh" "$@" >"$stub/out" 2>&1
  echo $?
}

check "every named secret present passes" "0" "$(run A=1 B=x -- A B)"
check "an unset secret fails" "1" "$(run A=1 -- A B)"
check "an EMPTY secret fails, not just an unset one" "1" "$(run A=1 B= -- A B)"
run A=1 -- A B >/dev/null
check "the failure names the missing secret" "1" "$(grep -c '::error::secret B is unset or empty' "$stub/out")"
run SIGNING_KEY=hunter2-secret-value -- SIGNING_KEY >/dev/null
check "a present secret's value is never printed" "0" "$(grep -c 'hunter2' "$stub/out")"
check "a name that is not a variable name is refused" "1" "$(run -- 'A;id')"

sdkmanager_says "All SDK package licenses accepted."
check "all licences accepted passes" "0" "$(run -- --android-sdk-licence)"

# What run 35509874855 printed on a healthy runner: six unaccepted, none of them needed. Every
# one after the first follows the previous prompt on the same line.
sdkmanager_says "6 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/6: License android-googletv-license:
Accept? (y/N): 2/6: License android-googlexr-license:
Accept? (y/N): 3/6: License android-sdk-arm-dbt-license:
Accept? (y/N): 4/6: License android-sdk-preview-license:
Accept? (y/N): 5/6: License google-gdk-license:
Accept? (y/N): 6/6: License mips-android-sysimage-license:
Accept? (y/N): "
check "a healthy runner's six unneeded licences pass" "0" "$(run -- --android-sdk-licence)"

sdkmanager_says "2 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/2: License android-googletv-license:
Accept? (y/N): 2/2: License android-sdk-license:
Accept? (y/N): "
check "a needed licence named mid-line, after a prompt, is caught" "1" "$(run -- --android-sdk-licence)"

sdkmanager_says "5 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/5: License android-googletv-license:
---------------------------------------
Accept? (y/N): 2/5: License google-gdk-license:"
check "licences the build never installs do not fail it" "0" "$(run -- --android-sdk-licence)"

sdkmanager_says "1 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/1: License android-sdk-license:
Accept? (y/N): "
check "an unaccepted android-sdk-license fails" "1" "$(run -- --android-sdk-licence)"
run -- --android-sdk-licence >/dev/null
check "and says which licence" "1" "$(grep -c 'licence android-sdk-license is not accepted' "$stub/out")"

sdkmanager_says "Warning: Could not create settings
java.lang.IllegalArgumentException"
check "output it cannot read fails rather than passing blind" "1" "$(run -- --android-sdk-licence)"

rm -f "$stub/sdkmanager"
check "no sdkmanager at all fails" "1" "$(run -- --android-sdk-licence)"

spec_summary
