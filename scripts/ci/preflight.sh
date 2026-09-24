#!/usr/bin/env bash
# PREFLIGHT: fail a workflow in seconds, at its first job, when it cannot succeed for want of
# configuration rather than code — a secret that is unset or empty, or an Android SDK licence the
# runner has not accepted. Without this those surface minutes in, as a signing step producing an
# unsigned APK, a `flyctl deploy` refusing an empty token, or Gradle declining to install a
# platform, and read like build failures.
#
# Usage:
#   preflight.sh [--android-sdk-licence] NAME...
#
#   NAME...                 environment variables that must be set and non-empty. The caller maps
#                           each secret into the step's `env:`; this script prints only the
#                           NAMES, never a value, not even its length.
#   --android-sdk-licence   also require the SDK licence Gradle needs (android-sdk-license) to be
#                           accepted, as `sdkmanager --licenses` reports it. Run it after the same
#                           android-actions/setup-android step the build uses.
#
# Tested by scripts/ci/preflight-test.sh against a stub sdkmanager.
set -uo pipefail

check_licences=false
names=()
for arg in "$@"; do
    case "$arg" in
        --android-sdk-licence) check_licences=true ;;
        -*) echo "unknown option $arg" >&2; exit 2 ;;
        *) names+=("$arg") ;;
    esac
done

problems=0
error() { echo "::error::$*"; problems=$((problems + 1)); }

for name in "${names[@]+"${names[@]}"}"; do
    if [[ ! "$name" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
        error "'$name' is not an environment variable name"
    elif [ -z "${!name:-}" ]; then
        error "secret $name is unset or empty — add it under Settings > Secrets and variables > Actions"
    else
        echo "secret $name: present"
    fi
done

# The licence Gradle's SDK auto-install needs for platforms, build-tools and the NDK. Everything
# else sdkmanager lists (Google TV, XR, MIPS, and the PREVIEW licence, which only preview packages
# carry) is for packages this project never installs; a healthy runner lists six of them.
RequiredLicences="android-sdk-license"

if [ "$check_licences" = true ]; then
    sdk="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-}}"
    sdkmanager=$(command -v sdkmanager || true)
    [ -z "$sdkmanager" ] && [ -n "$sdk" ] && [ -x "$sdk/cmdline-tools/latest/bin/sdkmanager" ] \
        && sdkmanager="$sdk/cmdline-tools/latest/bin/sdkmanager"
    if [ -z "$sdkmanager" ]; then
        error "no sdkmanager on PATH or under \$ANDROID_HOME — cannot check the SDK licences"
    else
        # Answer "y" to reviewing the unaccepted licences and "n" to each, so every one is NAMED
        # ("1/6: License android-googletv-license:") and none is accepted by this check.
        out="$({ echo y; yes n 2>/dev/null | head -n 100; } | "$sdkmanager" --licenses 2>&1)"
        unaccepted="$(printf '%s\n' "$out" | sed -n 's/.*[0-9]\/[0-9]*: License \([A-Za-z0-9._-]*\):.*/\1/p' | sort -u)"
        # Here-strings, not `printf | grep -q`: grep -q exits at its first match, printf dies of
        # SIGPIPE on a licence text this long, and pipefail reports the MATCH as a failure.
        if ! grep -qE 'All SDK package licenses accepted|licenses not accepted' <<< "$out"; then
            error "sdkmanager --licenses said neither 'all accepted' nor 'N not accepted' — cannot tell:"
            printf '%s\n' "$out" | tail -n 20
        fi
        for licence in $RequiredLicences; do
            if grep -qx "$licence" <<< "$unaccepted"; then
                error "Android SDK licence $licence is not accepted — Gradle cannot install the platform or build-tools it needs"
            fi
        done
        [ -n "$unaccepted" ] && echo "unaccepted licences this build does not need: $(printf '%s\n' "$unaccepted" | grep -vxF -f <(tr ' ' '\n' <<< "$RequiredLicences") | tr '\n' ' ')"
        echo "Android SDK licence checked: $RequiredLicences"
    fi
fi

if [ "$problems" -gt 0 ]; then
    echo "preflight: $problems problem(s) — nothing after this job can succeed until they are fixed."
    exit 1
fi
echo "preflight: ok"
