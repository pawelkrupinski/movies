#!/usr/bin/env bash
# After an sbt test run FAILED: rerun each failed test, alone, three times, in this same job, and
# say whether it is DETERMINISTIC or FLAKY (scripts/ci/flake_verdict.py).
#
# The job stays red either way — the step that ran the suite already failed it, and nothing here
# can turn that green. A rerun is never a fix; what this adds is the answer to "would it have
# passed on a rerun?", recorded while the run is still here to ask. Eight Main runs in four weeks
# went green on a rerun of the same SHA, and each of those flakes was forgotten the moment it did.
#
# Usage: rerun-failed-sbt.sh <junit dir> <label> [<out dir>]
#   <junit dir>  where the failed run wrote its reports (target/test-reports/unit or …/it)
#   <label>      names the suite in the job summary and the ledger ("unit tests", "e2e (rest)")
#   <out dir>    where to keep the reruns' reports and flaky.tsv (default: target/flake-rerun)
#
# Each rerun is `<module>/<Config>/testOnly <class> -- -z <test name>` through one sbt server
# (`sbt --client`), so the three reruns of a test cost seconds each rather than an sbt boot each.
# More than MaxTests failures is a broken build, not a flake hunt: those are reported NOT RERUN.
set -uo pipefail

reports="${1:?junit report dir}"
label="${2:?label}"
out="${3:-target/flake-rerun}"
Reruns=3
MaxTests=5

cd "$(git rev-parse --show-toplevel)" || exit 1
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

mkdir -p "$out/original" "$out/rerun"
# The reruns rewrite TEST-<class>.xml in place; keep the failed run's reports to judge against,
# and put them back at the end so the published check-run shows the run that actually failed.
cp -R "$reports/." "$out/original/"

failed="$(python3 "$here/flake_verdict.py" failed "$out/original")"
if [ -z "$failed" ]; then
    echo "No failed test in $reports — the failure was not a test (compile error, timeout, a guard step)."
    python3 "$here/flake_verdict.py" verdict --label "$label" --original "$out/original" --rerun "$out/rerun"
    exit 0
fi

# <module> <Config> for a test class, from where its source lives.
locate() {
    local cls="$1" simple path file
    simple="${cls##*.}"
    path="$(tr . / <<< "$cls").scala"
    file=""
    for candidate in */src/*/scala/"$path"; do
        [ -f "$candidate" ] && { file="$candidate"; break; }
    done
    # A class in a file named for something else.
    [ -z "$file" ] && file="$(git grep --untracked -lE "(class|object) $simple\\b" -- '*/src/test/*.scala' '*/src/it/*.scala' '*/src/page/*.scala' | head -n 1)"
    [ -z "$file" ] && return 1
    local module="${file%%/*}" scope
    scope="$(cut -d/ -f3 <<< "$file")"
    case "$scope" in
        test) echo "$module Test" ;;
        it)   echo "$module IntegrationTest" ;;
        page) echo "$module PageTest" ;;
        *)    return 1 ;;
    esac
}

count=0
while IFS=$'\x1f' read -r _project cls name; do
    count=$((count + 1))
    if [ "$count" -gt "$MaxTests" ]; then
        echo "More than $MaxTests failed tests — not rerunning the rest."
        break
    fi
    if ! where="$(locate "$cls")"; then
        echo "::warning::cannot find the source of $cls — not rerun"
        continue
    fi
    read -r module config <<< "$where"
    # sbt's argument parser takes a double-quoted string; escape what would end it.
    quoted="\"$(sed 's/\\/\\\\/g; s/"/\\"/g' <<< "$name")\""
    for k in $(seq 1 "$Reruns"); do
        echo "::group::rerun $k/$Reruns: $cls — $name"
        rm -f "$reports/TEST-$cls.xml"
        sbt --client "$module/$config/testOnly $cls -- -z $quoted"
        echo "::endgroup::"
        if [ -f "$reports/TEST-$cls.xml" ]; then
            cp "$reports/TEST-$cls.xml" "$out/rerun/$count-$k-TEST-$cls.xml"
        else
            # No report at all (the rerun could not even start): count it as a failure of this test.
            printf '<testsuite name="%s"><testcase classname="%s" name="%s"><failure message="rerun wrote no report"/></testcase></testsuite>\n' \
                "$cls" "$cls" "$(sed 's/&/\&amp;/g; s/</\&lt;/g; s/"/\&quot;/g' <<< "$name")" > "$out/rerun/$count-$k-TEST-$cls.xml"
        fi
    done
done <<< "$failed"
sbt --client shutdown >/dev/null 2>&1 || true

cp -R "$out/original/." "$reports/"
python3 "$here/flake_verdict.py" verdict --label "$label" \
    --original "$out/original" --rerun "$out/rerun" --flaky-out "$out/flaky.tsv"
