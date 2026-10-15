#!/usr/bin/env bash
# Shellcheck the `run:` blocks of composite actions (.github/actions/*/action.yml).
#
#   scripts/ci/shellcheck-action-runs.sh [action.yml ...]    # default: every composite action
#
# WHY. actionlint shellchecks a WORKFLOW's `run:` blocks but not a composite action's — it does
# not lint action.yml files at all — so the ~40 bash steps in .github/actions/ had no lint: the
# pre-push hook and CI ran actionlint over workflows only. This extracts each block as actionlint
# does for a workflow (every `${{ … }}` expression becomes a same-length run of underscores, the
# script runs under GitHub's `bash -e -o pipefail`) and shellchecks it, reporting findings at
# their line in the action file.
#
# SHELLCHECK_OPTS is honoured as shellcheck itself honours it; the default here is `-S warning`,
# the pre-push hook's and CI's threshold. Exits 1 on any finding, 0 when clean.
# Tested by scripts/ci/shellcheck-action-runs-test.sh.
set -euo pipefail

command -v shellcheck >/dev/null || { echo "shellcheck-action-runs: shellcheck is not installed" >&2; exit 2; }
export SHELLCHECK_OPTS="${SHELLCHECK_OPTS:--S warning}"

if [ "$#" -eq 0 ]; then
  repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
  set -- "$repo"/.github/actions/*/action.yml
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Writes $work/<n>.sh per run block, and "<n> <file> <yml line of the block's first line>" to $work/map.
n=0
for action in "$@"; do
  awk -v out="$work" -v file="$action" -v start="$n" '
    function flush() {
      if (inblock) { close(script); inblock = 0 }
    }
    function expressions(s) {
      while (match(s, /\$\{\{[^}]*\}\}/)) {
        pad = sprintf("%" RLENGTH "s", ""); gsub(/ /, "_", pad)
        s = substr(s, 1, RSTART - 1) pad substr(s, RSTART + RLENGTH)
      }
      return s
    }
    function open_script(line) {
      count++; script = out "/" (start + count) ".sh"
      print "#!/usr/bin/env bash" > script
      print "set -eo pipefail" > script
      print (start + count) " " file " " line >> (out "/map")
    }
    BEGIN { count = 0; inblock = 0 }
    {
      if (inblock) {
        indent = match($0, /[^ ]/) - 1
        if ($0 ~ /^[ ]*$/) { print "" > script; next }
        if (indent > keyindent) {
          if (bodyindent < 0) bodyindent = indent
          print expressions(substr($0, bodyindent + 1)) > script
          next
        }
        flush()
      }
      if (match($0, /^[ ]*(- )?run:[ ]*/)) {
        value = substr($0, RLENGTH + 1)
        keyindent = match($0, /[^ -]/) - 1
        if (value ~ /^[|>][-+]?[ ]*$/) {
          open_script(NR + 1); inblock = 1; bodyindent = -1
        } else {
          open_script(NR)
          if (value ~ /^'\''.*'\''[ ]*$/) { sub(/[ ]*$/, "", value); value = substr(value, 2, length(value) - 2); gsub(/'\'''\''/, "'\''", value) }
          else if (value ~ /^".*"[ ]*$/) { sub(/[ ]*$/, "", value); value = substr(value, 2, length(value) - 2) }
          print expressions(value) > script
          close(script)
        }
      }
    }
    END { flush(); print count > (out "/count") }
  ' "$action"
  n=$((n + $(cat "$work/count")))
done

status=0
[ -f "$work/map" ] || exit 0
while read -r id file line; do
  # gcc format: <path>:<line>:<col>: <level>: <message>. Two header lines precede the block.
  # Excluded as actionlint excludes them for a workflow's blocks: each misfires on what an
  # expression or the step's `env:` puts there (a constant `[ "___" = true ]`, an env var unset in
  # the script text).
  if ! findings="$(cd "$(dirname "$file")" && shellcheck -f gcc -e SC1091,SC2194,SC2050,SC2154,SC2157 "$work/$id.sh")"; then
    status=1
    # The column is the block's, not the file's (YAML indentation is stripped), so it is left out.
    while IFS=: read -r _ at _ rest; do
      echo "$file:$((line + at - 3)):$rest"
    done <<< "$findings"
  fi
done < "$work/map"
exit "$status"
