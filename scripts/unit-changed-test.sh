#!/usr/bin/env bash
# Self-test for scripts/unit-changed over a scratch repository: the paths committed since the merge base, the
# uncommitted and the untracked go to the hook's --unit-plan, and its commands to one sbt-warm call; --plan prints them;
# nothing touched runs nothing.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/scratch-git.sh
. "$REPO_ROOT/scripts/scratch-git.sh"

testroot="$(cd "$(mktemp -d)" && pwd -P)"; trap 'rm -rf "$testroot"' EXIT
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

origin="$testroot/origin.git"; repo="$testroot/repo"; log="$testroot/log"
scratch_repo "$origin" --bare
scratch_repo "$repo" -b main
mkdir -p "$repo/scripts/hooks"
cp "$REPO_ROOT/scripts/unit-changed" "$REPO_ROOT/scripts/tool-conventions.sh" "$repo/scripts/"
# The plan stub: a test command per module named by a path's first segment, the paths it was handed logged.
cat > "$repo/scripts/hooks/pre-push" <<STUB
#!/usr/bin/env bash
[ "\$1" = --unit-plan ] || exit 9
while IFS= read -r path; do echo "path \$path" >> "$log"; case "\$path" in */src/*) echo "\${path%%/*}/Test/test" ;; esac; done | sort -u
STUB
printf '#!/usr/bin/env bash\nprintf "sbt-warm"; printf " [%%s]" "$@"; echo\n' > "$repo/scripts/sbt-warm"
chmod +x "$repo/scripts/hooks/pre-push" "$repo/scripts/sbt-warm"
echo a > "$repo/a.txt"; scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
scratch_git "$repo" remote add origin "$origin"; scratch_git "$repo" push -q origin HEAD:main; scratch_git "$repo" fetch -q origin

out="$(cd "$repo" && scripts/unit-changed)"
check "nothing touched: says so, runs nothing" "yes" "$(grep -q 'no unit tests to run' <<< "$out" && ! grep -q sbt-warm <<< "$out" && echo yes || echo no)"

mkdir -p "$repo/common/src" "$repo/worker/src" "$repo/web/src"
echo x > "$repo/common/src/A.scala"; scratch_git "$repo" add -A; scratch_git "$repo" commit -qm committed
echo y > "$repo/worker/src/B.scala"; scratch_git "$repo" add worker; echo z > "$repo/web/src/C.scala"
: > "$log"
out="$(cd "$repo" && scripts/unit-changed)"
check "committed, staged and untracked paths: each module's command, in one sbt-warm call" \
  "sbt-warm [common/Test/test] [web/Test/test] [worker/Test/test]" "$(tail -1 <<< "$out")"
check "...the plan handed each path once" "3" "$(grep -c '^path ' "$log")"
check "--plan prints the commands, runs nothing" "common/Test/test|web/Test/test|worker/Test/test" \
  "$(cd "$repo" && scripts/unit-changed --plan | paste -sd'|' -)"

if [ "$fails" -ne 0 ]; then echo "unit-changed: $fails failed"; exit 1; fi
echo "unit-changed: all checks passed"
