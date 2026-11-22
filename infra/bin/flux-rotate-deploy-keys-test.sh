#!/usr/bin/env bash
# infra/bin/flux-rotate-deploy-keys: a dry run touches nothing; --apply mints both keys, hands them to Flux's
# Secrets and only then removes the old keys; it refuses to rotate for sources not on GitHub, or for a github.com
# host key GitHub does not publish. gh, ssh (the cluster's kubectl runs over ssh) and ssh-keygen are fakes.
# Run: bash infra/bin/flux-rotate-deploy-keys-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m flux-rotate-deploy-keys\n'
work="$(mktemp -d)"; work="$(cd "$work" && pwd -P)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/state"
url=ssh://git@github.com/pawelkrupinski/movies-gitops.git

# ssh-keygen: a fake (CI's script-test image has no OpenSSH). `-f <path>` writes a key pair named by the path; `-lf
# <file|->` prints a fingerprint, `SHA256:<cksum of the key>` (the field after its `ssh-` type).
cat > "$work/bin/ssh-keygen" <<'EOF'
#!/usr/bin/env bash
case " $* " in
  *" -lf "*)
    file=${*: -1}; key=$( { if [ "$file" = - ]; then cat; else cat "$file"; fi; } | awk '{ for (i = 1; i < NF; i++) if ($i ~ /^ssh-/) { print $(i + 1); exit } }')
    echo "256 SHA256:$(printf '%s' "$key" | cksum | cut -d' ' -f1) comment (ED25519)" ;;
  *)
    file=${*: -1}; echo private > "$file"; echo "ssh-ed25519 KEY$(basename "$file")$RANDOM comment" > "$file.pub" ;;
esac
EOF
chmod +x "$work/bin/ssh-keygen"
fingerprint() { printf '%s' "$1" | cksum | cut -d' ' -f1; }

# github.com's host key as the fake cluster scans it, and the fingerprint the fake /meta publishes
echo "github.com ssh-ed25519 HOSTKEY" > "$work/state/keyscan"
fingerprint HOSTKEY > "$work/state/meta"

cat > "$work/bin/gh" <<'EOF'
#!/usr/bin/env bash
echo "gh $*" >> "$STATE/calls"
case "$1 $2" in
  "auth status") exit 0 ;;
  "repo deploy-key")
    case "$3" in
      list) printf '17\tflux-git-read (kinowo k3s) 20261031T150000Z\t(read_only true)\n' ;;
      add | delete) ;;
    esac ;;
  "api meta") cat "$STATE/meta" ;;
  "api repos/pawelkrupinski/movies-gitops") echo "  repo pawelkrupinski/movies-gitops" ;;
  "api -H")
    case "$4" in
      *gotk-sync.yaml*) printf 'url: %s\nname: flux-git-read\n' "$URL" ;;
      *automation.yaml*) printf 'url: %s\nname: flux-git-write\n' "$URL" ;;
    esac ;;
esac
EOF
cat > "$work/bin/ssh" <<'EOF'
#!/usr/bin/env bash
cmd=${*: -1}
echo "ssh $cmd" >> "$STATE/calls"
case "$cmd" in
  *ssh-keyscan*) cat "$STATE/keyscan" ;;
  *"get gitrepository movies-write"*) echo "$URL flux-git-write" ;;
  *"get gitrepository movies "*) echo "${MOVIES_SOURCE:-$URL flux-git-read}" ;;
  *"apply -f -"*) cat >> "$STATE/applied" ;;
esac
EOF
chmod +x "$work/bin/gh" "$work/bin/ssh"

rotate() {
  rm -f "$work/state/calls" "$work/state/applied"
  env STATE="$work/state" URL="$url" PATH="$work/bin:$PATH" "$REPO_ROOT/infra/bin/flux-rotate-deploy-keys" "$@" \
    > "$work/out" 2>&1
  echo $?
}
calls() { grep -c "$1" "$work/state/calls" 2>/dev/null | tr -d ' '; }
line_of() { grep -n "$1" "$work/state/calls" | head -1 | cut -d: -f1; }

check "a dry run exits 0" 0 "$(rotate)"
check "...registers, deletes and asks the cluster nothing" "0 0 0" \
  "$(calls 'deploy-key add') $(calls 'deploy-key delete') $(calls '^ssh ')"
check "...and names the old key it would remove" 1 "$(grep -c 'would: gh repo deploy-key delete 17' "$work/out")"

check "--apply exits 0" 0 "$(rotate --apply)"
check "...registers a read and a write key" "2 1" "$(calls 'deploy-key add') $(calls 'deploy-key add .*--allow-write')"
check "...hands both to Flux's Secrets" "1 1" \
  "$(grep -c '"name":"flux-git-read"' <(jq -c . "$work/state/applied")) $(grep -c '"name":"flux-git-write"' <(jq -c . "$work/state/applied"))"
check "...removes the old key only after both Secrets are applied" "true" \
  "$([ "$(line_of 'deploy-key delete 17')" -gt "$(grep -n 'apply -f -' "$work/state/calls" | tail -1 | cut -d: -f1)" ] && echo true || echo false)"
check "...and moves no source: no manifest applied, no GitRepository patched" "0 0" \
  "$(grep -c '"kind":"GitRepository"' <(jq -c . "$work/state/applied")) $(calls 'patch gitrepository')"

check "a source not on GitHub's URL is refused" 1 "$(MOVIES_SOURCE="https://forge.example/movies-gitops.git flux-git-read" rotate --apply)"
check "...before any key is minted" 0 "$(calls 'deploy-key add')"

echo "github.com ssh-ed25519 OTHERKEY" > "$work/state/keyscan"
check "a github.com host key GitHub does not publish is refused" 1 "$(rotate --apply)"
check "...before any key is registered" 0 "$(calls 'deploy-key add')"

spec_summary
