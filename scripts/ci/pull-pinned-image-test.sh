#!/usr/bin/env bash
# pull-pinned-image.sh against a stub `docker` whose registries serve, refuse, stall or hand over the wrong bytes on cue.
# Run: bash scripts/ci/pull-pinned-image-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m pull-pinned-image.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
digest=sha256:$(printf 'a%.0s' $(seq 64))
printf '# a comment\nmongo\tdocker.io/library/mongo:8.3.17@%s\tghcr.io/owner/mongo\n' "$digest" > "$stub_dir/pins.tsv"
export PINNED_IMAGES="$stub_dir/pins.tsv"
ghcr="ghcr.io/owner/mongo@$digest"
gcr="mirror.gcr.io/library/mongo@$digest"
hub="docker.io/library/mongo@$digest"

# STUB_GHCR / STUB_GCR / STUB_HUB: `up` serves the pull, `down` refuses it, `stalls` never ends it, `wrong` serves an
# image whose RepoDigests name another digest; STUB_GHCR=private refuses a logged-in pull and serves an anonymous one.
# STUB_DAEMON_MIRROR is the daemon's own Docker Hub mirror. A `login` records what it read on stdin in $STUB_LOGIN.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
ref="${*: -1}"
state() { case "$1" in ghcr.io/*) echo "${STUB_GHCR:-up}" ;; mirror.gcr.io/*) echo "${STUB_GCR:-up}" ;; *) echo "${STUB_HUB:-up}" ;; esac; }
case "$1" in
  pull)
    case "$(state "$ref")" in
      down) exit 1 ;;
      stalls) exec sleep 600 ;;
      private) [ -e "$STUB_LOG.loggedin" ] && exit 1 ;;
    esac
    echo "$ref" >> "$STUB_LOG.pulled"; exit 0 ;;
  image)
    grep -qxF -- "$ref" "$STUB_LOG.pulled" 2>/dev/null || exit 1
    [ "$(state "$ref")" = wrong ] && { echo "${ref%@*}@sha256:$(printf 'b%.0s' $(seq 64))"; exit 0; }
    echo "$ref"; exit 0 ;;
  info) [ -n "${STUB_DAEMON_MIRROR:-}" ] && echo "$STUB_DAEMON_MIRROR"; exit 0 ;;
  login) cat >> "$STUB_LOGIN"; [ "$2" = ghcr.io ] && : > "$STUB_LOG.loggedin"; exit 0 ;;
  logout) rm -f "$STUB_LOG.loggedin"; exit 0 ;;
esac
exit 0
STUB
chmod +x "$stub_dir/docker"
export STUB_LOG="$stub_dir/log" STUB_LOGIN="$stub_dir/login"

# pull [args...] -> "<exit status> <stdout>"
pull() {
  : > "$STUB_LOG"; : > "$STUB_LOGIN"; rm -f "$STUB_LOG.pulled" "$STUB_LOG.loggedin"
  local out status
  out=$(PATH="$stub_dir:$PATH" PINNED_PULL_SECONDS=1 PINNED_GHCR_PULL_SECONDS=1 \
    bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" mongo "$@" 2> "$stub_dir/err")
  status=$?
  echo "$status $out"
}
pulls() { grep '^pull' "$STUB_LOG" | sed 's/^pull -q //' | tr '\n' ' ' | sed 's/ $//'; }

check "the pin's fields, read without pulling" "docker.io/library/mongo 8.3.17 $digest ghcr.io/owner/mongo" \
  "$(for f in repository tag digest mirror; do bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" --pin mongo $f; done | tr '\n' ' ' | sed 's/ $//')"
check "...and an unknown name refused" "1" "$(bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" --pin redis tag 2>/dev/null; echo $?)"

check "the GHCR mirror serves the pinned digest first, its ref on stdout" "0 $ghcr" "$(pull)"
check "...pulling nothing else" "$ghcr" "$(pulls)"
check "...logged in with the job's token, on stdin" "0 $ghcr|tok" "$(GHCR_TOKEN=tok GITHUB_ACTOR=me pull)|$(cat "$STUB_LOGIN")"
check "...as the actor, never with the token on a command line" "1 0" \
  "$(grep -c '^login ghcr.io -u me --password-stdin$' "$STUB_LOG") $(grep -c tok "$STUB_LOG")"
check "...and without a token, anonymously" "0" "$(pull >/dev/null; grep -c '^login' "$STUB_LOG")"
check "a token GHCR refuses (no packages: read): the last try is anonymous" "0 $ghcr" "$(STUB_GHCR=private GHCR_TOKEN=tok pull)"
check "...after logging out" "1" "$(grep -c '^logout ghcr.io' "$STUB_LOG")"

check "a GHCR that refuses falls back to mirror.gcr.io" "0 $gcr" "$(STUB_GHCR=down pull)"
check "...after its tries" "$ghcr $ghcr $gcr" "$(pulls)"
check "...saying so" "1" "$(grep -c "^::warning::mongo: the GHCR mirror (ghcr.io/owner/mongo) did not serve" "$stub_dir/err")"
check "a GHCR pull that stalls is bounded, and falls back too" "0 $gcr" "$(STUB_GHCR=stalls pull)"
check "...saying it stalled" "2" "$(grep -c 'stalled past 1s' "$stub_dir/err")"
check "GHCR and mirror.gcr.io both failing: Docker Hub (run 44526198751)" "0 $hub" "$(STUB_GHCR=down STUB_GCR=stalls pull)"
check "...logged in as the CI account when its token is set" "0 $hub|s3cret" \
  "$(STUB_GHCR=down STUB_GCR=down DOCKERHUB_USERNAME=ci-user DOCKERHUB_TOKEN=s3cret pull)|$(cat "$STUB_LOGIN")"
check "...the token on stdin only" "1 0" "$(grep -c '^login docker.io -u ci-user --password-stdin$' "$STUB_LOG") $(grep -c s3cret "$STUB_LOG")"
check "...and anonymously without one" "0" "$(STUB_GHCR=down STUB_GCR=down pull >/dev/null; grep -c '^login' "$STUB_LOG")"
check "no source serving it fails, naming the pin" "1 " "$(STUB_GHCR=down STUB_GCR=down STUB_HUB=down pull)"
check "...as an error" "1" "$(grep -c "^::error::no source served mongo (docker.io/library/mongo@$digest)" "$stub_dir/err")"

check "an image whose digest is not the pin is refused, and the next source tried" "0 $gcr" "$(STUB_GHCR=wrong PINNED_GHCR_ATTEMPTS=1 pull)"
check "...saying which" "1" "$(grep -c "^::error::$ghcr was pulled but does not carry $digest" "$stub_dir/err")"

check "an image already here is used as it is, nothing pulled" "$gcr 0" \
  "$(: > "$STUB_LOG"; echo "$gcr" > "$STUB_LOG.pulled"
     echo "$(PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" mongo 2>/dev/null) $(grep -c '^pull' "$STUB_LOG")")"
check "a daemon with a Docker Hub mirror of its own pulls Docker Hub's name first, through it" "0 $hub" \
  "$(STUB_DAEMON_MIRROR=http://mirror.internal:3012/ pull)"
check "...trying no other registry first" "$hub" "$(pulls)"

check "-- runs the command after the pull, the ref in PINNED_IMAGE" "0 ran $ghcr" "$(pull -- bash -c 'echo ran "$PINNED_IMAGE"')"
check "...and not when the pull failed" "1 " "$(STUB_GHCR=down STUB_GCR=down STUB_HUB=down pull -- echo ran)"

spec_summary
