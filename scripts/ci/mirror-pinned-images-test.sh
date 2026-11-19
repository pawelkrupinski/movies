#!/usr/bin/env bash
# mirror-pinned-images.sh against a stub `skopeo` over a directory of registries' manifests.
# Run: bash scripts/ci/mirror-pinned-images-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m mirror-pinned-images.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
sha() { if command -v sha256sum > /dev/null; then sha256sum; else shasum -a 256; fi | cut -d' ' -f1; }
printf '{"pinned":"index"}\n' > "$stub_dir/pinned.json"
printf '{"other":"index"}\n' > "$stub_dir/other.json"
digest="sha256:$(sha < "$stub_dir/pinned.json")"
other="sha256:$(sha < "$stub_dir/other.json")"
printf '# a comment\nmongo\tdocker.io/library/mongo:8.3.17@%s\tghcr.io/owner/mongo\n' "$digest" > "$stub_dir/pins.tsv"
export PINNED_IMAGES="$stub_dir/pins.tsv"

# The registries: $STUB_REG/<ref with / and : and @ as _> holds the manifest a ref serves. A copy writes the source's
# manifest to the destination, unless STUB_COPY_FAIL names a source host to refuse. A `login` records stdin.
cat > "$stub_dir/skopeo" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
key() { local r=${1#docker://}; echo "$STUB_REG/${r//[\/:@]/_}"; }
case "$1" in
  inspect) [ -f "$(key "${*: -1}")" ] || exit 1; cat "$(key "${*: -1}")" ;;
  copy) src="${*: -2:1}"; dst="${*: -1}"
        [[ -n "${STUB_COPY_FAIL:-}" && "$src" == *"$STUB_COPY_FAIL"* ]] && exit 1
        [ -f "$(key "$src")" ] || exit 1
        cp "$(key "$src")" "$(key "$dst")" ;;
  login) cat >> "$STUB_LOGIN" ;;
esac
STUB
chmod +x "$stub_dir/skopeo"
export STUB_LOG="$stub_dir/log" STUB_LOGIN="$stub_dir/login" STUB_REG="$stub_dir/reg"
put() { cp "$stub_dir/$2.json" "$STUB_REG/${1//[\/:@]/_}"; }

# mirror -> "<exit status>", the output in $stub_dir/out; every registry starts as upstream serves the pin
mirror() {
  rm -rf "$STUB_REG"; mkdir -p "$STUB_REG"; : > "$STUB_LOG"; : > "$STUB_LOGIN"
  put "docker.io/library/mongo@$digest" pinned; put "docker.io/library/mongo:8.3.17" pinned
  put "mirror.gcr.io/library/mongo@$digest" pinned
  for setup in "${SETUP[@]+"${SETUP[@]}"}"; do eval "$setup"; done
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/mirror-pinned-images.sh" > "$stub_dir/out" 2>&1
  echo "$?"
}
ghcr_now() { sha < "$STUB_REG/ghcr.io_owner_mongo_8.3.17" 2>/dev/null | sed 's/^/sha256:/'; }

SETUP=()
check "an image not on GHCR yet is copied there by digest, every platform, the digest preserved" "0" "$(mirror)"
check "...from Docker Hub's pinned digest" "1" \
  "$(grep -c "^copy --all --preserve-digests --retry-times 3 docker://docker.io/library/mongo@$digest docker://ghcr.io/owner/mongo:8.3.17$" "$STUB_LOG")"
check "...and GHCR then serves the pin" "$digest" "$(ghcr_now)"
check "...saying so" "1" "$(grep -c "^mongo: ghcr.io/owner/mongo:8.3.17 is now the pin, $digest" "$stub_dir/out")"

SETUP=('put ghcr.io/owner/mongo:8.3.17 pinned')
check "an image GHCR already serves at the pin is left alone" "0" "$(mirror)"
check "...nothing copied" "0" "$(grep -c '^copy' "$STUB_LOG")"

SETUP=('put ghcr.io/owner/mongo:8.3.17 other')
check "DRIFT -- GHCR serving another digest (a bumped pin) -- is flagged and the pin copied over it" "0" "$(mirror)"
check "...as a warning naming both digests" "1" \
  "$(grep -c "^::warning::mongo: DRIFT -- GHCR's ghcr.io/owner/mongo:8.3.17 is $other, the pin is $digest" "$stub_dir/out")"
check "...and GHCR then serves the pin" "$digest" "$(ghcr_now)"

SETUP=('STUB_COPY_FAIL=docker.io; export STUB_COPY_FAIL')
check "Docker Hub refusing the copy: from mirror.gcr.io instead" "0 $digest" "$(mirror) $(ghcr_now)"
check "...saying so" "1" "$(grep -c '^::warning::mongo: the copy from docker://docker.io/library/mongo@.* failed' "$stub_dir/out")"
unset STUB_COPY_FAIL

SETUP=('rm "$STUB_REG"/docker.io_* "$STUB_REG"/mirror.gcr.io_*')
check "no source to copy from fails the run, loudly" "1" "$(mirror)"
check "...an error saying CI's pulls skip GHCR" "1" \
  "$(grep -c '^::error::mongo: GHCR.s ghcr.io/owner/mongo:8.3.17 is missing after the copy, not the pin .* CI pulls of mongo skip GHCR' "$stub_dir/out")"

SETUP=('put ghcr.io/owner/mongo:8.3.17 other' 'STUB_COPY_FAIL=docker; export STUB_COPY_FAIL' 'rm "$STUB_REG"/mirror.gcr.io_*')
check "a drifted GHCR the copy cannot fix fails, naming what it serves" "1" "$(mirror)"
check "...the error naming GHCR's digest" "1" "$(grep -c "^::error::mongo: GHCR's ghcr.io/owner/mongo:8.3.17 is $other after the copy" "$stub_dir/out")"
unset STUB_COPY_FAIL

SETUP=('put ghcr.io/owner/mongo:8.3.17 pinned' 'put docker.io/library/mongo:8.3.17 other')
check "the upstream tag moving off the pin is only a notice" "0" "$(mirror)"
check "...naming where it points now" "1" \
  "$(grep -c "^::notice::mongo: docker.io/library/mongo:8.3.17 now points at $other, not the pinned $digest" "$stub_dir/out")"

SETUP=()
check "GHCR logged in with the job's token on stdin, never on a command line" "0 tok 0" \
  "$(GHCR_TOKEN=tok GITHUB_ACTOR=me mirror) $(cat "$STUB_LOGIN") $(grep -c tok "$STUB_LOG")"
check "...as the actor" "1" "$(grep -c '^login ghcr.io -u me --password-stdin$' "$STUB_LOG")"

printf 'mongo\tdocker.io/library/mongo:8.3.17\tghcr.io/owner/mongo\n' > "$stub_dir/pins.tsv"
check "a pin with no digest fails" "1" "$(mirror)"
printf '# only comments\n' > "$stub_dir/pins.tsv"
check "a table pinning nothing fails" "1" "$(mirror)"

spec_summary
