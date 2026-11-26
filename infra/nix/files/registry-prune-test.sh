#!/usr/bin/env bash
# registry-prune.sh against a fake registry: a local HTTP server answering the Docker Registry v2 calls it makes
# (catalog, tags, manifest digests, manifest deletes), over a storage directory whose tag links carry the push times.
# Run: bash infra/nix/files/registry-prune-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m registry-prune.sh\n'

work="$(mktemp -d)"
server=""
trap '[ -n "$server" ] && kill "$server" 2>/dev/null; rm -rf "$work"' EXIT

# The fake registry: $work/state.json is {repo: {tag: digest}}; a manifest DELETE drops every tag on that digest.
cat > "$work/registry.py" <<'PY'
import json, os, sys
from http.server import BaseHTTPRequestHandler, HTTPServer
state_file, port_file = sys.argv[1], sys.argv[2]
fail_once = state_file + ".fail-once"
def load():
    with open(state_file) as f: return json.load(f)
def save(state):
    with open(state_file, "w") as f: json.dump(state, f)
class Registry(BaseHTTPRequestHandler):
    def log_message(self, *args): pass
    def answer(self, status, body=b"", headers=()):
        self.send_response(status)
        for k, v in headers: self.send_header(k, v)
        self.send_header("Content-Length", str(len(body))); self.end_headers()
        if self.command != "HEAD": self.wfile.write(body)
    def manifest(self):
        parts = self.path.split("?")[0].split("/")  # /v2/<repo>/manifests/<ref>
        return parts[2], parts[4]
    def do_GET(self):
        state = load(); path = self.path.split("?")[0]
        if path == "/v2/_catalog":
            return self.answer(200, json.dumps({"repositories": sorted(state)}).encode())
        if path.endswith("/tags/list"):
            repo = path.split("/")[2]
            return self.answer(200, json.dumps({"name": repo, "tags": sorted(state.get(repo, {}))}).encode())
        if "/manifests/" in path:
            repo, ref = self.manifest(); tags = state.get(repo, {})
            if self.command == "HEAD" and os.path.exists(fail_once) and open(fail_once).read().strip() == ref:
                os.remove(fail_once); return self.answer(500)  # one transient registry error on this tag
            digest = tags.get(ref) or (ref if ref in tags.values() else None)
            return self.answer(200, b"{}", [("Docker-Content-Digest", digest)]) if digest else self.answer(404)
        self.answer(404)
    do_HEAD = do_GET
    def do_DELETE(self):
        state = load(); repo, ref = self.manifest(); tags = state.get(repo, {})
        if ref not in tags.values(): return self.answer(404)
        state[repo] = {t: d for t, d in tags.items() if d != ref}; save(state)
        self.answer(202)
server = HTTPServer(("127.0.0.1", 0), Registry)
with open(port_file + ".tmp", "w") as f: f.write(str(server.server_address[1]))
os.rename(port_file + ".tmp", port_file)
server.serve_forever()
PY

sha_a=$(printf 'a%.0s' $(seq 1 40)); sha_b=$(printf 'b%.0s' $(seq 1 40)); sha_c=$(printf 'c%.0s' $(seq 1 40))
cat > "$work/state.json" <<JSON
{"movies-worker": {
   "main-20261105040000-1111111": "sha256:old1", "main-20261106040000-2222222": "sha256:new1",
   "main-20261107030000-3333333": "sha256:new2", "latest": "sha256:new2", "$sha_c": "sha256:new2",
   "$sha_a": "sha256:handoff", "$sha_b": "sha256:abandoned"},
 "scratch": {"some-tag": "sha256:scratch"}}
JSON
storage="$work/storage"
pushed() {  # pushed <repo> <tag> <minutes ago>: the tag link the registry writes at a push, dated then
  local link="$storage/docker/registry/v2/repositories/$1/_manifests/tags/$2/current/link"
  mkdir -p "$(dirname "$link")"; : > "$link"
  python3 -I -c 'import os, sys, time; t = time.time() - 60 * int(sys.argv[2]); os.utime(sys.argv[1], (t, t))' "$link" "$3"
}
pushed movies-worker "$sha_a" 10
pushed movies-worker "$sha_b" $((60 * 48))
pushed movies-worker "$sha_c" $((60 * 48))

python3 -I "$work/registry.py" "$work/state.json" "$work/port" & server=$!
wait_until "the fake registry is listening" '[ -s "$work/port" ]'
api="http://127.0.0.1:$(cat "$work/port")/v2"

out=$(bash "$REPO_ROOT/infra/nix/files/registry-prune.sh" "$api" 2 "$storage" 1440 2>&1); code=$?
tags() { python3 -I -c 'import json,sys; print(" ".join(sorted(json.load(open(sys.argv[1])).get(sys.argv[2], {}))))' "$work/state.json" "$1"; }
left=$(tags movies-worker)
has() { case " $left " in *" $1 "*) echo kept ;; *) echo deleted ;; esac; }

check "the prune runs clean" "0" "$code"
# Main #2257: ci pushed <sha> at 03:49, the prune ran at 04:00 and deleted it, and publish's re-tag at 04:01 found
# nothing. A commit-sha tag pushed within the window is the hand-off to publish: never a prune's to take.
check "a commit-sha image pushed minutes ago, not yet re-tagged main-*, is kept" "kept" "$(has "$sha_a")"
check "...one pushed long ago and never published is deleted" "deleted" "$(has "$sha_b")"
check "the newest <keep> main-* tags are kept" "kept kept" "$(has main-20261107030000-3333333) $(has main-20261106040000-2222222)"
check "...an older one is deleted" "deleted" "$(has main-20261105040000-1111111)"
check "latest, and an old sha tag on a kept image's digest, are kept" "kept kept" "$(has latest) $(has "$sha_c")"
check "a repository with no main-* tag is left alone" "some-tag" "$(tags scratch)"
check "...and said so" "1" "$(grep -c 'scratch: no main-\* tags, left alone' <<<"$out")"

# A transient error reading a keeper's digest must not turn into a delete of that keeper: the repository is skipped
# whole (fail closed) and the run says it failed, while other repositories are still pruned.
cat > "$work/state.json" <<JSON
{"flaky": {"main-20261105040000-1111111": "sha256:oldf", "main-20261107030000-3333333": "sha256:newf",
           "latest": "sha256:newf"},
 "other": {"main-20261105040000-1111111": "sha256:oldo", "main-20261107030000-3333333": "sha256:newo"}}
JSON
printf 'main-20261107030000-3333333' > "$work/state.json.fail-once"
out=$(bash "$REPO_ROOT/infra/nix/files/registry-prune.sh" "$api" 1 "$storage" 1440 2>&1); code=$?
check "a failed keeper digest deletes nothing from that repository" \
  "latest main-20261105040000-1111111 main-20261107030000-3333333" "$(tags flaky)"
check "...and the run exits non-zero, naming the repository" "1 1" "$code $(grep -c 'flaky: .*skipped' <<<"$out")"
check "...while another repository is still pruned" "main-20261107030000-3333333" "$(tags other)"

spec_summary
