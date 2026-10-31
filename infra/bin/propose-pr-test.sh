#!/usr/bin/env bash
# infra/bin/propose-pr's offline self-test: a bare repository stands in for the forge's git (a global
# `url.insteadOf` sends the http remote to it, so the script's own auth_git is untouched) and a fake
# pulls API on a local port records what it is asked. Proves: the first run pushes the branch
# as ci-bot and opens the PR against main; the next updates that PR in place; a branch carrying
# someone else's commit is left alone; a shallow checkout (fetch-gitops' depth 1) is no false alarm;
# a branch list that cannot be read fails the run instead of overwriting the branch;
# an empty patch proposes nothing. No network.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Every git below acts on a scratch repository only, whatever a hook exported (scripts/scratch-git.sh).
. "${here}/../../scripts/scratch-git.sh"
tmp="$(mktemp -d)"
api_pid=""
cleanup() { [ -n "${api_pid}" ] && kill "${api_pid}" 2>/dev/null; rm -rf "${tmp}"; }
trap cleanup EXIT
fail() { echo "FAIL: $*" >&2; exit 1; }

# ---- the fake pulls API: GET lists the PRs it holds, POST opens #7, PATCH logs; every call appended to calls.log
cat >"${tmp}/api.py" <<'PY'
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer
log, prs = sys.argv[1], []
class H(BaseHTTPRequestHandler):
    def log_message(self, *a): pass
    def body(self):
        return json.loads(self.rfile.read(int(self.headers.get("Content-Length") or 0)) or b"{}")
    def reply(self, obj):
        data = json.dumps(obj).encode()
        self.send_response(200); self.send_header("Content-Length", str(len(data))); self.end_headers()
        self.wfile.write(data)
    def note(self, body):
        with open(log, "a") as f:
            f.write(json.dumps({"method": self.command, "path": self.path, "auth": self.headers.get("Authorization"),
                                "body": body}) + "\n")
    def do_GET(self):
        self.note(None); self.reply(prs)
    def do_POST(self):
        b = self.body(); self.note(b); prs.append({"number": 7, "head": {"ref": b["head"]}}); self.reply({"number": 7})
    def do_PATCH(self):
        self.note(self.body()); self.reply({"number": 7})
server = HTTPServer(("127.0.0.1", 0), H)
print(server.server_address[1], flush=True)
server.serve_forever()
PY
python3 "${tmp}/api.py" "${tmp}/calls.log" >"${tmp}/port" &
api_pid=$!
# Wait for the port line itself, to a deadline that fails loudly (never a counted fall-through).
deadline=$((SECONDS + 120))
until [ -s "${tmp}/port" ]; do
  [ "${SECONDS}" -lt "${deadline}" ] || fail "the fake API did not start within 120 s"
  command sleep 0.05
done
port="$(cat "${tmp}/port")"
[ -n "${port}" ] || fail "the fake API did not start"
server="http://127.0.0.1:${port}"

# ---- the forge's repository, and a checkout of its main at depth 1 (as fetch-gitops makes). The
#      global config is this run's own file (scratch-git.sh's is /dev/null): the http remote -> the bare repository.
repo=acme/gitops
cat >"${tmp}/gitconfig" <<EOF
[url "file://${tmp}/forge/"]
	insteadOf = ${server}/
[protocol "file"]
	allow = always
EOF
export GIT_CONFIG_GLOBAL="${tmp}/gitconfig"
scratch_repo "${tmp}/forge/${repo}.git" --bare -b main
scratch_git "${tmp}" clone -q "file://${tmp}/forge/${repo}.git" seed 2>/dev/null
scratch_git "${tmp}/seed" -c user.email=p@x commit -q --allow-empty -m "older history"
printf 'image: acme/tool:v1.0.0\n' >"${tmp}/seed/app.yaml"
scratch_git "${tmp}/seed" add app.yaml
scratch_git "${tmp}/seed" -c user.email=p@x commit -q -m base
scratch_git "${tmp}/seed" push -q origin main
clone_checkout() { rm -rf "${tmp}/checkout"; scratch_git "${tmp}" clone -q --depth 1 "file://${tmp}/forge/${repo}.git" checkout; }
clone_checkout

# ---- the bot's change in the form image_pins.py --gitops-out writes it: a plain -p1 unified diff, no git header
mkdir "${tmp}/out"
cat >"${tmp}/out/changes.patch" <<'EOF'
--- a/app.yaml
+++ b/app.yaml
@@ -1 +1 @@
-image: acme/tool:v1.0.0
+image: acme/tool:v1.2.0
EOF
echo "Image pins: tool v1.2.0" >"${tmp}/out/title"
echo "the body" >"${tmp}/out/body.md"

propose() {
  env -u GITHUB_ACTIONS GITHUB_SERVER_URL="${server}" GITHUB_API_URL="${server}" GITHUB_REPOSITORY=other/repo PROPOSE_TOKEN=s3cret \
    "${here}/propose-pr" -C "${tmp}/checkout" --repo "${repo}" --branch image-pins/all "${tmp}/out"
}
remote_branch() { scratch_git "${tmp}/forge/${repo}.git" "$@"; }

# 1. first run: branch pushed as ci-bot on top of main, PR opened against main, the token on a header only
propose >"${tmp}/run1" || fail "first run: $(cat "${tmp}/run1")"
[ "$(remote_branch log -1 --format=%ae image-pins/all)" = ci-bot@kinowo.invalid ] || fail "the branch is not ci-bot's"
[ "$(remote_branch rev-parse image-pins/all~1)" = "$(remote_branch rev-parse main)" ] || fail "the branch is not main + one"
remote_branch show image-pins/all:app.yaml | grep -qx 'image: acme/tool:v1.2.0' || fail "the patch is not on the branch"
post="$(grep '"POST"' "${tmp}/calls.log")" || fail "no PR opened"
python3 -c 'import json,sys; c=json.loads(sys.argv[1]); b=c["body"]
assert c["path"] == "/repos/acme/gitops/pulls", c["path"]
assert (b["head"], b["base"], b["title"], b["body"]) == ("image-pins/all", "main", "Image pins: tool v1.2.0", "the body\n"), b
assert c["auth"] == "token s3cret", c["auth"]' "${post}" || fail "the PR was opened wrong"
grep -q "opened PR #7" "${tmp}/run1" || fail "first run did not say it opened the PR"

# 2. a week later main has moved and the job's checkout is a fresh depth-1 clone of it: the open PR is
#    updated in place and the branch rebuilt on the new main (the shallow checkout must not read the
#    branch's base, main's own older commit, as someone else's)
scratch_git "${tmp}/seed" -c user.email=p@x commit -q --allow-empty -m "main moves on"
scratch_git "${tmp}/seed" push -q origin main
clone_checkout
propose >"${tmp}/run2" || fail "second run: $(cat "${tmp}/run2")"
grep -q "updated PR #7" "${tmp}/run2" || fail "second run did not update PR #7: $(cat "${tmp}/run2")"
[ "$(grep -c '"POST"' "${tmp}/calls.log")" = 1 ] || fail "second run opened another PR"
grep '"PATCH"' "${tmp}/calls.log" | grep -q '/repos/acme/gitops/pulls/7' || fail "second run patched no PR"
[ "$(remote_branch rev-parse image-pins/all~1)" = "$(remote_branch rev-parse main)" ] || fail "second run did not rebuild on main"

# 3. someone else's commit on the branch: left alone, nothing pushed
scratch_git "${tmp}" clone -q -b image-pins/all "file://${tmp}/forge/${repo}.git" person
scratch_git "${tmp}/person" -c user.email=p@x commit -q --allow-empty -m "drop one bump"
scratch_git "${tmp}/person" push -q origin image-pins/all
before="$(remote_branch rev-parse image-pins/all)"
clone_checkout
propose >"${tmp}/run3" || fail "third run: $(cat "${tmp}/run3")"
grep -q "left it alone" "${tmp}/run3" || fail "third run did not leave the branch alone: $(cat "${tmp}/run3")"
[ "$(remote_branch rev-parse image-pins/all)" = "${before}" ] || fail "third run overwrote a person's commit"

# 3b. the branch list cannot be read (the forge down for a moment): the run fails, and never takes the
#     failed read for "no branch" and force-pushes over the person's commit
mkdir -p "${tmp}/flaky-forge"
real_git="$(command -v git)"
cat >"${tmp}/flaky-forge/git" <<EOF
#!/bin/sh
for a in "\$@"; do [ "\$a" = ls-remote ] && { echo "fatal: unable to access the forge" >&2; exit 128; }; done
exec "${real_git}" "\$@"
EOF
chmod +x "${tmp}/flaky-forge/git"
clone_checkout
if PATH="${tmp}/flaky-forge:${PATH}" propose >"${tmp}/run3b" 2>&1; then fail "an unreadable branch list did not fail the run: $(cat "${tmp}/run3b")"; fi
[ "$(remote_branch rev-parse image-pins/all)" = "${before}" ] || fail "an unreadable branch list overwrote a person's commit"

# 4. an empty patch proposes nothing
: >"${tmp}/out/changes.patch"
propose | grep -qx "nothing to propose" || fail "an empty patch was proposed"

echo "ok: propose-pr self-test"
