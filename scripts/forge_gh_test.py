#!/usr/bin/env python3
"""Unit tests for scripts/forge-gh: which forge a checkout reads, and the Forgejo subset answered as gh answers it --
run artifacts out of the generic package registry, release assets through their attachment records, runs in GitHub's
shape, gh's own error words for an absent artifact or asset -- against a fake Forgejo on a local port. The GitHub side
is a stub `gh` on the PATH. Then tool-conventions.sh's gh_download over it: an absent artifact is absent (3).

    python3 scripts/forge_gh_test.py
"""
import importlib.machinery
import importlib.util
import datetime
import io
import json
import os
import subprocess
import tarfile
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "forge-gh")
_loader = importlib.machinery.SourceFileLoader("forge_gh", TOOL)
_spec = importlib.util.spec_from_loader("forge_gh", _loader)
forge_gh = importlib.util.module_from_spec(_spec)
_loader.exec_module(forge_gh)

TOKEN = "t0ken"


def tgz(files: dict) -> bytes:
    buf = io.BytesIO()
    with tarfile.open(fileobj=buf, mode="w:gz") as tar:
        for name, content in files.items():
            data = content.encode()
            info = tarfile.TarInfo(name)
            info.size = len(data)
            tar.addfile(info, io.BytesIO(data))
    return buf.getvalue()


RUN = {"id": 528, "index_in_repo": 193, "workflow_id": "record-scrape-fixtures.yml", "status": "success",
       "commit_sha": "a744eff384b44bbe7f8a481c0f75acb28e82311c", "prettyref": "main", "trigger_event": "workflow_dispatch",
       "created": "2026-10-20T10:15:24Z", "started": "2026-10-20T10:21:08Z", "updated": "2026-10-20T10:52:08Z",
       "html_url": "http://forgejo.test/o/movies/actions/runs/193", "title": "Record scrape fixtures"}
OLDER = dict(RUN, id=500, index_in_repo=170, status="failure", created="2026-10-19T10:00:00Z", commit_sha="b" * 40)
# Forgejo's `failure` the moment a run's first job failed, while its takes row still runs (measure run 2848)
FAILING = dict(RUN, id=2848, index_in_repo=1788, status="failure", workflow_id="identity-model-convergence.yml")
# the CI's actions API (port 3010): each run's jobs in GitHub's shape
JOBS = {"2848": [{"name": "order-independence-p01 (germany)", "status": "completed", "conclusion": "failure"},
                 {"name": "takes (germany)", "status": "in_progress", "conclusion": None}],
        "500": [{"name": "takes (germany)", "status": "completed", "conclusion": "failure"}]}


class Fake(BaseHTTPRequestHandler):
    """Forgejo 15's answers, for repository o/movies; every route 401s without the token."""
    seen = []
    packages = {"528": {"scrape-fixtures-de": tgz({"scrapes-de.tar.gz": "corpus-de"}),
                        "convergence-diagnostics-de-convergence": tgz({"de-takes/de.json.gz": "takes"})}}
    assets = {"hermetic-de.txt": b"528 2026-10-20T10:22:31Z enrichment-de-528.tar.zst\n",
              "enrichment-de-528.tar.zst": b"tree"}

    def log_message(self, *a):
        pass

    def send(self, code, body=b"", ctype="application/json"):
        if isinstance(body, (dict, list)):
            body = json.dumps(body).encode()
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):  # noqa: N802
        Fake.seen.append(self.path)
        if self.headers.get("Authorization") != f"token {TOKEN}":
            return self.send(404 if "/api/v1/repos" in self.path else 401, {"message": "not found"})
        path, _, query = self.path.partition("?")
        base = "/api/v1/repos/o/movies"
        if path == f"{base}/actions/runs/528":
            return self.send(200, RUN)
        if path == f"{base}/actions/runs/2848":
            return self.send(200, FAILING)
        jobs = "/repos/o/movies/actions/runs/"
        if path.startswith(jobs) and path.endswith("/jobs"):
            got = JOBS.get(path[len(jobs):-len("/jobs")])
            if got is None:
                return self.send(404, {"message": "no such run"})
            return self.send(200, {"total_count": len(got), "jobs": got if "page=1" in query else []})
        if path == f"{base}/actions/runs":
            runs = [RUN, OLDER]
            if "workflow_id=identity-model-convergence.yml" in query:
                runs = [FAILING]
            elif "workflow_id=record-scrape-fixtures.yml" not in query:
                runs = []
            if "status=success" in query:
                runs = [r for r in runs if r["status"] == "success"]
            return self.send(200, {"workflow_runs": runs if "page=1" in query else [], "total_count": len(runs)})
        pkg = "/api/v1/packages/o/generic/ci-artifacts-movies/"
        if path.startswith(pkg) and path.endswith("/files"):
            run = path[len(pkg):-len("/files")]
            if run not in Fake.packages:
                return self.send(404, {"message": "package does not exist"})
            return self.send(200, [{"name": f"{n}.tar.gz", "size": len(b)} for n, b in Fake.packages[run].items()])
        dl = "/api/packages/o/generic/ci-artifacts-movies/"
        if path.startswith(dl):
            run, name = path[len(dl):].split("/", 1)
            return self.send(200, Fake.packages[run][name[: -len(".tar.gz")]], "application/gzip")
        if path == f"{base}/releases/tags/convergence-fixtures":
            host = f"http://{self.headers['Host']}"
            return self.send(200, {"id": 2, "tag_name": "convergence-fixtures", "name": "fixtures", "assets": [
                {"id": i, "name": n, "size": len(b), "browser_download_url": f"{host}/attachments/{n}"}
                for i, (n, b) in enumerate(Fake.assets.items())]})
        if path.startswith(f"{base}/releases/tags/"):
            return self.send(404, {"message": "release does not exist"})
        if path.startswith("/attachments/"):
            return self.send(200, Fake.assets[path[len("/attachments/"):]], "application/octet-stream")
        if path == "/api/v1/broken":
            return self.send(502, {"message": "bad gateway"})
        return self.send(404, {"message": "no route"})


    posted = []

    def do_DELETE(self):  # noqa: N802
        if self.headers.get("Authorization") != f"token {TOKEN}":
            return self.send(401, {"message": "unauthorized"})
        prefix = "/api/v1/repos/o/movies/releases/2/assets/"
        if self.path.startswith(prefix):
            ids = {str(i): n for i, n in enumerate(Fake.assets)}
            Fake.assets.pop(ids[self.path[len(prefix):]], None)
            return self.send(204)
        return self.send(404, {"message": "no route"})

    def do_POST(self):  # noqa: N802
        if self.headers.get("Authorization") != f"token {TOKEN}":
            return self.send(401, {"message": "unauthorized"})
        if self.path.startswith("/api/v1/repos/o/movies/releases/2/assets?name="):
            raw = self.rfile.read(int(self.headers.get("Content-Length") or 0))
            name = self.path.split("name=", 1)[1]
            # the multipart body's one part: its content between the blank line and the closing boundary
            Fake.assets[name] = raw.split(b"\r\n\r\n", 1)[1].rsplit(b"\r\n--", 1)[0]
            return self.send(201, {"name": name})
        body = json.loads(self.rfile.read(int(self.headers.get("Content-Length") or 0)) or b"{}")
        Fake.posted.append((self.path, body))
        if self.path == "/api/v1/repos/o/movies/actions/workflows/identity-model-convergence.yml/dispatches":
            if body.get("ref") == "refs/heads/missing":
                return self.send(422, {"message": "ref does not exist"})
            return self.send(204)
        return self.send(404, {"message": "no route"})


class ForgeGhTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = ThreadingHTTPServer(("127.0.0.1", 0), Fake)
        threading.Thread(target=cls.server.serve_forever, daemon=True).start()
        cls.url = f"http://127.0.0.1:{cls.server.server_address[1]}"

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()

    def setUp(self):
        self.work = tempfile.mkdtemp()
        self.env = dict(os.environ, KINOWO_FORGE="forgejo", KINOWO_FORGEJO_URL=self.url, FORGEJO_API_TOKEN=TOKEN,
                        KINOWO_FORGEJO_ACTIONS_URL=self.url,
                        KINOWO_REPO="o/movies")
        self.env.pop("GITHUB_ACTIONS", None)

    def tearDown(self):
        subprocess.run(["rm", "-rf", self.work])

    def gh(self, *argv, env=None):
        return subprocess.run([TOOL, *argv], capture_output=True, text=True, env=env or self.env, timeout=60)

    # ── which forge ──
    def test_a_forgejo_remote_reads_forgejo_and_github_alone_reads_github(self):
        env = {}
        self.assertEqual("forgejo", forge_gh.forge(env, ["https://github.com/p/movies.git",
                                                         "ssh://forgejo-git/p/movies.git"]))
        self.assertEqual("forgejo", forge_gh.forge(env, ["http://forgejo.internal:3000/p/movies.git"]))
        self.assertEqual("github", forge_gh.forge(env, ["https://github.com/p/movies.git", "git@github.com:p/m.git"]))

    def test_kinowo_forge_overrides_and_a_ci_job_runs_its_own_gh(self):
        forgejo = ["ssh://forgejo-git/p/movies.git"]
        self.assertEqual("github", forge_gh.forge({"KINOWO_FORGE": "github"}, forgejo))
        self.assertEqual("forgejo", forge_gh.forge({"KINOWO_FORGE": "forgejo"}, []))
        self.assertEqual("github", forge_gh.forge({"GITHUB_ACTIONS": "true"}, forgejo))

    def test_on_github_it_is_gh_itself(self):
        os.makedirs(f"{self.work}/bin")
        with open(f"{self.work}/bin/gh", "w") as f:
            f.write('#!/usr/bin/env bash\necho "stub gh: $*"; exit 7\n')
        os.chmod(f"{self.work}/bin/gh", 0o755)
        env = dict(self.env, KINOWO_FORGE="github", PATH=f"{self.work}/bin{os.pathsep}{os.environ['PATH']}")
        done = self.gh("run", "view", "1", "--json", "x", env=env)
        self.assertEqual((7, "stub gh: run view 1 --json x\n"), (done.returncode, done.stdout))

    # ── artifacts ──
    def test_one_named_artifact_unpacks_into_the_dir(self):
        done = self.gh("run", "download", "528", "-R", "o/movies", "--name", "scrape-fixtures-de", "--dir", self.work)
        self.assertEqual(0, done.returncode, done.stderr)
        with open(f"{self.work}/scrapes-de.tar.gz") as f:
            self.assertEqual("corpus-de", f.read())
        self.assertEqual(["scrapes-de.tar.gz"], os.listdir(self.work))

    def test_a_pattern_unpacks_each_artifact_into_its_own_dir(self):
        done = self.gh("run", "download", "528", "--pattern", "convergence-diagnostics-*", "--dir", self.work)
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertTrue(os.path.isfile(f"{self.work}/convergence-diagnostics-de-convergence/de-takes/de.json.gz"))

    def test_an_absent_artifact_or_run_says_it_as_gh_does(self):
        done = self.gh("run", "download", "528", "--name", "scrape-fixtures-xx", "--dir", self.work)
        self.assertEqual((1, "no artifact matches any of the names or patterns provided\n"),
                         (done.returncode, done.stderr))
        done = self.gh("run", "download", "9", "--name", "scrape-fixtures-de", "--dir", self.work)
        self.assertEqual((1, "no valid artifacts found to download\n"), (done.returncode, done.stderr))

    def test_the_artifact_listing_in_githubs_shape(self):
        done = self.gh("api", "repos/o/movies/actions/runs/528/artifacts?per_page=100", "--paginate",
                       "-q", '.artifacts[] | select(.name == "scrape-fixtures-de" and (.expired | not)) | .name')
        self.assertEqual((0, "scrape-fixtures-de\n"), (done.returncode, done.stdout), done.stderr)

    # ── releases ──
    def test_a_release_asset_by_pattern_into_the_dir(self):
        done = self.gh("release", "download", "convergence-fixtures", "-R", "o/movies", "--pattern",
                       "enrichment-de-528.tar.*", "--dir", f"{self.work}/archive", "--clobber")
        self.assertEqual(0, done.returncode, done.stderr)
        with open(f"{self.work}/archive/enrichment-de-528.tar.zst", "rb") as f:
            self.assertEqual(b"tree", f.read())

    def test_a_release_asset_to_stdout(self):
        done = self.gh("release", "download", "convergence-fixtures", "--pattern", "hermetic-de.txt", "--output", "-")
        self.assertEqual((0, "528 2026-10-20T10:22:31Z enrichment-de-528.tar.zst\n"), (done.returncode, done.stdout))

    def test_an_absent_asset_or_release_says_it_as_gh_does(self):
        done = self.gh("release", "download", "convergence-fixtures", "--pattern", "enrichment-de-1.tar.gz")
        self.assertEqual((1, "no assets match the file pattern\n"), (done.returncode, done.stderr))
        done = self.gh("release", "download", "nope", "--pattern", "x")
        self.assertEqual((1, "release not found\n"), (done.returncode, done.stderr))

    def test_a_release_asset_uploaded_reads_back(self):
        path = f"{self.work}/measure-result-abc.json"
        with open(path, "w") as f:
            f.write('{"verdict": 0}')
        done = self.gh("release", "upload", "convergence-fixtures", path)
        self.assertEqual(0, done.returncode, done.stderr)
        done = self.gh("release", "download", "convergence-fixtures", "--pattern", "measure-result-abc.json", "-O", "-")
        self.assertEqual((0, '{"verdict": 0}'), (done.returncode, done.stdout), done.stderr)
        done = self.gh("release", "upload", "convergence-fixtures", path)
        self.assertEqual((1, "asset under the same name already exists: [measure-result-abc.json]\n"),
                         (done.returncode, done.stderr))
        with open(path, "w") as f:
            f.write('{"verdict": 1}')
        done = self.gh("release", "upload", "convergence-fixtures", path, "--clobber")
        self.assertEqual(0, done.returncode, done.stderr)
        done = self.gh("release", "download", "convergence-fixtures", "--pattern", "measure-result-abc.json", "-O", "-")
        self.assertEqual('{"verdict": 1}', done.stdout)
        Fake.assets.pop("measure-result-abc.json", None)

    def test_an_existing_file_is_kept_without_clobber(self):
        with open(f"{self.work}/hermetic-de.txt", "w") as f:
            f.write("mine")
        done = self.gh("release", "download", "convergence-fixtures", "--pattern", "hermetic-de.txt", "-D", self.work)
        self.assertEqual(1, done.returncode)
        with open(f"{self.work}/hermetic-de.txt") as f:
            self.assertEqual("mine", f.read())

    # ── runs ──
    def test_a_run_in_githubs_shape(self):
        done = self.gh("run", "view", "528", "--json", "databaseId,number,headSha,status,conclusion,createdAt",
                       "-q", '"\\(.databaseId) \\(.number) \\(.headSha[:9]) \\(.status) \\(.conclusion) \\(.createdAt)"')
        self.assertEqual("528 193 a744eff38 completed success 2026-10-20T10:15:24Z\n", done.stdout, done.stderr)
        done = self.gh("api", "repos/o/movies/actions/runs/528", "--jq", ".run_started_at")
        self.assertEqual("2026-10-20T10:21:08Z\n", done.stdout, done.stderr)

    def test_a_failed_run_whose_jobs_still_run_is_in_progress_until_they_end(self):
        # measure run 2848: Forgejo said `failure` at its first failed job; identity-measure took it for done and
        # collected an hour before any takes row had uploaded its artifact
        view = ("run", "view", "2848", "--json", "status,conclusion", "-q", '"\\(.status) \\(.conclusion)"')
        done = self.gh(*view)
        self.assertEqual("in_progress \n", done.stdout, done.stderr)
        done = self.gh("api", "repos/o/movies/actions/runs/2848", "--jq", '"\\(.status) \\(.conclusion)"')
        self.assertEqual("in_progress null\n", done.stdout, done.stderr)
        done = self.gh("run", "list", "--workflow", "identity-model-convergence.yml", "--json", "status", "-q", ".[].status")
        self.assertEqual("in_progress\n", done.stdout, done.stderr)
        JOBS["2848"][1].update(status="completed", conclusion="success")
        try:
            self.assertEqual("completed failure\n", self.gh(*view).stdout)
        finally:
            JOBS["2848"][1].update(status="in_progress", conclusion=None)

    def test_a_runs_jobs_are_read_from_the_actions_api(self):
        # identity-drift-ci --for-red reads a red's failing takes-drift rows this way
        done = self.gh("run", "view", "2848", "--json", "databaseId,jobs",
                       "-q", '.jobs[] | "\\(.name)\t\\(.conclusion)"')
        self.assertEqual("order-independence-p01 (germany)\tfailure\ntakes (germany)\tnull\n", done.stdout, done.stderr)

    def test_a_failed_runs_queued_job_is_waiting_while_the_run_is_recent_and_dead_once_it_is_quiet(self):
        fj = forge_gh.Forgejo("o/movies", self.env)
        queued = [{"name": "takes (spain)", "status": "queued", "completed_at": None},
                  {"name": "order-independence-p01 (germany)", "status": "completed", "completed_at": "2026-10-26T04:58:19Z"}]
        run = dict(FAILING, updated="2026-10-26T04:58:19Z")
        at = lambda t: datetime.datetime.fromisoformat(t + "+00:00")  # noqa: E731
        JOBS["2848"], kept = queued, JOBS["2848"]
        try:
            self.assertEqual("running", fj.settled(run, now=at("2026-10-26T05:05:00"))["status"])
            # 2848's timings and drop-measure-branch: queued, never to start, the run quiet since 06:01
            self.assertEqual("failure", fj.settled(run, now=at("2026-10-26T05:30:00"))["status"])
        finally:
            JOBS["2848"] = kept

    def test_a_failed_runs_unreadable_jobs_are_no_answer(self):
        env = dict(self.env, KINOWO_FORGEJO_ACTIONS_URL=self.url + "/nowhere")
        done = self.gh("run", "view", "2848", "--json", "status", env=env)
        self.assertNotEqual(0, done.returncode)
        self.assertIn("HTTP 404", done.stderr)
        self.assertEqual("", done.stdout)

    def test_runs_listed_by_workflow_name_and_status(self):
        done = self.gh("run", "list", "--workflow", "Record scrape fixtures", "--status", "success", "-L", "100",
                       "--json", "databaseId,createdAt", "-q", '.[] | "\\(.databaseId)\\t\\(.createdAt)"')
        self.assertEqual("528\t2026-10-20T10:15:24Z\n", done.stdout, done.stderr)

    def test_a_dispatch_posts_the_ref_and_inputs_to_the_workflows_file(self):
        Fake.posted.clear()
        done = self.gh("workflow", "run", "Identity model convergence", "--ref", "measure/u/abc-1",
                       "-f", "takes-only=true", "-f", "recording=528")
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual([("/api/v1/repos/o/movies/actions/workflows/identity-model-convergence.yml/dispatches",
                           {"ref": "measure/u/abc-1", "inputs": {"takes-only": "true", "recording": "528"}})], Fake.posted)
        done = self.gh("workflow", "run", "identity-model-convergence.yml", "--ref", "refs/heads/missing")
        self.assertEqual(1, done.returncode)
        self.assertIn("HTTP 422", done.stderr)

    def test_runs_listed_by_event(self):
        done = self.gh("run", "list", "--workflow", "Record scrape fixtures", "--event", "workflow_dispatch",
                       "--json", "databaseId", "-q", ".[].databaseId")
        self.assertEqual("528\n500\n", done.stdout, done.stderr)
        done = self.gh("run", "list", "--workflow", "Record scrape fixtures", "--event", "push", "--json", "databaseId")
        self.assertEqual("[]\n", done.stdout, done.stderr)

    def test_which_names_the_forge(self):
        self.assertEqual("forgejo\n", self.gh("--which").stdout)

    def test_a_run_log_is_refused_naming_itself(self):
        done = self.gh("run", "view", "528", "--log")
        self.assertEqual(1, done.returncode)
        self.assertIn("no Forgejo API serves a run's log", done.stderr)

    def test_a_failed_read_is_an_http_error_and_no_token_is_one_too(self):
        done = self.gh("api", "broken")
        self.assertEqual(1, done.returncode)
        self.assertTrue(done.stderr.startswith("HTTP 502"), done.stderr)
        env = dict(self.env, FORGEJO_API_TOKEN="", PATH="/usr/bin:/bin")  # no `secrets` on the PATH
        done = self.gh("run", "view", "528", "--json", "databaseId", env=env)
        self.assertEqual(4, done.returncode)
        self.assertIn("no Forgejo token", done.stderr)

    # ── through tool-conventions.sh's gh_download ──
    def gh_download(self, *argv):
        script = f'. "{HERE}/tool-conventions.sh"; GH_DOWNLOAD_ATTEMPTS=1 gh_download 30 "$@"'
        return subprocess.run(["bash", "-c", script, "gh_download", *argv], capture_output=True, text=True,
                              env=dict(self.env, KINOWO_GH=""), timeout=60).returncode

    def test_gh_download_reads_forgejo_present_absent_and_failed_apart(self):
        self.assertEqual(0, self.gh_download("run", "download", "528", "-R", "o/movies", "--name",
                                             "scrape-fixtures-de", "--dir", self.work))
        self.assertEqual(3, self.gh_download("run", "download", "528", "--name", "scrape-fixtures-xx",
                                             "--dir", f"{self.work}/x"))
        self.assertEqual(3, self.gh_download("release", "download", "convergence-fixtures", "--pattern", "nope.tar.gz"))
        env_down = dict(self.env, KINOWO_FORGEJO_URL="http://127.0.0.1:9")  # nothing listens
        script = f'. "{HERE}/tool-conventions.sh"; GH_DOWNLOAD_ATTEMPTS=1 gh_download 30 "$@"'
        done = subprocess.run(["bash", "-c", script, "x", "release", "download", "convergence-fixtures", "--pattern",
                               "a"], capture_output=True, text=True, env=dict(env_down, KINOWO_GH=""), timeout=60)
        self.assertEqual(1, done.returncode, done.stderr)


if __name__ == "__main__":
    unittest.main()
