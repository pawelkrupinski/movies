"""The fixture scripts/identity-slice's black-box tests share (identity_slice_cli_test.py, identity_slice_cli_serve_test.py,
identity_slice_cli_modes_test.py): a scratch repository holding a copy of the tool and the scripts it runs, the files it
drops into every ref (scripts/identity-slice-tool-files.txt), at two commits (`base`, `head`: build.sbt changed); a
recording already in the cache, so nothing is downloaded; and stub binaries -- sbt printing a classpath, java standing in
for IdentitySlice (the base's and the head's takes), IdentityExplain, IdentityDrift and the arrival walk, and for a kept
JVM (serve.py, answering each request on its socket), the mirror's freshness check -- each appending what it ran to
<work>/calls. Ported from identity-slice-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path). The cache lies under a short directory of its own: a kept JVM's socket path must fit a unix
socket's 104 bytes, which the case's scratch directory under macOS's TMPDIR does not leave room for."""
from __future__ import annotations

import glob
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import zipfile
from typing import Dict, List, Mapping, Optional, Set

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_slice_cli.py", "scripts/identity-slice")
ROOT = cli_harness.ROOT
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"
BACKLOG = "test/resources/fixtures/identity-flip-backlog.tsv"
# the scripts besides scripts/*.py, *.sh, scripts/ci/*.py and scripts/lib/*.py the tool runs
EXTENSIONLESS = ("identity-slice", "identity-explain", "identity-slice-tool-files.txt")
HEADER = "selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation"


def tool_files() -> List[str]:
    """The files the tool drops into every ref, read from its own list: a hand-kept copy drifted from it."""
    with open(os.path.join(ROOT, "scripts/identity-slice-tool-files.txt")) as f:
        return [line.strip() for line in f if line.strip() and not line.startswith("#")]


# java records its arguments and writes the takes the base and the head ref take; IdentityDrift ends as STUB_DRIFT says,
# the arrival walk prints STUB_WALK; --serve is a kept JVM (serve.py), or one that never listens (STUB_SERVE_HANG)
JAVA = r'''#!/usr/bin/env bash
echo "java $PWD $*" >> "$STUB/calls"
case " $* " in *" scripts.IdentityDrift "*)
  case "${STUB_DRIFT:-clean}" in
    clean) echo "## de: 1 listings"; echo "## DRIFT VERDICT: CLEAN"; exit 0 ;;
    red) echo "## de: 1 listings, 1 divergence(s)"; echo "## DRIFT VERDICT: RED"; exit 1 ;;
    oom) echo "java.lang.OutOfMemoryError: Java heap space" >&2; exit 1 ;;
    exit1) echo "boom" >&2; exit 1 ;;
    killed) exit 137 ;;
  esac ;;
esac
case " $* " in *" services.identity.PopulationMonotonicitySpec "*)
  printf '%b' "${STUB_WALK:-# drift-slices/de/kino-lalka-1: 0 flip(s) in 1.0s\n}"; exit "${STUB_WALK_RC:-0}" ;;
esac
[ -z "${STUB_EMIT_SLICE:-}" ] || [ -z "$(printf '%s' "$*" | grep -- --emit-slice)" ] || echo "--emit-slice: $STUB_EMIT_SLICE"
out="" ref="" serve=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2 ;; --ref) ref=$2 ;; --serve) serve=$2 ;; esac; shift; done
[ -z "$serve" ] || [ -z "${STUB_SERVE_HANG:-}" ] || { echo $$ >> "$STUB_SERVE_HANG"; exec perl -e 'sleep 86400' scripts.IdentitySlice; }
[ -z "$serve" ] || exec python3 -I -B "$STUB/serve.py" "$serve" "$STUB/calls" scripts.IdentitySlice
[ -n "$out" ] || { echo "explained"; exit 0; }
{ printf '%s\n' "$STUB_HEADER"
  if [ "$ref" = "$STUB_BASE" ]; then
    printf 'k\tKino|Lalka\tKino\tLalka\ttmdb:1\tLalka (1968)\tOwnMatch\t0.9\ttmdb:1\t\t\t\tWRONG\twhy\n'
    printf 'k\tKino|Emil\tKino\tEmil\ttmdb:2\tEmil (2001)\tOwnMatch\t0.9\ttmdb:2\t\t\t\tRIGHT\twhy\n'
    printf 'k\tKino|Sinn\tKino\tSinn\tnone\t\tNoCandidate\t0\tnone\t\t\t\t-\twhy\n'
  else
    printf 'k\tKino|Lalka\tKino\tLalka\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Emil\tKino\tEmil\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Sinn\tKino\tSinn\ttmdb:9\tSinn (1990)\tOwnMatch\t0.8\ttmdb:9\t\t\t\tWRONG\twhy\n'
  fi; } > "$out"
echo "decided"
[ -z "${STUB_INCOMPLETE:-}" ] || { echo "SLICE INCOMPLETE" >&2; exit 3; }
# the tool edited while it runs (a rebase, a pull, an agent's edit): its text shifted under the reader
[ -z "${STUB_REWRITE:-}" ] || { { printf '%09000d\n' 0 | tr 0 '#'; cat "$STUB_REWRITE"; } > "$STUB_REWRITE.new"; cat "$STUB_REWRITE.new" > "$STUB_REWRITE"; }
'''
SERVE = r'''import os, socket, struct, sys
path, calls = sys.argv[1], sys.argv[2]
s = socket.socket(socket.AF_UNIX); s.bind(path); s.listen(1)
while True:
    c, _ = s.accept()
    n = struct.unpack(">i", c.recv(4))[0]
    data = b""
    while len(data) < n:
        data += c.recv(n - len(data))
    args = data.decode().split("\0")
    open(calls, "a").write("request " + " ".join(args) + "\n")
    out = args[args.index("--out") + 1]
    open(out, "w").write(os.environ["STUB_HEADER"] + "\n"
                         "k\tKino|Lalka\tKino\tLalka\ttmdb:1\tLalka (1968)\tOwnMatch\t0.9\ttmdb:1\t\t\t\tWRONG\twhy\n")
    c.sendall(b"decided by the kept JVM\n\0EXIT 0\n"); c.close()
'''
# sbt prints a classpath, as `export` does
SBT = '''#!/usr/bin/env bash
echo "sbt $PWD" >> "$STUB/calls"
echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"
'''
# the mirror's freshness check: records how it was asked, answers as $FRESHNESS_EXIT says
FRESHNESS = '''#!/usr/bin/env bash
echo "freshness $*" >> "$STUB/calls"
echo "de  kinowo_de_prod_mirror: synced …"
exit "${FRESHNESS_EXIT:-0}"
'''
# xargs as GNU's, CI's: with no input it still runs its command once -- macOS's runs nothing (main 38707274817)
XARGS = '''#!/usr/bin/env bash
input=$(mktemp); cat > "$input"
if [ ! -s "$input" ]; then
  while [ "${1:-}" != "${1#-}" ]; do shift; done
  rm -f "$input"; exec "$@" < /dev/null
fi
exec "%s" "$@" < "$input"
'''


def zinc_build(checkout: str, module: str, *classes: str) -> None:
    """scratch-zinc.sh's scratch_zinc_build: each class file (and its .tasty) under <module>'s classes, and a stand-in
    Zinc analysis listing them as its products -- a class file written after it is an orphan no build produced."""
    target = os.path.join(checkout, module, "target/scala-3.x")
    for c in classes:
        os.makedirs(os.path.dirname(os.path.join(target, "classes", c)), exist_ok=True)
        for path, text in ((c, "class\n"), (c[:-len(".class")] + ".tasty", "tasty\n")):
            with open(os.path.join(target, "classes", path), "w") as f:
                f.write(text)
    products = [f"${{BASE}}/{module}/target/scala-3.x/classes/{c}" for c in classes]
    body = b"".join(_proto_string(p) for p in products + [f"${{BASE}}/{module}/src/main/scala/Source.scala"])
    os.makedirs(os.path.join(target, "zinc"), exist_ok=True)
    with zipfile.ZipFile(os.path.join(target, "zinc/inc_compile_3.zip"), "w") as z:
        z.writestr("inc_compile.bin", body)


def _proto_string(text: str) -> bytes:
    data, n, varint = text.encode(), len(text.encode()), bytearray()
    while True:
        varint.append((n & 0x7F) | (0x80 if n > 0x7F else 0))
        n >>= 7
        if not n:
            return b"\x0a" + bytes(varint) + data


def count(pattern: str, text: str) -> int:
    """`grep -c`: the lines of `text` the regex matches."""
    return sum(1 for line in text.splitlines() if re.search(pattern, line))


def keeper_of(jvm: int) -> List[int]:
    """The keeper watching <jvm>: the process whose arguments hold `--keep-served <jvm>` as two words, not a subshell
    of its poll (which carries the same command line for a moment) -- a substring match would take a co-agent's keeper
    of JVM 4242 for one of JVM 424."""
    rows = subprocess.run(["ps", "-axo", "pid=,ppid=,args="], capture_output=True, text=True).stdout.splitlines()
    parents: Dict[int, int] = {}
    for row in rows:
        words = row.split()
        if len(words) > 2 and any(w == "--keep-served" and nxt == str(jvm) for w, nxt in zip(words[2:], words[3:])):
            parents[int(words[0])] = int(words[1])
    return sorted(pid for pid, parent in parents.items() if parent not in parents)


def children(pid: int, command: str) -> Set[int]:
    """The pids of <pid>'s children running <command> (its first word)."""
    rows = subprocess.run(["ps", "-axo", "pid=,ppid=,args="], capture_output=True, text=True).stdout.splitlines()
    return {int(w[0]) for w in (r.split() for r in rows)
            if len(w) > 2 and w[1] == str(pid) and os.path.basename(w[2]) == command}


class SliceCase(cli_harness.ToolCase):
    """The scratch repository (`self.repo_dir`, commits `self.base` and `self.head`), the cache (`self.cache`), the
    stubs, and `slice`: the tool run there, its stderr kept in `self.err`."""

    def setUp(self) -> None:
        super().setUp()
        self.cache = os.path.realpath(tempfile.mkdtemp(prefix="slice-", dir="/tmp"))
        self.repo_dir = self.path("repo")
        self.err = ""
        self._lay_out()
        self.git(self.repo_dir, "init", "-q")
        self.base = self.commit_all("base")
        with open(self.path("repo/build.sbt"), "a") as f:
            f.write("// head\n")
        self.head = self.commit_all("head")
        self._stubs()
        self._recording()
        self.tool = os.path.join(self.repo_dir, os.path.relpath(TOOL, ROOT))

    def _lay_out(self) -> None:
        self.copy_scripts("repo")
        for name in EXTENSIONLESS:
            shutil.copy2(os.path.join(ROOT, "scripts", name), self.path("repo/scripts"))
        for f in tool_files():
            self.write("repo/" + f, f"// {f}\n")
        for module in ("scrapers", "geodata"):
            self.write(f"repo/{module}/src/main/Module.scala", f"// {module}\n")
        for d in ("common/src", "identity/src", "testkit/src", "test/resources/fixtures/identity-unmatched"):
            os.makedirs(self.path("repo", d), exist_ok=True)
        self.write("repo/build.sbt", "build\n")
        os.chmod(self.write("repo/scripts/local-mirror/freshness", FRESHNESS), 0o755)
        self.write("repo/" + JUDGEMENTS, "country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\t"
                   "judged\tevidence\n")
        self.write("repo/" + BACKLOG, "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n")

    def _stubs(self) -> None:
        xargs = shutil.which("xargs") or "/usr/bin/xargs"
        for name, text in (("bin/java", JAVA), ("bin/sbt", SBT), ("bin/xargs", XARGS % xargs)):
            os.chmod(self.write(name, text), 0o755)
        self.write("serve.py", SERVE)

    def _recording(self) -> None:
        """Recording 777's pair, already in the cache: nothing is downloaded."""
        fixtures = os.path.join(self.cache, "recordings/777/pair/test/resources/fixtures")
        os.makedirs(os.path.join(fixtures, "enrichment-de"))
        os.makedirs(os.path.join(fixtures, "corpus"))
        with open(os.path.join(fixtures, "corpus/cinema-scrapes-de.json.gz"), "w") as f:
            f.write("corpus\n")
        with open(os.path.join(fixtures, "corpus/prod-coverage-de.json"), "w") as f:
            f.write('{"recordedAt":"2026-10-14T05:00:00Z"}\n')

    def tearDown(self) -> None:
        self._end_kept_jvms()
        super().tearDown()

    def _end_kept_jvms(self) -> None:
        """Whatever a case left running in the cache (kept JVMs, their keepers) stopped, then the cache removed."""
        self.stop_served()
        for pidf in glob.glob(os.path.join(self.cache, "serve", "*.pid")):
            self.kill(self.content(pidf))
        shutil.rmtree(self.cache, ignore_errors=True)

    @staticmethod
    def kill(pid: str) -> None:
        if pid.isdigit():
            try:
                os.kill(int(pid), signal.SIGKILL)
            except OSError:
                pass

    def tool_env(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, str]:
        env: Dict[str, Optional[str]] = {
            "STUB": self.work, "STUB_BASE": self.base, "STUB_HEADER": HEADER, "KINOWO_IDENTITY_CACHE": self.cache,
            "PATH": self.path("bin") + os.pathsep + os.environ.get("PATH", ""),
            "KINOWO_IDENTITY_SLICE_KEEPER_POLL": "0.1"}
        env.update(extra or {})
        return self.env(env)

    def slice(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None, cwd: Optional[str] = None,
              tool: Optional[str] = None) -> cli_harness.Done:
        """The tool (or `tool`) with `args`, from `cwd` (the scratch directory by default); its stderr in `self.err`."""
        done = cli_harness.run_bounded([tool or self.tool, *args], env=self.tool_env(env), cwd=cwd or self.work,
                                       stdin=None)
        self.err = done.stderr
        return done

    def start(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None,
              tool: Optional[str] = None) -> "subprocess.Popen[str]":
        """The tool in the background, in a process group of its own, its stdout dropped and its stderr into <work>/err."""
        with open(self.path("err"), "w") as err:
            return subprocess.Popen([tool or self.tool, *args], env=self.tool_env(env), cwd=self.work, text=True,
                                    stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=err,
                                    start_new_session=True)

    def commit_all(self, message: str) -> str:
        return self.commit_tree(self.repo_dir, message)

    def stop_served(self) -> None:
        self.slice("--stop-served")

    def calls(self) -> str:
        """What the stubs ran (java, sbt, the freshness check, each kept JVM's requests), a line each."""
        return self.read("calls") if os.path.exists(self.path("calls")) else ""

    def clear_calls(self) -> None:
        if os.path.exists(self.path("calls")):
            os.remove(self.path("calls"))

    def last(self, pattern: str) -> str:
        """The last match of `pattern` in the calls (`grep -o ... | tail -1`)."""
        found = re.findall(pattern, self.calls())
        return found[-1] if found else ""

    def ref_tree(self, sha: str, *parts: str) -> str:
        return os.path.join(self.cache, "refs", sha, *parts)

    def pid_files(self) -> List[str]:
        return sorted(glob.glob(os.path.join(self.cache, "serve", "*.pid")))

    def served_pid(self) -> int:
        """The one kept JVM's pid."""
        files = self.pid_files()
        self.assertEqual(1, len(files), files)
        return int(self.content(files[0]))

    @staticmethod
    def content(path: str) -> str:
        try:
            with open(path) as f:
                return f.read().strip()
        except OSError:
            return ""

    @staticmethod
    def lock_state(lock: str) -> str:
        """`held` while some process holds `lock`'s kernel lock, else `free` (shell-spec.sh's spec_lock_state)."""
        done = subprocess.run(["bash", "-c", '. "$1"; if lock_busy "$2"; then echo held; else echo free; fi', "_",
                               os.path.join(ROOT, "scripts/jvm-gate.sh"), lock],
                              env=cli_harness.isolated_env(os.environ), capture_output=True, text=True)
        return done.stdout.strip()
