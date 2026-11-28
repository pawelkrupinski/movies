"""THE FOOTPRINT MEASURE of identity-measure (--footprint; scripts/identity_measure_cli.py, its header): a title-rule
change proved, on the laptop, to move no take -- the base's stored title readings, one trace a country, read again
under the head's rules (services.movies.NormalizerTraceCheck, compiled in the head's tree) -- or said why the full
measure must decide."""
from __future__ import annotations

import gzip
import hashlib
import json
import os
import re
import shlex
import sys
from typing import List, Optional, Tuple

import tool_process
from processes import tool_python
from identity_measure_cli_context import HERE, IDENTITY_TOOLS, Context, say
from identity_measure_cli_paths import TakePaths
from identity_measure_cli_results import Results

TITLE_RULES = "common/src/main/scala/services/titles/rules/"
TRACE_CHECK = "testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala"
# testkit's runtime classpath in the head's tree, compiled once (sbt only when a source is newer, under a JVM slot),
# then the JVM every identity tool runs in (identity-tools.sh's IDENTITY_JAVA), one word a line
CLASSPATH = r'''tree=$1 cp=$1/target/identity-tools/testkit-classpath log=$1/target/identity-tools/testkit-sbt.log
if ! [ -s "$cp" ] || [ -n "$(find "$tree/geodata/src" "$tree/common/src" "$tree/identity/src" "$tree/testkit/src" "$tree/build.sbt" -newer "$cp" -name '*.s*' -print -quit 2> /dev/null)" ]; then
  mkdir -p "$(dirname "$cp")"
  echo "identity-measure: footprint: compiling the head's title rules in $tree" >&2
  jvm_slot_take identity-measure
  if ! (cd "$tree" && sbt $(jvm_cpu_options | sed 's/^/-J/') -batch "testkit/compile" "export testkit/Runtime/fullClasspath") > "$log" 2>&1; then
    jvm_slot_release; tail -20 "$log" >&2; echo "identity-measure: footprint: the head's compile failed ($log)" >&2; false
  else
    jvm_slot_release
    grep -E '^/[^ ]*testkit/target/' "$log" | tail -n 1 > "$cp"
    [ -s "$cp" ] || { rm -f "$cp"; echo "identity-measure: footprint: no classpath in sbt's output ($log)" >&2; false; }
  fi
fi && head -1 "$cp" && printf '%s\n' "${IDENTITY_JAVA[@]}"'''


def rule_path(path: str) -> bool:
    """Whether the resolve reads a change there only through the title normaliser's readings."""
    if path == TITLE_RULES + "TitleRulePlaceholders.scala":  # FormatTags reads it directly
        return False
    return path in ("common/src/main/resources/title-rules.tsv",
                    "common/src/main/scala/services/titles/TitleNormalizer.scala") \
        or (path.startswith(TITLE_RULES) and path.endswith(".scala"))


class Footprint:
    def __init__(self, c: Context, paths: TakePaths, results: Results) -> None:
        self.c, self.paths, self.results = c, paths, results
        self.release = c.env.get("TAKES_RELEASE_TAG") or "identity-takes"
        self.started = c.clock()

    def measure(self) -> Optional[int]:
        """The verdict when no take can move (its table printed); None (said why) when the full measure decides."""
        self.started = self.c.clock()
        if not self.title_rules_only():
            return None
        traces = self.traces()
        check = self.check_command() if traces else None
        if traces is None or check is None:
            return None
        out = self.c.scratch_dir()
        moved: List[str] = []
        for cc, trace in traces:
            verdict = self.check(check, cc, trace, out)
            if verdict is None:
                return None
            moved += [cc] if verdict else []
        if moved:
            say(f"footprint: {','.join(moved)} can move -- the full measure decides ({self.spent()}s spent)")
            return None
        return self.unmoved([cc for cc, _ in traces], out)

    def spent(self) -> int:
        return int(self.c.clock() - self.started)

    def title_rules_only(self) -> bool:
        s = self.c.state
        patterns = self.paths.patterns(s.commit)
        if patterns is None:
            say(f"footprint: {s.commit} has no .github/convergence-paths.txt -- the full measure")
            return False
        for path in self.paths.moving(s.base, s.commit, patterns):
            if not rule_path(path):
                say(f"footprint: {path} can move a take outside the title rules -- no trace says what it decides: the "
                    "full measure")
                return False
        return True

    def traces(self) -> Optional[List[Tuple[str, str]]]:
        """The base's traces, one a footprint country, over the recording: --traces' or the release's."""
        s, given = self.c.state, self.c.o.traces
        if given:
            found: List[Tuple[str, str]] = []
            for cc in s.countries.split(","):
                path = os.path.join(given, f"{cc}.jsonl.gz")
                if not os.path.isfile(path) or not os.path.getsize(path):
                    say(f"footprint: no {path} -- the full measure")
                    return None
                say(f"footprint: {cc}'s base readings: {path} (--traces: you vouch they are {s.base[:9]}'s)")
                found.append((cc, path))
            return found if self.code() else None
        return self.released()

    def code(self) -> Optional[str]:
        """The takes check's identity paths' tree at the base: the key a side's trace is stored under."""
        done = self.c.runner.capture(tool_process.Command(
            tool_python.isolated(os.path.join(HERE, "ci", "identity_takes_check_paths.py")), self.c.env))
        paths = done.stdout.split()
        listing = self.c.git.out("ls-tree", "-r", self.c.state.base, "--", *paths, quiet=True) if paths else None
        if listing is None:
            say("footprint: no identity paths in the takes check -- the full measure")
            return None
        return hashlib.sha256(listing.encode()).hexdigest()[:24]

    def released(self) -> Optional[List[Tuple[str, str]]]:
        code = self.code()
        names = self.c.gh(["release", "view", self.release, "--json", "assets", "--jq",
                           '.assets | sort_by(.createdAt) | reverse | .[] | "\\(.name) \\(.size)"']) if code else None
        if code and names is None:
            say("footprint: could not list the release's traces -- the full measure")
        if not code or names is None:
            return None
        found: List[Tuple[str, str]] = []
        for cc in self.c.state.countries.split(","):
            trace = self.trace(cc, code, names)
            if trace is None:
                return None
            found.append((cc, trace))
        return found

    def trace(self, cc: str, code: str, names: str) -> Optional[str]:
        s = self.c.state
        side = f"n{s.recording}" if s.recording else "p[0-9]+"
        found = re.search(rf"(?m)^(takes-trace-{cc}-{code}-{side}\.gz) (\S*)", names)
        if not found:
            over = f"recording {s.recording}" if s.recording else "the pinned pair"
            say(f"footprint: no trace of {cc} at the base's identity code ({code}, {s.base[:9]}) over {over} -- the "
                "full measure")
            return None
        asset, size = found.group(1), found.group(2)
        # kept by its name and size: the newest trace of a code over a recording replaces an older one of the name
        trace = os.path.join(self.c.cache, "footprint", f"{asset[:-3]}.{size}.jsonl.gz")
        if not (os.path.isfile(trace) and os.path.getsize(trace)) and not self.download(asset, trace):
            say(f"footprint: could not download {asset} -- the full measure")
            return None
        say(f"footprint: {cc}'s base readings: {asset}")
        return trace

    def download(self, asset: str, trace: str) -> bool:
        part = trace + ".part.gz"
        try:
            if not self.c.github.download(self.release, asset, part, 1800):
                return False
            with gzip.open(part) as packed, open(trace + ".part", "wb") as f:
                f.write(packed.read())
            os.replace(trace + ".part", trace)
            return True
        except OSError:
            return False
        finally:
            for left in (part, trace + ".part"):
                if os.path.exists(left):
                    os.remove(left)

    def check_command(self) -> Optional[List[str]]:
        """NormalizerTraceCheck in the head's tree (IDENTITY_MEASURE_TRACE_CHECK: a test's stand-in, no tree or JVM)."""
        given = self.c.env.get("IDENTITY_MEASURE_TRACE_CHECK")
        if given:
            return [given]
        laid = self.c.shell(IDENTITY_TOOLS, "identity_tree", [self.c.state.commit, TRACE_CHECK])
        if laid.code != 0 or not laid.stdout.strip():
            say(f"footprint: could not lay out {self.c.state.commit[:9]} -- the full measure")
            return None
        built = self.c.shell(IDENTITY_TOOLS, "eval", [CLASSPATH.replace("$1", shlex.quote(laid.stdout.strip().split("\n")[-1]))])
        words = built.stdout.splitlines()
        if built.code != 0 or len(words) < 2:
            return None
        return [*words[1:], "-cp", words[0], "services.movies.NormalizerTraceCheck"]

    def check(self, check: List[str], cc: str, trace: str, out: str) -> Optional[bool]:
        """Whether the country's readings read otherwise (or cannot be made again); None when they could not be
        checked."""
        differ, log = os.path.join(out, f"differ-{cc}.tsv"), os.path.join(out, f"check-{cc}.txt")
        rc = self.c.runner.run(tool_process.Command([*check, "--out", differ, trace], self.c.env, log=log))
        with open(log, errors="replace") as f:
            text = f.read()
        said = [line for line in text.splitlines() if line.startswith("[normalizer-trace]")]
        say(f"footprint: {cc}: {said[-1][len('[normalizer-trace] '):] if said else ''}")
        if rc == 1:
            say(f"footprint: {cc} reads titles otherwise -- takes may move (the first of them: country, method, "
                "arguments, base, head):")
            with open(differ, errors="replace") as f:
                sys.stderr.write("".join(f"    {line}" for line in f.readlines()[:20]))
        elif rc == 3:
            say(f"footprint: {cc}: some readings cannot be made again (above; {differ}) -- no proof either way")
        elif rc != 0:
            sys.stderr.write(text)
            say(f"footprint: {cc}'s readings could not be checked -- the full measure")
            return None
        return rc != 0

    def unmoved(self, countries: List[str], out: str) -> int:
        """No take can move: the measure's own table, every footprint country unmoved."""
        s = self.c.state
        for cc in countries:
            os.makedirs(os.path.join(out, f"identity-takes-{cc}"))
            with open(os.path.join(out, f"identity-takes-{cc}", "base.txt"), "w") as f:
                f.write(s.base + "\n")
            with open(os.path.join(out, f"identity-takes-{cc}", "verdict.txt"), "w") as f:
                f.write(f"identity takes ({cc}): footprint -- every one of the base's title readings reads alike under "
                        "the head's rules: no take can have moved\n")
        judgements, rows, table = (os.path.join(out, n) for n in ("identity-judgements.tsv", "rows.json", "table.txt"))
        self.c.judgements_into(s.commit, judgements)
        say(f"footprint: proved in {self.spent()}s")
        self.c.say_commit_and_base()
        with open(rows, "w") as f:
            json.dump({cc: "footprint" for cc in countries}, f)
        rc = self.c.classify(out, judgements, rows, table, ["--kind", "footprint"])
        self.results.store("footprint", rc, "footprint", table)
        return rc
