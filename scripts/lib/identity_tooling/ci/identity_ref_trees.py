"""A REF'S CODE LAID OUT AND COMPILED for the local identity tools that run a JVM under it (scripts/lib; title_trace.py,
venue_replay.py): identity-tools.sh's identity_tree (the ref laid out in the shared cache with this checkout's tool
files dropped in), identity_classpath (its worker test classpath, compiled once) and IDENTITY_JAVA, called through
tool_shell until that library moves; each failure None, said on stderr by the library itself.

  RefTrees(env, runner, tool).tree(ref, files)  the laid-out tree's path
  .classpath(tree)                              the tree's classpath
  .java()                                       IDENTITY_JAVA's argv (java and its options)
  .run_java(tree, args, options, stdout)        IDENTITY_JAVA <options> -cp <the tree's classpath> <args> run from the
                                                tree, its stdout into the file `stdout` (None: this tool's), its stderr
                                                this tool's as it comes; 1 when the tree does not compile
  .function(name, args)                         any other identity-tools.sh function's answer
  .call(name, args)                             ... the whole call: its status and output
  .log(line)                                    identity_log's line: `[<tool>] <line>` on stderr"""
from __future__ import annotations

import sys
from typing import List, Mapping, Optional, Sequence

import tool_process
from processes import tool_shell

IDENTITY_TOOLS = "scripts/identity-tools.sh"


class RefTrees:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner, tool: str) -> None:
        self.env, self.runner, self.tool = env, runner, tool

    def call(self, name: str, args: Sequence[str]) -> tool_process.Captured:
        return tool_shell.shell_function(self.runner, self.env, IDENTITY_TOOLS, name, args,
                                         prelude=f"IDENTITY_TOOL={self.tool}")

    def function(self, name: str, args: Sequence[str]) -> Optional[str]:
        done = self.call(name, args)
        return done.stdout.rstrip("\n") if done.code == 0 else None

    def tree(self, ref: str, files: Sequence[str]) -> Optional[str]:
        return self.function("identity_tree", [ref, *files])

    def classpath(self, tree: str) -> Optional[str]:
        return self.function("identity_classpath", [tree])

    def java(self) -> Optional[List[str]]:
        argv = self.function("printf '%s\\n' \"${IDENTITY_JAVA[@]}\"", [])
        return None if argv is None else argv.split("\n")

    def run_java(self, tree: str, args: Sequence[str], options: Sequence[str] = (),
                 stdout: Optional[str] = None) -> int:
        classpath, java = self.classpath(tree), self.java()
        if classpath is None or java is None:
            return 1
        argv = [*java, *options, "-cp", classpath, *args]
        if stdout is not None:
            argv = ["sh", "-c", 'out=$1; shift; exec "$@" > "$out"', "sh", stdout, *argv]
        return self.runner.run(tool_process.Command(argv, self.env, cwd=tree))

    def log(self, line: str) -> None:
        print(f"[{self.tool}] {line}", file=sys.stderr, flush=True)
