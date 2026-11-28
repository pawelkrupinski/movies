"""DECIDING A SLICE AT A REF (scripts/identity_slice_cli.py): a tree's scripts.IdentitySlice run over the recording --
through the ref's KEPT JVM with --serve (one per tree, ref, country, recording and filed answers, started when none is,
its JVM slot held by its keeper beside it: `--keep-served`), else in a JVM of its own under a JVM slot. Every JVM
sequence that takes a slot is identity-tools.sh's (identity_java_slotted_to, identity_kept_jvm_start,
identity_kept_jvm_request): a slot is a lock its process holds, never this one's."""
from __future__ import annotations

import os
from typing import List, Sequence

from identity_tooling.ci.identity_kept_jvms import KeptJvmFiles, alive, is_socket, read, remove  # (scripts/lib)
from identity_slice_cli_context import LABEL, Context
from identity_slice_cli_keys import Keys

SLICE = "scripts.IdentitySlice"
KEPT_PATTERN = r"scripts\.IdentitySlice"
GONE = (111, 112)  # kept_jvm_call: nothing listens at the socket / the JVM went away mid-request


class Slicer:
    def __init__(self, ctx: Context, keys: Keys, keeper: str) -> None:
        self.ctx, self.keys, self.keeper = ctx, keys, keeper
        self.kept = KeptJvmFiles(ctx.runner, ctx.env, ctx.log, os.path.join(ctx.cache, "serve"), KEPT_PATTERN)
        # A kept JVM's idle exit: as sbt-warm's kept server's, 10 minutes, not 30 -- each holds a JVM slot while idle
        self.idle_minutes = ctx.knob("KINOWO_IDENTITY_SLICE_IDLE_MINUTES", "10")

    def run_at(self, tree: str, ref: str, out: str, args: Sequence[str], stdout: str = "-") -> int:
        """That tree's IdentitySlice of the slice `args` select, its takes into `out` and what it prints into `stdout`."""
        o = self.ctx.o
        code = self.keys.code_key(self.keys.ref_of(ref))
        more: List[str] = ["--index-key", code]
        results = self.keys.results_of(code, o.run)
        if results:
            more += ["--results", results]
        if o.threads:
            more += ["--threads", o.threads]
        if o.serve == "1" and not o.mirror and not o.faith:
            return self.serve_at(tree, ref, out, [*more, *args], stdout)
        cp = self.ctx.classpath(tree)
        if cp is None:
            return 1
        return self.ctx.java(tree, stdout, "-", ["-cp", cp, SLICE, *self.opened(ref), "--labels", o.labels, "--families",
                                                 o.families, "--out", out, *self.ctx.mirror_args, *more, *args])

    def opened(self, ref: str) -> List[str]:
        """What a JVM opens the recording with: the country, the recording, its cache and the ref."""
        o = self.ctx.o
        return [o.cc, "--recording", self.ctx.recording, "--cache", f"{self.ctx.cache}/slice/{o.run}", "--ref", ref]

    def serve_at(self, tree: str, ref: str, out: str, args: Sequence[str], stdout: str) -> int:
        """The batch sent to the ref's kept JVM, started when none is: the recording opened and the index loaded once
        per (tree, ref, country, recording, filed answers), not once per call. A JVM that died is started again."""
        o = self.ctx.o
        key = Keys.serve_key(tree, ref, o.cc, self.ctx.recording, o.families)
        sock = self.kept.path(key, ".sock")
        for _ in range(2):
            if not is_socket(sock) or not alive(read(self.kept.path(key, ".pid")) or "0"):
                if not self.start(tree, ref, key, sock):
                    return 1
            # this request's cores (jvm-gate.sh): the keeper holds only the memory
            argv = self.ctx.command("identity_kept_jvm_request", [LABEL, "slice", "-", sock, "--labels", o.labels,
                                                                  "--out", out, *args]).argv
            rc = self.ctx.run(argv, stdout)
            if rc not in GONE:
                return rc
            self.ctx.log(f"the kept JVM went away ({rc}): starting another")
            remove(sock)
        return 1

    def start(self, tree: str, ref: str, key: str, sock: str) -> bool:
        """A kept JVM of the tree started and listening at `sock`; False (said) when it did not."""
        cp = self.ctx.classpath(tree)
        if cp is None:
            return False
        os.makedirs(self.kept.dir, exist_ok=True)
        remove(sock)
        # an edit of this checkout is a new ref key, and each kept JVM holds up to 6 GB: the older state's is stopped
        self.kept.drop_older(key, tree, f"stopped the kept JVM of an older state of {tree}")
        log = self.kept.path(key, ".log")
        self.ctx.log(f"starting a kept JVM for {ref[:24]} (log {log}; stops after {self.idle_minutes} idle minutes)")
        # ONE JVM, ONE SLOT: this call takes a slot (the memory gate first) and hands it to a keeper beside the JVM,
        # which holds it until the JVM has gone; a call reaching a live kept JVM takes none
        java = ["-cp", cp, SLICE, *self.opened(ref), "--index-key", self.keys.code_key(self.keys.ref_of(ref)),
                "--families", self.ctx.o.families, "--serve", sock, "--idle-minutes", self.idle_minutes]
        return self.listening(self.kept.start(self.ctx.shell, LABEL, tree, key, self.keeper, java), sock, log)

    def listening(self, pid: str, sock: str, log: str) -> bool:
        """Waits for the JVM to listen: one alive but never listening (stuck loading, a deadlock) is stopped at the
        deadline, not waited on forever."""
        limit = int(self.ctx.knob("KINOWO_IDENTITY_SLICE_START_SECONDS", "900"))
        started = self.ctx.clock()
        while not is_socket(sock):
            if not alive(pid):
                self.ctx.tail(20, log)
                self.ctx.log("the kept JVM did not start")
                return False
            if int(self.ctx.clock()) - int(started) >= limit:
                self.kept.stop(pid)
                remove(self.kept.path(os.path.basename(log)[:-len(".log")], ".pid"))
                self.ctx.tail(20, log)
                self.ctx.log(f"the kept JVM did not listen within {limit}s: stopped ($KINOWO_IDENTITY_SLICE_START_SECONDS)")
                return False
            self.ctx.sleep(1)
        return True

    def stop_all(self) -> None:
        self.kept.stop_all((".sock", ".tree", ".keeper.log"))
