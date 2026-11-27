#!/usr/bin/env python3
"""Full corpus reset: wipe the scrape, enrichment, staging and read-model collections so the worker rebuilds them (incidents)

Wipes the scrape / enrichment / staging / read-model collections so the worker re-scrapes every cinema from scratch and
re-projects the read model; serving is empty until that completes (a few minutes of sparse repertoire -- acceptable per
the "short downtime is fine" rule).

Target: PROD -- the `web-<cc>` + `worker-<cc>` Deployments on k3s, prod Mongo over the ssh tunnel
scripts/lib/prod_tunnel.py opens to the Hetzner Mongo host. Both are scaled to ZERO first so the worker can't race a
half-finished scrape into a collection we're dropping, and scaled back only after the wipe lands (or, when the reset
does not finish, on the way out). Needs a kubeconfig for the cluster.

Drops: detailCache, freshness, movies, tasks, web_movies, web_screenings and the identity model's derived state
(identity_model_families, identity_model_meta, identity_traces, identity_slot_fingerprints) -- everything the
scrape->identity projection pipeline rebuilds from scratch.
  NOT dropped -- operational state, curated config, and accounts the worker does not rebuild: uptimeBuckets,
  uptimeServiceTags (monitoring history), users, userStates (accounts). Add filmwebFallback / filmwebFallbackMeta
  (per-cinema fallback prober state) to COLLECTIONS if you want a fully-clean slate that re-derives those too. (Title
  rules are code, not a collection: see common/src/main/scala/services/titles/rules/.) scheduled_runs is kept too
  (worker scheduling -- dropping it re-fires every sweep at once).

Reads MONGODB_URI (prod tunnel) from the environment, else from the Touch ID vault (this tool runs itself again under
`secrets run movies MONGODB_URI`), and MONGODB_DB from the environment (default `kinowo`, Poland's database; the
country's, `kinowo_<cc>`, from KINOWO_RESET_COUNTRY, default pl).

Usage:  scripts/reset-corpus.sh [--dry-run] [--yes]
          --dry-run   list which collections WOULD be dropped, change nothing
          --yes       skip the confirmation prompt"""
from __future__ import annotations

import os
import sys
import time
from dataclasses import dataclass
from typing import List, Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
sys.path.insert(0, HERE)
import machine  # noqa: E402
import prod_tunnel  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
import vault_env  # noqa: E402

MONGOSH_AT = os.path.join(HERE, "local-mirror", "mongosh-at.sh")
COLLECTIONS = ["detailCache", "freshness", "movies", "tasks", "web_movies", "web_screenings",
               "identity_model_families", "identity_model_meta", "identity_traces", "identity_slot_fingerprints"]
say = tool_cli.say_as("reset")


@dataclass(frozen=True)
class Options:
    dry: bool = False
    yes: bool = False


def say_out(line: str) -> None:
    print(f"[reset] {line}", flush=True)


def say_error(line: str) -> None:
    print(f"[reset] {line}", file=sys.stderr, flush=True)


def parsed(argv: List[str]) -> Options:
    dry = yes = False
    for arg in argv:
        if arg == "--dry-run":
            dry = True
        elif arg == "--yes":
            yes = True
        else:
            raise tool_cli.UsageError(f"unknown argument: {arg} (see the header for usage)")
    return Options(dry, yes)


def country_database(country: str) -> str:
    return "kinowo" if country == "pl" else f"kinowo_{country}"


def wipe_script(database: str, dry: bool) -> str:
    """The mongosh program: `getSiblingDB` names the database explicitly -- the tunnel URI may carry none, so a bare
    `db` would resolve to `test`."""
    wanted = ",".join(f'"{c}"' for c in COLLECTIONS)
    return f"""
  var dry = {1 if dry else 0} == 1;
  var target = db.getSiblingDB('{database}');
  var wanted = [{wanted}];
  var existing = target.getCollectionNames();
  wanted.forEach(function (c) {{
    if (existing.indexOf(c) === -1) {{ print('  skip       ' + c + ' (absent)'); return; }}
    if (!dry) target.getCollection(c).drop();
    print('  ' + (dry ? 'would drop ' : 'dropped   ') + c);
  }});
"""


class Fatal(Exception):
    def __init__(self, line: str, code: int = 1) -> None:
        super().__init__(line)
        self.code = code


class Reset:
    def __init__(self, env: Mapping[str, str], options: Options, uri: str, runner: tool_process.Runner) -> None:
        self.env, self.options, self.runner = env, options, runner
        self.namespace = env.get("KUBE_NAMESPACE") or "kinowo"
        self.country = env.get("KINOWO_RESET_COUNTRY") or "pl"
        self.database = country_database(self.country)
        self.tunnel = prod_tunnel.ProdTunnel("reset", uri, env, [MONGOSH_AT], lambda seconds: time.sleep(float(seconds)))
        self.uri = uri
        self.scaled_down = False

    def kubectl(self, *args: str) -> int:
        return self.runner.run(tool_process.Command(["kubectl", "-n", self.namespace, *args], self.env))

    def scale(self, name: str, replicas: int) -> None:
        """Fatal on failure: the point of scaling to zero is that nothing writes while collections are dropped, so a
        scale that silently failed would leave the worker racing the wipe."""
        say_out(f"scaling {name} to {replicas}...")
        if self.kubectl("scale", f"deploy/{name}", f"--replicas={replicas}") != 0:
            raise Fatal(f"could not scale {name} -- is your kubeconfig pointed at the cluster?")
        if replicas == 0:
            code = self.kubectl("rollout", "status", f"deploy/{name}", "--timeout=120s")
            if code != 0:
                raise Fatal("", code)

    def confirmed(self) -> bool:
        if sys.stdin.isatty():
            sys.stderr.write("[reset] This wipes the prod corpus. Type 'wipe' to proceed: ")
            sys.stderr.flush()
        return sys.stdin.readline().rstrip("\n") == "wipe"

    def wipe(self) -> None:
        say_out(f"{'[dry-run] ' if self.options.dry else ''}collections in '{self.database}':")
        code = self.runner.run(tool_process.Command(
            [MONGOSH_AT, self.uri, "--quiet", "--eval", wipe_script(self.database, self.options.dry)], self.env))
        if code != 0:
            raise Fatal("", code)

    def scale_back_up(self) -> None:
        """Web FIRST (serving matters most), then the worker; every deployment is attempted even when one fails, and
        `scaled_down` stays True until all are up so the failure path retries them. Fatal naming what stayed down."""
        failed: List[str] = []
        for tier in ("web", "worker"):
            try:
                self.scale(f"{tier}-{self.country}", 1)
            except Fatal as e:
                say_error(str(e))
                failed.append(f"{tier}-{self.country}")
        if failed:
            raise Fatal(f"could not scale back up: {' '.join(failed)}")
        self.scaled_down = False

    def scale_back_up_after_a_failure(self) -> None:
        if self.scaled_down:
            say_error(f"the reset did not finish -- scaling web-{self.country} and worker-{self.country} back up")
            try:
                self.scale_back_up()
            except Fatal as e:
                say_error(f"{e} -- scale them up by hand")

    def run(self) -> int:
        dry = self.options.dry
        say_out(f"prod -- target db '{self.database}'; {'would ' if dry else ''}drop: {' '.join(COLLECTIONS)}")
        say_out(f"will scale down+up: deploy/web-{self.country}, deploy/worker-{self.country} (ns {self.namespace})")
        if not dry and not self.options.yes and not self.confirmed():
            say_out("aborted.")
            return 1
        try:
            self.reset()
        except Fatal as e:
            if str(e):
                say_error(str(e))
            return e.code
        finally:
            self.scale_back_up_after_a_failure()
            self.tunnel.close()
        say_out("dry-run only -- nothing changed." if dry else
                "done. Worker will re-scrape + re-project; serving is sparse until it catches up.")
        return 0

    def reset(self) -> None:
        # The tunnel FIRST: scaled to zero and then finding prod unreachable, the tool exited with the web tier down.
        # Fatal, unlike the mirror daemon's retry: doing a drop against the wrong (or no) database is not a thing to
        # keep retrying into.
        if not self.tunnel.ensure():
            raise Fatal("prod Mongo not reachable via the tunnel")
        if not self.options.dry:
            self.scaled_down = True
            self.scale(f"web-{self.country}", 0)
            self.scale(f"worker-{self.country}", 0)
        self.wipe()
        if not self.options.dry:
            self.scale_back_up()


def tunnel_form(uri: str, env: Mapping[str, str]) -> str:
    return prod_tunnel.tunnel_uri(uri, prod_tunnel.free_port(env, os.getpid()) or "27017")


def main(argv: List[str], env: Mapping[str, str]) -> int:
    try:
        options = parsed(argv)
    except tool_cli.UsageError as e:
        say_error(str(e))
        return 1
    if not vault_env.exec_under_vault(["MONGODB_URI"], [sys.executable, os.path.abspath(__file__), *argv]):
        print("[vault] MONGODB_URI not set, and no `secrets` CLI on PATH to read it from the vault", file=sys.stderr)
        return 1
    country = env.get("KINOWO_RESET_COUNTRY") or "pl"
    expected = country_database(country)
    if env.get("MONGODB_DB") and env["MONGODB_DB"] != expected:
        # The database is the COUNTRY's (models.Country.mongoDb); it must be the one whose web/worker get scaled to
        # zero, so an exported MONGODB_DB naming another country's is refused rather than wiped while that country serves.
        say_error(f"MONGODB_DB={env['MONGODB_DB']} is not {country}'s database ({expected}) -- refusing")
        return 1
    tool_cli.raise_on_signals()
    try:
        return Reset(env, options, tunnel_form(env["MONGODB_URI"], env), tool_process.SubprocessRunner()).run()
    except tool_cli.Interrupted as e:
        return 128 + e.signum


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
