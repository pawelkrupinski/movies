"""The `sbt` of the tools' e2e worlds that run scripts/sbt-warm (sbt_warm_e2e_test, land_precheck_e2e_test) or the identity tools
(identity_stamp_carry_e2e_test, identity_tools_e2e_test): `sbt --client
<command>` as sbt's client answers it, at the process boundary. The first call in a checkout starts a stand-in server (a
`sleep` carrying a server's command line, its socket in project/target/active.json, its pid appended to $E2E_SERVERS);
$E2E_SBT_MODE's file says whether commands pass (`pass`), fail (`fail`), name a failing spec (`spec-fails`), never reach
a server (`never-ran`) or fail once on a stale TASTy read (`stale-once`). The identity tools' own `sbt -batch <tasks>`
answers the worker's classpath and the stamps task whatever the mode (IDENTITY_BUILD)."""
import os
from typing import Dict, List, Optional

import e2e_world

# The identity tools' own `sbt -batch <tasks>` (identity-tools.sh: no client, no server): the worker's classpath, and the
# stamps task, whose three digests the SCENARIO controls through the tree's own files -- each stamp's digest is a hash of the
# paths and text under identity/src/main/scala/e2e/<stamp>/, as the build's is of the sources it digests, so a commit that
# moves a file or edits a line moves the digest, and one that touches nothing there leaves it.
IDENTITY_BUILD = """
STAMPS = ("identity-rules", "venue-slot", "agreement-rules")
if "-batch" in argv:
    if "identity/Compile/managedResources" in argv:
        here = "identity/target/scala-3.x/resource_managed/main/services/identity"
        os.makedirs(here, exist_ok=True)
        for stamp in STAMPS:
            digest = hashlib.sha256(stamp.encode())
            for base, _, names in sorted(os.walk("identity/src/main/scala/e2e/" + stamp)):
                for name in sorted(names):
                    digest.update((os.path.join(base, name) + "\\n").encode() + open(os.path.join(base, name), "rb").read())
            with open(os.path.join(here, stamp + "-digest.txt"), "w") as f:
                f.write(digest.hexdigest() + "\\n")
            with open(os.path.join(here, stamp + "-version.txt"), "w") as f:
                f.write("v" + stamp + "\\n")
    elif "export worker/Test/fullClasspath" in argv:
        out(os.getcwd() + "/worker/target/scala-3.x/test-classes:/lib/a.jar")
    sys.exit(0)
"""
SERVER = " ".join(("java", "-jar", "/fake/sbt-launch.jar", "--detach-stdio"))  # as sbt --client's server shows it
# `sbt --client <command>`: the first call in a checkout starts the stand-in server (never left holding our pipes), the
# mode file says whether commands pass, fail, or fail once with a stale TASTy
STUB = f"""
import hashlib, re, subprocess
{IDENTITY_BUILD}
mode = open(os.environ["E2E_SBT_MODE"]).read().strip()
active = "project/target/active.json"
if not os.path.exists(active):
    os.makedirs("project/target", exist_ok=True)
    server = subprocess.Popen(["bash", "-c", "trap '' QUIT; exec -a '{SERVER}' sleep 120"], start_new_session=True,
                              stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    with open(os.environ["E2E_SERVERS"], "a") as kept:
        kept.write(str(server.pid) + "\\n")
    with open(active, "w") as f:
        f.write('{{"uri":"local://' + os.getcwd() + '/sock"}}\\n')
if mode == "fail":
    out("[error] boom"); sys.exit(1)
if mode == "spec-fails":
    out("[info] SomeLintSpec:"); out("[info] - names no wall clock *** FAILED ***"); out("[info]   Foo.scala uses Instant.now")
    out("[error] Failed tests:"); sys.exit(1)
if mode == "never-ran":
    out("[error] failed to connect to server"); sys.exit(1)
if mode == "stale-once" and argv != ["--client", "staleTastyDrop"] and not os.path.exists(os.environ["E2E_SBT_MODE"] + ".done"):
    open(os.environ["E2E_SBT_MODE"] + ".done", "w").close()
    out("[error] Ignoring analyses of x due to error in reading TASTy"); sys.exit(1)
"""


class StubSbt:
    """The stub sbt installed in a world, and what its scenario asks of it."""

    def __init__(self, world: "e2e_world.World") -> None:
        self.w = world
        self.mode_file, self.servers_file = (os.path.join(world.root, n) for n in ("sbt-mode", "sbt-servers"))
        self.set_mode("pass")
        world.write(world.root, "sbt-servers", "")
        world.shim("sbt", STUB)

    def set_mode(self, mode: str) -> None:
        self.w.write(self.w.root, "sbt-mode", mode)
        if self.w.files.exists(self.mode_file + ".done"):
            self.w.files.remove(self.mode_file + ".done")

    def env(self) -> Dict[str, Optional[str]]:
        """The environment of a scenario's sbt-warm calls: its state, slot and cache directories inside the world."""
        root = self.w.root
        return {"E2E_SBT_MODE": self.mode_file, "E2E_SERVERS": self.servers_file,
                "KINOWO_SBT_WARM_STATE": os.path.join(root, "state"), "KINOWO_IDENTITY_CACHE": os.path.join(root, "identity"),
                "KINOWO_SBT_CACHE": os.path.join(root, "cache"), "KINOWO_SBT_CACHE_PUBLISH": "0",
                "KINOWO_JVM_SLOTS": "1", "KINOWO_JVM_SLOT_MAX_WAIT": "10", "KINOWO_MEMORY_GATE": "0",
                "KINOWO_SBT_WARM_KEEPER_POLL": "0.2", "KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS": "2", "MONGODB_URI": None}

    def servers(self) -> List[int]:
        return [int(p) for p in self.w.read(self.servers_file).split()]

    def stop(self, tree: str) -> None:
        """What a scenario owes at its end: the checkout's keeper and server stopped, any stand-in left killed."""
        self.w.run([os.path.join(tree, "scripts/sbt-warm"), "--stop"], tree, env=self.env(), bound=60)
        for pid in self.servers():
            e2e_world.machine.send(pid, 9)
