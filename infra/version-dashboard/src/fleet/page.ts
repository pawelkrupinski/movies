import type { FastifyInstance } from "fastify";
import { INFRA_DIR } from "../config.js";
import type { Page } from "../page.js";
import { FleetJobs } from "./jobs.js";
import { FleetLive, realSources } from "./live.js";
import type { FleetState } from "./model.js";
import { renderFleet } from "./view.js";

export function createFleetPage(wake: { onWake(listener: () => void): void }): Page<FleetState> {
  const page = createFleetPageFrom(new FleetLive((onRosterChange) => realSources(INFRA_DIR, onRosterChange)));
  // Every poller re-reads at once when the Mac wakes: a sleep outlives every interval.
  wake.onWake(() => void page.live.refreshNow());
  return page;
}

/**
 * The page over any live state (tests pass one built on fakes).
 *
 * EVERY ACTION IS A POST, AND THERE IS NO GET THAT DOES ANYTHING. Not a REST nicety: a GET that
 * switches a host is one a browser prefetch, a link preview or a history restore can fire on its
 * own.
 */
export function createFleetPageFrom(live: FleetLive): Page<FleetState> & { live: FleetLive; jobs: FleetJobs } {
  const jobs = new FleetJobs({
    machineOf: (name) => live.machine(name),
    onSwitched: (machine, closure) => void live.awaitSwitchLanded(machine, closure),
  });
  return {
    name: "fleet",
    path: "/nixos",
    title: "NixOS fleet",
    store: live.store,
    live,
    jobs,
    start: () => live.start(),
    stop: () => live.stop(),
    renderBody: (snapshot) => renderFleet(snapshot.state, Date.now() / 1000).__raw,
    routes(app: FastifyInstance) {
      app.post("/fleet-apply", async (request, reply) => {
        const { status, payload } = jobs.start(request.body);
        return reply.code(status).send(payload);
      });
      app.get<{ Querystring: { job?: string; from?: string } }>("/fleet-apply/log", async (request) => {
        const from = Number.parseInt(request.query.from ?? "0", 10);
        return jobs.log(request.query.job ?? "", Number.isFinite(from) ? Math.max(0, from) : 0);
      });
      app.get<{ Querystring: { machine?: string } }>("/fleet-apply/machine", async (request) => live.readOne(request.query.machine ?? ""));
      // Re-polls every source now and answers at once: the new state arrives over the stream.
      app.post("/nixos/refresh", async () => {
        void live.refreshNow();
        return { ok: true };
      });
    },
  };
}
