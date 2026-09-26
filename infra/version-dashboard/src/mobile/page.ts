import type { FastifyInstance } from "fastify";
import { REPO_DIR } from "../config.js";
import type { Page } from "../page.js";
import type { MobileSources } from "./build.js";
import { MobileLive } from "./live.js";
import type { MobileState } from "./model.js";
import { androidReleaseState, ascApi, iosReleaseState, playClient } from "./stores.js";
import { renderMobile } from "./view.js";

type WakeSource = { readonly onWake: (listener: () => void) => void };

/** The real sources: this checkout's git, App Store Connect and the Play Developer API. */
export function mobileSources(repoDir: string = REPO_DIR): MobileSources {
  const asc = ascApi(repoDir);
  const play = playClient(repoDir);
  return { repoDir, ios: () => iosReleaseState(asc.get), android: () => androidReleaseState(play), now: Date.now };
}

export function createMobilePage(wake: WakeSource, live = new MobileLive(mobileSources())): Page<MobileState> & { live: MobileLive } {
  // A sleep outlives the 10-minute timer; the page re-reads the stores as soon as the Mac wakes.
  wake.onWake(() => void live.rebuild());
  return {
    name: "mobile",
    path: "/mobile",
    title: "Mobile releases",
    store: live.store,
    live,
    start: () => live.start(),
    stop: () => live.stop(),
    renderBody: (snapshot) => renderMobile(snapshot.state, Date.now()).__raw,
    routes(app: FastifyInstance) {
      // Starts a build and answers at once: the new state reaches every tab over the event stream,
      // so the wait is visible on the page rather than in a request somebody is staring at.
      app.post("/mobile/refresh", async () => {
        void live.rebuild();
        return { ok: true };
      });
    },
  };
}
