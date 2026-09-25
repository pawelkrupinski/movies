import { createFleetPage } from "./fleet/page.js";
import { createMobilePage } from "./mobile/page.js";
import type { Page } from "./page.js";
import { WakeDetector } from "./wake.js";

/** The composition root: the one place sources are built and handed to the pages. The first page
 * is also what `/` redirects to. */
export function createPages(): Page<unknown>[] {
  const wake = new WakeDetector();
  const pages = [createFleetPage(wake), createMobilePage(wake)] as Page<unknown>[];
  const [first] = pages;
  if (first) {
    // The wake detector is shared, so it starts and stops with the process, not with either page.
    const start = first.start.bind(first);
    const stop = first.stop.bind(first);
    first.start = async () => {
      wake.start();
      await start();
    };
    first.stop = () => {
      stop();
      wake.stop();
    };
  }
  return pages;
}
