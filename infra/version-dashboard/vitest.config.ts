import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["test/**/*.test.ts"],
    environment: "node",
    // Sub-second tests flaked at the 5s default under runner contention in the bitcashier dashboard
    // (b09a483 era); the budget is generous, the tests are not slow.
    testTimeout: 15_000,
    // Every external reach goes through src/exec.ts and src/http.ts; setup makes both refuse, so
    // no test can ssh to a host, dispatch a deploy or read a live cluster by accident.
    setupFiles: ["test/setup.ts"],
  },
});
