import { setExecutor } from "../src/exec.js";
import { setHttpClient } from "../src/http.js";

// No test may reach a real host, cluster, registry or GitHub: a stray ssh from a test could run
// `switch-to-configuration` on a production machine. A test that needs a command or a response
// installs its own fake for the duration and restores the refusal after.
setExecutor(async (argv) => {
  throw new Error(`test tried to run a real command: ${argv.join(" ")}`);
});
setHttpClient(async (url) => {
  throw new Error(`test tried to reach the network: ${url}`);
});
