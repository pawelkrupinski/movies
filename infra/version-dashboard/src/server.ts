import Fastify, { type FastifyReply, type FastifyRequest } from "fastify";
import { build } from "esbuild";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { HOST, PORT, ROOT } from "./config.js";
import { drain, isDraining, runningWork } from "./lifecycle.js";
import type { Page } from "./page.js";
import { createPages } from "./pages.js";
import { shell } from "./shell.js";

const HEARTBEAT_MS = 15_000;

async function bundle(name: string): Promise<string> {
  const result = await build({
    entryPoints: [join(ROOT, "web", `${name}.ts`)],
    bundle: true,
    format: "esm",
    target: "es2022",
    write: false,
    sourcemap: "inline",
  });
  return result.outputFiles[0]?.text ?? "";
}

function streamSnapshots(page: Page<unknown>, request: FastifyRequest, reply: FastifyReply): void {
  reply.hijack();
  const out = reply.raw;
  out.writeHead(200, {
    "Content-Type": "text/event-stream",
    "Cache-Control": "no-store",
    Connection: "keep-alive",
  });
  const send = (snapshot: unknown) => out.write(`event: snapshot\ndata: ${JSON.stringify(snapshot)}\n\n`);
  send(page.store.get());
  const unsubscribe = page.store.subscribe(send);
  const heartbeat = setInterval(() => out.write(`: ${Date.now()}\n\n`), HEARTBEAT_MS);
  request.raw.on("close", () => {
    clearInterval(heartbeat);
    unsubscribe();
  });
}

async function main(): Promise<void> {
  const pages = createPages();
  const app = Fastify({ logger: { level: "warn" }, forceCloseConnections: true });
  // NO-STORE ON EVERYTHING. The log endpoint is polled with an increasing offset; a cached answer
  // to it is worse than no answer, because it looks current.
  app.addHook("onSend", async (_request, reply) => {
    reply.header("Cache-Control", "no-store");
  });

  const assets = new Map<string, { type: string; body: string }>();
  assets.set("styles.css", { type: "text/css", body: await readFile(join(ROOT, "web", "styles.css"), "utf8") });
  for (const page of pages) assets.set(`${page.name}.js`, { type: "text/javascript", body: await bundle(page.name) });
  app.get<{ Params: { file: string } }>("/assets/:file", async (request, reply) => {
    const asset = assets.get(request.params.file);
    if (!asset) return reply.code(404).send("not found");
    return reply.type(asset.type).send(asset.body);
  });

  const [home] = pages;
  if (home) app.get("/", async (_request, reply) => reply.redirect(home.path));
  for (const page of pages) {
    for (const path of [page.path, `${page.path}/`]) {
      app.get(path, async (_request, reply) => reply.type("text/html; charset=utf-8").send(shell(page, pages)));
    }
    app.get(`/events/${page.name}`, (request, reply) => streamSnapshots(page, request, reply));
    page.routes?.(app);
  }

  // What the watchdog probes. HTTP rather than a bare TCP connect, so a wedged event loop -- which
  // still accepts connections at the kernel -- reads as down.
  app.get("/healthz", async () => ({
    ok: true,
    draining: isDraining(),
    running: runningWork(),
    pages: Object.fromEntries(pages.map((page) => {
      const { version, checkedAt } = page.store.get();
      return [page.name, { version, checkedAt }];
    })),
  }));

  let stopping = false;
  const shutdown = async (signal: string) => {
    if (stopping) return;
    stopping = true;
    const busy = runningWork();
    if (busy.length) console.log(`${signal}: draining ${busy.map((work) => work.label).join(", ")} before exit`);
    await drain();
    pages.forEach((page) => page.stop());
    await app.close();
    process.exit(0);
  };
  process.on("SIGTERM", () => void shutdown("SIGTERM"));
  process.on("SIGINT", () => void shutdown("SIGINT"));

  // Bind before any source starts: the watchdog restarts a process whose port does not answer,
  // and a slow start-up that looked dead would be killed mid-build into a crash loop.
  await app.listen({ port: PORT, host: HOST });
  for (const page of pages) console.log(`kinowo ${page.title} on http://${HOST}:${PORT}${page.path}`);
  for (const page of pages) await page.start();
}

main().catch((error: unknown) => {
  console.error(error);
  process.exit(1);
});
