import { afterEach, describe, expect, it } from "vitest";
import { HttpError, httpRequest, setHttpClient, type HttpClient } from "../src/http.js";

// httpRequest used to hand back any status for the caller to interpret -- the shape that let a
// failed read be read as "no data". A non-2xx now throws unless the caller names it.
describe("httpRequest", () => {
  let restore: HttpClient | null = null;
  afterEach(() => {
    if (restore) setHttpClient(restore);
    restore = null;
  });
  const answering = (status: number, body = "") => {
    restore = setHttpClient(async () => ({ status, headers: new Headers(), body }));
  };

  it("returns a 2xx answer", async () => {
    answering(204);
    expect((await httpRequest("https://x/", { timeoutMs: 1 })).status).toBe(204);
  });

  it("throws an HttpError on a non-2xx the caller did not accept", async () => {
    answering(503, "<html>Service Unavailable</html>");
    await expect(httpRequest("https://x/", { timeoutMs: 1 })).rejects.toBeInstanceOf(HttpError);
    answering(404);
    await expect(httpRequest("https://x/", { timeoutMs: 1 })).rejects.toThrow("HTTP 404 from https://x/");
  });

  it("returns a non-2xx the caller listed as an answer", async () => {
    answering(404, "none");
    const response = await httpRequest("https://x/", { timeoutMs: 1, acceptStatuses: [404] });
    expect(response.status).toBe(404);
    answering(500);
    await expect(httpRequest("https://x/", { timeoutMs: 1, acceptStatuses: [404] })).rejects.toBeInstanceOf(HttpError);
  });
});
