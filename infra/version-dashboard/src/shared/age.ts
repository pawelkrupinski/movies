/**
 * A unix timestamp as a short age, "never" for none. Both pages use it, on the server's first
 * render and in the browser's re-renders -- so `now` is always passed in, and the two agree.
 */
export function ago(timestamp: number | null | undefined, now: number): string {
  if (!timestamp) return "never";
  const seconds = Math.trunc(now - timestamp);
  if (seconds < 90) return `${seconds}s ago`;
  if (seconds < 5400) return `${Math.floor(seconds / 60)}m ago`;
  return `${Math.floor(seconds / 3600)}h ago`;
}
