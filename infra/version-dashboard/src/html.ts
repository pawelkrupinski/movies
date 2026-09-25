/** Escape text for HTML body and attribute context. Every interpolation in a view goes through
 * `html` below, which calls this unless a value is explicitly `raw`. */
export function escapeHtml(value: string | number | boolean | null | undefined): string {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

export interface Raw {
  readonly __raw: string;
}

export const raw = (markup: string): Raw => ({ __raw: markup });

type Part = Raw | string | number | boolean | null | undefined | readonly Part[];

function renderPart(part: Part): string {
  if (part === null || part === undefined || part === false) return "";
  if (typeof part !== "object") return escapeHtml(part);
  if ("__raw" in part) return part.__raw;
  return part.map(renderPart).join("");
}

/**
 * Tagged template for markup: interpolations are escaped by default, arrays are joined, `false`
 * and nullish render nothing, and nested `html` results pass through unescaped. Replaces the
 * Python page's hand-escaped f-strings, where every value had to remember its own `esc()`.
 */
export function html(strings: TemplateStringsArray, ...values: Part[]): Raw {
  let out = strings[0] ?? "";
  values.forEach((value, index) => {
    out += renderPart(value) + (strings[index + 1] ?? "");
  });
  return raw(out);
}
