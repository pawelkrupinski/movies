/**
 * "What's new" text for both stores.
 *
 * The App Store refuses to submit a version with an empty whatsNew once any version of the app
 * has been released, and Play shows its release notes on the listing. Neither is kept in the repo,
 * so a release either brings its own (--notes-dir, one `<locale>.txt` per store locale or per
 * language: `pl.txt` covers Play's pl-PL and the App Store's pl) or ships the generic line below.
 */
import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

const GENERIC: Readonly<Record<string, string>> = {
  pl: "Poprawki błędów i ulepszenia.",
  en: "Bug fixes and improvements.",
  de: "Fehlerbehebungen und Verbesserungen.",
  es: "Corrección de errores y mejoras.",
};

/** Play caps a release note at 500 characters; the App Store allows 4000. */
export const PLAY_NOTES_LIMIT = 500;

export type Notes = (locale: string) => string;

const language = (locale: string) => locale.split(/[-_]/)[0]?.toLowerCase() ?? locale;

/** Notes per locale: the exact locale's file, else its language's, else the generic line. */
export function notesFrom(files: ReadonlyMap<string, string>): Notes {
  return (locale) => files.get(locale) ?? files.get(language(locale)) ?? GENERIC[language(locale)] ?? (GENERIC["en"] as string);
}

/** `<locale>.txt` files out of `dir`, trimmed; an empty file is refused rather than shipped blank. */
export async function readNotesDir(dir: string): Promise<Map<string, string>> {
  const files = new Map<string, string>();
  for (const name of await readdir(dir)) {
    if (!name.endsWith(".txt")) continue;
    const text = (await readFile(join(dir, name), "utf8")).trim();
    if (!text) throw new Error(`${join(dir, name)} is empty`);
    files.set(name.slice(0, -".txt".length), text);
  }
  return files;
}
