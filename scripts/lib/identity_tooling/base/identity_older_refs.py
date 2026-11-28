"""A REF FROM BEFORE A MOVE COMPILES WITH THE TOOL FILES OF TODAY (scripts/lib; identity-tools.sh's
identity_tree_older_titles and identity_tree_older_models): a tool file naming the classes' new names compiles in a ref
laid out before they moved, through one file there exporting them under the new names.

  older_titles(tree)  in a ref from before the title text took its own package (services.titles, 2026-11-18): each
                      top-level class of services.movies / services.resolution that moved, services.titlerules as
                      services.titles.rules and services.billing as services.titles.billing, exported under the new
                      name (only those the ref has). A ref after the move has the real ones, and gets none.
  older_models(tree)  in a ref from before the corpus' value types moved into models (ListingKey, ShowtimesDigest,
                      EmbeddedYear, TrailerEmbed; 2026-11-18), a file exporting them under their new names from
                      services.movies. A ref after the move has the real ones."""
from __future__ import annotations

import glob
import os
import re
from typing import List, Sequence

from seams.tool_files import Files

FROM_MOVIES = ("TitleNormalizer", "TitleWords", "FormatTags", "SequelMarker", "RelayBanner", "TitleText",
               "TitleContainment", "BbfcCertificate", "TitleSeason", "TitleFacts", "StageHouse")
FROM_RESOLUTION = ("TitleMatch", "YearWindow", "SearchTitles")
SUBPACKAGES = (("titlerules", "rules"), ("billing", "billing"))
VALUE_TYPES = ("ListingKey", "ShowtimesDigest", "EmbeddedYear", "TrailerEmbed")
SCALA = "common/src/main/scala"


def declares(path: str, declared: "re.Pattern[str]") -> bool:
    with open(path, encoding="utf-8", errors="replace") as source:
        return any(declared.match(line) for line in source)


def exports(package: str, directory: str, names: Sequence[str]) -> List[str]:
    """The export clauses of a package's names its <directory> declares."""
    found: List[str] = []
    for name in names:
        path = os.path.join(directory, f"{name}.scala")
        declared = re.compile(rf"^(final |sealed |abstract |case |private\[[a-z]+\] )*(class|object|trait) {name}\b")
        if os.path.isfile(path) and declares(path, declared):
            found.append(f"export {package}.{name}")
    return found


def written(files: Files, path: str, lines: Sequence[str]) -> None:
    """The lines as the file, unless it holds them already (an unchanged copy keeps its time, and is not compiled again)."""
    text = "\n".join(lines).rstrip("\n") + "\n"
    files.make_directories(os.path.dirname(path))
    if files.read_text_or(path, "") != text or not files.exists(path):
        files.write_text(path, text)


def older_titles(files: Files, tree: str) -> None:
    main = os.path.join(tree, SCALA, "services")
    if not (os.path.isfile(os.path.join(main, "movies", "TitleNormalizer.scala"))
            and not os.path.isfile(os.path.join(main, "titles", "TitleNormalizer.scala"))):
        return
    lines = ["package services.titles", "",
             "// identity-tools.sh (identity_tree_older_titles): this ref predates services.titles"]
    lines += exports("services.movies", os.path.join(main, "movies"), FROM_MOVIES)
    lines += exports("services.resolution", os.path.join(main, "resolution"), FROM_RESOLUTION)
    for source, target in SUBPACKAGES:
        lines += subpackage(os.path.join(main, source), source, target)
    written(files, os.path.join(main, "titles", "OlderRefTitles.scala"), lines)


def subpackage(directory: str, source: str, target: str) -> List[str]:
    if not os.path.isdir(directory):
        return []
    lines = ["", f"package {target} {{"]
    for path in sorted(glob.glob(os.path.join(directory, "*.scala"))):
        name = os.path.basename(path)[:-len(".scala")]
        lines += ["  " + clause for clause in exports(f"services.{source}", directory, [name])]
    return lines + ["}"]


def older_models(files: Files, tree: str) -> None:
    movies = os.path.join(tree, SCALA, "services", "movies")
    models = os.path.join(tree, SCALA, "models")
    if not (os.path.isfile(os.path.join(movies, "ListingKey.scala"))
            and not os.path.isfile(os.path.join(models, "ListingKey.scala"))):
        return
    lines = ["package models", "", "// identity-tools.sh (identity_tree_older_models): this ref predates models.ListingKey"]
    lines += [f"export services.movies.{n}" for n in VALUE_TYPES if os.path.isfile(os.path.join(movies, f"{n}.scala"))]
    written(files, os.path.join(models, "OlderRefModels.scala"), lines)
