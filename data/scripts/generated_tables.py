"""Write a generated table as the tab-separated classpath resource `models.GeneratedTable`
reads: one row a line, cells joined by tabs, a list cell's items joined by `|`, an absent
optional cell empty. Refuses any cell the split could not give back exactly."""
import pathlib

LIST_SEPARATOR = "|"


def list_cell(items):
    items = list(items)
    for item in items:
        if LIST_SEPARATOR in item or item == "":
            raise SystemExit(f"list item {item!r} is empty or holds {LIST_SEPARATOR!r}")
    return LIST_SEPARATOR.join(items)


def option_cell(value):
    if value == "":
        raise SystemExit("an optional cell's present value must not be empty")
    return "" if value is None else value


def write_table(path, rows):
    """Write `rows` (sequences of str cells) to `path`, creating its directory."""
    lines = []
    for row in rows:
        for cell in row:
            if not isinstance(cell, str) or "\t" in cell or "\n" in cell or "\r" in cell:
                raise SystemExit(f"{path}: cell {cell!r} is not a single-line string without tabs")
        lines.append("\t".join(row))
    path = pathlib.Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(line + "\n" for line in lines), encoding="utf-8")
