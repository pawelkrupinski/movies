"""What the CI report tools share about a field, one copy each: a value written into a Markdown table cell, and an
ISO-8601 instant as GitHub's API and the run logs write it (a trailing `Z` for UTC) read back.

    sys.path.insert(0, "<repo>/scripts/ci"); from report_fields import cell, parse_instant
"""

import datetime


def cell(text):
    """`text` as one Markdown table cell: a `|` escaped (it would end the cell), a newline flattened (it would end the row)."""
    return str(text).replace("|", "\\|").replace("\n", " ")


def parse_instant(text):
    """An ISO-8601 instant, `Z` read as UTC: Python 3.9's fromisoformat (the floor, run-script-tests' PythonFloor)
    reads `+00:00` only."""
    return datetime.datetime.fromisoformat(text.replace("Z", "+00:00"))
