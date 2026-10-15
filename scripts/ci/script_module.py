"""A tool whose file name is no module name -- `check-security-headers.py`, an extension-less `scripts/<tool>` -- imported
for its test. The ONE by-path loader the Python tests share: a tool named like a module is imported plainly instead.

    sys.path.insert(0, "<repo>/scripts/ci"); import script_module
    check = script_module.load(path)            # module named after the file, `-` as `_`
"""

import importlib.machinery
import importlib.util
import os
import re


def load(path, name=None):
    """The module at `path`, executed as an import (its `__main__` block does not run). `spec_from_file_location`
    alone infers a loader from the suffix, so an extension-less file gets none: the source loader is named here."""
    name = name or re.sub(r"\W", "_", os.path.basename(str(path)).rsplit(".py", 1)[0])
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_file_location(name, str(path), loader=loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module
