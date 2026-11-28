"""GITHUB ACTIONS' WORKFLOW COMMANDS, the one way the Python tools speak to a step (scripts/lib): an annotation
(`::error title=T::message`), a log group, a step output (`name=value` appended to $GITHUB_OUTPUT) and the step summary
(appended to $GITHUB_STEP_SUMMARY) and a variable for the job's later steps (`NAME=value` appended to $GITHUB_ENV). Off
Actions -- those files unset or empty -- an output or a summary goes nowhere, as a
shell tool's `>> "${GITHUB_OUTPUT:-/dev/null}"` does; annotations are plain stdout lines either way."""
from __future__ import annotations

from typing import Mapping, TextIO


class Actions:
    def __init__(self, env: Mapping[str, str], out: TextIO) -> None:
        self.env = env
        self.out = out

    def annotate(self, level: str, title: str, message: str) -> None:
        """`level` is error, warning or notice."""
        self.say(f"::{level} title={title}::{message}")

    def group(self, title: str) -> None:
        self.say(f"::group::{title}")

    def end_group(self) -> None:
        self.say("::endgroup::")

    def say(self, line: str) -> None:
        self.out.write(line + "\n")
        self.out.flush()

    def output(self, name: str, value: str) -> None:
        self._append("GITHUB_OUTPUT", f"{name}={value}\n")

    def export(self, name: str, value: str) -> None:
        self._append("GITHUB_ENV", f"{name}={value}\n")

    def summary(self, text: str) -> None:
        self._append("GITHUB_STEP_SUMMARY", text)

    def _append(self, variable: str, text: str) -> None:
        path = self.env.get(variable, "")
        if path:
            with open(path, "a") as f:
                f.write(text)
