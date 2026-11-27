"""THE SHELL READER the claude-hooks share: a Bash command as words and operators, the way the shell splits it.

  command_words(command)       the words and operators of `command`, its backslash-newline continuations joined and its
                               heredoc bodies dropped; ValueError when shlex cannot tokenize it
  tokens(text)                 the raw split of one piece of shell text (shlex runs adjacent operators together,
                               `)|` or `&&\\n`: split apart here)
  without_heredoc_bodies(text) `text` with every heredoc's body removed (its lines up to the delimiter line)
  is_punctuation(word)         whether a token is a shell operator
  ASSIGNMENT, HEREDOC, SHELLS  the shapes the guards match on

Stdlib only; each guard imports it from scripts/lib, not by loading another guard's file."""
from __future__ import annotations

import re
import shlex
from typing import List

SHELLS = {"bash", "sh", "zsh"}  # `bash -c '<script>'`: the script is read as a command of its own
ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
HEREDOC = re.compile(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")


def without_heredoc_bodies(command: str) -> str:
    """The command with every heredoc's body removed (its lines up to the delimiter line)."""
    lines, out, closing = command.split("\n"), [], []
    for line in lines:
        if closing:
            if line.strip() == closing[0]:
                closing.pop(0)
            continue
        out.append(line)
        closing.extend(m.group(2) for m in HEREDOC.finditer(line))
    return "\n".join(out)


OPERATORS = sorted(["&&", "||", ";;", "|&", "&>>", "&>", ">>", ">&", "<&", "<<<", "<<", "<>", ">|",
                    "(", ")", ";", "&", "|", "<", ">", "\n"], key=len, reverse=True)


def tokens(command: str) -> List[str]:
    """Words and shell operators; shlex runs adjacent operators together (`)|`, `&&\\n`), split them apart here."""
    lexer = shlex.shlex(command, posix=True, punctuation_chars="();<>|&\n")
    lexer.whitespace = " \t\r"
    lexer.whitespace_split = True
    lexer.commenters = "#"
    out = []
    for token in lexer:
        if not is_punctuation(token):
            out.append(token)
            continue
        while token:
            operator = next(o for o in OPERATORS if token.startswith(o))
            out.append(operator)
            token = token[len(operator):]
    return out


def is_punctuation(token: str) -> bool:
    return token != "" and all(c in "();<>|&\n" for c in token)


def command_words(command: str) -> List[str]:
    """The words and operators of a whole command: a backslash-newline continues the line (never a separator), a
    heredoc's body is no command."""
    return tokens(without_heredoc_bodies(command.replace("\\\n", " ")))
