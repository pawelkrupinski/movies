"""THE PYTHON FUNCTION MEASURES (scripts/py-quality/report.py), from the source's own syntax tree (stdlib `ast`):

  cognitive   Sonar's cognitive complexity: +1 for each if / elif / else, loop, except, ternary and run of one boolean
              operator; if, loops, except and ternaries add their nesting depth too (a nested def or lambda deepens it)
  cyclomatic  McCabe's: 1, +1 for each if / elif, loop, except, ternary, comprehension `for` and `if`, assert, and each
              boolean operand past the first
  inner-defs  the named functions a function holds (nested `def`s, at any depth; lambdas are no def)
  lines       the def's physical lines, decorators to its last line

Each `def` is measured by itself: a nested one's body counts toward its own measures, not its holder's (the JS ratchet
measures a callback alike), though the holder's inner-defs counts it."""
from __future__ import annotations

import ast
from dataclasses import dataclass
from typing import Iterator, List, Optional, Tuple, Union

FunctionNode = Union[ast.FunctionDef, ast.AsyncFunctionDef]
LOOPS = (ast.For, ast.AsyncFor, ast.While)


@dataclass(frozen=True)
class Function:
    name: str        # qualified within its file: `Class.method`, `outer.inner`
    line: int
    lines: int
    cognitive: int
    cyclomatic: int
    inner_defs: int


def own_children(node: ast.AST) -> Iterator[ast.AST]:
    """The node's children, a nested def's or class's body left out (each is measured by itself)."""
    for child in ast.iter_child_nodes(node):
        if not isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            yield child


def cyclomatic(node: ast.AST) -> int:
    total = 0
    for child in own_children(node):
        total += cyclomatic(child)
    if isinstance(node, (ast.If, ast.IfExp, ast.ExceptHandler, ast.Assert) + LOOPS):
        total += 1
    elif isinstance(node, ast.BoolOp):
        total += len(node.values) - 1
    elif isinstance(node, ast.comprehension):
        total += 1 + len(node.ifs)
    return total


def is_elif(node: ast.If, parent: Optional[ast.AST]) -> bool:
    return isinstance(parent, ast.If) and len(parent.orelse) == 1 and parent.orelse[0] is node


def if_increment(node: ast.If, parent: Optional[ast.AST], nesting: int) -> int:
    """An `if` adds 1 and its nesting (an `elif` no nesting: it continues its if), and 1 for a plain `else`."""
    plain_else = 1 if node.orelse and not is_elif_chain(node) else 0
    return 1 + (0 if is_elif(node, parent) else nesting) + plain_else


def increment(node: ast.AST, parent: Optional[ast.AST], nesting: int) -> Tuple[int, bool]:
    """What `node` adds to the cognitive complexity at `nesting`, and whether it deepens its children's nesting."""
    if isinstance(node, ast.If):
        return if_increment(node, parent, nesting), True
    if isinstance(node, LOOPS + (ast.ExceptHandler, ast.IfExp)):
        return 1 + nesting, True
    if isinstance(node, ast.BoolOp):
        return 1, False  # a run of one operator (`a and b and c` is one BoolOp; `a and b or c` two)
    return 0, isinstance(node, ast.Lambda)


def is_elif_chain(node: ast.If) -> bool:
    """Whether the `else` of this if is an `elif` (counted on that if itself) rather than a plain `else`."""
    return len(node.orelse) == 1 and isinstance(node.orelse[0], ast.If)


def cognitive(node: ast.AST, parent: Optional[ast.AST] = None, nesting: int = 0) -> int:
    total = 0
    for child in own_children(node):
        add, deepens = increment(child, node, nesting)
        inner = nesting if is_elif_child(child, node) else nesting + (1 if deepens else 0)
        total += add + cognitive(child, node, inner)
    return total


def is_elif_child(child: ast.AST, node: ast.AST) -> bool:
    return isinstance(child, ast.If) and is_elif(child, node)


def inner_defs(node: ast.AST) -> int:
    return sum(1 for n in ast.walk(node) if n is not node and isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef)))


def first_line(node: FunctionNode) -> int:
    return min([node.lineno] + [d.lineno for d in node.decorator_list])


def measure(node: FunctionNode, name: str) -> Function:
    end = node.end_lineno or node.lineno
    return Function(name=name, line=node.lineno, lines=end - first_line(node) + 1, cognitive=cognitive(node),
                    cyclomatic=1 + cyclomatic(node), inner_defs=inner_defs(node))


def functions(tree: ast.AST, prefix: str = "") -> List[Function]:
    """Every def of `tree`, at any depth, each measured by itself, in source order."""
    out: List[Function] = []
    for node in ast.iter_child_nodes(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            out.append(measure(node, prefix + node.name))
            out += functions(node, f"{prefix}{node.name}.")
        elif isinstance(node, ast.ClassDef):
            out += functions(node, f"{prefix}{node.name}.")
        else:
            out += functions(node, prefix)
    return sorted(out, key=lambda f: f.line)
