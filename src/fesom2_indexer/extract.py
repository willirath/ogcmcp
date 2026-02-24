"""Extract code structure from FESOM2 Fortran 90 source files.

Handles:
- Free-form F90: MODULE / CONTAINS / END MODULE structure
- Subroutines and functions nested inside modules
- Top-level subroutines/functions (module_name = '')
- pFUnit test files (.pf): @macro lines stripped before parsing
- Minimal CPP (#ifdef/#endif blocks passed through; guards not tracked)
- USE statements for inter-module dependency graph
- CALL statements for call graph
"""

import re
from dataclasses import dataclass, field
from pathlib import Path


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

@dataclass
class ModuleRecord:
    name: str
    file: str
    start_line: int
    end_line: int
    uses: list[str] = field(default_factory=list)


@dataclass
class SubroutineRecord:
    name: str
    module_name: str   # '' for top-level (outside any MODULE)
    file: str
    start_line: int
    end_line: int
    source_text: str
    calls: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Regex patterns
# ---------------------------------------------------------------------------

# MODULE name — but NOT "END MODULE" or "MODULE PROCEDURE"
RE_MODULE_START = re.compile(
    r'^\s*MODULE\s+(?!PROCEDURE\b)(\w+)\s*(?:!.*)?$', re.IGNORECASE
)
RE_MODULE_END = re.compile(r'^\s*END\s+MODULE\b', re.IGNORECASE)

# SUBROUTINE / FUNCTION with optional prefixes
_PREFIXES = r'(?:(?:PURE|ELEMENTAL|RECURSIVE|MODULE|IMPURE)\s+)*'
RE_SUB_START = re.compile(
    rf'^\s*{_PREFIXES}SUBROUTINE\s+(\w+)', re.IGNORECASE
)
RE_FUNC_START = re.compile(
    rf'^\s*{_PREFIXES}(?:\w+(?:\s*\([^)]*\))?\s+)?FUNCTION\s+(\w+)', re.IGNORECASE
)
RE_SUB_END = re.compile(
    r'^\s*END\s*(?:SUBROUTINE|FUNCTION)\b', re.IGNORECASE
)
# Bare END (ends whatever is open — module or subroutine)
RE_BARE_END = re.compile(r'^\s*END\s*(?:!.*)?$', re.IGNORECASE)

# Inner block openers/closers that use END but are not subroutine ends
_INNER = r'DO|IF|SELECT|WHERE|FORALL|ASSOCIATE|BLOCK|CRITICAL|ENUM'
RE_INNER_END = re.compile(rf'^\s*END\s*(?:{_INNER})\b', re.IGNORECASE)
RE_INNER_START = re.compile(rf'^\s*(?:{_INNER})\b', re.IGNORECASE)

# USE statement — capture module name (stops at comma or colon)
RE_USE = re.compile(
    r'^\s*USE\s+(?:,\s*\w+\s*::\s*)?(\w+)', re.IGNORECASE
)
RE_CALL = re.compile(r'\bCALL\s+(\w+)', re.IGNORECASE)

# pFUnit macro lines
RE_PFUNIT = re.compile(r'^\s*@')

# CPP lines (pass through; not tracked for FESOM2)
RE_CPP = re.compile(r'^\s*#')


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _preprocess(lines: list[str]) -> list[str]:
    """Strip pFUnit @macro lines (replace with blank to preserve line numbers)."""
    return ['' if RE_PFUNIT.match(l) else l for l in lines]


def _is_sub_or_func_start(line: str) -> tuple[bool, str]:
    """Return (True, name) if line opens a subroutine or function."""
    m = RE_SUB_START.match(line) or RE_FUNC_START.match(line)
    if m:
        return True, m.group(1)
    return False, ''


def _find_end(lines: list[str], start: int) -> int:
    """Find the END SUBROUTINE/FUNCTION/bare-END that closes the routine
    starting at `start`.  Returns the line index of the closing END."""
    depth = 0  # count of nested contained subroutines/functions
    j = start + 1
    while j < len(lines):
        l = lines[j]
        if RE_CPP.match(l) or not l.strip() or l.strip().startswith('!'):
            j += 1
            continue
        is_open, _ = _is_sub_or_func_start(l)
        if is_open:
            depth += 1
        elif RE_SUB_END.match(l):
            if depth == 0:
                return j
            depth -= 1
        elif RE_BARE_END.match(l) and not RE_INNER_END.match(l):
            if depth == 0:
                return j
            depth -= 1
        j += 1
    return len(lines) - 1


def _extract_calls(lines: list[str], start: int, end: int, self_name: str) -> list[str]:
    """Collect unique CALL targets within lines[start:end+1]."""
    seen: dict[str, None] = {}
    for l in lines[start:end + 1]:
        for m in RE_CALL.finditer(l):
            name = m.group(1).upper()
            if name != self_name.upper():
                seen[name] = None
    return list(seen)


def _extract_uses(lines: list[str], start: int, end: int) -> list[str]:
    """Collect unique USE targets within lines[start:end+1]."""
    seen: dict[str, None] = {}
    for l in lines[start:end + 1]:
        m = RE_USE.match(l)
        if m:
            seen[m.group(1)] = None
    return list(seen)


# ---------------------------------------------------------------------------
# Main extraction
# ---------------------------------------------------------------------------

def extract_file(path: Path) -> tuple[list[ModuleRecord], list[SubroutineRecord]]:
    """Return (modules, subroutines) extracted from a FESOM2 F90 / .pf file."""
    try:
        raw_lines = path.read_text(errors='replace').splitlines(keepends=True)
    except OSError:
        return [], []

    lines = _preprocess(raw_lines)
    rel = str(path)

    modules: list[ModuleRecord] = []
    subroutines: list[SubroutineRecord] = []

    i = 0
    while i < len(lines):
        line = lines[i]

        # --- MODULE start ---
        m = RE_MODULE_START.match(line)
        if m:
            mod_name = m.group(1)
            mod_start = i
            mod_uses: list[str] = []
            i += 1

            # Walk module body until END MODULE
            while i < len(lines):
                l = lines[i]

                if RE_MODULE_END.match(l):
                    # Close the module
                    modules.append(ModuleRecord(
                        name=mod_name,
                        file=rel,
                        start_line=mod_start + 1,
                        end_line=i + 1,
                        uses=list(dict.fromkeys(mod_uses)),
                    ))
                    i += 1
                    break

                # USE statements at module scope (before or after CONTAINS)
                mu = RE_USE.match(l)
                if mu:
                    used = mu.group(1)
                    mod_uses.append(used)
                    i += 1
                    continue

                # Subroutine / function inside module
                is_open, sub_name = _is_sub_or_func_start(l)
                if is_open:
                    sub_start = i
                    sub_end = _find_end(lines, sub_start)
                    source = ''.join(raw_lines[sub_start:sub_end + 1])
                    calls = _extract_calls(lines, sub_start, sub_end, sub_name)
                    # Roll sub-level USE statements up to the enclosing module
                    for used in _extract_uses(lines, sub_start, sub_end):
                        mod_uses.append(used)
                    subroutines.append(SubroutineRecord(
                        name=sub_name,
                        module_name=mod_name,
                        file=rel,
                        start_line=sub_start + 1,
                        end_line=sub_end + 1,
                        source_text=source,
                        calls=calls,
                    ))
                    i = sub_end + 1
                    continue

                i += 1
            continue

        # --- Top-level subroutine / function (outside any MODULE) ---
        is_open, sub_name = _is_sub_or_func_start(line)
        if is_open:
            sub_start = i
            sub_end = _find_end(lines, sub_start)
            source = ''.join(raw_lines[sub_start:sub_end + 1])
            calls = _extract_calls(lines, sub_start, sub_end, sub_name)
            subroutines.append(SubroutineRecord(
                name=sub_name,
                module_name='',
                file=rel,
                start_line=sub_start + 1,
                end_line=sub_end + 1,
                source_text=source,
                calls=calls,
            ))
            i = sub_end + 1
            continue

        i += 1

    return modules, subroutines
