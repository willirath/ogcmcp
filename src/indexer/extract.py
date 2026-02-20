"""Regex-based extraction of code structure from MITgcm Fortran source.

Works on both fixed-form (.F) and free-form (.F90) without CPP preprocessing.
"""

import re
from dataclasses import dataclass, field
from pathlib import Path

# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

@dataclass
class SubroutineRecord:
    name: str
    file: str
    package: str
    line_start: int
    line_end: int
    source_text: str
    calls: list[str] = field(default_factory=list)
    namelist_params: list[tuple[str, str]] = field(default_factory=list)
    diag_fills: list[tuple[str, str]] = field(default_factory=list)
    cpp_guards: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Regex patterns
# ---------------------------------------------------------------------------

RE_SUB_START = re.compile(r'^\s*SUBROUTINE\s+(\w+)', re.IGNORECASE)

# Matches bare END or END SUBROUTINE [name], but not END DO / END IF / etc.
_END_KEYWORDS = r'DO|IF|WHERE|FORALL|SELECT|TYPE|MODULE|PROGRAM|INTERFACE|BLOCK|ASSOCIATE|CRITICAL|ENUM|FUNCTION'
RE_SUB_END = re.compile(
    rf'^\s{{0,9}}END\s*(?:SUBROUTINE\b[^!\n]*)?\s*(?:!.*)?$',
    re.IGNORECASE,
)
RE_END_BLOCK = re.compile(
    rf'^\s+(?:END\s+(?:{_END_KEYWORDS})|END(?:{_END_KEYWORDS}))\b',
    re.IGNORECASE,
)

RE_CALL = re.compile(r'^\s+CALL\s+(\w+)', re.IGNORECASE)
# B2 fix: also match CALL embedded after IF/ELSEIF on the same line
RE_CALL_INLINE = re.compile(r'\bCALL\s+(\w+)', re.IGNORECASE)
RE_NAMELIST_START = re.compile(r'\bNAMELIST\s*/(\w+)/', re.IGNORECASE)
RE_DIAG_FILL = re.compile(
    r"CALL\s+DIAGNOSTICS_FILL\s*\(\s*(\w+)\s*,\s*'([^']+)'", re.IGNORECASE
)

RE_IFDEF  = re.compile(r'^#ifdef\s+(\w+)',  re.IGNORECASE)
RE_IFNDEF = re.compile(r'^#ifndef\s+(\w+)', re.IGNORECASE)
RE_ENDIF  = re.compile(r'^#endif\b',        re.IGNORECASE)

# Fixed-form: comment if C/*/! in column 1
RE_COMMENT_FIXED = re.compile(r'^[Cc*!]')
# Fixed-form: continuation if column 6 is non-blank, non-zero
def _is_continuation_fixed(line: str) -> bool:
    return len(line) > 5 and line[5] not in (' ', '0', '\n', '\r', '')


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _package_from_path(path: Path) -> str:
    parts = path.parts
    if 'pkg' in parts:
        i = parts.index('pkg')
        return parts[i + 1] if i + 1 < len(parts) else 'unknown'
    if 'model' in parts:
        return 'model'
    if 'eesupp' in parts:
        return 'eesupp'
    return 'unknown'


def _extract_namelist_params(lines: list[str], start: int, fixed_form: bool) -> tuple[str, list[str], int]:
    """Given the NAMELIST declaration line index, collect all continuation
    lines and return (group_name, [param_names], last_line_index)."""
    first = lines[start]
    m = RE_NAMELIST_START.search(first)
    if not m:
        return '', [], start
    group = m.group(1)

    # Collect the declaration text across continuation lines
    text = first[m.end():]
    i = start + 1
    while i < len(lines):
        line = lines[i]
        if fixed_form:
            if RE_COMMENT_FIXED.match(line):
                i += 1
                continue
            if _is_continuation_fixed(line):
                text += ' ' + line[6:]
                i += 1
                continue
        else:
            stripped = line.rstrip()
            if stripped.endswith('&'):
                text += ' ' + stripped[:-1]
                i += 1
                continue
            # Also handle leading & on next line (free-form style)
            lstripped = line.lstrip()
            if lstripped.startswith('&'):
                text += ' ' + lstripped[1:]
                i += 1
                continue
        break

    # Parse comma-separated identifiers from accumulated text
    params = [p.strip() for p in re.split(r'[,\s&]+', text) if re.match(r'^\w+$', p.strip())]
    return group, params, i - 1


# ---------------------------------------------------------------------------
# Main extraction
# ---------------------------------------------------------------------------

RE_OPTIONS_DEFINE = re.compile(r'^#define\s+(\w+)', re.IGNORECASE)
RE_OPTIONS_UNDEF  = re.compile(r'^#undef\b',        re.IGNORECASE)
RE_OPTIONS_COMMENT = re.compile(r'^[Cc]\s*(.*)')


def extract_package_options(path: Path) -> list[tuple[str, str, str]]:
    """Extract (package_name, cpp_flag, description) from a *_OPTIONS.h file.

    The package name is taken from the directory immediately under pkg/.
    Description is the text of the nearest preceding C-style comment line,
    stripped of leading C/c and whitespace. Flags defined via #undef are
    ignored. The header guard #define (matching the filename) is skipped.
    """
    package = _package_from_path(path)
    header_guard = path.name.upper().replace('.', '_').replace('-', '_')

    try:
        lines = path.read_text(errors='replace').splitlines()
    except OSError:
        return []

    results: list[tuple[str, str, str]] = []
    last_comment = ''
    for line in lines:
        stripped = line.strip()
        if cm := RE_OPTIONS_COMMENT.match(stripped):
            text = cm.group(1).strip()
            # Skip commented-out #define lines (e.g. "c#define FLAG")
            if text.startswith('#'):
                continue
            # Strip bullet 'o ' prefix and '>>>' arrow prefix
            text = re.sub(r'^o\s+', '', text)
            text = re.sub(r'^>+\s*', '', text)
            last_comment = text
        elif m := RE_OPTIONS_DEFINE.match(stripped):
            flag = m.group(1)
            if flag.upper() != header_guard:  # case-insensitive guard check
                results.append((package, flag, last_comment))
            last_comment = ''
        elif RE_OPTIONS_UNDEF.match(stripped):
            last_comment = ''  # #undef resets; don't bleed into the next #define
        elif stripped and not stripped.startswith('#'):
            last_comment = ''

    return results


def extract_file(path: Path) -> list[SubroutineRecord]:
    """Extract all subroutine records from a Fortran source file."""
    fixed_form = path.suffix == '.F'
    package = _package_from_path(path)
    rel_path = str(path)

    try:
        text = path.read_text(errors='replace')
    except OSError:
        return []

    lines = text.splitlines(keepends=True)

    # --- Pass 1: collect #ifdef guard context per line ---
    # guard_stack[i] = set of active CPP flags at line i
    active_guards: list[str] = []       # stack of currently open flags
    line_guards: list[frozenset] = []   # active guard set at each line
    for line in lines:
        if m := RE_IFDEF.match(line):
            active_guards.append(m.group(1))
        elif m := RE_IFNDEF.match(line):
            active_guards.append('!' + m.group(1))  # '!' prefix = ifndef
        elif RE_ENDIF.match(line):
            if active_guards:
                active_guards.pop()
        line_guards.append(frozenset(active_guards))

    # --- Pass 2: find subroutine boundaries and extract contents ---
    records: list[SubroutineRecord] = []
    i = 0
    while i < len(lines):
        line = lines[i]

        # Skip comment lines (fixed-form)
        if fixed_form and RE_COMMENT_FIXED.match(line):
            i += 1
            continue

        m = RE_SUB_START.match(line)
        if not m:
            i += 1
            continue

        sub_name = m.group(1)
        sub_start = i
        sub_guards = set(line_guards[i])

        # Find the matching END
        depth = 0
        j = i + 1
        while j < len(lines):
            l = lines[j]
            if fixed_form and RE_COMMENT_FIXED.match(l):
                j += 1
                continue
            if RE_SUB_START.match(l):
                depth += 1
            elif RE_END_BLOCK.match(l):
                pass  # END DO / END IF etc. — don't count
            elif RE_SUB_END.match(l):
                if depth == 0:
                    break
                depth -= 1
            j += 1

        sub_end = j
        source_lines = lines[sub_start:sub_end + 1]
        source_text = ''.join(source_lines)

        # Collect CPP guards active anywhere in this subroutine's range
        all_guards: set[str] = set()
        for gi in range(sub_start, min(sub_end + 1, len(line_guards))):
            all_guards.update(line_guards[gi])

        # Extract calls, namelist refs, diagnostics_fills from subroutine body
        calls: list[str] = []
        namelist_params: list[tuple[str, str]] = []
        diag_fills: list[tuple[str, str]] = []

        k = sub_start
        while k <= sub_end and k < len(lines):
            l = lines[k]

            if fixed_form and RE_COMMENT_FIXED.match(l):
                k += 1
                continue

            # CALL statements
            # Primary: line starts with whitespace then CALL (covers the common case)
            if cm := RE_CALL.match(l):
                callee = cm.group(1).upper()
                if callee != sub_name.upper():  # skip self-calls from misparse
                    calls.append(callee)
            else:
                # B2 fix: catch inline "IF (cond) CALL FOO(...)" patterns
                for cm in RE_CALL_INLINE.finditer(l):
                    callee = cm.group(1).upper()
                    if callee != sub_name.upper():
                        calls.append(callee)

            # NAMELIST declarations
            if RE_NAMELIST_START.search(l):
                group, params, last = _extract_namelist_params(lines, k, fixed_form)
                for p in params:
                    namelist_params.append((p, group))
                k = last  # skip consumed continuation lines

            # DIAGNOSTICS_FILL — B1 fix: join with the next continuation line
            # so that the array name and quoted field can be on separate lines.
            if 'DIAGNOSTICS_FILL' in l.upper():
                # Build a joined version of this line + any immediate continuation
                joined = l.rstrip('\n\r')
                nk = k + 1
                while nk < len(lines) and nk <= sub_end:
                    nl = lines[nk]
                    if fixed_form:
                        if RE_COMMENT_FIXED.match(nl):
                            nk += 1
                            continue
                        if _is_continuation_fixed(nl):
                            joined += ' ' + nl[6:].rstrip('\n\r')
                            nk += 1
                            continue
                    else:
                        stripped_nl = nl.rstrip()
                        lstripped_nl = nl.lstrip()
                        if stripped_nl.rstrip().endswith('&'):
                            joined += ' ' + stripped_nl[:-1]
                            nk += 1
                            continue
                        if lstripped_nl.startswith('&'):
                            joined += ' ' + lstripped_nl[1:].rstrip('\n\r')
                            nk += 1
                            continue
                    break
                if dm := RE_DIAG_FILL.search(joined):
                    diag_fills.append((dm.group(2), dm.group(1)))

            k += 1

        records.append(SubroutineRecord(
            name=sub_name,
            file=rel_path,
            package=package,
            line_start=sub_start + 1,  # 1-indexed
            line_end=sub_end + 1,
            source_text=source_text,
            calls=list(dict.fromkeys(calls)),  # deduplicate, preserve order
            namelist_params=namelist_params,
            diag_fills=diag_fills,
            cpp_guards=[g for g in all_guards if not g.startswith('!')],
        ))

        i = sub_end + 1

    return records
