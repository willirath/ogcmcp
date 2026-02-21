"""Parse MITgcm RST documentation into plain-text sections for embedding.

Each RST file is split into sections delimited by heading underlines.  The
text of each section is cleaned of RST markup (directives, inline roles,
table grid characters) and returned as a dict:

    {"file": "getting_started/getting_started.rst",
     "section": "Where to find information",
     "text": "...plain text..."}

The file path is relative to the doc root passed to ``iter_sections``.
"""

import re
from pathlib import Path

# RST underline characters that delimit section headings.
_UNDERLINE_CHARS = frozenset("=-~+#*^")

# Directive patterns: lines starting with ".. " (directives, labels, comments).
_DIRECTIVE_RE = re.compile(r"^\s*\.\.")

# Inline role: :rolename:`content` → content  (strip role, keep label text)
_ROLE_RE = re.compile(r":[a-zA-Z][a-zA-Z0-9_-]*:`([^`]*)`")

# Interpreted text: `text` (backtick-only, no role) → text
_BACKTICK_RE = re.compile(r"`([^`]+)`")

# RST grid table border lines: rows of +-+-+ or +=+=+
_TABLE_BORDER_RE = re.compile(r"^[+|][-=+|]+[+|]\s*$")


def _is_underline(line: str, text_line: str) -> bool:
    """Return True if line looks like an RST section underline for text_line."""
    stripped = line.rstrip()
    if not stripped:
        return False
    ch = stripped[0]
    if ch not in _UNDERLINE_CHARS:
        return False
    if not all(c == ch for c in stripped):
        return False
    # Underline must be at least as long as the heading text.
    return len(stripped) >= len(text_line.rstrip())


def _clean_text(lines: list[str]) -> str:
    """Strip RST markup from a list of lines and return plain text."""
    out = []
    skip_indent: int | None = None  # indentation level of a directive block

    for line in lines:
        # Skip continuation lines of a directive block.
        if skip_indent is not None:
            stripped = line.rstrip()
            if stripped == "" or (stripped and (len(line) - len(line.lstrip())) > skip_indent):
                continue
            else:
                skip_indent = None

        # Skip directive / label / comment lines.
        if _DIRECTIVE_RE.match(line):
            indent = len(line) - len(line.lstrip())
            skip_indent = indent
            continue

        # Skip table border lines.
        if _TABLE_BORDER_RE.match(line):
            continue

        # Apply inline substitutions.
        cleaned = _ROLE_RE.sub(r"\1", line)
        cleaned = _BACKTICK_RE.sub(r"\1", cleaned)
        out.append(cleaned.rstrip())

    # Collapse runs of blank lines to a single blank.
    result_lines: list[str] = []
    prev_blank = False
    for ln in out:
        is_blank = ln.strip() == ""
        if is_blank and prev_blank:
            continue
        result_lines.append(ln)
        prev_blank = is_blank

    return "\n".join(result_lines).strip()


def _split_sections(lines: list[str]) -> list[tuple[str, list[str]]]:
    """Return list of (heading_text, body_lines) for each RST section.

    The heading is the text line immediately before an underline.  Everything
    between one heading and the next belongs to that section.  Content before
    the first heading is collected under the empty-string heading "".
    """
    sections: list[tuple[str, list[str]]] = []
    current_heading = ""
    current_body: list[str] = []

    i = 0
    while i < len(lines):
        line = lines[i]
        # Check if the *next* line is an underline for this line.
        if i + 1 < len(lines) and _is_underline(lines[i + 1], line):
            # Save previous section.
            sections.append((current_heading, current_body))
            current_heading = line.strip()
            current_body = []
            i += 2  # skip heading text and underline
            # Also skip an optional overline above the title (i-2 already consumed)
            continue
        current_body.append(line)
        i += 1

    # Final section.
    sections.append((current_heading, current_body))
    return sections


def iter_sections(doc_root: Path) -> list[dict]:
    """Return one dict per RST section found under doc_root.

    Skips RST files that contain no sections (e.g. pure toctree files).
    Returns a list sorted by file path then section order.

    Each dict has keys:
        file    – path relative to doc_root (str, forward slashes)
        section – heading text (empty string for pre-heading preamble)
        text    – cleaned plain text of the section body
    """
    results = []
    for rst_path in sorted(doc_root.rglob("*.rst")):
        rel = rst_path.relative_to(doc_root).as_posix()
        lines = rst_path.read_text(encoding="utf-8", errors="replace").splitlines()
        for heading, body_lines in _split_sections(lines):
            text = _clean_text(body_lines)
            if not text:
                continue
            results.append({"file": rel, "section": heading, "text": text})
    return results
