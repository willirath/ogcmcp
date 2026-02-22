"""Tests for src/docs_indexer/parse.py using synthetic RST fixtures."""

import textwrap
from pathlib import Path

import pytest

from src.docs_indexer.parse import _clean_text, _is_underline, _split_sections, iter_headers, iter_sections


# ── _is_underline ──────────────────────────────────────────────────────────────

def test_underline_equals():
    assert _is_underline("=============", "Section Title")

def test_underline_dashes():
    assert _is_underline("-----------", "Sub heading")

def test_underline_must_be_long_enough():
    assert not _is_underline("===", "A very long heading line")

def test_underline_mixed_chars_rejected():
    assert not _is_underline("=-=-=", "Title")

def test_underline_empty_line_rejected():
    assert not _is_underline("", "Title")

def test_underline_plain_text_rejected():
    assert not _is_underline("not underline", "Title")


# ── _split_sections ────────────────────────────────────────────────────────────

RST_TWO_SECTIONS = textwrap.dedent("""\
    Preamble text.

    First Section
    =============

    Body of first section.

    Second Section
    --------------

    Body of second section.
""").splitlines()

def test_split_preamble_heading():
    sections = _split_sections(RST_TWO_SECTIONS)
    headings = [h for h, _ in sections]
    assert "" in headings            # preamble has empty heading
    assert "First Section" in headings
    assert "Second Section" in headings

def test_split_correct_count():
    sections = _split_sections(RST_TWO_SECTIONS)
    # preamble + 2 real sections
    assert len(sections) == 3

def test_split_body_assigned():
    sections = _split_sections(RST_TWO_SECTIONS)
    bodies = {h: b for h, b in sections}
    assert any("Body of first section" in ln for ln in bodies["First Section"])
    assert any("Body of second section" in ln for ln in bodies["Second Section"])


# ── _clean_text ────────────────────────────────────────────────────────────────

def test_clean_inline_role_stripped():
    lines = ["See :varlink:`f0` for details."]
    result = _clean_text(lines)
    assert ":varlink:" not in result
    assert "f0" in result

def test_clean_backtick_stripped():
    lines = ["Run `make depend` first."]
    result = _clean_text(lines)
    assert "`" not in result
    assert "make depend" in result

def test_clean_directive_removed():
    lines = [
        "Some text.",
        ".. math::",
        "   x = y + z",
        "More text.",
    ]
    result = _clean_text(lines)
    assert "x = y + z" not in result
    assert "Some text." in result
    assert "More text." in result

def test_clean_table_border_removed():
    lines = [
        "+------+------+",
        "| col1 | col2 |",
        "+======+======+",
        "| val1 | val2 |",
        "+------+------+",
    ]
    result = _clean_text(lines)
    assert "+" not in result.replace("col1", "").replace("col2", "").replace("val1", "").replace("val2", "")

def test_clean_consecutive_blanks_collapsed():
    lines = ["text", "", "", "", "more"]
    result = _clean_text(lines)
    assert "\n\n\n" not in result

def test_clean_label_directive_removed():
    lines = [".. _my-label:", "", "Real content."]
    result = _clean_text(lines)
    assert ".. _my-label:" not in result
    assert "Real content." in result

def test_clean_cite_role_keeps_key():
    lines = ["See :cite:`adcroft97` for details."]
    result = _clean_text(lines)
    assert "adcroft97" in result


# ── iter_sections ──────────────────────────────────────────────────────────────

@pytest.fixture
def tmp_doc_root(tmp_path):
    """Create a minimal synthetic doc tree with two RST files."""
    (tmp_path / "pkg").mkdir()

    (tmp_path / "intro.rst").write_text(textwrap.dedent("""\
        Overview
        ========

        This is the overview section.

        Details
        -------

        This section has :varlink:`f0` and more.
    """))

    (tmp_path / "pkg" / "rbcs.rst").write_text(textwrap.dedent("""\
        RBCS Package
        ------------

        The RBCS package relaxes temperature at rate :varlink:`tauRelaxT`.

        .. math::

           dT/dt = -mask * (T - T_rbc) / tau

        More prose after math.
    """))

    return tmp_path


def test_iter_sections_finds_both_files(tmp_doc_root):
    sections = iter_sections(tmp_doc_root)
    files = {s["file"] for s in sections}
    assert "intro.rst" in files
    assert "pkg/rbcs.rst" in files

def test_iter_sections_heading_extracted(tmp_doc_root):
    sections = iter_sections(tmp_doc_root)
    headings = {s["section"] for s in sections}
    assert "Overview" in headings
    assert "Details" in headings
    assert "RBCS Package" in headings

def test_iter_sections_text_nonempty(tmp_doc_root):
    sections = iter_sections(tmp_doc_root)
    for s in sections:
        assert s["text"].strip(), f"Empty text in section {s['section']} of {s['file']}"

def test_iter_sections_math_directive_stripped(tmp_doc_root):
    sections = iter_sections(tmp_doc_root)
    rbcs = next(s for s in sections if s["section"] == "RBCS Package")
    assert "dT/dt" not in rbcs["text"]
    assert "More prose after math" in rbcs["text"]

def test_iter_sections_role_stripped(tmp_doc_root):
    sections = iter_sections(tmp_doc_root)
    details = next(s for s in sections if s["section"] == "Details")
    assert ":varlink:" not in details["text"]
    assert "f0" in details["text"]

def test_iter_sections_empty_rst_skipped(tmp_doc_root):
    # A file with only directives and no prose should produce no sections.
    (tmp_doc_root / "empty.rst").write_text(".. toctree::\n   intro\n   pkg/rbcs\n")
    sections = iter_sections(tmp_doc_root)
    files = {s["file"] for s in sections}
    assert "empty.rst" not in files


# ── iter_headers ───────────────────────────────────────────────────────────────

@pytest.fixture
def tmp_verification_root(tmp_path):
    """Minimal synthetic verification tree with two experiments."""
    mitgcm = tmp_path / "MITgcm"
    for exp in ("rotating_tank", "basin"):
        code = mitgcm / "verification" / exp / "code"
        code.mkdir(parents=True)
        (code / "SIZE.h").write_text(
            f"C experiment {exp}\n"
            f"      INTEGER sNx\n"
            f"      PARAMETER ( sNx = 30 )\n"
        )
        (code / "CPP_OPTIONS.h").write_text(
            f"C CPP options for {exp}\n"
            f"#define ALLOW_DIAGNOSTICS\n"
        )
    # Empty file — should be skipped.
    (mitgcm / "verification" / "rotating_tank" / "code" / "empty.h").write_text("   \n")
    return mitgcm / "verification"


def test_iter_headers_finds_files(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    filenames = {h["file"] for h in headers}
    assert any("rotating_tank/code/SIZE.h" in f for f in filenames)
    assert any("basin/code/SIZE.h" in f for f in filenames)


def test_iter_headers_section_is_filename(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    sections = {h["section"] for h in headers}
    assert "SIZE.h" in sections
    assert "CPP_OPTIONS.h" in sections


def test_iter_headers_file_relative_to_mitgcm(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    for h in headers:
        assert h["file"].startswith("verification/")


def test_iter_headers_text_nonempty(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    for h in headers:
        assert h["text"].strip()


def test_iter_headers_skips_empty_files(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    filenames = {h["file"] for h in headers}
    assert not any("empty.h" in f for f in filenames)


def test_iter_headers_has_required_keys(tmp_verification_root):
    headers = iter_headers(tmp_verification_root)
    for h in headers:
        assert "file" in h
        assert "section" in h
        assert "text" in h
