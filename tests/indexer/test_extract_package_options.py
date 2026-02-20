"""Tests for extract_package_options in src/indexer/extract.py.

Basic tests verify correct behaviour. Adversarial tests marked xfail document
known bugs — they assert the *correct* expected output and are expected to fail
until the bugs are fixed.
"""

from pathlib import Path

import pytest

from src.indexer.extract import extract_package_options


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _write(tmp_path: Path, content: str, pkg: str = "mypkg", filename: str = "MYPKG_OPTIONS.h") -> Path:
    d = tmp_path / "MITgcm" / "pkg" / pkg
    d.mkdir(parents=True)
    f = d / filename
    f.write_text(content)
    return f


# ---------------------------------------------------------------------------
# Basic behaviour
# ---------------------------------------------------------------------------

def test_flags_extracted(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Enable feature A
#define ALLOW_FEATURE_A
C Enable feature B
#define ALLOW_FEATURE_B
#endif
""")
    flags = [r[1] for r in extract_package_options(f)]
    assert "ALLOW_FEATURE_A" in flags
    assert "ALLOW_FEATURE_B" in flags


def test_header_guard_excluded(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
#define ALLOW_FEATURE
#endif
""")
    flags = [r[1] for r in extract_package_options(f)]
    assert "MYPKG_OPTIONS_H" not in flags


def test_package_name_from_path(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
#define ALLOW_FEATURE
#endif
""", pkg="gmredi")
    assert all(r[0] == "gmredi" for r in extract_package_options(f))


def test_description_from_preceding_comment(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Enable the sponge layer
#define ALLOW_SPONGE
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_SPONGE"] == "Enable the sponge layer"


def test_no_description_gives_empty_string(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
#define ALLOW_NODESC
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_NODESC"] == ""


def test_undef_excluded(tmp_path):
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Disabled flag
#undef ALLOW_DISABLED
#define ALLOW_ENABLED
#endif
""")
    flags = [r[1] for r in extract_package_options(f)]
    assert "ALLOW_DISABLED" not in flags
    assert "ALLOW_ENABLED" in flags


def test_missing_file_returns_empty(tmp_path):
    f = tmp_path / "MITgcm" / "pkg" / "mypkg" / "MISSING_OPTIONS.h"
    f.parent.mkdir(parents=True)
    assert extract_package_options(f) == []


def test_bullet_o_stripped_from_comment(tmp_path):
    """C o <desc> style — the bullet 'o' should be stripped."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C o enable the feature
#define ALLOW_FEATURE
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_FEATURE"] == "enable the feature"


def test_blank_line_preserves_comment(tmp_path):
    """A single blank line between comment and #define does not reset the description."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Enable feature X

#define ALLOW_X
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_X"] == "Enable feature X"


def test_sibling_defines_only_first_gets_description(tmp_path):
    """Multiple consecutive #defines share a comment; only first gets it."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Enable individual open boundaries
#define ALLOW_OBCS_NORTH
#define ALLOW_OBCS_SOUTH
#define ALLOW_OBCS_EAST
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_OBCS_NORTH"] == "Enable individual open boundaries"
    assert results["ALLOW_OBCS_SOUTH"] == ""
    assert results["ALLOW_OBCS_EAST"] == ""


def test_non_allow_flags_included(tmp_path):
    """Non-ALLOW_* flags like GM_BOLUS_ADVEC are captured too."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Use Bolus advection
#define GM_BOLUS_ADVEC
#endif
""")
    flags = [r[1] for r in extract_package_options(f)]
    assert "GM_BOLUS_ADVEC" in flags


# ---------------------------------------------------------------------------
# Adversarial tests — known bugs, marked xfail
# ---------------------------------------------------------------------------

def test_description_starting_with_o_not_mangled(tmp_path):
    """lstrip('o') eats the leading 'o' from words like 'overflow', 'output', 'options'."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C overflow protection for sponge layer
#define ALLOW_SPONGE
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_SPONGE"] == "overflow protection for sponge layer"


def test_bullet_o_before_o_word_not_double_stripped(tmp_path):
    """'C o obsolete flag' → lstrip('o') strips the bullet 'o', then the 'o' from 'obsolete'."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C o obsolete flag kept for back-compat
#define ALLOW_OLD_THING
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_OLD_THING"] == "obsolete flag kept for back-compat"


def test_comment_above_undef_does_not_bleed(tmp_path):
    """A comment before a #undef should not be attached to the following #define."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Description of the disabled flag
#undef ALLOW_DISABLED
#define ALLOW_OTHER
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_OTHER"] == ""


def test_commented_out_define_not_treated_as_description(tmp_path):
    """Lines like 'c#define FLAG' (disabled via comment) should not pollute last_comment."""
    f = _write(tmp_path, """\
#ifndef MYPKG_OPTIONS_H
#define MYPKG_OPTIONS_H
C Note: comment out the #define below to enable from command line
c#define ALLOW_OPTIONAL
#define ALLOW_REAL
#endif
""")
    results = {r[1]: r[2] for r in extract_package_options(f)}
    assert results["ALLOW_REAL"] == "Note: comment out the #define below to enable from command line"


def test_lowercase_filename_header_guard_excluded(tmp_path):
    """Header guard '#define mypkg_options_h' should be excluded (case-insensitive match)."""
    f = _write(tmp_path, """\
#ifndef mypkg_options_h
#define mypkg_options_h
#define ALLOW_FEATURE
#endif
""", filename="mypkg_options.h")
    flags = [r[1] for r in extract_package_options(f)]
    assert "mypkg_options_h" not in flags
    assert "ALLOW_FEATURE" in flags
