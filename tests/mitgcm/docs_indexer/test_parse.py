"""Tests for src/mitgcm_docs_indexer/parse.py — iter_headers only.

RST parsing tests (_is_underline, _split_sections, _clean_text, iter_sections)
live in tests/rst_parser/test_parser.py.
"""

from pathlib import Path

import pytest

from src.mitgcm.docs_indexer.parse import iter_headers


# ── iter_headers ───────────────────────────────────────────────────────────────

@pytest.fixture
def tmp_mitgcm_root(tmp_path):
    """Minimal synthetic MITgcm tree: two verification experiments plus core headers."""
    mitgcm = tmp_path / "MITgcm"

    # verification experiments
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

    # core model headers
    model_inc = mitgcm / "model" / "inc"
    model_inc.mkdir(parents=True)
    (model_inc / "PARAMS.h").write_text("C Core model parameters\n      COMMON /PARM_L/ usingCartesianGrid\n")
    (model_inc / "DYNVARS.h").write_text("C Dynamical variables\n      COMMON /DYNVARS_R/ uVel, vVel\n")

    # execution environment headers
    eesupp_inc = mitgcm / "eesupp" / "inc"
    eesupp_inc.mkdir(parents=True)
    (eesupp_inc / "EXCH.h").write_text("C Exchange buffer declarations\n      COMMON /EXCH/ exchUseMPI\n")

    # package headers
    pkg_obcs = mitgcm / "pkg" / "obcs"
    pkg_obcs.mkdir(parents=True)
    (pkg_obcs / "OBCS_FIELDS.h").write_text("C OBCS boundary fields\n      COMMON /OBCS_FIELDS/ OBNu, OBNv\n")

    return mitgcm


def test_iter_headers_finds_verification_files(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    filenames = {h["file"] for h in headers}
    assert any("rotating_tank/code/SIZE.h" in f for f in filenames)
    assert any("basin/code/SIZE.h" in f for f in filenames)


def test_iter_headers_finds_model_inc(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    filenames = {h["file"] for h in headers}
    assert "model/inc/PARAMS.h" in filenames
    assert "model/inc/DYNVARS.h" in filenames


def test_iter_headers_finds_eesupp_inc(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    filenames = {h["file"] for h in headers}
    assert "eesupp/inc/EXCH.h" in filenames


def test_iter_headers_section_is_filename(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    sections = {h["section"] for h in headers}
    assert "SIZE.h" in sections
    assert "CPP_OPTIONS.h" in sections
    assert "PARAMS.h" in sections
    assert "EXCH.h" in sections


def test_iter_headers_paths_are_relative(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    for h in headers:
        assert not h["file"].startswith("/"), f"Expected relative path, got: {h['file']}"


def test_iter_headers_text_nonempty(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    for h in headers:
        assert h["text"].strip()


def test_iter_headers_skips_empty_files(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    filenames = {h["file"] for h in headers}
    assert not any("empty.h" in f for f in filenames)


def test_iter_headers_finds_pkg_headers(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    filenames = {h["file"] for h in headers}
    assert "pkg/obcs/OBCS_FIELDS.h" in filenames


def test_iter_headers_has_required_keys(tmp_mitgcm_root):
    headers = iter_headers(tmp_mitgcm_root)
    for h in headers:
        assert "file" in h
        assert "section" in h
        assert "text" in h
