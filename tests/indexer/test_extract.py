"""Tests for src/indexer/extract.py.

Uses synthetic Fortran snippets rather than real MITgcm files so tests
are fast, self-contained, and not tied to a specific MITgcm commit.
"""

import tempfile
from pathlib import Path

import pytest

from src.indexer.extract import extract_file


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _write(content: str, suffix: str = ".F") -> Path:
    """Write content to a temp file and return its Path."""
    f = tempfile.NamedTemporaryFile(
        suffix=suffix, mode="w", delete=False, encoding="utf-8"
    )
    f.write(content)
    f.close()
    return Path(f.name)


# ---------------------------------------------------------------------------
# Subroutine detection — fixed-form
# ---------------------------------------------------------------------------

FIXED_SIMPLE = """\
C     A comment
      SUBROUTINE MYSUB( x, y )
      IMPLICIT NONE
      RETURN
      END
"""

def test_fixed_form_subroutine_name():
    recs = extract_file(_write(FIXED_SIMPLE))
    assert len(recs) == 1
    assert recs[0].name == "MYSUB"


def test_fixed_form_line_range():
    recs = extract_file(_write(FIXED_SIMPLE))
    r = recs[0]
    assert r.line_start == 2
    assert r.line_end == 5


def test_fixed_form_source_text_included():
    recs = extract_file(_write(FIXED_SIMPLE))
    assert "IMPLICIT NONE" in recs[0].source_text


# ---------------------------------------------------------------------------
# Subroutine detection — free-form
# ---------------------------------------------------------------------------

FREE_SIMPLE = """\
subroutine mysub(x, y)
  implicit none
  real, intent(in) :: x, y
end subroutine mysub
"""

def test_free_form_subroutine_name():
    recs = extract_file(_write(FREE_SIMPLE, suffix=".F90"))
    assert len(recs) == 1
    assert recs[0].name.upper() == "MYSUB"


# ---------------------------------------------------------------------------
# Multiple subroutines in one file
# ---------------------------------------------------------------------------

FIXED_MULTI = """\
      SUBROUTINE ALPHA( x )
      IMPLICIT NONE
      RETURN
      END
      SUBROUTINE BETA( y )
      IMPLICIT NONE
      RETURN
      END
"""

def test_multiple_subroutines():
    recs = extract_file(_write(FIXED_MULTI))
    names = [r.name for r in recs]
    assert "ALPHA" in names
    assert "BETA" in names
    assert len(recs) == 2


def test_multiple_subroutines_non_overlapping_lines():
    recs = extract_file(_write(FIXED_MULTI))
    alpha = next(r for r in recs if r.name == "ALPHA")
    beta  = next(r for r in recs if r.name == "BETA")
    assert alpha.line_end < beta.line_start


# ---------------------------------------------------------------------------
# CALL extraction
# ---------------------------------------------------------------------------

FIXED_CALLS = """\
      SUBROUTINE CALLER( )
      IMPLICIT NONE
      CALL FOO( x )
      CALL BAR( y, z )
      CALL FOO( x )
      RETURN
      END
"""

def test_calls_extracted():
    recs = extract_file(_write(FIXED_CALLS))
    assert set(recs[0].calls) == {"FOO", "BAR"}


def test_calls_deduplicated():
    recs = extract_file(_write(FIXED_CALLS))
    assert recs[0].calls.count("FOO") == 1


# ---------------------------------------------------------------------------
# NAMELIST extraction
# ---------------------------------------------------------------------------

FIXED_NAMELIST = """\
      SUBROUTINE INI_PARMS( )
      IMPLICIT NONE
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dTargetResidual,
     & cg3dMaxIters, cg3dTargetResidual
      RETURN
      END
"""

def test_namelist_group_extracted():
    recs = extract_file(_write(FIXED_NAMELIST))
    groups = {g for _, g in recs[0].namelist_params}
    assert "PARM02" in groups


def test_namelist_params_extracted():
    recs = extract_file(_write(FIXED_NAMELIST))
    params = {p for p, _ in recs[0].namelist_params}
    assert "cg3dMaxIters" in params
    assert "cg2dTargetResidual" in params


# ---------------------------------------------------------------------------
# DIAGNOSTICS_FILL extraction
# ---------------------------------------------------------------------------

FIXED_DIAG = """\
      SUBROUTINE DIAG_SUB( )
      IMPLICIT NONE
      CALL DIAGNOSTICS_FILL(myArray,'MYFIELD ',k,1,2,bi,bj,myThid)
      RETURN
      END
"""

def test_diag_fill_field_name():
    recs = extract_file(_write(FIXED_DIAG))
    fields = [f.strip() for f, _ in recs[0].diag_fills]
    assert "MYFIELD" in fields


def test_diag_fill_array_name():
    recs = extract_file(_write(FIXED_DIAG))
    arrays = [a for _, a in recs[0].diag_fills]
    assert "myArray" in arrays


# ---------------------------------------------------------------------------
# CPP guard extraction
# ---------------------------------------------------------------------------

FIXED_GUARDS = """\
#ifdef ALLOW_NONHYDROSTATIC
      SUBROUTINE NH_SUB( )
      IMPLICIT NONE
      RETURN
      END
#endif
"""

def test_cpp_guard_attributed():
    recs = extract_file(_write(FIXED_GUARDS))
    assert len(recs) == 1
    assert "ALLOW_NONHYDROSTATIC" in recs[0].cpp_guards


FIXED_NO_GUARD = """\
      SUBROUTINE PLAIN_SUB( )
      IMPLICIT NONE
      RETURN
      END
"""

def test_no_spurious_guards():
    recs = extract_file(_write(FIXED_NO_GUARD))
    assert recs[0].cpp_guards == []


# ---------------------------------------------------------------------------
# Package attribution
# ---------------------------------------------------------------------------

def test_package_model(tmp_path):
    f = tmp_path / "MITgcm" / "model" / "src" / "foo.F"
    f.parent.mkdir(parents=True)
    f.write_text(FIXED_SIMPLE)
    recs = extract_file(f)
    assert recs[0].package == "model"


def test_package_pkg(tmp_path):
    f = tmp_path / "MITgcm" / "pkg" / "diagnostics" / "foo.F"
    f.parent.mkdir(parents=True)
    f.write_text(FIXED_SIMPLE)
    recs = extract_file(f)
    assert recs[0].package == "diagnostics"
