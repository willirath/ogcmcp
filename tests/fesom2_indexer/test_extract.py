"""Tests for src/fesom2_indexer/extract.py.

Uses synthetic F90 snippets — no real FESOM2 files required.
"""

import tempfile
from pathlib import Path

import pytest

from src.fesom2_indexer.extract import extract_file


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _write(content: str, suffix: str = ".F90") -> Path:
    f = tempfile.NamedTemporaryFile(
        suffix=suffix, mode="w", delete=False, encoding="utf-8"
    )
    f.write(content)
    f.close()
    return Path(f.name)


# ---------------------------------------------------------------------------
# Module detection
# ---------------------------------------------------------------------------

SIMPLE_MODULE = """\
MODULE my_mod
  IMPLICIT NONE
CONTAINS
  SUBROUTINE do_stuff()
  END SUBROUTINE
END MODULE my_mod
"""

def test_module_name_detected():
    mods, _ = extract_file(_write(SIMPLE_MODULE))
    assert len(mods) == 1
    assert mods[0].name == "my_mod"

def test_module_line_range():
    mods, _ = extract_file(_write(SIMPLE_MODULE))
    assert mods[0].start_line == 1
    assert mods[0].end_line == 6

def test_subroutine_inside_module():
    _, subs = extract_file(_write(SIMPLE_MODULE))
    assert len(subs) == 1
    assert subs[0].name == "do_stuff"

def test_subroutine_module_name():
    _, subs = extract_file(_write(SIMPLE_MODULE))
    assert subs[0].module_name == "my_mod"


# ---------------------------------------------------------------------------
# Data-only module (no CONTAINS)
# ---------------------------------------------------------------------------

DATA_MODULE = """\
MODULE constants
  real, parameter :: pi = 3.14159
  real, parameter :: g  = 9.81
END MODULE constants
"""

def test_data_only_module_recorded():
    mods, subs = extract_file(_write(DATA_MODULE))
    assert len(mods) == 1
    assert mods[0].name == "constants"
    assert subs == []


# ---------------------------------------------------------------------------
# USE statements
# ---------------------------------------------------------------------------

USE_MODULE = """\
MODULE ocean_dyn
  USE mesh_module
  USE param_module
  IMPLICIT NONE
CONTAINS
  SUBROUTINE step()
    USE time_module
  END SUBROUTINE
END MODULE ocean_dyn
"""

def test_module_uses_collected():
    mods, _ = extract_file(_write(USE_MODULE))
    assert "mesh_module" in mods[0].uses
    assert "param_module" in mods[0].uses

def test_use_inside_subroutine_also_collected():
    # USE inside a subroutine is still at module scope for dependency purposes
    mods, _ = extract_file(_write(USE_MODULE))
    assert "time_module" in mods[0].uses

def test_uses_deduplicated():
    src = """\
MODULE foo
  USE bar
  USE bar
CONTAINS
END MODULE foo
"""
    mods, _ = extract_file(_write(src))
    assert mods[0].uses.count("bar") == 1


# ---------------------------------------------------------------------------
# CALL extraction
# ---------------------------------------------------------------------------

CALL_MODULE = """\
MODULE driver
CONTAINS
  SUBROUTINE run()
    CALL init_mesh()
    CALL compute(x)
    IF (flag) CALL cleanup()
  END SUBROUTINE
END MODULE driver
"""

def test_calls_extracted():
    _, subs = extract_file(_write(CALL_MODULE))
    assert "init_mesh" in [c.lower() for c in subs[0].calls]
    assert "compute" in [c.lower() for c in subs[0].calls]

def test_inline_call_extracted():
    _, subs = extract_file(_write(CALL_MODULE))
    assert "cleanup" in [c.lower() for c in subs[0].calls]

def test_calls_deduplicated():
    src = """\
MODULE foo
CONTAINS
  SUBROUTINE bar()
    CALL baz()
    CALL baz()
  END SUBROUTINE
END MODULE foo
"""
    _, subs = extract_file(_write(src))
    assert subs[0].calls.count("BAZ") == 1


# ---------------------------------------------------------------------------
# Source text
# ---------------------------------------------------------------------------

def test_source_text_contains_body():
    _, subs = extract_file(_write(SIMPLE_MODULE))
    assert "do_stuff" in subs[0].source_text.lower()

def test_source_text_line_range_consistent():
    _, subs = extract_file(_write(SIMPLE_MODULE))
    s = subs[0]
    assert s.end_line >= s.start_line


# ---------------------------------------------------------------------------
# Multiple subroutines in one module
# ---------------------------------------------------------------------------

MULTI_SUB = """\
MODULE multi
CONTAINS
  SUBROUTINE alpha()
  END SUBROUTINE

  SUBROUTINE beta()
    CALL alpha()
  END SUBROUTINE

  FUNCTION gamma_fn(x) RESULT(y)
    real, intent(in) :: x
    real :: y
    y = x * 2.0
  END FUNCTION
END MODULE multi
"""

def test_multiple_subroutines_extracted():
    _, subs = extract_file(_write(MULTI_SUB))
    names = [s.name for s in subs]
    assert "alpha" in names
    assert "beta" in names

def test_function_extracted():
    _, subs = extract_file(_write(MULTI_SUB))
    names = [s.name for s in subs]
    assert "gamma_fn" in names

def test_cross_subroutine_call():
    _, subs = extract_file(_write(MULTI_SUB))
    beta = next(s for s in subs if s.name == "beta")
    assert "ALPHA" in beta.calls


# ---------------------------------------------------------------------------
# Top-level subroutine (no MODULE wrapper)
# ---------------------------------------------------------------------------

TOPLEVEL = """\
SUBROUTINE standalone(x)
  real, intent(in) :: x
  CALL helper(x)
END SUBROUTINE
"""

def test_toplevel_subroutine_detected():
    mods, subs = extract_file(_write(TOPLEVEL))
    assert mods == []
    assert len(subs) == 1
    assert subs[0].name == "standalone"

def test_toplevel_module_name_is_empty():
    _, subs = extract_file(_write(TOPLEVEL))
    assert subs[0].module_name == ""


# ---------------------------------------------------------------------------
# pFUnit .pf files
# ---------------------------------------------------------------------------

PFUNIT_SRC = """\
module my_tests
  use funit; implicit none
contains

  @test
  subroutine test_something()
    @assertEqual(1, 1)
  end subroutine

end module my_tests
"""

def test_pfunit_module_detected():
    mods, _ = extract_file(_write(PFUNIT_SRC, suffix=".pf"))
    assert len(mods) == 1
    assert mods[0].name == "my_tests"

def test_pfunit_subroutine_detected():
    _, subs = extract_file(_write(PFUNIT_SRC, suffix=".pf"))
    assert len(subs) == 1
    assert subs[0].name == "test_something"

def test_pfunit_macros_do_not_appear_as_modules():
    _, subs = extract_file(_write(PFUNIT_SRC, suffix=".pf"))
    # @test and @assertEqual should not be parsed as subroutine names
    names = [s.name for s in subs]
    assert all(not n.startswith("@") for n in names)


# ---------------------------------------------------------------------------
# CPP guards pass through cleanly
# ---------------------------------------------------------------------------

CPP_SRC = """\
MODULE guarded
CONTAINS
#ifdef ENABLE_OPENACC
  SUBROUTINE gpu_path()
  END SUBROUTINE
#endif
  SUBROUTINE cpu_path()
  END SUBROUTINE
END MODULE guarded
"""

def test_cpp_guarded_subroutine_still_indexed():
    _, subs = extract_file(_write(CPP_SRC))
    names = [s.name for s in subs]
    assert "gpu_path" in names
    assert "cpu_path" in names


# ---------------------------------------------------------------------------
# Multiple modules in one file
# ---------------------------------------------------------------------------

TWO_MODULES = """\
MODULE mod_a
  IMPLICIT NONE
END MODULE mod_a

MODULE mod_b
  USE mod_a
CONTAINS
  SUBROUTINE run()
  END SUBROUTINE
END MODULE mod_b
"""

def test_two_modules_in_one_file():
    mods, _ = extract_file(_write(TWO_MODULES))
    names = [m.name for m in mods]
    assert "mod_a" in names
    assert "mod_b" in names

def test_second_module_use_collected():
    mods, _ = extract_file(_write(TWO_MODULES))
    mod_b = next(m for m in mods if m.name == "mod_b")
    assert "mod_a" in mod_b.uses


# ---------------------------------------------------------------------------
# pipeline.source_files smoke test
# ---------------------------------------------------------------------------

def test_source_files_lists_f90():
    from src.fesom2_indexer.pipeline import source_files
    files = source_files()
    exts = {p.suffix for p in files}
    assert ".F90" in exts

def test_source_files_excludes_recom_library():
    from src.fesom2_indexer.pipeline import source_files
    files = source_files()
    paths = [str(p) for p in files]
    # The recom/ subdir (gsw_*, DNAD, etc.) must not be included
    assert not any("int_recom/recom/" in p for p in paths)

def test_source_files_includes_recom_drivers():
    from src.fesom2_indexer.pipeline import source_files
    files = source_files()
    paths = [str(p) for p in files]
    assert any("recom_main" in p for p in paths)

def test_source_files_excludes_async_cpp():
    from src.fesom2_indexer.pipeline import source_files
    files = source_files()
    paths = [str(p) for p in files]
    assert not any("async_threads_cpp" in p for p in paths)

def test_source_files_includes_pf_tests():
    from src.fesom2_indexer.pipeline import source_files
    files = source_files()
    exts = {p.suffix for p in files}
    assert ".pf" in exts
