"""Adversarial tests for src/indexer/extract.py.

These tests use synthetic MITgcm-style Fortran snippets that capture edge
cases likely to confuse the regex patterns in the extractor.  Each test
documents the specific MITgcm pattern being exercised and the failure mode
it probes.

Tests that expose confirmed bugs are marked @pytest.mark.xfail(strict=True)
so the suite stays green while the bugs are tracked.
"""

import tempfile
from pathlib import Path

import pytest

from src.indexer.extract import extract_file


# ---------------------------------------------------------------------------
# Helper (same pattern as existing test suite)
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
# Edge case 1: MITgcm U/O/I continuation markers in column 6
#
# MITgcm uses letters U (update), O (output), I (input) in column 6 of
# argument-list continuation lines (see cg3d.F, diags_rho.F, dynamics.F).
# The extractor's _is_continuation_fixed() checks col 6 != ' ', '0', '\n'.
# These letters ARE valid continuation markers, but they spell out words
# like 'I', 'O', 'U' which might accidentally match RE_COMMENT_FIXED if
# the line is misread, or might trip up namelist parsing if the
# continuation text is consumed incorrectly.  Here the subroutine argument
# list is split over continuation lines with U/O/I markers.
# ---------------------------------------------------------------------------

FIXED_UOI_CONTINUATION = """\
      SUBROUTINE CG3D_LIKE(
     U                cg3d_b, cg3d_x,
     O                firstResidual, lastResidual,
     I                myIter, myThid )
      IMPLICIT NONE
      RETURN
      END
"""

def test_uoi_continuation_subroutine_detected():
    """U/O/I markers in column 6 are continuation chars; subroutine must be found."""
    recs = extract_file(_write(FIXED_UOI_CONTINUATION))
    assert len(recs) == 1
    assert recs[0].name.upper() == "CG3D_LIKE"


def test_uoi_continuation_correct_line_range():
    """Subroutine with U/O/I continuation lines must have correct END line."""
    recs = extract_file(_write(FIXED_UOI_CONTINUATION))
    r = recs[0]
    # SUBROUTINE is on line 1, END is on line 7
    assert r.line_start == 1
    assert r.line_end == 7


# ---------------------------------------------------------------------------
# Edge case 2: CBOP/CEOP documentation block before subroutine
#
# MITgcm wraps every subroutine in CBOP ... CEOP markers (column-1 C lines).
# Between CBOP and the SUBROUTINE statement there is often:
#   C     !ROUTINE: MYSUB
# which mentions the subroutine name in a comment.  RE_SUB_START must NOT
# match that comment line and must find the real declaration.
# ---------------------------------------------------------------------------

FIXED_CBOP_CEOP = """\
CBOP
C     !ROUTINE: REAL_SUB
C     !INTERFACE:
      SUBROUTINE REAL_SUB( myThid )
C     !DESCRIPTION: does something
      IMPLICIT NONE
      INTEGER myThid
CEOP
      RETURN
      END
"""

def test_cbop_comment_not_mistaken_for_subroutine():
    """The C !ROUTINE: name line inside CBOP must not create a spurious record."""
    recs = extract_file(_write(FIXED_CBOP_CEOP))
    assert len(recs) == 1


def test_cbop_correct_name():
    """With CBOP/CEOP, subroutine name must come from the actual SUBROUTINE line."""
    recs = extract_file(_write(FIXED_CBOP_CEOP))
    assert recs[0].name.upper() == "REAL_SUB"


# ---------------------------------------------------------------------------
# Edge case 3: DIAGNOSTICS_FILL call split across continuation lines
#
# In diags_rho.F the call appears as:
#   CALL DIAGNOSTICS_FILL( rho3d, 'RHOAnoma',
#  &                               0, Nr, 0, 1, 1, myThid )
# RE_DIAG_FILL searches the *single* line for
#   CALL\s+DIAGNOSTICS_FILL\s*\(\s*(\w+)\s*,\s*'([^']+)'
# This works only when the array name and field string are on the same line.
# When split across lines the regex will miss the field name.
# ---------------------------------------------------------------------------

# BUG: when DIAGNOSTICS_FILL arguments are split across lines, the field
# name falls on the continuation line and RE_DIAG_FILL cannot see it
# because it only searches single lines.  The array name is on line N but
# the quoted field name is on line N+1.

FIXED_DIAG_SPLIT = """\
      SUBROUTINE DIAG_SPLIT_SUB( )
      IMPLICIT NONE
      CALL DIAGNOSTICS_FILL( rho3d,
     &                       'RHOAnoma',
     &                       0, Nr, 0, 1, 1, myThid )
      RETURN
      END
"""

@pytest.mark.xfail(strict=True, reason=(
    "known bug: RE_DIAG_FILL only searches single lines; when the quoted "
    "field name is on a continuation line it is not captured."
))
def test_diag_fill_split_across_continuation_lines():
    """DIAGNOSTICS_FILL with array on line N and field string on line N+1 must be captured."""
    recs = extract_file(_write(FIXED_DIAG_SPLIT))
    fields = [f.strip() for f, _ in recs[0].diag_fills]
    assert "RHOAnoma" in fields


# ---------------------------------------------------------------------------
# Edge case 4: NAMELIST with many parameters across many continuation lines
#   (as in ini_parms.F PARM01 which spans ~60 continuation lines)
#
# Each continuation line starts with 5 spaces, a continuation char ('&'),
# then content from column 7 onward.  The collector in
# _extract_namelist_params uses _is_continuation_fixed which checks col 5
# (0-indexed) is non-blank/non-zero.  In MITgcm the marker is often '&' in
# column 6 (1-indexed), i.e. index 5.  This should work, but we test that
# ALL params across many lines are found.
# ---------------------------------------------------------------------------

FIXED_NAMELIST_LONG = """\
      SUBROUTINE INI_MANY( )
      IMPLICIT NONE
      NAMELIST /BIGLIST/
     & alpha, beta,
     & gamma, delta,
     & epsilon, zeta,
     & eta, theta,
     & iota, kappa,
     & lambda, mu
      RETURN
      END
"""

def test_namelist_long_all_params_found():
    """NAMELIST spread over 6 continuation lines must yield all 12 parameters."""
    recs = extract_file(_write(FIXED_NAMELIST_LONG))
    params = {p for p, _ in recs[0].namelist_params}
    expected = {
        "alpha", "beta", "gamma", "delta", "epsilon", "zeta",
        "eta", "theta", "iota", "kappa", "lambda", "mu",
    }
    assert expected == params


def test_namelist_long_correct_group():
    """All params from a long NAMELIST must map to the correct group."""
    recs = extract_file(_write(FIXED_NAMELIST_LONG))
    groups = {g for _, g in recs[0].namelist_params}
    assert groups == {"BIGLIST"}


# ---------------------------------------------------------------------------
# Edge case 5: Nested #ifdef blocks around a subroutine
#
# In dynamics.F there are deeply nested #ifdef guards.  The guard collector
# pushes to a stack on #ifdef/#ifndef and pops on #endif.  When there are
# two levels of #ifdef, the subroutine should report guards from BOTH levels.
# ---------------------------------------------------------------------------

FIXED_NESTED_IFDEF = """\
#ifdef OUTER_FLAG
#ifdef INNER_FLAG
      SUBROUTINE NESTED_SUB( )
      IMPLICIT NONE
      RETURN
      END
#endif
#endif
"""

def test_nested_ifdef_both_guards_reported():
    """Subroutine inside two #ifdef levels must have both guards in cpp_guards."""
    recs = extract_file(_write(FIXED_NESTED_IFDEF))
    assert len(recs) == 1
    assert "OUTER_FLAG" in recs[0].cpp_guards
    assert "INNER_FLAG" in recs[0].cpp_guards


# ---------------------------------------------------------------------------
# Edge case 6: #ifdef/#else/#endif — subroutine only in the #else branch
#
# The guard stack is maintained by push-on-#ifdef / pop-on-#endif.  An
# #else block doesn't change the stack depth, so a subroutine inside an
# #else branch would still carry the #ifdef guard of the outer block even
# though it's in the negative branch.  We just verify detection works and
# no crash occurs.  Guard semantics in else-branches are not expected to be
# correct (the extractor has no else-awareness).
# ---------------------------------------------------------------------------

FIXED_IFDEF_ELSE = """\
#ifdef HAVE_FEATURE
C     alternative A — not compiled when HAVE_FEATURE is absent
      SUBROUTINE BRANCH_A( )
      IMPLICIT NONE
      RETURN
      END
#else
      SUBROUTINE BRANCH_B( )
      IMPLICIT NONE
      RETURN
      END
#endif
"""

def test_ifdef_else_both_subroutines_detected():
    """Both subroutines in an #ifdef/#else block must be detected."""
    recs = extract_file(_write(FIXED_IFDEF_ELSE))
    names = {r.name.upper() for r in recs}
    assert "BRANCH_A" in names
    assert "BRANCH_B" in names


def test_ifdef_else_branch_b_carries_guard():
    """
    BRANCH_B is in the #else branch; the extractor (which has no else-awareness)
    will still report HAVE_FEATURE as a guard because the #ifdef was still
    pushed and not yet popped.  This is documented behaviour (imprecise but
    not a crash).
    """
    recs = extract_file(_write(FIXED_IFDEF_ELSE))
    branch_b = next(r for r in recs if r.name.upper() == "BRANCH_B")
    # The guard HAVE_FEATURE is on the stack during the #else body because
    # #endif hasn't been seen yet.  We assert the guard is present (not that
    # it is correct) — this documents the known limitation.
    assert "HAVE_FEATURE" in branch_b.cpp_guards


# ---------------------------------------------------------------------------
# Edge case 7: "END" inside a string or comment must not terminate subroutine
#
# The END detector regex is:
#   ^\s{0,9}END\s*(?:SUBROUTINE\b[^!\n]*)?\s*(?:!.*)?$
# A line like:
#   WRITE(*,*) 'END of computation'
# starts with spaces and contains END, but after word 'END' comes ' of ...'
# which is not a valid END statement.  Verify that such lines don't
# prematurely terminate subroutine scanning.
#
# Also: a C comment in column 1 with 'END' must not count.
# ---------------------------------------------------------------------------

FIXED_END_IN_STRING = """\
      SUBROUTINE ENDTEST( )
      IMPLICIT NONE
      WRITE(*,*) 'END of computation'
C     END: this comment mentions END
      CALL WORKER( )
      RETURN
      END
"""

def test_end_in_string_does_not_terminate_early():
    """'END' appearing inside a string or comment must not close the subroutine."""
    recs = extract_file(_write(FIXED_END_IN_STRING))
    assert len(recs) == 1
    assert "WORKER" in recs[0].calls


def test_end_in_string_correct_line_end():
    """Subroutine with 'END' in a string should end at the real END line."""
    recs = extract_file(_write(FIXED_END_IN_STRING))
    # END is on line 7
    assert recs[0].line_end == 7


# ---------------------------------------------------------------------------
# Edge case 8: Bare "END" with leading spaces > 9 (outside RE_SUB_END window)
#
# RE_SUB_END is: ^\s{0,9}END\s*...
# Fixed-form Fortran has code in columns 7-72.  A bare END statement will
# have 6 spaces before it (standard MITgcm style).  But what if there are
# exactly 10 spaces?  The regex requires at most 9 leading spaces.
# Six-space indent is fine; test that a 6-space END is still detected.
# ---------------------------------------------------------------------------

FIXED_END_SIX_SPACES = """\
      SUBROUTINE SIXSPACE( )
      IMPLICIT NONE
      RETURN
      END
"""

def test_end_with_six_leading_spaces_detected():
    """Standard MITgcm 6-space indent on END must be detected by RE_SUB_END."""
    recs = extract_file(_write(FIXED_END_SIX_SPACES))
    assert len(recs) == 1
    assert recs[0].line_end == 4


# ---------------------------------------------------------------------------
# Edge case 9: ENDDO / ENDIF / END DO / END IF inside subroutine
#
# RE_END_BLOCK matches lines like "      END DO", "      ENDIF", "      ENDDO".
# RE_SUB_END must NOT match those lines.  Verify the boundary detector
# does not stop at ENDDO or END IF.
# ---------------------------------------------------------------------------

FIXED_ENDDO_ENDIF = """\
      SUBROUTINE LOOPY( n )
      IMPLICIT NONE
      INTEGER n, i
      DO i = 1, n
        IF ( i .GT. 5 ) THEN
          WRITE(*,*) i
        END IF
      END DO
      RETURN
      END
"""

def test_enddo_endif_does_not_terminate_subroutine():
    """END DO and END IF inside subroutine must not trigger subroutine boundary."""
    recs = extract_file(_write(FIXED_ENDDO_ENDIF))
    assert len(recs) == 1
    # The real END is on line 10
    assert recs[0].line_end == 10


FIXED_ENDDO_COMPACT = """\
      SUBROUTINE COMPACT_LOOP( n )
      IMPLICIT NONE
      INTEGER n, i
      DO i = 1, n
        IF ( i .GT. 0 ) THEN
          WRITE(*,*) i
        ENDIF
      ENDDO
      RETURN
      END
"""

def test_compact_enddo_endif_does_not_terminate_subroutine():
    """Compact ENDDO and ENDIF (no space) must not trigger subroutine boundary."""
    recs = extract_file(_write(FIXED_COMPACT_ENDDO_ENDDO))
    assert len(recs) == 1
    assert recs[0].line_end == 10


# ---------------------------------------------------------------------------
# Edge case 10: CALL inside a Fortran comment line (C in column 1)
#
# Fixed-form: a line starting with 'C' in column 1 is a comment.
# RE_CALL is: ^\s+CALL\s+(\w+)
# Since RE_COMMENT_FIXED skips C-lines before RE_CALL is tested, a commented
# CALL should not appear in the calls list.  However the call extraction loop
# checks RE_COMMENT_FIXED and then immediately tries RE_CALL — so the skip
# only works if the comment check comes first.
# ---------------------------------------------------------------------------

FIXED_CALL_IN_COMMENT = """\
      SUBROUTINE CALLER_SUB( )
      IMPLICIT NONE
C     CALL GHOST_FUNC( x )
      CALL REAL_FUNC( x )
      RETURN
      END
"""

def test_call_in_comment_not_extracted():
    """A CALL inside a C-comment line must not appear in the calls list."""
    recs = extract_file(_write(FIXED_CALL_IN_COMMENT))
    calls = [c.upper() for c in recs[0].calls]
    assert "GHOST_FUNC" not in calls


def test_real_call_after_comment_extracted():
    """The real CALL after a comment-CALL line must still be extracted."""
    recs = extract_file(_write(FIXED_CALL_IN_COMMENT))
    calls = [c.upper() for c in recs[0].calls]
    assert "REAL_FUNC" in calls


# ---------------------------------------------------------------------------
# Edge case 11: Subroutine name mentioned in a comment before the declaration
#
# MITgcm CBOP blocks include lines like:
#   C     !ROUTINE: DYNAMICS
# and even the calling-sequence block lists subroutine names after "|--".
# RE_SUB_START = ^\s*SUBROUTINE\s+(\w+)  — this won't match C-comment lines.
# But what about a free-form .F90 comment (!) with the word SUBROUTINE?
# ---------------------------------------------------------------------------

FIXED_SUB_NAME_IN_COMMENT = """\
C     This file contains: SUBROUTINE PHANTOM (not real)
C     The real one follows.
      SUBROUTINE REAL_ONE( )
      IMPLICIT NONE
      RETURN
      END
"""

def test_subroutine_in_comment_not_detected():
    """'SUBROUTINE' mentioned inside a C-comment must not create a spurious record."""
    recs = extract_file(_write(FIXED_SUB_NAME_IN_COMMENT))
    assert len(recs) == 1
    assert recs[0].name.upper() == "REAL_ONE"


# ---------------------------------------------------------------------------
# Edge case 12: Free-form file (.F90) with SUBROUTINE in a ! comment
# ---------------------------------------------------------------------------

FREE_SUB_IN_COMMENT = """\
! This file has SUBROUTINE PHANTOM in a comment
subroutine real_f90(x)
  implicit none
  real, intent(in) :: x
end subroutine real_f90
"""

def test_free_form_subroutine_in_comment_not_detected():
    """In .F90, 'SUBROUTINE' in a ! comment must not create a spurious record."""
    recs = extract_file(_write(FREE_SUB_IN_COMMENT, suffix=".F90"))
    assert len(recs) == 1
    assert recs[0].name.lower() == "real_f90"


# ---------------------------------------------------------------------------
# Edge case 13: RE_SUB_START matches with leading spaces (free-form)
#
# The extractor's RE_SUB_START is ^\s*SUBROUTINE\s+(\w+) which allows
# arbitrary leading whitespace.  In free-form it is common to indent.
# Confirm the name is still captured correctly.
# ---------------------------------------------------------------------------

FREE_INDENTED_SUB = """\
  subroutine indented_sub(x)
    implicit none
    real, intent(in) :: x
  end subroutine indented_sub
"""

def test_free_form_indented_subroutine_detected():
    """Indented SUBROUTINE declaration in .F90 must be detected."""
    recs = extract_file(_write(FREE_INDENTED_SUB, suffix=".F90"))
    assert len(recs) == 1
    assert recs[0].name.lower() == "indented_sub"


# ---------------------------------------------------------------------------
# Edge case 14: END SUBROUTINE with name — must close correctly
# ---------------------------------------------------------------------------

FIXED_END_SUBROUTINE_NAME = """\
      SUBROUTINE NAMED_END( )
      IMPLICIT NONE
      RETURN
      END SUBROUTINE NAMED_END
"""

def test_end_subroutine_name_closes_correctly():
    """'END SUBROUTINE name' form must be recognised as the end."""
    recs = extract_file(_write(FIXED_END_SUBROUTINE_NAME))
    assert len(recs) == 1
    assert recs[0].line_end == 4


# ---------------------------------------------------------------------------
# Edge case 15: RE_SUB_END on a line with trailing inline comment
#
# RE_SUB_END: ^\s{0,9}END\s*(?:SUBROUTINE\b[^!\n]*)?\s*(?:!.*)?$
# A bare END with a trailing ! comment must still match.
# ---------------------------------------------------------------------------

FIXED_END_WITH_COMMENT = """\
      SUBROUTINE TRAILCOMMENT( )
      IMPLICIT NONE
      RETURN
      END ! end of TRAILCOMMENT
"""

def test_end_with_trailing_comment_detected():
    """'END ! comment' must be recognised as the subroutine end."""
    recs = extract_file(_write(FIXED_END_WITH_COMMENT))
    assert len(recs) == 1
    assert recs[0].line_end == 4


# ---------------------------------------------------------------------------
# Edge case 16: CALL on a continuation line (not in column 7+)
#
# RE_CALL is ^\s+CALL\s+(\w+) — it requires CALL at the start of a line
# (after whitespace).  In MITgcm a CALL whose arguments span lines has the
# continuation character in col 6 and the rest of the args on subsequent
# lines; the CALL itself is always on its own line.  But what about a CALL
# that is the continuation of an IF statement?
#
#   IF (flag) CALL HIDDEN_FUNC(x)
#
# RE_CALL requires leading whitespace then CALL; since "IF (flag) CALL ..."
# has "IF" first, the regex won't match — HIDDEN_FUNC won't be captured.
# This is a known limitation (not a bug per se) but we document it.
# ---------------------------------------------------------------------------

FIXED_CALL_IN_IF = """\
      SUBROUTINE INLINE_IF( flag )
      IMPLICIT NONE
      LOGICAL flag
      IF ( flag ) CALL HIDDEN_FUNC( x )
      CALL VISIBLE_FUNC( x )
      RETURN
      END
"""

@pytest.mark.xfail(strict=True, reason=(
    "known bug: RE_CALL only matches lines starting with whitespace+CALL; "
    "a CALL embedded in 'IF (cond) CALL FOO' is not captured."
))
def test_call_inline_if_extracted():
    """CALL embedded in 'IF (cond) CALL FOO(x)' on one line must be extracted."""
    recs = extract_file(_write(FIXED_CALL_IN_IF))
    calls = [c.upper() for c in recs[0].calls]
    assert "HIDDEN_FUNC" in calls


def test_call_after_inline_if_still_extracted():
    """Normal CALL on its own line after an inline-IF CALL must still work."""
    recs = extract_file(_write(FIXED_CALL_IN_IF))
    calls = [c.upper() for c in recs[0].calls]
    assert "VISIBLE_FUNC" in calls


# ---------------------------------------------------------------------------
# Edge case 17: Two NAMELIST groups in the same subroutine
#
# ini_parms.F has many NAMELIST declarations.  The extractor must handle
# multiple separate NAMELIST statements, each with their own group name.
# ---------------------------------------------------------------------------

FIXED_MULTI_NAMELIST = """\
      SUBROUTINE MULTI_NML( )
      IMPLICIT NONE
      NAMELIST /GROUP_A/ alpha, beta
      NAMELIST /GROUP_B/ gamma, delta
      RETURN
      END
"""

def test_multi_namelist_both_groups_found():
    """Two NAMELIST declarations in one subroutine must both be captured."""
    recs = extract_file(_write(FIXED_MULTI_NAMELIST))
    groups = {g for _, g in recs[0].namelist_params}
    assert "GROUP_A" in groups
    assert "GROUP_B" in groups


def test_multi_namelist_params_assigned_to_correct_group():
    """Parameters from each NAMELIST group must map to the correct group name."""
    recs = extract_file(_write(FIXED_MULTI_NAMELIST))
    params = {p: g for p, g in recs[0].namelist_params}
    assert params.get("alpha") == "GROUP_A"
    assert params.get("gamma") == "GROUP_B"


# ---------------------------------------------------------------------------
# Edge case 18: Subroutine with only ENDDO / ENDIF  (no bare END)
#
# If a subroutine ends with ENDDO/ENDIF followed immediately by 'END',
# the scanner must not confuse ENDDO with the subroutine END.
# ---------------------------------------------------------------------------

FIXED_SUB_ENDS_WITH_ENDDO = """\
      SUBROUTINE ONLY_LOOPS( n )
      IMPLICIT NONE
      INTEGER n, i
      DO i = 1, n
        WRITE(*,*) i
      ENDDO
      END
"""

def test_sub_ends_with_enddo_correct_boundary():
    """Subroutine whose last executable is ENDDO must still end at bare END."""
    recs = extract_file(_write(FIXED_SUB_ENDS_WITH_ENDDO))
    assert len(recs) == 1
    assert recs[0].line_end == 7


# ---------------------------------------------------------------------------
# Edge case 19: DIAGNOSTICS_FILL with no space before '(' and field name
#   padded with trailing spaces inside quotes, as used in MITgcm
#   e.g. CALL DIAGNOSTICS_FILL(arr,'MYFIELD ',k,...) — note trailing space
# ---------------------------------------------------------------------------

FIXED_DIAG_PADDED_FIELD = """\
      SUBROUTINE PADDED_DIAG( )
      IMPLICIT NONE
      CALL DIAGNOSTICS_FILL(myArr,'MYFLD   ',0,1,0,1,1,myThid)
      RETURN
      END
"""

def test_diag_fill_padded_field_name_extracted():
    """Field name with trailing spaces inside quotes must be captured (incl. spaces)."""
    recs = extract_file(_write(FIXED_DIAG_PADDED_FIELD))
    raw_fields = [f for f, _ in recs[0].diag_fills]
    # The raw field value from the regex will include trailing spaces
    assert any("MYFLD" in f for f in raw_fields)


# ---------------------------------------------------------------------------
# Edge case 20: #ifdef guard on same line as SUBROUTINE (non-standard but
#   possible in macro-heavy code via ## concatenation patterns).
#   More importantly: #endif appearing AFTER the END of a subroutine.
#   The guards set should include the flag even though #endif comes after END.
# ---------------------------------------------------------------------------

FIXED_GUARD_ENDS_AFTER_SUB = """\
#ifdef ALLOW_SOMETHING
      SUBROUTINE GUARDED_SUB( )
      IMPLICIT NONE
      RETURN
      END
#endif
"""

def test_guard_after_end_still_attributed():
    """#ifdef guard that closes after the END must still appear in cpp_guards."""
    recs = extract_file(_write(FIXED_GUARD_ENDS_AFTER_SUB))
    assert len(recs) == 1
    assert "ALLOW_SOMETHING" in recs[0].cpp_guards


# ---------------------------------------------------------------------------
# Edge case 21: Compact ENDDO at column 7 (no leading spaces beyond 6)
#
# RE_END_BLOCK requires ^\s+ (at least one space) before END keyword.
# "      ENDDO" has 6 leading spaces — matches \s+.
# But what about "ENDDO" at column 1?  That is non-standard but let's
# also verify the standard 6-space case is correctly handled.
# ---------------------------------------------------------------------------

FIXED_ENDDO_COL7 = """\
      SUBROUTINE COL7_LOOP( n )
      IMPLICIT NONE
      INTEGER n, i
      DO i=1,n
      ENDDO
      END
"""

def test_enddo_at_col7_not_subroutine_end():
    """ENDDO with 6 leading spaces must not terminate subroutine scanning."""
    recs = extract_file(_write(FIXED_ENDDO_COL7))
    assert len(recs) == 1
    assert recs[0].line_end == 6


# ---------------------------------------------------------------------------
# Edge case 22: RE_SUB_END — a line "      END\n" with exactly 6 spaces
#   (the standard MITgcm indent) and nothing else.  Test that the regex
#   matches it even though \s{0,9} is the limit (6 < 9).
# ---------------------------------------------------------------------------

FIXED_EXACT_SIX_SPACE_END = """\
      SUBROUTINE SIX_SP( )
      RETURN
      END
"""

def test_standard_six_space_end_detected():
    """Standard 6-space bare END (MITgcm norm) must close the subroutine."""
    recs = extract_file(_write(FIXED_EXACT_SIX_SPACE_END))
    assert len(recs) == 1


# ---------------------------------------------------------------------------
# Edge case 23: ENDDO with zero leading spaces (column 1)
#
# RE_END_BLOCK requires ^\s+ (one or more spaces).  If ENDDO appears at
# column 1 it will NOT match RE_END_BLOCK, so the extractor might treat it
# as a bare END (via RE_SUB_END matching "ENDDO"?).  Actually RE_SUB_END
# requires ^\s{0,9}END\s*(?:SUBROUTINE...)?$ and "ENDDO" would have "DO"
# after END with no whitespace — the pattern END\s* would match "END"
# then \s* matches nothing, then the (?:SUBROUTINE...)? is optional and
# the remaining "DO\n" would fail the $ anchor only if the rest of the
# pattern consumes it — let's check.
#
# Actually RE_SUB_END = ^\s{0,9}END\s*(?:SUBROUTINE\b[^!\n]*)?\s*(?:!.*)?$
# "ENDDO" -> ^\s{0,9} matches 0 spaces, "END" matches "END",
# \s* matches nothing, the optional SUBROUTINE group fails to match,
# \s* matches nothing, (?:!.*)? matches nothing,
# $ needs to match here but "DO" is still remaining -> FAILS. Good.
#
# But "ENDIF" at column 1? Same logic: END matches, then "IF" remains, $ fails.
# RE_END_BLOCK: ^\s+(?:END\s+(?:DO|IF|...)|END(?:DO|IF|...))
# also requires ^\s+ so won't match col-1 ENDDO.
#
# So col-1 ENDDO won't be caught by either RE_SUB_END or RE_END_BLOCK.
# The subroutine boundary will be missed... or the scanner will just skip
# that line and keep looking.  Let's verify.
# ---------------------------------------------------------------------------

FIXED_ENDDO_COL1 = """\
      SUBROUTINE COL1_LOOP( n )
      IMPLICIT NONE
      INTEGER n, i
      DO i=1,n
        WRITE(*,*) i
ENDDO
      END
"""

def test_enddo_at_col1_not_subroutine_end():
    """ENDDO at column 1 (non-standard) must not close the subroutine prematurely."""
    recs = extract_file(_write(FIXED_ENDDO_COL1))
    assert len(recs) == 1
    # Real END is line 7
    assert recs[0].line_end == 7


# ---------------------------------------------------------------------------
# Edge case 24: Multiple DIAGNOSTICS_FILL calls in the same subroutine
#   with different field names — deduplication must NOT be applied to diag_fills
# ---------------------------------------------------------------------------

FIXED_MULTI_DIAG = """\
      SUBROUTINE MULTI_DIAG_SUB( )
      IMPLICIT NONE
      CALL DIAGNOSTICS_FILL(arr1,'FIELD1  ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(arr2,'FIELD2  ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(arr1,'FIELD1  ',0,1,0,1,1,myThid)
      RETURN
      END
"""

def test_multi_diag_all_fills_captured():
    """Multiple DIAGNOSTICS_FILL calls (including duplicates) must all be captured."""
    recs = extract_file(_write(FIXED_MULTI_DIAG))
    fields = [f for f, _ in recs[0].diag_fills]
    # calls is deduplicated, but diag_fills should not be
    assert len(fields) == 3


# ---------------------------------------------------------------------------
# Edge case 25: Blank file / file with no subroutines
# ---------------------------------------------------------------------------

def test_empty_file_returns_no_records():
    """An empty .F file must return an empty list, not raise."""
    recs = extract_file(_write(""))
    assert recs == []


def test_only_comments_returns_no_records():
    """A .F file with only C-comments and no subroutines must return []."""
    content = "C  Just a comment\nC  Another comment\n"
    recs = extract_file(_write(content))
    assert recs == []


# ---------------------------------------------------------------------------
# Fix the typo in test_compact_enddo_endif_does_not_terminate_subroutine
# (the variable name used in the test body above must match what's defined)
# ---------------------------------------------------------------------------

# Redefine to avoid NameError from the typo above
FIXED_COMPACT_ENDDO_ENDDO = FIXED_ENDDO_COMPACT  # alias used in the test body
