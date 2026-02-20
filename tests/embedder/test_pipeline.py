"""Tests for chunking logic in src/embedder/pipeline.py.

All tests use synthetic strings — no DuckDB or ollama required.
"""

from src.embedder.pipeline import OVERLAP, MAX_CHARS, _chunk_text, _doc_chunks


# ---------------------------------------------------------------------------
# _chunk_text
# ---------------------------------------------------------------------------

def test_short_text_returns_single_chunk():
    text = "A" * (MAX_CHARS - 1)
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    assert len(chunks) == 1
    assert chunks[0] == text


def test_exact_max_chars_returns_single_chunk():
    text = "A" * MAX_CHARS
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    assert len(chunks) == 1


def test_long_text_splits_into_multiple_chunks():
    text = "A" * (MAX_CHARS * 2)
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    assert len(chunks) >= 2


def test_each_chunk_at_most_max_chars():
    text = "A" * (MAX_CHARS * 3 + 77)
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    assert all(len(c) <= MAX_CHARS for c in chunks)


def test_overlap_shared_between_adjacent_chunks():
    # Make text long enough to produce at least two full-length chunks.
    text = "A" * MAX_CHARS + "B" * MAX_CHARS
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    assert len(chunks) >= 2
    # The tail of chunk 0 equals the head of chunk 1.
    assert chunks[0][-OVERLAP:] == chunks[1][:OVERLAP]


def test_no_overlap_joins_to_original():
    # With overlap=0 the chunks partition the text exactly.
    text = "ABCDE" * 1000  # 5000 chars, longer than MAX_CHARS
    chunks = _chunk_text(text, MAX_CHARS, 0)
    assert "".join(chunks) == text


def test_empty_string_returns_one_empty_chunk():
    chunks = _chunk_text("", MAX_CHARS, OVERLAP)
    assert chunks == [""]


def test_all_positions_covered():
    # Every character position in the original text appears in at least one chunk.
    text = "X" * (MAX_CHARS * 2 + 317)
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    step = MAX_CHARS - OVERLAP
    covered = [False] * len(text)
    pos = 0
    for chunk in chunks:
        for j in range(len(chunk)):
            if pos + j < len(text):
                covered[pos + j] = True
        pos += step
    assert all(covered)


# ---------------------------------------------------------------------------
# _doc_chunks
# ---------------------------------------------------------------------------

def test_short_subroutine_one_entry():
    entries = _doc_chunks(1, "SHORT", "model/src/short.F", "model", "X" * 100)
    assert len(entries) == 1
    assert entries[0][2]["n_chunks"] == 1


def test_long_subroutine_multiple_entries():
    entries = _doc_chunks(7, "LONG", "pkg/foo/long.F", "foo", "X" * (MAX_CHARS * 3))
    assert len(entries) >= 3


def test_chunk_ids_are_unique():
    entries = _doc_chunks(42, "BIG", "pkg/bar/big.F", "bar", "X" * (MAX_CHARS * 3))
    ids = [e[0] for e in entries]
    assert len(ids) == len(set(ids))


def test_chunk_ids_include_db_id():
    entries = _doc_chunks(99, "SUB", "model/src/sub.F", "model", "X" * (MAX_CHARS * 2))
    assert all(e[0].startswith("99_") for e in entries)


def test_all_chunks_carry_same_db_id():
    entries = _doc_chunks(42, "BIG", "pkg/bar/big.F", "bar", "X" * (MAX_CHARS * 3))
    assert all(e[2]["db_id"] == 42 for e in entries)


def test_chunk_index_is_sequential():
    entries = _doc_chunks(5, "SUB", "pkg/x/s.F", "x", "X" * (MAX_CHARS * 3))
    indices = [e[2]["chunk_index"] for e in entries]
    assert indices == list(range(len(entries)))


def test_n_chunks_consistent():
    entries = _doc_chunks(5, "SUB", "pkg/x/s.F", "x", "X" * (MAX_CHARS * 3))
    n = len(entries)
    assert all(e[2]["n_chunks"] == n for e in entries)


def test_doc_text_contains_subroutine_header():
    entries = _doc_chunks(1, "CG3D", "model/src/cg3d.F", "model", "X" * 100)
    assert entries[0][1].startswith("SUBROUTINE CG3D [model]")


def test_header_present_in_every_chunk():
    entries = _doc_chunks(3, "MYSUB", "pkg/p/s.F", "p", "X" * (MAX_CHARS * 3))
    assert all("SUBROUTINE MYSUB [p]" in e[1] for e in entries)
