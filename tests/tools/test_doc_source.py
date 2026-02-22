"""Tests for get_doc_source using a synthetic ChromaDB fixture."""

import pytest
import chromadb

from src.tools import get_doc_source
from src.embedder.pipeline import OVERLAP


@pytest.fixture(scope="module")
def docs_chroma(tmp_path_factory):
    """Synthetic docs ChromaDB collection with two sections and one .h file.

    Section A: single chunk (short text)
    Section B: two chunks (text > MAX_CHARS, with overlap)
    SIZE.h:     single chunk (raw header content)
    """
    path = tmp_path_factory.mktemp("docs_chroma")
    client = chromadb.PersistentClient(path=str(path))
    col = client.get_or_create_collection(
        name="mitgcm_docs", metadata={"hnsw:space": "cosine"}
    )

    # Section A — single chunk, RST section
    file_a = "pkg/diagnostics.rst"
    section_a = "Overview"
    header_a = f"[{file_a}] {section_a}\n"
    text_a = "Line one.\nLine two.\nLine three.\n"
    col.add(
        ids=["doc_0_0"],
        documents=[header_a + text_a],
        metadatas=[{"file": file_a, "section": section_a,
                    "chunk_index": 0, "n_chunks": 1, "section_id": "doc_0"}],
        embeddings=[[0.1] * 768],
    )

    # Section B — two chunks with OVERLAP chars overlap
    file_b = "getting_started/getting_started.rst"
    section_b = "Compiling MITgcm"
    header_b = f"[{file_b}] {section_b}\n"
    # Build synthetic multi-chunk text: 'A'*4000 then 'B'*200 (the overlap region) then 'C'*200
    chunk0_raw = "A" * 4000
    chunk1_raw = "A" * OVERLAP + "C" * 200   # first OVERLAP chars overlap with end of chunk0
    col.add(
        ids=["doc_1_0", "doc_1_1"],
        documents=[header_b + chunk0_raw, header_b + chunk1_raw],
        metadatas=[
            {"file": file_b, "section": section_b,
             "chunk_index": 0, "n_chunks": 2, "section_id": "doc_1"},
            {"file": file_b, "section": section_b,
             "chunk_index": 1, "n_chunks": 2, "section_id": "doc_1"},
        ],
        embeddings=[[0.2] * 768, [0.3] * 768],
    )

    # SIZE.h — single chunk, raw header content, section = filename
    file_h = "verification/rotating_tank/code/SIZE.h"
    section_h = "SIZE.h"
    header_h = f"[{file_h}] {section_h}\n"
    text_h = "      INTEGER sNx\n      PARAMETER ( sNx = 30 )\n      INTEGER sNy\n      PARAMETER ( sNy = 30 )\n"
    col.add(
        ids=["hdr_0_0"],
        documents=[header_h + text_h],
        metadatas=[{"file": file_h, "section": section_h,
                    "chunk_index": 0, "n_chunks": 1, "section_id": "hdr_0"}],
        embeddings=[[0.4] * 768],
    )

    return path


def test_returns_none_for_unknown(docs_chroma):
    result = get_doc_source("no/such/file.rst", "No Section", _chroma_path=docs_chroma)
    assert result is None


def test_single_chunk_has_required_keys(docs_chroma):
    result = get_doc_source("pkg/diagnostics.rst", "Overview", _chroma_path=docs_chroma)
    assert result is not None
    for key in ("file", "section", "total_lines", "offset", "lines"):
        assert key in result


def test_single_chunk_file_and_section(docs_chroma):
    result = get_doc_source("pkg/diagnostics.rst", "Overview", _chroma_path=docs_chroma)
    assert result["file"] == "pkg/diagnostics.rst"
    assert result["section"] == "Overview"


def test_single_chunk_header_stripped(docs_chroma):
    result = get_doc_source("pkg/diagnostics.rst", "Overview", _chroma_path=docs_chroma)
    joined = "\n".join(result["lines"])
    assert "[pkg/diagnostics.rst]" not in joined
    assert "Line one." in joined


def test_single_chunk_lines(docs_chroma):
    result = get_doc_source("pkg/diagnostics.rst", "Overview", _chroma_path=docs_chroma)
    assert result["lines"] == ["Line one.", "Line two.", "Line three."]


def test_size_h_returns_content(docs_chroma):
    result = get_doc_source(
        "verification/rotating_tank/code/SIZE.h", "SIZE.h", _chroma_path=docs_chroma
    )
    assert result is not None
    joined = "\n".join(result["lines"])
    assert "sNx" in joined
    assert "sNy" in joined
    assert "[verification" not in joined


def test_multi_chunk_overlap_stripped(docs_chroma):
    result = get_doc_source(
        "getting_started/getting_started.rst", "Compiling MITgcm",
        _chroma_path=docs_chroma,
    )
    assert result is not None
    full = "".join(result["lines"])
    # Reassembled text should be chunk0_raw + 'C'*200 (OVERLAP stripped from chunk1)
    assert full == "A" * 4000 + "C" * 200


def test_pagination_offset(docs_chroma):
    full = get_doc_source("pkg/diagnostics.rst", "Overview", _chroma_path=docs_chroma)
    paged = get_doc_source("pkg/diagnostics.rst", "Overview", offset=1, limit=1, _chroma_path=docs_chroma)
    assert paged["lines"] == [full["lines"][1]]
    assert paged["offset"] == 1
    assert paged["total_lines"] == full["total_lines"]


def test_pagination_limit(docs_chroma):
    result = get_doc_source("pkg/diagnostics.rst", "Overview", limit=1, _chroma_path=docs_chroma)
    assert len(result["lines"]) == 1
