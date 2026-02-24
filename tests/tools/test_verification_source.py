"""Tests for get_verification_source using a synthetic ChromaDB fixture."""

import pytest
import chromadb

from src.tools import get_verification_source
from src.embedder.pipeline import OVERLAP


@pytest.fixture(scope="module")
def vrf_chroma(tmp_path_factory):
    """Synthetic mitgcm_verification ChromaDB collection.

    exp1/input/data      — single chunk (short namelist)
    exp1/code/SIZE.h     — single chunk (Fortran header)
    exp2/input/data      — two chunks with overlap (long namelist)
    """
    path = tmp_path_factory.mktemp("vrf_chroma")
    client = chromadb.PersistentClient(path=str(path))
    col = client.get_or_create_collection(
        name="mitgcm_verification", metadata={"hnsw:space": "cosine"}
    )

    # exp1/input/data — single chunk
    file_a = "verification/exp1/input/data"
    text_a = " &PARM01\n tRef=10*0.,\n /\n"
    col.add(
        ids=["vrf_exp1_data_0"],
        documents=[f"[{file_a}]\n" + text_a],
        metadatas=[{"file": file_a, "experiment": "exp1",
                    "filename": "data", "chunk_index": 0}],
        embeddings=[[0.1] * 768],
    )

    # exp1/code/SIZE.h — single chunk
    file_b = "verification/exp1/code/SIZE.h"
    text_b = "      INTEGER sNx\n      PARAMETER ( sNx = 20 )\n"
    col.add(
        ids=["vrf_exp1_SIZE_0"],
        documents=[f"[{file_b}]\n" + text_b],
        metadatas=[{"file": file_b, "experiment": "exp1",
                    "filename": "SIZE.h", "chunk_index": 0}],
        embeddings=[[0.2] * 768],
    )

    # exp2/input/data — two chunks with OVERLAP overlap
    file_c = "verification/exp2/input/data"
    chunk0_raw = "A" * 3000
    chunk1_raw = "A" * OVERLAP + "B" * 300
    col.add(
        ids=["vrf_exp2_data_0", "vrf_exp2_data_1"],
        documents=[f"[{file_c}]\n" + chunk0_raw, f"[{file_c}]\n" + chunk1_raw],
        metadatas=[
            {"file": file_c, "experiment": "exp2",
             "filename": "data", "chunk_index": 0},
            {"file": file_c, "experiment": "exp2",
             "filename": "data", "chunk_index": 1},
        ],
        embeddings=[[0.3] * 768, [0.4] * 768],
    )

    return path


def test_returns_none_for_unknown(vrf_chroma):
    result = get_verification_source(
        "verification/no_such_exp/input/data", _chroma_path=vrf_chroma
    )
    assert result is None


def test_single_chunk_required_keys(vrf_chroma):
    result = get_verification_source(
        "verification/exp1/input/data", _chroma_path=vrf_chroma
    )
    assert result is not None
    for key in ("file", "total_lines", "offset", "lines"):
        assert key in result


def test_single_chunk_file_echoed(vrf_chroma):
    result = get_verification_source(
        "verification/exp1/input/data", _chroma_path=vrf_chroma
    )
    assert result["file"] == "verification/exp1/input/data"


def test_single_chunk_header_stripped(vrf_chroma):
    result = get_verification_source(
        "verification/exp1/input/data", _chroma_path=vrf_chroma
    )
    joined = "\n".join(result["lines"])
    assert "[verification/exp1" not in joined
    assert "PARM01" in joined


def test_size_h_content(vrf_chroma):
    result = get_verification_source(
        "verification/exp1/code/SIZE.h", _chroma_path=vrf_chroma
    )
    assert result is not None
    joined = "\n".join(result["lines"])
    assert "sNx" in joined
    assert "[verification" not in joined


def test_multi_chunk_overlap_stripped(vrf_chroma):
    result = get_verification_source(
        "verification/exp2/input/data", _chroma_path=vrf_chroma
    )
    assert result is not None
    full = "".join(result["lines"])
    assert full == "A" * 3000 + "B" * 300


def test_pagination_offset(vrf_chroma):
    full = get_verification_source(
        "verification/exp1/input/data", _chroma_path=vrf_chroma
    )
    paged = get_verification_source(
        "verification/exp1/input/data", offset=1, limit=1, _chroma_path=vrf_chroma
    )
    assert paged["lines"] == [full["lines"][1]]
    assert paged["offset"] == 1
    assert paged["total_lines"] == full["total_lines"]


def test_pagination_limit(vrf_chroma):
    result = get_verification_source(
        "verification/exp1/input/data", limit=1, _chroma_path=vrf_chroma
    )
    assert len(result["lines"]) == 1
