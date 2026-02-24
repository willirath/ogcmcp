"""Tests for src/fesom2_embedder/pipeline.py — chunking logic only.

No DuckDB or ollama required.
"""

from src.embedder.pipeline import MAX_CHARS, OVERLAP
from src.fesom2_embedder.pipeline import _doc_chunks


# ---------------------------------------------------------------------------
# _doc_chunks
# ---------------------------------------------------------------------------

def test_short_subroutine_one_chunk():
    entries = _doc_chunks(1, "oce_init", "src/oce_init.F90", "oce_modules", "X" * 100)
    assert len(entries) == 1
    assert entries[0][2]["n_chunks"] == 1


def test_long_subroutine_multiple_chunks():
    entries = _doc_chunks(7, "ice_step", "src/ice_step.F90", "ice_modules", "X" * (MAX_CHARS * 3))
    assert len(entries) >= 3


def test_chunk_ids_are_unique():
    entries = _doc_chunks(42, "oce_dyn", "src/oce_dyn.F90", "oce_dyn", "X" * (MAX_CHARS * 3))
    ids = [e[0] for e in entries]
    assert len(ids) == len(set(ids))


def test_chunk_ids_include_db_id():
    entries = _doc_chunks(99, "oce_tra", "src/oce_tra.F90", "oce_tra", "X" * (MAX_CHARS * 2))
    assert all(e[0].startswith("99_") for e in entries)


def test_all_chunks_carry_same_module_name():
    entries = _doc_chunks(42, "ice_EVP", "src/ice_EVP.F90", "ice_EVP", "X" * (MAX_CHARS * 3))
    assert all(e[2]["module_name"] == "ice_EVP" for e in entries)


def test_metadata_has_module_name_not_package():
    entries = _doc_chunks(1, "gen_comm", "src/gen_comm.F90", "gen_comm", "short code")
    meta = entries[0][2]
    assert "module_name" in meta
    assert "package" not in meta


def test_chunk_index_is_sequential():
    entries = _doc_chunks(5, "io_netcdf", "src/io_netcdf.F90", "io_netcdf", "X" * (MAX_CHARS * 3))
    indices = [e[2]["chunk_index"] for e in entries]
    assert indices == list(range(len(entries)))


def test_n_chunks_consistent():
    entries = _doc_chunks(5, "io_restart", "src/io_restart.F90", "io_restart", "X" * (MAX_CHARS * 3))
    n = len(entries)
    assert all(e[2]["n_chunks"] == n for e in entries)


def test_header_format_subroutine_module():
    entries = _doc_chunks(1, "oce_fer_gm", "src/oce_fer_gm.F90", "oce_fer_gm", "X" * 100)
    assert entries[0][1].startswith("SUBROUTINE oce_fer_gm [oce_fer_gm]")


def test_header_present_in_every_chunk():
    entries = _doc_chunks(3, "ice_maEVP", "src/ice_maEVP.F90", "ice_maEVP", "X" * (MAX_CHARS * 3))
    assert all("SUBROUTINE ice_maEVP [ice_maEVP]" in e[1] for e in entries)


def test_each_chunk_at_most_max_chars_plus_header():
    entries = _doc_chunks(1, "BIG", "src/big.F90", "big_module", "X" * (MAX_CHARS * 3 + 77))
    header_len = len("SUBROUTINE BIG [big_module]\n")
    assert all(len(e[1]) <= MAX_CHARS + header_len for e in entries)
