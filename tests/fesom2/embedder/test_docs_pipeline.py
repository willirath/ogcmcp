"""Tests for src/fesom2/embedder/docs_pipeline.py — file chunking helpers."""

from pathlib import Path

from src.fesom2.embedder.docs_pipeline import _file_chunks, _iter_extra_files


# ── _file_chunks ──────────────────────────────────────────────────────────────


def test_file_chunks_short_text_one_chunk():
    chunks = _file_chunks("extra_0", "visualization/README.md", "hello world")
    assert len(chunks) == 1


def test_file_chunks_id_format():
    chunks = _file_chunks("extra_7", "src/foo.h", "content")
    assert chunks[0][0] == "extra_7_0"


def test_file_chunks_metadata_file():
    chunks = _file_chunks("extra_1", "src/foo.h", "content")
    assert chunks[0][2]["file"] == "src/foo.h"


def test_file_chunks_header_in_document():
    chunks = _file_chunks("extra_2", "src/bar.inc", "some content")
    assert chunks[0][1].startswith("[src/bar.inc]\n")


def test_file_chunks_n_chunks_consistent():
    from src.embed_utils import MAX_CHARS
    chunks = _file_chunks("extra_3", "src/big.h", "X" * (MAX_CHARS * 3))
    n = len(chunks)
    assert n >= 2
    assert all(c[2]["n_chunks"] == n for c in chunks)


# ── _iter_extra_files ─────────────────────────────────────────────────────────


def test_iter_extra_files_empty_root(tmp_path):
    result = _iter_extra_files(tmp_path)
    assert result == []


def test_iter_extra_files_finds_src_h(tmp_path):
    src = tmp_path / "src"
    src.mkdir()
    (src / "mesh_def.h").write_text("! mesh macros\n#define NOD2D 1000")
    result = _iter_extra_files(tmp_path)
    files = [r["file"] for r in result]
    assert "src/mesh_def.h" in files


def test_iter_extra_files_finds_src_inc(tmp_path):
    src = tmp_path / "src"
    src.mkdir()
    (src / "gather.inc").write_text("! gather template\nsome code")
    result = _iter_extra_files(tmp_path)
    files = [r["file"] for r in result]
    assert "src/gather.inc" in files


def test_iter_extra_files_finds_visualization_readme(tmp_path):
    viz = tmp_path / "visualization"
    viz.mkdir()
    (viz / "README.md").write_text("# Visualization overview")
    result = _iter_extra_files(tmp_path)
    files = [r["file"] for r in result]
    assert "visualization/README.md" in files


def test_iter_extra_files_finds_nested_viz_readme(tmp_path):
    tool = tmp_path / "visualization" / "pyfesom2"
    tool.mkdir(parents=True)
    (tool / "README.md").write_text("# pyfesom2 tool")
    result = _iter_extra_files(tmp_path)
    files = [r["file"] for r in result]
    assert "visualization/pyfesom2/README.md" in files


def test_iter_extra_files_skips_empty(tmp_path):
    src = tmp_path / "src"
    src.mkdir()
    (src / "empty.h").write_text("   \n  ")
    result = _iter_extra_files(tmp_path)
    files = [r["file"] for r in result]
    assert "src/empty.h" not in files


def test_iter_extra_files_text_field(tmp_path):
    src = tmp_path / "src"
    src.mkdir()
    content = "! header content"
    (src / "foo.h").write_text(content)
    result = _iter_extra_files(tmp_path)
    assert result[0]["text"] == content
