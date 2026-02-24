"""Tests for src/fesom2_embedder/nml_pipeline.py — namelist document building.

No DuckDB or ollama required.
"""

from src.fesom2_embedder.nml_pipeline import _nml_doc


def test_document_text_format():
    _, doc, _ = _nml_doc("step_per_day", "timestep", "config/namelist.config",
                          "number of time steps per day")
    assert doc == "step_per_day (timestep in config/namelist.config): number of time steps per day"


def test_document_includes_param_name():
    _, doc, _ = _nml_doc("K_GM_max", "oce_dyn", "config/namelist.oce", "max GM diffusivity")
    assert "K_GM_max" in doc


def test_document_includes_group():
    _, doc, _ = _nml_doc("K_GM_max", "oce_dyn", "config/namelist.oce", "max GM diffusivity")
    assert "oce_dyn" in doc


def test_document_includes_description():
    _, doc, _ = _nml_doc("run_length", "timestep", "config/namelist.config", "total run length")
    assert "total run length" in doc


def test_chroma_id_format():
    chroma_id, _, _ = _nml_doc("step_per_day", "timestep", "config/namelist.config", "desc")
    assert chroma_id == "nml_timestep_step_per_day"


def test_chroma_id_is_lowercase():
    chroma_id, _, _ = _nml_doc("K_GM_max", "oce_dyn", "config/namelist.oce", "desc")
    assert chroma_id == chroma_id.lower()


def test_chroma_id_unique_per_group_param():
    id1, _, _ = _nml_doc("alpha", "group_a", "config/namelist.test", "desc")
    id2, _, _ = _nml_doc("alpha", "group_b", "config/namelist.test", "desc")
    assert id1 != id2


def test_metadata_param_name():
    _, _, meta = _nml_doc("step_per_day", "timestep", "config/namelist.config", "desc")
    assert meta["param_name"] == "step_per_day"


def test_metadata_group():
    _, _, meta = _nml_doc("step_per_day", "timestep", "config/namelist.config", "desc")
    assert meta["namelist_group"] == "timestep"


def test_metadata_config_file():
    _, _, meta = _nml_doc("step_per_day", "timestep", "config/namelist.config", "desc")
    assert meta["config_file"] == "config/namelist.config"


def test_empty_description_produces_valid_doc():
    _, doc, _ = _nml_doc("some_param", "some_group", "config/namelist.oce", "")
    assert "some_param" in doc
    assert "some_group" in doc
