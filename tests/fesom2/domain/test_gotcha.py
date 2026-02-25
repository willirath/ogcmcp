"""Tests for src/fesom2/domain/gotcha.py."""

from src.fesom2.domain.gotcha import lookup_gotcha, CATALOGUE


# ── catalogue structure ──────────────────────────────────────────────────────

def test_catalogue_nonempty():
    assert len(CATALOGUE) > 0


def test_each_entry_has_required_keys():
    for entry in CATALOGUE:
        assert "title" in entry
        assert "keywords" in entry
        assert "summary" in entry
        assert "detail" in entry


def test_keywords_are_lowercase():
    for entry in CATALOGUE:
        for kw in entry["keywords"]:
            assert kw == kw.lower(), f"Keyword not lowercase: {kw!r}"


# ── lookup_gotcha ─────────────────────────────────────────────────────────────

def test_unknown_returns_empty():
    assert lookup_gotcha("xyzzy_not_a_real_topic") == []


def test_ale_found():
    results = lookup_gotcha("ALE")
    assert len(results) >= 1
    assert any("ALE" in r["title"] or "ale" in " ".join(r["keywords"]) for r in results)


def test_evp_found():
    results = lookup_gotcha("EVP sea ice")
    assert len(results) >= 1


def test_step_per_day_found():
    results = lookup_gotcha("step_per_day")
    assert len(results) >= 1


def test_metis_found():
    results = lookup_gotcha("METIS partition")
    assert len(results) >= 1


def test_metis_gotcha_mentions_binary_path():
    results = lookup_gotcha("METIS partition")
    metis = next(r for r in results if "METIS" in r["title"])
    assert "/fesom2/bin/fesom_meshpart" in metis["detail"]


def test_node_element_found():
    results = lookup_gotcha("node element")
    assert len(results) >= 1


def test_forcing_interpolation_found():
    results = lookup_gotcha("forcing interpolation weights")
    assert len(results) >= 1


def test_output_namelist_found():
    results = lookup_gotcha("namelist.io output")
    assert len(results) >= 1


def test_cavity_found():
    results = lookup_gotcha("ice shelf cavity")
    assert len(results) >= 1


def test_toy_ocean_found():
    results = lookup_gotcha("toy_ocean")
    assert len(results) >= 1


def test_toy_ocean_neverworld2_keyword():
    results = lookup_gotcha("neverworld2 windstress")
    assert len(results) >= 1


def test_toy_module_variables_found():
    results = lookup_gotcha("do_wind")
    assert len(results) >= 1


def test_toy_module_variables_compile_time():
    results = lookup_gotcha("compile time module variable")
    assert len(results) >= 1


def test_case_insensitive():
    lower = lookup_gotcha("ale vertical coordinate")
    upper = lookup_gotcha("ALE Vertical Coordinate")
    assert lower == upper


def test_results_have_all_keys():
    results = lookup_gotcha("ALE")
    for r in results:
        assert "title" in r
        assert "keywords" in r
        assert "summary" in r
        assert "detail" in r


def test_summary_is_nonempty():
    for entry in CATALOGUE:
        assert entry["summary"].strip()


def test_detail_is_nonempty():
    for entry in CATALOGUE:
        assert entry["detail"].strip()
