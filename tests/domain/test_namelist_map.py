"""Unit tests for src/domain/namelist_map.py."""

from src.domain.namelist_map import get_namelist_structure, _derive_file, _generic_description


def test_returns_dict():
    """get_namelist_structure() should return a dict."""
    result = get_namelist_structure()
    assert isinstance(result, dict)


def test_core_files_present():
    """Core namelist files must always be present (explicit entries)."""
    result = get_namelist_structure()
    for file in ("data", "eedata", "data.pkg", "data.eos", "data.diagnostics"):
        assert file in result, f"{file} missing from structure"


def test_data_has_parm01_to_parm05():
    """data file must contain PARM01 through PARM05."""
    groups = get_namelist_structure()["data"]
    for g in ("PARM01", "PARM02", "PARM03", "PARM04", "PARM05"):
        assert g in groups, f"{g} missing from data"


def test_eedata_has_eeparms():
    """eedata must contain EEPARMS."""
    assert "EEPARMS" in get_namelist_structure()["eedata"]


def test_data_pkg_has_packages():
    """data.pkg must contain PACKAGES."""
    assert "PACKAGES" in get_namelist_structure()["data.pkg"]


def test_data_eos_has_eos_parm01():
    """data.eos must contain EOS_PARM01."""
    assert "EOS_PARM01" in get_namelist_structure()["data.eos"]


def test_data_exf_has_exf_groups():
    """data.exf must contain EXF_NML_01 through EXF_NML_04."""
    groups = get_namelist_structure()["data.exf"]
    for g in ("EXF_NML_01", "EXF_NML_02", "EXF_NML_03", "EXF_NML_04"):
        assert g in groups, f"{g} missing from data.exf"


def test_descriptions_are_nonempty_strings():
    """Every description in the map must be a non-empty string."""
    result = get_namelist_structure()
    for file, groups in result.items():
        for group, desc in groups.items():
            assert isinstance(desc, str) and desc, f"{file}/{group} has empty description"


def test_sorted_by_file():
    """Keys should be sorted alphabetically."""
    result = get_namelist_structure()
    keys = list(result.keys())
    assert keys == sorted(keys)


def test_db_derived_entries_present():
    """Groups from DuckDB index should appear when the DB is available."""
    result = get_namelist_structure()
    # OBCS, KPP, and RBCS are common packages; they should be present
    all_groups = {g for groups in result.values() for g in groups}
    for g in ("OBCS_PARM01", "KPP_PARM01", "RBCS_PARM01"):
        assert g in all_groups, f"{g} missing (DB-derived entry expected)"


def test_derive_file_basic():
    """_derive_file strips common suffixes and lowercases."""
    assert _derive_file("OBCS_PARM01") == "data.obcs"
    assert _derive_file("KPP_PARM01") == "data.kpp"
    assert _derive_file("RBCS_PARM02") == "data.rbcs"
    assert _derive_file("GGL90_PARM01") == "data.ggl90"
    assert _derive_file("SEAICE_PARM01") == "data.seaice"


def test_derive_file_nml_suffix():
    """_derive_file handles _NML and _NML_NN suffixes."""
    assert _derive_file("FLT_NML") == "data.flt"
    assert _derive_file("CAL_NML") == "data.cal"


def test_derive_file_forcing_const():
    """_derive_file handles _FORCING and _CONST suffixes."""
    assert _derive_file("BLING_FORCING") == "data.bling"
    assert _derive_file("BULKF_CONST") == "data.bulkf"


def test_generic_description_nonempty():
    """_generic_description should return a non-empty string for any group."""
    for group in ("OBCS_PARM01", "BLING_FORCING", "THSICE_CONST", "FLT_NML"):
        desc = _generic_description(group)
        assert isinstance(desc, str) and desc


def test_parm01_description_mentions_viscosity():
    """PARM01 description should mention viscosity."""
    desc = get_namelist_structure()["data"]["PARM01"]
    assert "visc" in desc.lower()


def test_eeparms_description_mentions_mpi():
    """EEPARMS description should mention MPI."""
    desc = get_namelist_structure()["eedata"]["EEPARMS"]
    assert "mpi" in desc.lower() or "MPI" in desc


def test_no_duplicate_groups():
    """Each group name should appear in at most one file."""
    result = get_namelist_structure()
    seen = {}
    for file, groups in result.items():
        for group in groups:
            assert group not in seen, (
                f"Group {group} appears in both {seen[group]} and {file}"
            )
            seen[group] = file


def test_gmredi_explicit():
    """GM_PARM01 should be in data.gmredi (non-obvious name)."""
    result = get_namelist_structure()
    assert "data.gmredi" in result
    assert "GM_PARM01" in result["data.gmredi"]


def test_exch2_explicit():
    """W2_EXCH2_PARM01 should be in data.exch2."""
    result = get_namelist_structure()
    assert "data.exch2" in result
    assert "W2_EXCH2_PARM01" in result["data.exch2"]


def test_diagnostics_explicit():
    """data.diagnostics must have DIAGNOSTICS_LIST and DIAG_STATIS_PARMS."""
    result = get_namelist_structure()
    assert "data.diagnostics" in result
    groups = result["data.diagnostics"]
    assert "DIAGNOSTICS_LIST" in groups
    assert "DIAG_STATIS_PARMS" in groups
    assert "numDiags" in groups["DIAGNOSTICS_LIST"]


def test_obcs_description_mentions_boundaries():
    """OBCS_PARM01 description should mention open boundary conditions."""
    result = get_namelist_structure()
    desc = result["data.obcs"]["OBCS_PARM01"]
    assert "boundary" in desc.lower() or "open" in desc.lower()


def test_rbcs_description_mentions_taurelax():
    """RBCS_PARM01 description should mention tauRelaxT."""
    result = get_namelist_structure()
    desc = result["data.rbcs"]["RBCS_PARM01"]
    assert "tauRelax" in desc
