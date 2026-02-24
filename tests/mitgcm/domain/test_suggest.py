"""Unit tests for src/domain/suggest.py."""

import pytest

from src.mitgcm.domain.suggest import suggest_experiment_config


def test_rotating_convection():
    """suggest_experiment_config('rotating_convection') should return a dict."""
    result = suggest_experiment_config("rotating_convection")
    assert result is not None


def test_convection_alias():
    """'convection' alias should return the same config as 'rotating_convection'."""
    canonical = suggest_experiment_config("rotating_convection")
    alias = suggest_experiment_config("convection")
    assert alias == canonical


def test_rotating_convection_alias():
    """'rotating convection' (with space) should also resolve correctly."""
    canonical = suggest_experiment_config("rotating_convection")
    alias = suggest_experiment_config("rotating convection")
    assert alias == canonical


def test_eady_alias():
    """'eady' alias should return the baroclinic_instability config."""
    result = suggest_experiment_config("eady")
    assert result is not None
    assert result["experiment_type"] == "baroclinic_instability"


def test_baroclinic_alias():
    """'baroclinic' alias should return the baroclinic_instability config."""
    result = suggest_experiment_config("baroclinic")
    assert result is not None
    assert result["experiment_type"] == "baroclinic_instability"


def test_unknown_returns_none():
    """An unrecognised experiment type should return None."""
    assert suggest_experiment_config("unknown_xyz") is None


def test_has_required_keys():
    """Result should have experiment_type, cpp_options, namelists, notes."""
    result = suggest_experiment_config("rotating_convection")
    for key in ("experiment_type", "cpp_options", "namelists", "notes"):
        assert key in result


def test_cpp_options_is_list():
    """cpp_options should be a list."""
    result = suggest_experiment_config("rotating_convection")
    assert isinstance(result["cpp_options"], list)


def test_notes_is_list():
    """notes should be a list."""
    result = suggest_experiment_config("rotating_convection")
    assert isinstance(result["notes"], list)


def test_rotating_convection_has_nonhydrostatic():
    """rotating_convection cpp_options should include ALLOW_NONHYDROSTATIC."""
    result = suggest_experiment_config("rotating_convection")
    assert "ALLOW_NONHYDROSTATIC" in result["cpp_options"]


def test_baroclinic_instability_namelists():
    """baroclinic_instability should have data and data.eos namelists."""
    result = suggest_experiment_config("baroclinic_instability")
    assert "data" in result["namelists"]
    assert "data.eos" in result["namelists"]


def test_case_insensitive():
    """Lookup should be case-insensitive."""
    result = suggest_experiment_config("ROTATING_CONVECTION")
    assert result is not None
    assert result["experiment_type"] == "rotating_convection"


def test_has_quickstart():
    """Result should include a quickstart key."""
    result = suggest_experiment_config("rotating_convection")
    assert "quickstart" in result


def test_quickstart_has_build_and_run():
    """quickstart should have build and run keys."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "build" in qs
    assert "run" in qs


def test_quickstart_has_dockerfiles():
    """quickstart should include dockerfile_amd64, dockerfile_arm64, and dockerfile_note."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "dockerfile_amd64" in qs
    assert "dockerfile_arm64" in qs
    assert "dockerfile_note" in qs


def test_quickstart_has_directory_structure():
    """quickstart should have a directory_structure dict."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "directory_structure" in qs
    assert isinstance(qs["directory_structure"], dict)


def test_quickstart_build_references_docker_image():
    """Both Dockerfiles should reference the runtime GHCR image."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "ghcr.io/willirath/ogcmcp" in qs["dockerfile_amd64"]
    assert "ghcr.io/willirath/ogcmcp" in qs["dockerfile_arm64"]


def test_quickstart_both_experiment_types():
    """Both experiment types should have a quickstart key."""
    for exp in ("rotating_convection", "baroclinic_instability"):
        result = suggest_experiment_config(exp)
        assert "quickstart" in result, f"{exp} missing quickstart"


def test_quickstart_notes_is_nonempty_list():
    """quickstart.notes should be a non-empty list."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert isinstance(qs["notes"], list)
    assert len(qs["notes"]) > 0


def test_quickstart_directory_structure_has_required_keys():
    """quickstart.directory_structure must include SIZE.h, input/data, data.diagnostics."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    ds = qs["directory_structure"]
    assert "code/SIZE.h" in ds
    assert "input/data" in ds
    assert "input/data.diagnostics" in ds


def test_quickstart_directory_structure_has_dockerfiles():
    """quickstart.directory_structure should include both platform Dockerfile entries."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    ds = qs["directory_structure"]
    assert "Dockerfile.amd64" in ds
    assert "Dockerfile.arm64" in ds


def test_quickstart_directory_structure_has_gen_input():
    """quickstart.directory_structure should include a gen_input.py entry."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "gen_input.py" in qs["directory_structure"]


def test_quickstart_run_references_docker_image():
    """quickstart run command should invoke docker run."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "docker run" in qs["run"]


def test_quickstart_build_uses_optfile():
    """Both Dockerfiles must include -optfile (required by genmake2)."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-optfile" in qs["dockerfile_amd64"]
    assert "-optfile" in qs["dockerfile_arm64"]


def test_quickstart_build_uses_rootdir():
    """Both Dockerfiles must include -rootdir."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-rootdir" in qs["dockerfile_amd64"]
    assert "-rootdir" in qs["dockerfile_arm64"]


def test_quickstart_build_is_docker_build():
    """quickstart build command should be docker build (not a complex docker run)."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "docker build" in qs["build"]


def test_quickstart_no_allow_run_as_root():
    """--allow-run-as-root must not appear in either Dockerfile (MPICH/hydra rejects it)."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "--allow-run-as-root" not in qs["dockerfile_amd64"]
    assert "--allow-run-as-root" not in qs["dockerfile_arm64"]


def test_quickstart_dockerfiles_use_mpi_flag():
    """-mpi flag must appear in genmake2 invocation in both Dockerfiles."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-mpi" in qs["dockerfile_amd64"]
    assert "-mpi" in qs["dockerfile_arm64"]


def test_quickstart_amd64_uses_correct_optfile():
    """AMD64 Dockerfile must use linux_amd64_gfortran optfile."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "linux_amd64_gfortran" in qs["dockerfile_amd64"]


def test_quickstart_arm64_uses_correct_optfile():
    """ARM64 Dockerfile must use linux_arm64_gfortran optfile."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "linux_arm64_gfortran" in qs["dockerfile_arm64"]


def test_quickstart_run_symlinks_input():
    """Both Dockerfiles should symlink input files into the run directory."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "input/*" in qs["dockerfile_amd64"]
    assert "input/*" in qs["dockerfile_arm64"]


def test_quickstart_run_mounts_output():
    """quickstart run command should mount an output directory."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-v" in qs["run"]
    assert "output" in qs["run"]


def test_quickstart_has_build_arm64():
    """quickstart should have a build_arm64 key."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "build_arm64" in qs
    assert "linux/arm64" in qs["build_arm64"]


def test_quickstart_build_amd64_has_platform_flag():
    """amd64 build command must include --platform linux/amd64."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "--platform linux/amd64" in qs["build"]


def test_quickstart_has_run_with_input_mount():
    """quickstart should have run_with_input_mount for namelist iteration."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "run_with_input_mount" in qs
    assert "input" in qs["run_with_input_mount"]
    assert ":ro" in qs["run_with_input_mount"]


def test_quickstart_notes_np_rebuild():
    """quickstart notes must warn that NP>1 requires a rebuild, not just -e NP=..."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    notes_text = " ".join(qs["notes"])
    assert "rebuild" in notes_text.lower() or "SIZE.h" in notes_text


def test_quickstart_notes_mentions_numpy():
    """quickstart notes should mention numpy as a prerequisite."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    notes_text = " ".join(qs["notes"])
    assert "numpy" in notes_text.lower()


def test_rotating_convection_has_exf():
    """rotating_convection cpp_options must include ALLOW_EXF."""
    result = suggest_experiment_config("rotating_convection")
    assert "ALLOW_EXF" in result["cpp_options"]


def test_rotating_convection_has_exf_namelist():
    """rotating_convection namelists should include data.exf."""
    result = suggest_experiment_config("rotating_convection")
    assert "data.exf" in result["namelists"]


def test_rotating_convection_note_no_obcs_for_surface_flux():
    """rotating_convection notes must not attribute surface flux to OBCS."""
    result = suggest_experiment_config("rotating_convection")
    notes_text = " ".join(result["notes"])
    assert "obcs" not in notes_text.lower() or "exf" in notes_text.lower()


def test_quickstart_directory_structure_has_data_exf():
    """quickstart directory_structure should include input/data.exf."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "input/data.exf" in qs["directory_structure"]


def test_dockerfiles_no_redundant_mkdir():
    """Dockerfiles must not contain 'mkdir -p /experiment/run' (WORKDIR creates it)."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "mkdir -p /experiment/run" not in qs["dockerfile_amd64"]
    assert "mkdir -p /experiment/run" not in qs["dockerfile_arm64"]
