"""Embedding pipeline for MITgcm verification experiment configuration files.

Indexes input/data*, input/eedata, code/*.h, and code/packages.conf from all
experiments under MITgcm/verification/ into the mitgcm_verification ChromaDB
collection.
"""

import json
import logging
import time
from pathlib import Path

import ollama

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)
logging.getLogger("httpx").setLevel(logging.WARNING)
logging.getLogger("httpcore").setLevel(logging.WARNING)

from src.embed_utils import BATCH_SIZE, EMBED_MODEL, MAX_CHARS, OVERLAP, _chunk_text
from src.mitgcm.embedder.store import CHROMA_PATH, get_verification_collection
from src.mitgcm.verification_indexer.catalogue import build_catalogue

CATALOGUE_PATH = Path("data/mitgcm/verification_catalogue.json")

MITGCM = Path("MITgcm")
EXPERIMENT_DIRS = [MITGCM / "verification"]

# Binary / generated suffixes to skip
_SKIP_SUFFIXES = {".bin", ".nc", ".data", ".meta", ".gz", ".tar", ".png", ".pdf"}


def _experiment_files(exp_dir: Path):
    """Yield (label, text) for all indexable text files in an experiment.

    label is a short human-readable identifier used as the document header:
    "<experiment_name>/<subdir>/<filename>"
    """
    exp_name = exp_dir.name

    # input/ — namelists and eedata
    input_dir = exp_dir / "input"
    if input_dir.exists():
        for p in sorted(input_dir.iterdir()):
            if not p.is_file():
                continue
            if p.suffix.lower() in _SKIP_SUFFIXES:
                continue
            # Only index known namelist/text files; skip binary blobs heuristically
            try:
                text = p.read_text(errors="strict")
            except (UnicodeDecodeError, PermissionError):
                continue
            yield f"verification/{exp_name}/input/{p.name}", text

    # code/ — headers and packages.conf
    code_dir = exp_dir / "code"
    if code_dir.exists():
        for p in sorted(code_dir.iterdir()):
            if not p.is_file():
                continue
            if p.suffix.lower() in _SKIP_SUFFIXES:
                continue
            if p.suffix not in (".h", ".conf", ".F", ".F90", ".f90") and p.name != "packages.conf":
                continue
            try:
                text = p.read_text(errors="strict")
            except (UnicodeDecodeError, PermissionError):
                continue
            yield f"verification/{exp_name}/code/{p.name}", text


def run(chroma_path: Path = CHROMA_PATH) -> None:
    collection = get_verification_collection(chroma_path)

    all_chunks: list[tuple[str, str, dict]] = []

    for base_dir in EXPERIMENT_DIRS:
        if not base_dir.exists():
            log.warning(f"{base_dir} not found — skipping")
            continue
        for exp_dir in sorted(base_dir.iterdir()):
            if not exp_dir.is_dir() or exp_dir.name == "README.md":
                continue
            exp_name = exp_dir.name
            for label, text in _experiment_files(exp_dir):
                header = f"[{label}]\n"
                for i, chunk in enumerate(_chunk_text(text, MAX_CHARS, OVERLAP)):
                    chunk_id = f"vrf_{exp_name}_{Path(label).name}_{i}"
                    all_chunks.append((
                        chunk_id,
                        header + chunk,
                        {
                            "experiment": exp_name,
                            "file": label,
                            "filename": Path(label).name,
                            "chunk_index": i,
                        },
                    ))

    total = len(all_chunks)
    log.info(f"Embedding {total} chunks from verification experiments...")

    for i in range(0, total, BATCH_SIZE):
        batch = all_chunks[i : i + BATCH_SIZE]
        ids = [c[0] for c in batch]
        docs = [c[1] for c in batch]
        metas = [c[2] for c in batch]

        try:
            embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
        except Exception as e:
            log.warning(f"batch {i // BATCH_SIZE} failed ({e}), retrying in 10s")
            time.sleep(10)
            try:
                embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
            except Exception as e2:
                log.warning(f"batch still failing ({e2}), falling back to one-at-a-time")
                keep: list[tuple] = []
                for chunk_id, d, meta in zip(ids, docs, metas):
                    for attempt in range(3):
                        try:
                            emb = ollama.embed(model=EMBED_MODEL, input=[d])["embeddings"][0]
                            keep.append((chunk_id, emb, d, meta))
                            break
                        except Exception as e3:
                            if "context length" in str(e3):
                                mid = len(d) // 2
                                log.warning(f"splitting {chunk_id} ({len(d)} chars) in two")
                                for suffix, half in (("_a", d[:mid]), ("_b", d[mid:])):
                                    try:
                                        emb = ollama.embed(model=EMBED_MODEL, input=[half])["embeddings"][0]
                                        keep.append((chunk_id + suffix, emb, half, meta))
                                    except Exception:
                                        log.warning(f"skipping {chunk_id}{suffix} after split")
                                break
                            if attempt == 2:
                                raise
                            time.sleep(10)
                if not keep:
                    continue
                ids, embeddings, docs, metas = zip(*keep)
                ids, embeddings, docs, metas = list(ids), list(embeddings), list(docs), list(metas)

        collection.upsert(ids=ids, embeddings=embeddings, documents=docs, metadatas=metas)
        done = min(i + BATCH_SIZE, total)
        if done % 100 == 0 or done == total:
            log.info(f"  Embedded {done}/{total}")

    log.info(f"Done. {collection.count()} chunks in mitgcm_verification collection.")

    # Save pre-built catalogue so list_verification_experiments_tool works
    # in the MCP image (which does not contain MITgcm/verification/).
    catalogue = build_catalogue()
    CATALOGUE_PATH.parent.mkdir(parents=True, exist_ok=True)
    CATALOGUE_PATH.write_text(json.dumps(catalogue, indent=2))
    log.info(f"Saved {len(catalogue)} experiment records to {CATALOGUE_PATH}")


if __name__ == "__main__":
    run()
