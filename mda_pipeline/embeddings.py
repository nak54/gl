"""
Step 4 — Embedding generation
=============================

Uses ``sentence-transformers`` (default: ``all-MiniLM-L6-v2``, dim=384)
to embed cleaned-text chunks.  Section metadata
(``TextChunk.section_name``) is preserved on every ``EmbeddedChunk``.

In Google Colab Pro this will automatically use the GPU if one is
attached, but works on CPU as well.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional

import numpy as np


@dataclass
class EmbeddedChunk:
    """A ``TextChunk`` plus its vector representation."""
    chunk_id: str
    source_document_id: str
    text: str
    embedding: List[float]
    embedding_model: str
    section_name: str
    metadata: Dict = field(default_factory=dict)


class EmbeddingGenerator:
    """Wraps a ``SentenceTransformer`` model with batching + progress."""

    def __init__(
        self,
        model_name: str = "sentence-transformers/all-MiniLM-L6-v2",
        device: Optional[str] = None,
        normalize_embeddings: bool = True,
    ):
        """
        Args:
            model_name: HF model id for sentence-transformers.
            device: ``"cuda"``, ``"cpu"``, or ``None`` to auto-detect.
            normalize_embeddings: Whether to L2-normalise output vectors.
                Cosine similarity becomes a dot product when on, which is
                how we configure Qdrant.
        """
        self.model_name = model_name
        self.device = device
        self.normalize = normalize_embeddings
        self.model = None
        self.embedding_dim: Optional[int] = None
        self.embeddings: List[EmbeddedChunk] = []
        self._load()

    # ------------------------------------------------------------------
    def _load(self) -> None:
        try:
            from sentence_transformers import SentenceTransformer
            import torch

            if self.device is None:
                self.device = "cuda" if torch.cuda.is_available() else "cpu"
            print(f"[embed] Loading {self.model_name} on {self.device}")
            self.model = SentenceTransformer(self.model_name, device=self.device)
            test_vec = self.model.encode("hello world", normalize_embeddings=self.normalize)
            self.embedding_dim = int(len(test_vec))
            print(f"[embed] Model ready, dim={self.embedding_dim}")
        except ImportError:
            print("[embed] sentence-transformers not installed — using random "
                  "fallback embeddings (NOT for production)")
            self.model = None
            self.embedding_dim = 384

    # ------------------------------------------------------------------
    def encode_one(self, text: str) -> List[float]:
        if self.model is None:
            rng = np.random.default_rng(abs(hash(text)) % (2 ** 32))
            v = rng.standard_normal(self.embedding_dim).astype("float32")
            if self.normalize:
                v /= np.linalg.norm(v) + 1e-12
            return v.tolist()
        v = self.model.encode(text, normalize_embeddings=self.normalize, convert_to_numpy=True)
        return v.tolist()

    # ------------------------------------------------------------------
    def embed_chunks(self, chunks: List, batch_size: int = 64) -> List[EmbeddedChunk]:
        """Embed a list of ``TextChunk``s in batches."""
        out: List[EmbeddedChunk] = []
        total = len(chunks)
        print(f"[embed] Embedding {total:,} chunks (batch_size={batch_size})...")

        for start in range(0, total, batch_size):
            batch = chunks[start:start + batch_size]
            texts = [c.text for c in batch]

            if self.model is not None:
                vecs = self.model.encode(
                    texts,
                    batch_size=batch_size,
                    show_progress_bar=False,
                    normalize_embeddings=self.normalize,
                    convert_to_numpy=True,
                )
            else:
                vecs = np.stack([np.array(self.encode_one(t)) for t in texts])

            for chunk, vec in zip(batch, vecs):
                out.append(EmbeddedChunk(
                    chunk_id=chunk.chunk_id,
                    source_document_id=chunk.source_document_id,
                    text=chunk.text,
                    embedding=vec.tolist(),
                    embedding_model=self.model_name,
                    section_name=chunk.section_name,
                    metadata={
                        "chunk_index": chunk.chunk_index,
                        "total_chunks": chunk.total_chunks,
                        "start_char": chunk.start_char,
                        "end_char": chunk.end_char,
                        "section_name": chunk.section_name,
                        **chunk.metadata,
                    },
                ))

            done = min(start + batch_size, total)
            if start == 0 or done % (batch_size * 10) == 0 or done == total:
                print(f"[embed]   {done:,}/{total:,}")

        self.embeddings = out
        return out

    # ------------------------------------------------------------------
    def verify(self) -> bool:
        if not self.embeddings:
            return False
        dims = {len(e.embedding) for e in self.embeddings}
        if len(dims) != 1:
            print(f"[embed] FAIL — inconsistent dims: {dims}")
            return False
        bad = 0
        for e in self.embeddings:
            for v in e.embedding:
                if not (-1e6 < v < 1e6):
                    bad += 1
                    break
        if bad:
            print(f"[embed] WARN — {bad} embeddings contain out-of-range values")
        return bad == 0

    def statistics(self) -> Dict:
        if not self.embeddings:
            return {}
        arr = np.asarray([e.embedding for e in self.embeddings], dtype="float32")
        norms = np.linalg.norm(arr, axis=1)
        return {
            "count": len(self.embeddings),
            "dim": self.embedding_dim,
            "model": self.model_name,
            "norm_mean": float(norms.mean()),
            "norm_min":  float(norms.min()),
            "norm_max":  float(norms.max()),
            "values_mean": float(arr.mean()),
            "values_std":  float(arr.std()),
        }

    def run(self, chunks: List, batch_size: int = 64) -> List[EmbeddedChunk]:
        print("\n" + "=" * 60)
        print("STEP 4: EMBEDDING GENERATION")
        print("=" * 60)
        out = self.embed_chunks(chunks, batch_size=batch_size)
        self.verify()
        print("\n[embed] Statistics:")
        for k, v in self.statistics().items():
            print(f"  {k}: {v}")
        return out


if __name__ == "__main__":
    from .text_chunking import TextChunker  # pragma: no cover

    chunker = TextChunker(chunk_size_tokens=64, overlap_percent=15)
    fake = type("D", (), dict(
        document_id="D1", accession="A", year="2024", cik="0", name="X",
        sic="", ein="", address="", naics3="", naics17Title="",
        is_ambiguous_naics3=False, mda_text="Hello world. " * 50, metadata={}))()
    chunks = chunker.run([fake])
    embedder = EmbeddingGenerator()
    embeds = embedder.run(chunks)
    print("Got", len(embeds), "embeddings of dim", embedder.embedding_dim)
