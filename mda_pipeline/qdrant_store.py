"""
Step 5 — Qdrant vector store
============================

Stores ``EmbeddedChunk`` vectors and their payload in Qdrant.

By default we use **Qdrant in local file mode** (``QdrantClient(path=...)``),
which writes a self-contained on-disk database — no server is required.
This is ideal for Google Colab Pro, where you can persist the resulting
folder to Google Drive between sessions.

If a ``url`` is supplied, a remote Qdrant cluster (or Docker server) is
used instead.
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from typing import Dict, List, Optional


@dataclass
class SearchResult:
    chunk_id: str
    text: str
    score: float
    section_name: str = ""
    metadata: Dict = field(default_factory=dict)


class QdrantVectorStore:
    """Lightweight Qdrant wrapper that prefers local-file mode."""

    def __init__(
        self,
        collection_name: str = "sec_mda",
        vector_size: int = 384,
        path: Optional[str] = "./qdrant_db",
        url: Optional[str] = None,
        api_key: Optional[str] = None,
        distance: str = "COSINE",
        recreate: bool = False,
    ):
        self.collection_name = collection_name
        self.vector_size = vector_size
        self.path = path
        self.url = url
        self.api_key = api_key
        self.distance = distance.upper()
        self.recreate = recreate
        self.client = None
        self._in_memory: Dict = {}
        self._connect()

    def _connect(self) -> None:
        try:
            from qdrant_client import QdrantClient
            from qdrant_client.models import Distance, VectorParams

            if self.url:
                print(f"[qdrant] Connecting to {self.url}")
                self.client = QdrantClient(url=self.url, api_key=self.api_key)
            else:
                print(f"[qdrant] Opening local DB at {self.path}")
                self.client = QdrantClient(path=self.path,query_encoder=None)

            existing = {c.name for c in self.client.get_collections().collections}

            if self.recreate and self.collection_name in existing:
                self.client.delete_collection(self.collection_name)
                existing.discard(self.collection_name)

            if self.collection_name not in existing:
                print(f"[qdrant] Creating collection '{self.collection_name}'"
                      f" (size={self.vector_size}, distance={self.distance})")
                self.client.create_collection(
                    collection_name=self.collection_name,
                    vectors_config=VectorParams(
                        size=self.vector_size,
                        distance=Distance[self.distance],
                    ),
                )
            else:
                info = self.client.get_collection(self.collection_name)
                print(f"[qdrant] Reusing collection '{self.collection_name}' "
                      f"with {info.points_count} points")
        except ImportError:
            print("[qdrant] qdrant-client not installed — using in-memory fallback")
            self.client = None

    @staticmethod
    def _payload_for(chunk) -> Dict:
        return {
            "chunk_id": chunk.chunk_id,
            "source_document_id": chunk.source_document_id,
            "text": chunk.text,
            "section_name": chunk.section_name,
            **chunk.metadata,
        }

    def store(self, embedded_chunks: List, batch_size: int = 256) -> int:
        if not embedded_chunks:
            return 0
        if self.client is None:
            for c in embedded_chunks:
                self._in_memory[c.chunk_id] = {
                    "text": c.text,
                    "embedding": c.embedding,
                    "section_name": c.section_name,
                    "metadata": c.metadata,
                }
            print(f"[qdrant] (in-mem) stored {len(embedded_chunks)} vectors")
            return len(embedded_chunks)

        from qdrant_client.models import PointStruct

        total = len(embedded_chunks)
        print(f"[qdrant] Upserting {total:,} vectors into "
              f"'{self.collection_name}' (batch={batch_size})...")
        sent = 0
        for start in range(0, total, batch_size):
            batch = embedded_chunks[start:start + batch_size]
            points = [
                PointStruct(
                    id=str(uuid.uuid5(uuid.NAMESPACE_OID, c.chunk_id)),
                    vector=c.embedding,
                    payload=self._payload_for(c),
                )
                for c in batch
            ]
            self.client.upsert(collection_name=self.collection_name, points=points, wait=False)
            sent += len(points)
            if start == 0 or sent % (batch_size * 5) == 0 or sent == total:
                print(f"[qdrant]   {sent:,}/{total:,}")
        return sent

    store_embeddings = store

    def search(
        self,
        query_embedding: List[float],
        top_k: int = 5,
        score_threshold: Optional[float] = None,
        filter_dict: Optional[Dict] = None,
    ) -> List[SearchResult]:
        if self.client is None:
            return self._search_in_memory(query_embedding, top_k, filter_dict)

        q_filter = None
        if filter_dict:
            from qdrant_client.models import Filter, FieldCondition, MatchValue
            must = [FieldCondition(key=k, match=MatchValue(value=v))
                    for k, v in filter_dict.items()]
            q_filter = Filter(must=must)

        hits = self.client.query(
            collection_name=self.collection_name,
            query_vector=query_embedding,
            limit=top_k,
            score_threshold=score_threshold,
            query_filter=q_filter,
        )
        out = []
        for h in hits:
            p = h.payload or {}
            out.append(SearchResult(
                chunk_id=p.get("chunk_id", str(h.id)),
                text=p.get("text", ""),
                score=h.score,
                section_name=p.get("section_name", ""),
                metadata={k: v for k, v in p.items() if k not in ("chunk_id", "text")},
            ))
        return out

    def _search_in_memory(
        self,
        q: List[float],
        top_k: int,
        filter_dict: Optional[Dict] = None,
    ) -> List[SearchResult]:
        import numpy as np
        qv = np.asarray(q, dtype="float32")
        qv /= np.linalg.norm(qv) + 1e-12
        results = []
        for cid, data in self._in_memory.items():
            if filter_dict:
                md = data.get("metadata", {})
                if any(str(md.get(k)) != str(v) for k, v in filter_dict.items()):
                    continue
            v = np.asarray(data["embedding"], dtype="float32")
            v /= np.linalg.norm(v) + 1e-12
            results.append(SearchResult(
                chunk_id=cid,
                text=data["text"],
                score=float(qv @ v),
                section_name=data.get("section_name", ""),
                metadata=data["metadata"],
            ))
        results.sort(key=lambda r: r.score, reverse=True)
        return results[:top_k]

    def stats(self) -> Dict:
        if self.client is None:
            return {"backend": "in-memory", "count": len(self._in_memory)}
        info = self.client.get_collection(self.collection_name)
        return {
            "backend": self.url or self.path,
            "collection": self.collection_name,
            "vector_size": self.vector_size,
            "distance": self.distance,
            "points": info.points_count,
        }

    def close(self) -> None:
        if self.client is not None:
            try:
                self.client.close()
            except Exception:
                pass
            self.client = None

    def run(self, embedded_chunks: List) -> int:
        print("\n" + "=" * 60)
        print("STEP 5: VECTOR STORE (QDRANT)")
        print("=" * 60)
        n = self.store(embedded_chunks)
        print("\n[qdrant] Stats:")
        for k, v in self.stats().items():
            print(f"  {k}: {v}")
        return n
