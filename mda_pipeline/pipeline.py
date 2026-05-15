"""
End-to-end pipeline orchestrator
================================

Ties every stage together::

    parquet -> SECDocument
            -> TableExtractor -> SQLite
            -> cleaned text  -> TextChunker (15% overlap, MD&A aware)
            -> EmbeddingGenerator
            -> QdrantVectorStore

The ``RAGPipeline.build()`` method runs all stages.  ``query()`` runs a
RAG search over Qdrant; ``query_tables()`` runs SQL filters over the
SQLite database.
"""

from __future__ import annotations

import time
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from .data_ingest import SecDataIngest
from .table_extraction import TableExtractor, SQLiteTableStore
from .text_chunking import TextChunker
from .embeddings import EmbeddingGenerator
from .qdrant_store import QdrantVectorStore, SearchResult


@dataclass
class RAGResult:
    query: str
    results: List[SearchResult]
    retrieval_time: float
    num_results: int
    query_embedding: List[float] = field(default_factory=list)


class RAGPipeline:
    """Single object that owns every stage of the pipeline."""

    def __init__(
        self,
        parquet_path: str,
        *,
        collection_name: str = "sec_mda",
        chunk_size_tokens: int = 512,
        overlap_percent: int = 15,
        embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2",
        qdrant_path: Optional[str] = "./qdrant_db",
        qdrant_url: Optional[str] = None,
        qdrant_api_key: Optional[str] = None,
        sqlite_path: str = "sec_tables.db",
        recreate_collection: bool = False,
        ingest_limit: Optional[int] = None,
        embedding_batch_size: int = 64,
        qdrant_batch_size: int = 256,
        device: Optional[str] = None,
    ):
        self.parquet_path = parquet_path
        self.collection_name = collection_name
        self.sqlite_path = sqlite_path
        self.embedding_batch_size = embedding_batch_size
        self.qdrant_batch_size = qdrant_batch_size

        # Pipeline stages
        self.ingest = SecDataIngest(parquet_path, limit=ingest_limit)
        self.table_extractor = TableExtractor()
        self.table_store = SQLiteTableStore(sqlite_path)
        self.chunker = TextChunker(
            chunk_size_tokens=chunk_size_tokens,
            overlap_percent=overlap_percent,
        )
        self.embedder = EmbeddingGenerator(embedding_model, device=device)
        self.vector_store = QdrantVectorStore(
            collection_name=collection_name,
            vector_size=self.embedder.embedding_dim or 384,
            path=qdrant_path,
            url=qdrant_url,
            api_key=qdrant_api_key,
            recreate=recreate_collection,
        )

        # State
        self.documents: List = []
        self.cleaned_documents: List = []
        self.extracted_tables: List = []
        self.chunks: List = []
        self.embedded_chunks: List = []
        self.is_built = False

    # ------------------------------------------------------------------
    def build(self) -> Dict:
        print("\n" + "=" * 70)
        print(" BUILDING SEC MD&A PIPELINE ".center(70, "="))
        print("=" * 70)
        print(f"parquet         : {self.parquet_path}")
        print(f"qdrant          : {self.vector_store.path or self.vector_store.url}")
        print(f"sqlite          : {self.sqlite_path}")
        print(f"embedding model : {self.embedder.model_name}")
        print(f"chunk size      : {self.chunker.chunk_size} chars "
              f"({self.chunker.chunk_size_tokens} tokens)")
        print(f"overlap         : {self.chunker.overlap_percent}% "
              f"({self.chunker.overlap} chars)")

        t0 = time.time()

        # 1) Ingest
        self.documents = self.ingest.run()

        # 2) Table extraction
        print("\n" + "=" * 60)
        print("STEP 2: TABLE EXTRACTION -> SQLite")
        print("=" * 60)
        self.cleaned_documents, self.extracted_tables = (
            self.table_extractor.extract_from_documents(self.documents)
        )
        tables_stored = self.table_store.store_tables(self.extracted_tables)
        print("[tables] SQLite stats:")
        for k, v in self.table_store.statistics().items():
            print(f"  {k}: {v}")

        # 3) Chunking
        self.chunks = self.chunker.run(self.cleaned_documents, strategy="mda")

        # 4) Embedding
        self.embedded_chunks = self.embedder.run(
            self.chunks, batch_size=self.embedding_batch_size,
        )

        # 5) Qdrant
        vectors_stored = self.vector_store.run(self.embedded_chunks)

        elapsed = time.time() - t0
        self.is_built = True

        stats = {
            "documents_ingested": len(self.documents),
            "tables_extracted": len(self.extracted_tables),
            "tables_stored": tables_stored,
            "chunks": len(self.chunks),
            "embeddings": len(self.embedded_chunks),
            "vectors_stored": vectors_stored,
            "embedding_dim": self.embedder.embedding_dim,
            "elapsed_seconds": round(elapsed, 2),
        }

        print("\n" + "=" * 70)
        print(" PIPELINE BUILD COMPLETE ".center(70, "="))
        print("=" * 70)
        for k, v in stats.items():
            print(f"  {k}: {v}")
        return stats

    # ------------------------------------------------------------------
    def query(
        self,
        query_text: str,
        *,
        top_k: int = 5,
        filter_dict: Optional[Dict] = None,
        verbose: bool = True,
    ) -> RAGResult:
        if not self.is_built:
            raise RuntimeError("Call build() before query().")

        t0 = time.time()
        q_vec = self.embedder.encode_one(query_text)
        results = self.vector_store.search(
            query_embedding=q_vec, top_k=top_k, filter_dict=filter_dict,
        )
        elapsed = time.time() - t0

        rag = RAGResult(
            query=query_text,
            query_embedding=q_vec,
            results=results,
            retrieval_time=elapsed,
            num_results=len(results),
        )
        if verbose:
            self._print(rag)
        return rag

    def _print(self, r: RAGResult) -> None:
        print(f"\nQuery: {r.query!r}  ({r.retrieval_time*1000:.1f} ms)")
        print("-" * 70)
        if not r.results:
            print("  (no results)")
            return
        for i, hit in enumerate(r.results, 1):
            preview = hit.text[:200].replace("\n", " ")
            print(f"{i}. score={hit.score:.4f}  section={hit.section_name!r}")
            print(f"   {hit.chunk_id}")
            print(f"   company={hit.metadata.get('company','?')}, "
                  f"year={hit.metadata.get('year','?')}, "
                  f"naics3={hit.metadata.get('naics3','?')}")
            print(f"   text: {preview}...")

    def batch_query(self, queries: List[str], top_k: int = 5) -> List[RAGResult]:
        return [self.query(q, top_k=top_k) for q in queries]

    # ------------------------------------------------------------------
    def query_tables(
        self,
        cik: Optional[str] = None,
        year: Optional[str] = None,
        naics3: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict]:
        return self.table_store.query_tables(
            cik=cik, year=year, naics3=naics3, limit=limit,
        )

    def info(self) -> Dict:
        return {
            "is_built": self.is_built,
            "parquet": self.parquet_path,
            "collection": self.collection_name,
            "sqlite_path": self.sqlite_path,
            "embedding_model": self.embedder.model_name,
            "embedding_dim": self.embedder.embedding_dim,
            "chunk_size_chars": self.chunker.chunk_size,
            "overlap_percent": self.chunker.overlap_percent,
            "vector_store": self.vector_store.stats(),
            "sqlite_stats": self.table_store.statistics(),
        }

    def close(self) -> None:
        self.vector_store.close()
        self.table_store.close()
