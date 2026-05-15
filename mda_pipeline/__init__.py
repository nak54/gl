"""
mda_pipeline
============

End-to-end pipeline for SEC MD&A (Item 7) filings:

    Documents
        |
    Extract Tables  ->  SQLite rows
        |
    Cleaned Text (tables removed)
        |
    Chunk (SEC MD&A structure aware, 15% overlap)
        |
    Embed (sentence-transformers)
        |
    Qdrant (text-only vectors)

Designed to run in Google Colab Pro: Qdrant runs in *local file* mode
(no server required) and all heavy work is batched.

Public API:
    from mda_pipeline import (
        SecDataIngest, TableExtractor, SQLiteTableStore,
        TextChunker, EmbeddingGenerator, QdrantVectorStore,
        RAGPipeline,
    )
"""

from .data_ingest import SecDataIngest, SECDocument
from .table_extraction import TableExtractor, SQLiteTableStore, ExtractedTable
from .text_chunking import TextChunker, TextChunk
from .embeddings import EmbeddingGenerator, EmbeddedChunk
from .qdrant_store import QdrantVectorStore, SearchResult
from .pipeline import RAGPipeline, RAGResult

__all__ = [
    "SecDataIngest",
    "SECDocument",
    "TableExtractor",
    "SQLiteTableStore",
    "ExtractedTable",
    "TextChunker",
    "TextChunk",
    "EmbeddingGenerator",
    "EmbeddedChunk",
    "QdrantVectorStore",
    "SearchResult",
    "RAGPipeline",
    "RAGResult",
]

__version__ = "1.0.0"
