"""
Complete RAG Pipeline Orchestration

This module orchestrates the entire RAG pipeline:
0. Table Extraction (extract tables to SQLite)
1. Data Ingest (from parquet)
2. Text Chunking (text only, no tables)
3. Embedding Generation (text embeddings)
4. Vector Store (Qdrant - text only)
5. Query Processing (RAG)
"""

from typing import List, Dict, Optional
from dataclasses import dataclass
import time

# Import pipeline components
from data_ingest import SecDataIngest
from text_chunking import TextChunker
from embeddings import EmbeddingGenerator
from qdrant_store import QdrantVectorStore, SearchResult
from table_extraction import TableExtractor, SQLiteTableStore


@dataclass
class RAGResult:
    """Result from RAG query"""
    query: str
    query_embedding: List[float]
    results: List[SearchResult]
    retrieval_time: float
    num_results: int


class RAGPipeline:
    """Complete RAG pipeline for SEC MDA documents"""

    def __init__(self,
                 parquet_path: str,
                 collection_name: str = "sec-mda",
                 chunk_size: int = 512,
                 chunk_overlap: int = 50,
                 embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2",
                 qdrant_host: str = "localhost",
                 qdrant_port: int = 6333,
                 db_path: str = "sec_tables.db"):
        """
        Initialize RAG pipeline

        Args:
            parquet_path: Path to SEC data parquet file
            collection_name: Qdrant collection name
            chunk_size: Size of text chunks in tokens
            chunk_overlap: Overlap between chunks
            embedding_model: Hugging Face model for embeddings
            qdrant_host: Qdrant server host
            qdrant_port: Qdrant server port
            db_path: Path to SQLite database for tables
        """
        self.parquet_path = parquet_path
        self.collection_name = collection_name
        self.db_path = db_path

        # Initialize pipeline components
        self.ingest = SecDataIngest(parquet_path)
        self.table_extractor = TableExtractor()
        self.table_store = SQLiteTableStore(db_path=db_path)
        self.chunker = TextChunker(chunk_size=chunk_size, overlap=chunk_overlap)
        self.embedder = EmbeddingGenerator(embedding_model)
        self.vector_store = QdrantVectorStore(
            collection_name=collection_name,
            vector_size=self.embedder.embedding_dim,
            qdrant_host=qdrant_host,
            qdrant_port=qdrant_port
        )

        # Pipeline state
        self.documents = None
        self.cleaned_documents = None
        self.extracted_tables = None
        self.chunks = None
        self.embedded_chunks = None
        self.is_initialized = False

    def build(self) -> Dict:
        """
        Build the complete RAG pipeline

        Returns:
            Dictionary with pipeline statistics
        """
        print("\n" + "="*70)
        print("BUILDING RAG PIPELINE FOR SEC MDA DOCUMENTS")
        print("="*70)
        print(f"Parquet file: {self.parquet_path}")
        print(f"Collection: {self.collection_name}")
        print(f"Embedding model: {self.embedder.model_name}")
        print(f"Database file: {self.db_path}")

        start_time = time.time()

        # Step 0: Ingest
        self.documents = self.ingest.run()

        # Step 1: Extract Tables to SQLite
        print("\n" + "="*70)
        print("STEP 0: TABLE EXTRACTION TO SQLITE")
        print("="*70)
        self.cleaned_documents, self.extracted_tables = self.table_extractor.extract_from_documents(
            self.documents
        )

        # Store tables in SQLite
        tables_stored = self.table_store.store_tables(self.extracted_tables)

        # Step 2: Chunk (using cleaned documents without tables)
        self.chunks = self.chunker.run(self.cleaned_documents, strategy='size')

        # Step 3: Embed
        self.embedded_chunks = self.embedder.run(self.chunks)

        # Step 4: Store in Qdrant
        stored = self.vector_store.run(self.embedded_chunks)

        elapsed = time.time() - start_time

        self.is_initialized = True

        # Get table store statistics
        table_stats = self.table_store.get_statistics()

        stats = {
            'documents_ingested': len(self.documents),
            'tables_extracted': len(self.extracted_tables),
            'tables_stored': tables_stored,
            'chunks_created': len(self.chunks),
            'embeddings_generated': len(self.embedded_chunks),
            'vectors_stored': stored,
            'build_time_seconds': elapsed,
            'embedding_dimension': self.embedder.embedding_dim,
            'collection_name': self.collection_name,
            'database_file': self.db_path,
            'table_store_stats': table_stats
        }

        print("\n" + "="*70)
        print("PIPELINE BUILD COMPLETE ✓")
        print("="*70)
        for key, value in stats.items():
            if key != 'table_store_stats':
                print(f"{key}: {value}")

        print("\nTable Store Statistics:")
        for key, value in table_stats.items():
            print(f"  {key}: {value}")

        return stats

    def query(self, query_text: str, top_k: int = 5) -> RAGResult:
        """
        Execute a RAG query

        Args:
            query_text: Query text
            top_k: Number of results to return

        Returns:
            RAGResult with search results
        """
        if not self.is_initialized:
            raise ValueError("Pipeline not initialized. Call build() first.")

        print(f"\nExecuting query: '{query_text}'")

        start_time = time.time()

        # Generate embedding for query
        query_embedding = self.embedder.generate_embedding(query_text)

        # Search vector store
        results = self.vector_store.search(
            query_embedding=query_embedding,
            top_k=top_k
        )

        retrieval_time = time.time() - start_time

        rag_result = RAGResult(
            query=query_text,
            query_embedding=query_embedding,
            results=results,
            retrieval_time=retrieval_time,
            num_results=len(results)
        )

        # Print results
        self._print_results(rag_result)

        return rag_result

    def _print_results(self, result: RAGResult):
        """Pretty print RAG results"""
        print(f"\nResults (Retrieved in {result.retrieval_time:.3f}s):")
        print("-" * 70)

        if not result.results:
            print("No results found.")
            return

        for i, res in enumerate(result.results, 1):
            print(f"\n{i}. Relevance Score: {res.score:.4f}")
            print(f"   Chunk ID: {res.chunk_id}")
            print(f"   Company: {res.metadata.get('company', 'N/A')}")
            print(f"   Filing Date: {res.metadata.get('filing_date', 'N/A')}")
            print(f"   Text Preview: {res.text[:200]}...")

    def batch_query(self, queries: List[str], top_k: int = 5) -> List[RAGResult]:
        """
        Execute multiple queries

        Args:
            queries: List of query texts
            top_k: Number of results per query

        Returns:
            List of RAGResult objects
        """
        print(f"\nExecuting batch of {len(queries)} queries...")
        results = []

        for i, query in enumerate(queries, 1):
            print(f"[{i}/{len(queries)}] {query}")
            result = self.query(query, top_k=top_k)
            results.append(result)

        return results

    def get_pipeline_info(self) -> Dict:
        """
        Get detailed pipeline information

        Returns:
            Dictionary with pipeline information
        """
        info = {
            'pipeline_initialized': self.is_initialized,
            'parquet_path': self.parquet_path,
            'collection_name': self.collection_name,
            'database_file': self.db_path,
            'embedding_model': self.embedder.model_name,
            'embedding_dimension': self.embedder.embedding_dim,
            'chunk_size': self.chunker.chunk_size,
            'chunk_overlap': self.chunker.overlap,
        }

        if self.is_initialized:
            info.update({
                'total_documents': len(self.documents),
                'total_tables_extracted': len(self.extracted_tables),
                'total_chunks': len(self.chunks),
                'total_embeddings': len(self.embedded_chunks),
                'vector_store_stats': self.vector_store.get_collection_stats(),
                'table_store_stats': self.table_store.get_statistics()
            })

        return info

    def query_tables(self, cik: Optional[str] = None,
                    year: Optional[str] = None,
                    limit: int = 100) -> List[Dict]:
        """
        Query extracted tables from SQLite

        Args:
            cik: Filter by CIK
            year: Filter by year
            limit: Maximum number of results

        Returns:
            List of table records
        """
        return self.table_store.query_tables(cik=cik, year=year, limit=limit)


def main():
    """
    Example usage of RAG pipeline with table extraction
    """
    # Configuration
    PARQUET_FILE = "path/to/mda_24_25_merged.parquet"
    COLLECTION_NAME = "sec-mda-filings"
    QDRANT_HOST = "localhost"
    QDRANT_PORT = 6333
    DB_FILE = "sec_tables.db"

    # Initialize pipeline
    rag = RAGPipeline(
        parquet_path=PARQUET_FILE,
        collection_name=COLLECTION_NAME,
        chunk_size=512,
        chunk_overlap=50,
        qdrant_host=QDRANT_HOST,
        qdrant_port=QDRANT_PORT,
        db_path=DB_FILE
    )

    # Build pipeline (includes table extraction)
    build_stats = rag.build()

    # Example text queries
    print("\n" + "="*70)
    print("EXECUTING EXAMPLE TEXT QUERIES (from vector DB)")
    print("="*70)

    text_queries = [
        "What are the main risk factors?",
        "How does the company's revenue compare to competitors?",
        "What market conditions affect operations?"
    ]

    for i, query in enumerate(text_queries[:2], 1):  # Run first 2 as examples
        print(f"\n{'='*70}")
        print(f"Text Query {i}: {query}")
        print('='*70)
        rag.query(query, top_k=3)

    # Example table queries
    print("\n" + "="*70)
    print("QUERYING EXTRACTED TABLES (from SQLite)")
    print("="*70)

    # Query tables by year
    print("\nTables from year 2024:")
    tables = rag.query_tables(year="2024", limit=5)
    for table in tables[:3]:
        print(f"  - {table['table_id']}: {table['table_title']}")

    # Query tables by CIK
    if len(tables) > 0:
        cik = tables[0]['cik']
        print(f"\nTables for CIK {cik}:")
        cik_tables = rag.query_tables(cik=cik, limit=5)
        for table in cik_tables[:3]:
            print(f"  - {table['table_id']}: {table['table_title']}")

    # Print pipeline info
    print("\n" + "="*70)
    print("PIPELINE INFORMATION")
    print("="*70)
    info = rag.get_pipeline_info()
    for key, value in info.items():
        if key not in ['vector_store_stats', 'table_store_stats']:
            print(f"{key}: {value}")

    print("\nVector Store Stats:")
    if 'vector_store_stats' in info:
        for key, value in info['vector_store_stats'].items():
            print(f"  {key}: {value}")

    print("\nTable Store Stats:")
    if 'table_store_stats' in info:
        for key, value in info['table_store_stats'].items():
            print(f"  {key}: {value}")


if __name__ == "__main__":
    main()
