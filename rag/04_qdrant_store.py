"""
Qdrant Vector Store Module

This module handles storing embeddings in Qdrant vector database.
Step 4: Vector Store Management (Qdrant)
"""

from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass
import uuid


@dataclass
class SearchResult:
    """Result from vector similarity search"""
    chunk_id: str
    text: str
    score: float
    metadata: Dict


class QdrantVectorStore:
    """Manages embeddings in Qdrant vector database"""

    def __init__(self, collection_name: str = "sec-mda",
                 vector_size: int = 384,
                 qdrant_host: str = "localhost",
                 qdrant_port: int = 6333,
                 prefer_grpc: bool = True):
        """
        Initialize Qdrant vector store

        Args:
            collection_name: Name of the collection to use/create
            vector_size: Dimension of embeddings
            qdrant_host: Qdrant server host
            qdrant_port: Qdrant server port
            prefer_grpc: Use gRPC for faster communication
        """
        self.collection_name = collection_name
        self.vector_size = vector_size
        self.host = qdrant_host
        self.port = qdrant_port
        self.prefer_grpc = prefer_grpc
        self.client = None
        self.point_count = 0
        self._initialize_client()

    def _initialize_client(self):
        """Initialize Qdrant client"""
        try:
            from qdrant_client import QdrantClient
            from qdrant_client.models import Distance, VectorParams

            print(f"Connecting to Qdrant at {self.host}:{self.port}...")

            # Create client
            if self.prefer_grpc:
                self.client = QdrantClient(
                    host=self.host,
                    port=self.port,
                    prefer_grpc=True
                )
            else:
                self.client = QdrantClient(
                    host=self.host,
                    port=self.port
                )

            print("Successfully connected to Qdrant ✓")

            # Check if collection exists, if not create it
            self._ensure_collection()

        except ImportError:
            print("Warning: qdrant-client not installed. Using in-memory storage.")
            self.client = None
            self.in_memory_store = {}

    def _ensure_collection(self):
        """Create collection if it doesn't exist"""
        from qdrant_client.models import Distance, VectorParams

        try:
            # Try to get collection info
            collection_info = self.client.get_collection(self.collection_name)
            print(f"Collection '{self.collection_name}' already exists")
            print(f"  Vectors: {collection_info.points_count}")
            self.point_count = collection_info.points_count
        except Exception:
            # Collection doesn't exist, create it
            print(f"Creating collection '{self.collection_name}'...")
            self.client.create_collection(
                collection_name=self.collection_name,
                vectors_config=VectorParams(
                    size=self.vector_size,
                    distance=Distance.COSINE
                )
            )
            print(f"Collection created ✓")
            self.point_count = 0

    def store_embeddings(self, embedded_chunks: List,
                        batch_size: int = 100,
                        skip_existing: bool = False) -> int:
        """
        Store embeddings in Qdrant

        Args:
            embedded_chunks: List of EmbeddedChunk objects
            batch_size: Number of vectors to insert per batch
            skip_existing: Skip duplicate chunk IDs

        Returns:
            Number of vectors stored
        """
        if self.client is None:
            return self._store_in_memory(embedded_chunks)

        from qdrant_client.models import PointStruct

        print(f"Storing {len(embedded_chunks)} embeddings in Qdrant...")

        stored_count = 0
        points_to_insert = []

        for i, chunk in enumerate(embedded_chunks):
            # Create unique numeric ID for this point
            point_id = hash(chunk.chunk_id) % (2**31)

            point = PointStruct(
                id=point_id,
                vector=chunk.embedding,
                payload={
                    'chunk_id': chunk.chunk_id,
                    'source_document_id': chunk.source_document_id,
                    'text': chunk.text,
                    **chunk.metadata
                }
            )
            points_to_insert.append(point)

            # Insert in batches
            if len(points_to_insert) >= batch_size or i == len(embedded_chunks) - 1:
                try:
                    self.client.upsert(
                        collection_name=self.collection_name,
                        points=points_to_insert
                    )
                    stored_count += len(points_to_insert)
                    print(f"  Stored {stored_count}/{len(embedded_chunks)} vectors")
                    points_to_insert = []
                except Exception as e:
                    print(f"Error storing batch: {e}")
                    raise

        self.point_count = stored_count
        print(f"Successfully stored {stored_count} vectors ✓")
        return stored_count

    def _store_in_memory(self, embedded_chunks: List) -> int:
        """Fallback in-memory storage"""
        for chunk in embedded_chunks:
            self.in_memory_store[chunk.chunk_id] = {
                'text': chunk.text,
                'embedding': chunk.embedding,
                'metadata': chunk.metadata
            }
        return len(embedded_chunks)

    def search(self, query_embedding: List[float],
              top_k: int = 5,
              score_threshold: Optional[float] = None) -> List[SearchResult]:
        """
        Search for similar vectors

        Args:
            query_embedding: Query vector
            top_k: Number of results to return
            score_threshold: Minimum similarity score (0-1 for cosine)

        Returns:
            List of SearchResult objects
        """
        if self.client is None:
            return self._search_in_memory(query_embedding, top_k)

        results = self.client.search(
            collection_name=self.collection_name,
            query_vector=query_embedding,
            limit=top_k,
            score_threshold=score_threshold
        )

        search_results = []
        for result in results:
            search_result = SearchResult(
                chunk_id=result.payload.get('chunk_id'),
                text=result.payload.get('text'),
                score=result.score,
                metadata={
                    k: v for k, v in result.payload.items()
                    if k not in ['chunk_id', 'text']
                }
            )
            search_results.append(search_result)

        return search_results

    def _search_in_memory(self, query_embedding: List[float],
                         top_k: int) -> List[SearchResult]:
        """Fallback in-memory search using cosine similarity"""
        import numpy as np

        results = []
        query_vec = np.array(query_embedding)

        for chunk_id, data in self.in_memory_store.items():
            doc_vec = np.array(data['embedding'])
            # Cosine similarity
            score = np.dot(query_vec, doc_vec) / (
                np.linalg.norm(query_vec) * np.linalg.norm(doc_vec)
            )

            results.append(SearchResult(
                chunk_id=chunk_id,
                text=data['text'],
                score=score,
                metadata=data['metadata']
            ))

        # Sort by score descending
        results.sort(key=lambda x: x.score, reverse=True)
        return results[:top_k]

    def delete_collection(self):
        """Delete the entire collection"""
        if self.client is None:
            self.in_memory_store = {}
            return

        try:
            self.client.delete_collection(self.collection_name)
            print(f"Deleted collection '{self.collection_name}' ✓")
            self.point_count = 0
        except Exception as e:
            print(f"Error deleting collection: {e}")

    def get_collection_stats(self) -> Dict:
        """
        Get collection statistics

        Returns:
            Dictionary with collection stats
        """
        if self.client is None:
            return {
                'storage': 'in-memory',
                'point_count': len(self.in_memory_store)
            }

        try:
            collection_info = self.client.get_collection(self.collection_name)
            return {
                'collection_name': self.collection_name,
                'vector_size': self.vector_size,
                'point_count': collection_info.points_count,
                'distance_metric': 'cosine'
            }
        except Exception as e:
            print(f"Error getting collection stats: {e}")
            return {}

    def run(self, embedded_chunks: List) -> int:
        """
        Run complete vector store pipeline

        Args:
            embedded_chunks: List of EmbeddedChunk objects from embedding step

        Returns:
            Number of vectors stored
        """
        print("\n" + "="*60)
        print("STEP 4: VECTOR STORE (QDRANT)")
        print("="*60)
        print(f"Collection: {self.collection_name}")
        print(f"Vector size: {self.vector_size}")

        stored = self.store_embeddings(embedded_chunks, batch_size=100)

        stats = self.get_collection_stats()
        print(f"\nVector Store Statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return stored


if __name__ == "__main__":
    # Example usage
    from embeddings import EmbeddingGenerator
    from text_chunking import TextChunker
    from data_ingest import SecDataIngest

    ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
    documents = ingest.run()

    chunker = TextChunker()
    chunks = chunker.run(documents)

    embedder = EmbeddingGenerator()
    embedded_chunks = embedder.run(chunks)

    vector_store = QdrantVectorStore(collection_name="sec-mda")
    stored = vector_store.run(embedded_chunks)
    print(f"\nSuccessfully stored {stored} vectors")
