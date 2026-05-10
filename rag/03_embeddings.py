"""
Embeddings Generation Module

This module handles generating vector embeddings for text chunks.
Step 3: Embedding Generation
"""

from typing import List, Tuple
from dataclasses import dataclass
import numpy as np


@dataclass
class EmbeddedChunk:
    """Text chunk with its embedding vector"""
    chunk_id: str
    source_document_id: str
    text: str
    embedding: List[float]
    embedding_model: str
    metadata: dict


class EmbeddingGenerator:
    """Handles embedding generation for text chunks"""

    def __init__(self, model_name: str = 'sentence-transformers/all-MiniLM-L6-v2'):
        """
        Initialize embedding generator

        Args:
            model_name: Hugging Face model name for embeddings
        """
        self.model_name = model_name
        self.model = None
        self.embedding_dim = None
        self.embeddings = []
        self._initialize_model()

    def _initialize_model(self):
        """Initialize the embedding model"""
        try:
            from sentence_transformers import SentenceTransformer
            print(f"Loading model: {self.model_name}")
            self.model = SentenceTransformer(self.model_name)
            # Get embedding dimension
            dummy_embedding = self.model.encode("test")
            self.embedding_dim = len(dummy_embedding)
            print(f"Model loaded. Embedding dimension: {self.embedding_dim}")
        except ImportError:
            print("Warning: sentence-transformers not installed. Using dummy embeddings.")
            self.model = None
            self.embedding_dim = 384  # Default dimension for all-MiniLM-L6-v2

    def generate_embedding(self, text: str) -> List[float]:
        """
        Generate embedding for a single text

        Args:
            text: Text to embed

        Returns:
            Embedding vector as list of floats
        """
        if self.model is None:
            # Dummy embedding for testing
            np.random.seed(hash(text) % (2**32))
            return np.random.randn(self.embedding_dim).tolist()

        embedding = self.model.encode(text, convert_to_tensor=False)
        return embedding.tolist() if isinstance(embedding, np.ndarray) else embedding

    def embed_chunks(self, chunks: List, batch_size: int = 32) -> List[EmbeddedChunk]:
        """
        Generate embeddings for multiple chunks

        Args:
            chunks: List of TextChunk objects
            batch_size: Number of chunks to process at once

        Returns:
            List of EmbeddedChunk objects
        """
        embedded_chunks = []

        print(f"Generating embeddings for {len(chunks)} chunks (batch_size={batch_size})...")

        for i in range(0, len(chunks), batch_size):
            batch = chunks[i:i + batch_size]
            texts = [chunk.text for chunk in batch]

            # Generate embeddings for batch
            if self.model is not None:
                embeddings = self.model.encode(texts, convert_to_tensor=False)
                embeddings = [e.tolist() for e in embeddings]
            else:
                # Dummy embeddings for testing
                embeddings = [self.generate_embedding(text) for text in texts]

            # Create EmbeddedChunk objects
            for chunk, embedding in zip(batch, embeddings):
                embedded_chunk = EmbeddedChunk(
                    chunk_id=chunk.chunk_id,
                    source_document_id=chunk.source_document_id,
                    text=chunk.text,
                    embedding=embedding,
                    embedding_model=self.model_name,
                    metadata={
                        'chunk_index': chunk.chunk_index,
                        'total_chunks': chunk.total_chunks,
                        'start_char': chunk.start_char,
                        'end_char': chunk.end_char,
                        **chunk.metadata
                    }
                )
                embedded_chunks.append(embedded_chunk)

            # Progress update
            if (i + batch_size) % (batch_size * 5) == 0:
                print(f"  Processed {min(i + batch_size, len(chunks))}/{len(chunks)} chunks")

        self.embeddings = embedded_chunks
        return embedded_chunks

    def verify_embeddings(self) -> bool:
        """
        Verify embeddings are valid

        Returns:
            True if all embeddings are valid
        """
        if not self.embeddings:
            print("No embeddings to verify")
            return False

        print("Verifying embeddings...")

        # Check all embeddings have same dimension
        dimensions = [len(e.embedding) for e in self.embeddings]
        if len(set(dimensions)) != 1:
            print(f"Error: Inconsistent embedding dimensions: {set(dimensions)}")
            return False

        # Check all embeddings are normalized or near-normalized
        norms = [
            sum(x**2 for x in e.embedding) ** 0.5
            for e in self.embeddings
        ]

        avg_norm = sum(norms) / len(norms)
        print(f"Average embedding norm: {avg_norm:.4f}")

        # Check no NaN or Inf values
        invalid = 0
        for e in self.embeddings:
            for val in e.embedding:
                if not (-1e6 < val < 1e6):
                    invalid += 1

        if invalid > 0:
            print(f"Warning: {invalid} invalid embedding values found")
            return False

        print(f"Verification passed ✓")
        print(f"  Total embeddings: {len(self.embeddings)}")
        print(f"  Embedding dimension: {self.embedding_dim}")

        return True

    def get_statistics(self) -> dict:
        """
        Get embedding statistics

        Returns:
            Dictionary with embedding stats
        """
        if not self.embeddings:
            return {}

        # Calculate embedding statistics
        all_embeddings = np.array([e.embedding for e in self.embeddings])

        return {
            'total_embeddings': len(self.embeddings),
            'embedding_dimension': self.embedding_dim,
            'model': self.model_name,
            'avg_norm': float(np.linalg.norm(all_embeddings, axis=1).mean()),
            'min_norm': float(np.linalg.norm(all_embeddings, axis=1).min()),
            'max_norm': float(np.linalg.norm(all_embeddings, axis=1).max()),
            'mean_values': float(all_embeddings.mean()),
            'std_values': float(all_embeddings.std())
        }

    def run(self, chunks: List) -> List[EmbeddedChunk]:
        """
        Run complete embedding pipeline

        Args:
            chunks: List of TextChunk objects from chunking step

        Returns:
            List of EmbeddedChunk objects
        """
        print("\n" + "="*60)
        print("STEP 3: EMBEDDING GENERATION")
        print("="*60)

        embedded_chunks = self.embed_chunks(chunks, batch_size=32)
        self.verify_embeddings()

        stats = self.get_statistics()
        print(f"\nEmbedding Statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return embedded_chunks


if __name__ == "__main__":
    # Example usage
    from text_chunking import TextChunker
    from data_ingest import SecDataIngest

    ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
    documents = ingest.run()

    chunker = TextChunker()
    chunks = chunker.run(documents)

    embedder = EmbeddingGenerator()
    embedded_chunks = embedder.run(chunks)
    print(f"\nSuccessfully embedded {len(embedded_chunks)} chunks")
