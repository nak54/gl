"""
Complete Example: RAG Pipeline with SEC MDA Data

This script demonstrates all steps of the RAG pipeline:
1. Load SEC data from parquet
2. Chunk the text
3. Generate embeddings
4. Store in Qdrant
5. Execute queries
"""

import sys
import os
from pathlib import Path

# Import pipeline components
from data_ingest import SecDataIngest
from text_chunking import TextChunker
from embeddings import EmbeddingGenerator
from qdrant_store import QdrantVectorStore
from rag_pipeline import RAGPipeline


def example_1_basic_pipeline():
    """
    Example 1: Run the complete pipeline end-to-end
    """
    print("\n" + "="*70)
    print("EXAMPLE 1: COMPLETE RAG PIPELINE")
    print("="*70)

    # Configuration
    parquet_file = "mda_24_25_merged.parquet"
    collection_name = "sec-mda-basic"

    # Check if parquet file exists
    if not Path(parquet_file).exists():
        print(f"Error: {parquet_file} not found")
        print("Please ensure the parquet file is in the current directory")
        return

    # Initialize RAG pipeline
    rag = RAGPipeline(
        parquet_path=parquet_file,
        collection_name=collection_name,
        chunk_size=512,
        chunk_overlap=50,
        qdrant_host="localhost",
        qdrant_port=6333
    )

    # Build pipeline
    print("\nBuilding RAG pipeline...")
    stats = rag.build()

    # Print build statistics
    print("\nBuild Statistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")

    # Execute sample queries
    sample_queries = [
        "What are the main business risks?",
        "How does the company generate revenue?",
        "What is the competitive landscape?",
    ]

    print("\n" + "="*70)
    print("SAMPLE QUERIES")
    print("="*70)

    for query in sample_queries:
        print(f"\nQuery: {query}")
        result = rag.query(query, top_k=3)
        print(f"Retrieved {result.num_results} results in {result.retrieval_time:.3f}s")


def example_2_step_by_step():
    """
    Example 2: Walk through each step individually
    """
    print("\n" + "="*70)
    print("EXAMPLE 2: STEP-BY-STEP EXECUTION")
    print("="*70)

    parquet_file = "mda_24_25_merged.parquet"

    if not Path(parquet_file).exists():
        print(f"Error: {parquet_file} not found")
        return

    # ===== STEP 1: DATA INGEST =====
    print("\n" + "-"*70)
    print("STEP 1: DATA INGEST")
    print("-"*70)

    ingest = SecDataIngest(parquet_file)
    documents = ingest.run()

    # Print sample document
    if documents:
        doc = documents[0]
        print(f"\nSample Document:")
        print(f"  ID: {doc.document_id}")
        print(f"  Company: {doc.company}")
        print(f"  Filing Date: {doc.filing_date}")
        print(f"  Text Length: {len(doc.mda_text)} characters")
        print(f"  First 200 chars: {doc.mda_text[:200]}...")

    # ===== STEP 2: TEXT CHUNKING =====
    print("\n" + "-"*70)
    print("STEP 2: TEXT CHUNKING")
    print("-"*70)

    chunker = TextChunker(chunk_size=512, overlap=50)
    chunks = chunker.run(documents, strategy='size')

    # Print sample chunks
    if chunks:
        print(f"\nTotal Chunks: {len(chunks)}")
        chunk = chunks[0]
        print(f"\nSample Chunk:")
        print(f"  ID: {chunk.chunk_id}")
        print(f"  Index: {chunk.chunk_index}/{chunk.total_chunks}")
        print(f"  Length: {len(chunk.text)} characters")
        print(f"  Text: {chunk.text[:200]}...")

    # ===== STEP 3: EMBEDDING GENERATION =====
    print("\n" + "-"*70)
    print("STEP 3: EMBEDDING GENERATION")
    print("-"*70)

    embedder = EmbeddingGenerator(
        model_name='sentence-transformers/all-MiniLM-L6-v2'
    )
    embedded_chunks = embedder.run(chunks)

    # Print sample embedding
    if embedded_chunks:
        emb = embedded_chunks[0]
        print(f"\nTotal Embeddings: {len(embedded_chunks)}")
        print(f"\nSample Embedding:")
        print(f"  Chunk ID: {emb.chunk_id}")
        print(f"  Dimension: {len(emb.embedding)}")
        print(f"  First 5 values: {emb.embedding[:5]}")

    # ===== STEP 4: VECTOR STORE =====
    print("\n" + "-"*70)
    print("STEP 4: VECTOR STORE (QDRANT)")
    print("-"*70)

    vector_store = QdrantVectorStore(
        collection_name="sec-mda-stepwise",
        vector_size=embedder.embedding_dim
    )
    stored = vector_store.run(embedded_chunks)
    print(f"\nSuccessfully stored {stored} vectors in Qdrant")


def example_3_custom_queries():
    """
    Example 3: Execute custom queries with result processing
    """
    print("\n" + "="*70)
    print("EXAMPLE 3: CUSTOM QUERIES WITH RESULT PROCESSING")
    print("="*70)

    parquet_file = "mda_24_25_merged.parquet"

    if not Path(parquet_file).exists():
        print(f"Error: {parquet_file} not found")
        return

    # Initialize pipeline
    rag = RAGPipeline(parquet_path=parquet_file)

    # Build (use existing collection if available)
    try:
        print("Building pipeline...")
        rag.build()
    except Exception as e:
        print(f"Note: Build encountered issue (this may be normal): {e}")

    # Define custom queries
    queries = {
        "Financial Performance": "What is the company's financial performance and key metrics?",
        "Risk Factors": "What are the main risk factors and challenges?",
        "Market Competition": "How does the company compete in the market?",
        "Growth Strategy": "What is the company's growth strategy?",
        "Regulatory Environment": "How do regulations affect the business?",
    }

    print("\nExecuting Custom Queries:")
    print("-" * 70)

    for category, query in queries.items():
        print(f"\n[{category}]")
        print(f"Query: {query}")

        result = rag.query(query, top_k=2)

        print(f"Results: {result.num_results} found ({result.retrieval_time:.3f}s)")
        for i, res in enumerate(result.results, 1):
            print(f"\n  {i}. Relevance: {res.score:.4f}")
            print(f"     Company: {res.metadata.get('company', 'N/A')}")
            print(f"     Text: {res.text[:150]}...")


def example_4_batch_processing():
    """
    Example 4: Process multiple queries in batch
    """
    print("\n" + "="*70)
    print("EXAMPLE 4: BATCH QUERY PROCESSING")
    print("="*70)

    parquet_file = "mda_24_25_merged.parquet"

    if not Path(parquet_file).exists():
        print(f"Error: {parquet_file} not found")
        return

    # Initialize pipeline
    rag = RAGPipeline(parquet_path=parquet_file)

    try:
        print("Building pipeline...")
        rag.build()
    except Exception as e:
        print(f"Note: {e}")

    # Batch queries
    queries = [
        "What are revenue trends?",
        "How does management view market conditions?",
        "What operational challenges exist?",
        "How is the company investing in R&D?",
        "What is the impact of macroeconomic factors?",
    ]

    print(f"\nProcessing {len(queries)} queries in batch...")
    results = rag.batch_query(queries, top_k=2)

    # Summarize batch results
    print("\n" + "="*70)
    print("BATCH RESULTS SUMMARY")
    print("="*70)

    total_results = sum(r.num_results for r in results)
    avg_time = sum(r.retrieval_time for r in results) / len(results)

    print(f"Total Results: {total_results}")
    print(f"Average Retrieval Time: {avg_time:.3f}s")

    for i, (query, result) in enumerate(zip(queries, results), 1):
        print(f"\n{i}. {query}")
        print(f"   Results: {result.num_results} | Time: {result.retrieval_time:.3f}s")
        if result.results:
            print(f"   Top Score: {result.results[0].score:.4f}")


def example_5_configuration_variations():
    """
    Example 5: Test different configurations
    """
    print("\n" + "="*70)
    print("EXAMPLE 5: CONFIGURATION VARIATIONS")
    print("="*70)

    parquet_file = "mda_24_25_merged.parquet"

    if not Path(parquet_file).exists():
        print(f"Error: {parquet_file} not found")
        return

    configs = [
        {
            "name": "Small Chunks",
            "chunk_size": 256,
            "overlap": 25,
            "collection": "sec-mda-small"
        },
        {
            "name": "Medium Chunks",
            "chunk_size": 512,
            "overlap": 50,
            "collection": "sec-mda-medium"
        },
        {
            "name": "Large Chunks",
            "chunk_size": 1024,
            "overlap": 100,
            "collection": "sec-mda-large"
        }
    ]

    print("\nTesting different chunking strategies...\n")

    for config in configs:
        print(f"\n{config['name']}:")
        print(f"  Chunk size: {config['chunk_size']} tokens")
        print(f"  Overlap: {config['overlap']} tokens")

        rag = RAGPipeline(
            parquet_path=parquet_file,
            collection_name=config['collection'],
            chunk_size=config['chunk_size'],
            chunk_overlap=config['overlap']
        )

        try:
            stats = rag.build()
            print(f"  Total chunks: {stats['chunks_created']}")
            print(f"  Build time: {stats['build_time_seconds']:.2f}s")
        except Exception as e:
            print(f"  Error: {e}")


def main():
    """
    Run examples
    """
    print("\n" + "="*70)
    print("RAG PIPELINE EXAMPLES")
    print("="*70)
    print("\nAvailable examples:")
    print("  1. Complete end-to-end pipeline")
    print("  2. Step-by-step execution")
    print("  3. Custom queries with result processing")
    print("  4. Batch query processing")
    print("  5. Configuration variations")
    print("\nRunning Example 1: Complete Pipeline")

    try:
        example_1_basic_pipeline()
        print("\n✓ Example 1 completed successfully")
    except Exception as e:
        print(f"\n✗ Example 1 failed: {e}")
        print("\nNote: Ensure Qdrant is running: docker run -p 6333:6333 qdrant/qdrant:latest")

    # Uncomment to run other examples
    # example_2_step_by_step()
    # example_3_custom_queries()
    # example_4_batch_processing()
    # example_5_configuration_variations()

    print("\n" + "="*70)
    print("Examples completed")
    print("="*70)


if __name__ == "__main__":
    main()
