"""
Pipeline Testing and Validation

This script tests each component of the RAG pipeline to ensure
correct functionality and data flow.
"""

import sys
from pathlib import Path
from typing import List, Tuple


def test_imports():
    """Test that all required modules can be imported"""
    print("\n" + "="*70)
    print("TEST 1: MODULE IMPORTS")
    print("="*70)

    required_modules = [
        ('pandas', 'Data processing'),
        ('numpy', 'Numerical computing'),
        ('qdrant_client', 'Qdrant vector database'),
        ('sentence_transformers', 'Embedding models'),
    ]

    failed = []
    for module_name, description in required_modules:
        try:
            __import__(module_name)
            print(f"✓ {module_name:25} ({description})")
        except ImportError as e:
            print(f"✗ {module_name:25} ({description})")
            print(f"  Error: {e}")
            failed.append(module_name)

    if failed:
        print(f"\nFailed imports: {', '.join(failed)}")
        print("Install with: pip install -r requirements.txt")
        return False

    print("\n✓ All imports successful")
    return True


def test_local_modules():
    """Test that local modules can be imported"""
    print("\n" + "="*70)
    print("TEST 2: LOCAL MODULE IMPORTS")
    print("="*70)

    local_modules = [
        ('data_ingest', 'Data ingestion module'),
        ('text_chunking', 'Text chunking module'),
        ('embeddings', 'Embeddings generation module'),
        ('qdrant_store', 'Qdrant integration module'),
        ('rag_pipeline', 'RAG pipeline orchestration'),
    ]

    failed = []
    for module_name, description in local_modules:
        try:
            __import__(module_name)
            print(f"✓ {module_name:25} ({description})")
        except ImportError as e:
            print(f"✗ {module_name:25} ({description})")
            print(f"  Error: {e}")
            failed.append(module_name)

    if failed:
        print(f"\nFailed imports: {', '.join(failed)}")
        print("Ensure all module files are in the current directory")
        return False

    print("\n✓ All local modules imported successfully")
    return True


def test_parquet_file(parquet_path: str = "mda_24_25_merged.parquet"):
    """Test that parquet file exists and is readable"""
    print("\n" + "="*70)
    print("TEST 3: PARQUET FILE VALIDATION")
    print("="*70)

    parquet_file = Path(parquet_path)

    if not parquet_file.exists():
        print(f"✗ File not found: {parquet_path}")
        print(f"\nPlace the parquet file in current directory:")
        print(f"  Current directory: {Path.cwd()}")
        return False

    print(f"✓ File exists: {parquet_file.name}")
    print(f"  Size: {parquet_file.stat().st_size / (1024**2):.2f} MB")

    try:
        import pandas as pd
        df = pd.read_parquet(parquet_file)
        print(f"✓ File is readable")
        print(f"  Rows: {len(df)}")
        print(f"  Columns: {len(df.columns)}")
        print(f"  Column names: {', '.join(df.columns.tolist())}")

        # Check for required column
        if 'mda_text' not in df.columns:
            print(f"\n✗ Required column 'mda_text' not found")
            return False

        print(f"✓ Required column 'mda_text' found")

        # Check data quality
        null_count = df['mda_text'].isna().sum()
        if null_count > 0:
            print(f"⚠ Warning: {null_count} null values in 'mda_text' column")

        return True
    except Exception as e:
        print(f"✗ Error reading file: {e}")
        return False


def test_data_ingest(parquet_path: str = "mda_24_25_merged.parquet"):
    """Test the data ingest module"""
    print("\n" + "="*70)
    print("TEST 4: DATA INGEST MODULE")
    print("="*70)

    try:
        from data_ingest import SecDataIngest

        ingest = SecDataIngest(parquet_path)
        print("✓ SecDataIngest initialized")

        # Load data
        df = ingest.load_parquet()
        print(f"✓ Loaded {len(df)} documents")

        # Validate
        ingest.validate_data()
        print("✓ Data validation passed")

        # Extract documents
        documents = ingest.extract_documents()
        print(f"✓ Extracted {len(documents)} valid documents")

        if documents:
            doc = documents[0]
            print(f"\nSample document:")
            print(f"  ID: {doc.document_id}")
            print(f"  Company: {doc.company}")
            print(f"  Text length: {len(doc.mda_text)} characters")

        # Statistics
        stats = ingest.get_statistics()
        print(f"\nData statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return True, documents
    except Exception as e:
        print(f"✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return False, None


def test_text_chunking(documents: List):
    """Test the text chunking module"""
    print("\n" + "="*70)
    print("TEST 5: TEXT CHUNKING MODULE")
    print("="*70)

    if not documents:
        print("✗ No documents available for chunking")
        return False, None

    try:
        from text_chunking import TextChunker

        # Test size-based chunking
        chunker = TextChunker(chunk_size=512, overlap=50)
        print("✓ TextChunker initialized")

        chunks = chunker.chunk_documents(documents[:1], strategy='size')  # Test with first doc
        print(f"✓ Created {len(chunks)} chunks from sample documents")

        if chunks:
            chunk = chunks[0]
            print(f"\nSample chunk:")
            print(f"  ID: {chunk.chunk_id}")
            print(f"  Text length: {len(chunk.text)} characters")
            print(f"  Index: {chunk.chunk_index}/{chunk.total_chunks}")

        # Full chunking
        all_chunks = chunker.chunk_documents(documents, strategy='size')
        print(f"\n✓ Total chunks created: {len(all_chunks)}")

        stats = chunker.get_statistics()
        print(f"\nChunking statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return True, all_chunks
    except Exception as e:
        print(f"✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return False, None


def test_embeddings(chunks: List):
    """Test the embeddings module"""
    print("\n" + "="*70)
    print("TEST 6: EMBEDDINGS MODULE")
    print("="*70)

    if not chunks:
        print("✗ No chunks available for embedding")
        return False, None

    try:
        from embeddings import EmbeddingGenerator

        embedder = EmbeddingGenerator(
            model_name='sentence-transformers/all-MiniLM-L6-v2'
        )
        print("✓ EmbeddingGenerator initialized")
        print(f"  Embedding dimension: {embedder.embedding_dim}")

        # Test with sample
        sample_embedding = embedder.generate_embedding("test text")
        print(f"✓ Generated sample embedding: {len(sample_embedding)} dimensions")

        # Embed chunks (small batch for testing)
        test_chunks = chunks[:5] if len(chunks) > 5 else chunks
        embedded = embedder.embed_chunks(test_chunks, batch_size=2)
        print(f"✓ Embedded {len(embedded)} test chunks")

        if embedded:
            emb = embedded[0]
            print(f"\nSample embedding:")
            print(f"  Dimension: {len(emb.embedding)}")
            print(f"  First 5 values: {emb.embedding[:5]}")

        # Verify embeddings
        embedder.embeddings = embedded
        embedder.verify_embeddings()

        return True, embedded
    except Exception as e:
        print(f"✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return False, None


def test_qdrant_connection():
    """Test connection to Qdrant"""
    print("\n" + "="*70)
    print("TEST 7: QDRANT CONNECTION")
    print("="*70)

    try:
        from qdrant_client import QdrantClient

        client = QdrantClient(host="localhost", port=6333)
        print("✓ Connected to Qdrant at localhost:6333")

        # Get server info
        health = client.get_models()
        print("✓ Qdrant server is responsive")

        # List collections
        collections = client.get_collections()
        print(f"✓ Found {len(collections.collections)} existing collections")

        for collection in collections.collections[:3]:  # Show first 3
            print(f"  - {collection.name}: {collection.points_count} points")

        return True
    except ConnectionError as e:
        print(f"✗ Connection error: {e}")
        print("\nEnsure Qdrant is running:")
        print("  docker-compose up -d")
        print("  or")
        print("  docker run -p 6333:6333 qdrant/qdrant:latest")
        return False
    except Exception as e:
        print(f"✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_vector_store(embedded_chunks: List):
    """Test vector store operations"""
    print("\n" + "="*70)
    print("TEST 8: VECTOR STORE MODULE")
    print("="*70)

    if not embedded_chunks:
        print("✗ No embedded chunks available")
        return False

    try:
        from qdrant_store import QdrantVectorStore

        store = QdrantVectorStore(
            collection_name="test-rag-pipeline",
            vector_size=len(embedded_chunks[0].embedding)
        )
        print("✓ QdrantVectorStore initialized")

        # Store embeddings
        stored = store.store_embeddings(embedded_chunks[:3], batch_size=10)  # Test with small batch
        print(f"✓ Stored {stored} vectors")

        # Get stats
        stats = store.get_collection_stats()
        print(f"\nVector store stats:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        # Test search
        if embedded_chunks:
            query_embedding = embedded_chunks[0].embedding
            results = store.search(query_embedding, top_k=2)
            print(f"\n✓ Search returned {len(results)} results")

            if results:
                for i, res in enumerate(results, 1):
                    print(f"  {i}. Score: {res.score:.4f}")

        return True
    except Exception as e:
        print(f"✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return False


def run_all_tests(parquet_path: str = "mda_24_25_merged.parquet"):
    """Run all pipeline tests"""
    print("\n" + "="*70)
    print("RAG PIPELINE VALIDATION TESTS")
    print("="*70)

    results = {
        'imports': False,
        'local_modules': False,
        'parquet': False,
        'ingest': False,
        'chunking': False,
        'embeddings': False,
        'qdrant': False,
        'vector_store': False
    }

    # Test 1: Imports
    results['imports'] = test_imports()

    if not results['imports']:
        print("\n✗ Cannot proceed without required packages")
        return results

    # Test 2: Local modules
    results['local_modules'] = test_local_modules()

    if not results['local_modules']:
        print("\n✗ Cannot proceed without local modules")
        return results

    # Test 3: Parquet file
    results['parquet'] = test_parquet_file(parquet_path)

    if not results['parquet']:
        print("\n✗ Cannot proceed without parquet file")
        return results

    # Test 4: Data ingest
    success, documents = test_data_ingest(parquet_path)
    results['ingest'] = success

    if not success:
        return results

    # Test 5: Text chunking
    success, chunks = test_text_chunking(documents)
    results['chunking'] = success

    if not success:
        return results

    # Test 6: Embeddings
    success, embedded_chunks = test_embeddings(chunks)
    results['embeddings'] = success

    if not success:
        return results

    # Test 7: Qdrant connection
    results['qdrant'] = test_qdrant_connection()

    if not results['qdrant']:
        return results

    # Test 8: Vector store
    results['vector_store'] = test_vector_store(embedded_chunks)

    # Print summary
    print("\n" + "="*70)
    print("TEST SUMMARY")
    print("="*70)

    for test_name, passed in results.items():
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"{test_name:20} {status}")

    all_passed = all(results.values())
    print("\n" + "="*70)
    if all_passed:
        print("✓ ALL TESTS PASSED - Pipeline is ready!")
    else:
        failed = [name for name, passed in results.items() if not passed]
        print(f"✗ TESTS FAILED: {', '.join(failed)}")
    print("="*70)

    return results


if __name__ == "__main__":
    parquet_file = "mda_24_25_merged.parquet"
    results = run_all_tests(parquet_file)
