# RAG Pipeline with Qdrant - Setup Guide

## Overview

This RAG (Retrieval-Augmented Generation) pipeline converts SEC filing MDA text into searchable vector embeddings stored in Qdrant vector database. The pipeline consists of 5 modular steps:

1. **Data Ingest** - Load parquet file with SEC filings
2. **Text Chunking** - Split documents into manageable chunks
3. **Embedding Generation** - Create vector embeddings for chunks
4. **Vector Store** - Store embeddings in Qdrant
5. **Query Processing** - Execute RAG queries

## Prerequisites

- Python 3.8+
- Qdrant server (local or remote)
- 8GB+ RAM (for embeddings)

## Installation

### 1. Install Python Dependencies

```bash
pip install -r requirements.txt
```

### 2. Set Up Qdrant

#### Option A: Docker (Recommended)

```bash
docker run -p 6333:6333 qdrant/qdrant:latest
```

This starts Qdrant on `localhost:6333`.

#### Option B: Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'
services:
  qdrant:
    image: qdrant/qdrant:latest
    container_name: qdrant
    ports:
      - "6333:6333"
    volumes:
      - ./qdrant_storage:/qdrant/storage
    environment:
      QDRANT_API_KEY: ""
```

Start with:
```bash
docker-compose up -d
```

#### Option C: Local Installation

Download from: https://github.com/qdrant/qdrant/releases

## Usage

### Quick Start

```python
from rag_pipeline import RAGPipeline

# Initialize pipeline
rag = RAGPipeline(
    parquet_path="path/to/mda_24_25_merged.parquet",
    collection_name="sec-mda-filings",
    qdrant_host="localhost",
    qdrant_port=6333
)

# Build the pipeline
build_stats = rag.build()

# Execute a query
result = rag.query("What are the main risks mentioned?", top_k=5)

# Print results
for i, res in enumerate(result.results, 1):
    print(f"{i}. Score: {res.score:.4f}")
    print(f"   Company: {res.metadata['company']}")
    print(f"   Text: {res.text[:200]}...")
```

### Step-by-Step Usage

#### Step 1: Data Ingest

```python
from data_ingest import SecDataIngest

ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
documents = ingest.run()

print(f"Loaded {len(documents)} documents")
```

#### Step 2: Text Chunking

```python
from text_chunking import TextChunker

chunker = TextChunker(chunk_size=512, overlap=50)
chunks = chunker.run(documents, strategy='size')

print(f"Created {len(chunks)} chunks")
```

#### Step 3: Embedding Generation

```python
from embeddings import EmbeddingGenerator

embedder = EmbeddingGenerator(
    model_name='sentence-transformers/all-MiniLM-L6-v2'
)
embedded_chunks = embedder.run(chunks)

print(f"Generated {len(embedded_chunks)} embeddings")
```

#### Step 4: Vector Store (Qdrant)

```python
from qdrant_store import QdrantVectorStore

vector_store = QdrantVectorStore(
    collection_name="sec-mda",
    vector_size=384,
    qdrant_host="localhost",
    qdrant_port=6333
)
stored = vector_store.run(embedded_chunks)

print(f"Stored {stored} vectors in Qdrant")
```

## Configuration

Create `.env` file:

```env
# Qdrant Configuration
QDRANT_HOST=localhost
QDRANT_PORT=6333
QDRANT_COLLECTION=sec-mda-filings

# Embedding Configuration
EMBEDDING_MODEL=sentence-transformers/all-MiniLM-L6-v2

# Chunking Configuration
CHUNK_SIZE=512
CHUNK_OVERLAP=50

# Data Configuration
PARQUET_FILE=path/to/mda_24_25_merged.parquet
```

Load configuration:

```python
from dotenv import load_dotenv
import os

load_dotenv()

QDRANT_HOST = os.getenv('QDRANT_HOST', 'localhost')
QDRANT_PORT = int(os.getenv('QDRANT_PORT', 6333))
```

## Data Format

Expected parquet file structure:

```
Column Name     Type      Description
-----------     ----      -----------
document_id     string    Unique document identifier
mda_text        string    Management Discussion & Analysis text
filing_date     string    Filing date (YYYY-MM-DD)
company         string    Company name
cik             string    SEC Central Index Key
[other cols]    *         Additional metadata columns
```

### Example:
```
document_id: "AAPL_10K_2024"
mda_text: "Apple Inc. management discussion of financial performance..."
filing_date: "2024-01-15"
company: "Apple Inc."
cik: "0000320193"
```

## Chunking Strategies

### Size-based Chunking (Default)

Splits text into fixed-size chunks with overlap:

```python
chunker = TextChunker(chunk_size=512, overlap=50)
chunks = chunker.chunk_by_size(text, doc_id, metadata)
```

- **Pros**: Consistent chunk sizes, predictable behavior
- **Cons**: May break sentences

### Section-based Chunking

Splits by document sections (Item 1, Item 7, etc.):

```python
chunks = chunker.chunk_by_sections(text, doc_id, metadata)
```

- **Pros**: Preserves document structure
- **Cons**: Uneven chunk sizes

## Performance Tuning

### Chunk Size

- **Smaller chunks (256-384 tokens)**: Better for focused queries, more chunks
- **Larger chunks (512-1024 tokens)**: Better context, fewer chunks

### Embedding Model

- **all-MiniLM-L6-v2** (384 dims): Fast, lightweight, good for production
- **all-mpnet-base-v2** (768 dims): Better quality, slower
- **all-large-v2** (1024 dims): Best quality, slowest

### Batch Size

```python
embedder = EmbeddingGenerator()
embeddings = embedder.embed_chunks(chunks, batch_size=64)
```

- Increase batch size for faster processing (uses more GPU memory)
- Decrease for lower memory usage

## Monitoring

### Check Qdrant Status

```python
from qdrant_client import QdrantClient

client = QdrantClient(host="localhost", port=6333)
collections = client.get_collections()
print(collections)
```

### Collection Statistics

```python
from qdrant_store import QdrantVectorStore

vector_store = QdrantVectorStore("sec-mda")
stats = vector_store.get_collection_stats()
print(stats)
```

## Troubleshooting

### Connection Error to Qdrant

```
Error: Could not connect to Qdrant at localhost:6333
```

**Solution**: Ensure Qdrant is running:
```bash
# Check if container is running
docker ps | grep qdrant

# If not, start it
docker run -p 6333:6333 qdrant/qdrant:latest
```

### Out of Memory During Embedding

**Solution**: Reduce batch size:
```python
embeddings = embedder.embed_chunks(chunks, batch_size=16)
```

### Parquet File Not Found

**Solution**: Use absolute path:
```python
import os
parquet_path = os.path.abspath("path/to/mda_24_25_merged.parquet")
```

## Next Steps

1. **Run Full Pipeline**: Execute `python rag_pipeline.py`
2. **Custom Queries**: Modify query list in `main()` function
3. **Batch Processing**: Use `rag.batch_query()` for multiple queries
4. **Integration**: Integrate with LLM for answer generation

## API Reference

### SecDataIngest

```python
ingest = SecDataIngest(parquet_path)
documents = ingest.run()  # Returns List[SECDocument]
```

### TextChunker

```python
chunker = TextChunker(chunk_size=512, overlap=50)
chunks = chunker.run(documents, strategy='size')  # Returns List[TextChunk]
```

### EmbeddingGenerator

```python
embedder = EmbeddingGenerator(model_name='...')
embedded = embedder.run(chunks)  # Returns List[EmbeddedChunk]
```

### QdrantVectorStore

```python
store = QdrantVectorStore(collection_name="sec-mda")
store.run(embedded_chunks)  # Stores vectors
results = store.search(query_embedding, top_k=5)  # Returns List[SearchResult]
```

### RAGPipeline

```python
rag = RAGPipeline(parquet_path="...")
rag.build()  # Build entire pipeline
result = rag.query("query text", top_k=5)  # Returns RAGResult
```

## Support

For issues or questions:
1. Check troubleshooting section
2. Review module docstrings
3. Enable debug logging
