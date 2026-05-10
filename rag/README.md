# RAG Pipeline: SEC MDA Documents with Qdrant

A complete Retrieval-Augmented Generation (RAG) pipeline for SEC Management Discussion & Analysis (MDA) documents using Qdrant vector database instead of Milvus.

## Overview

This implementation provides modular, production-ready code for:
- **Ingesting** SEC filing data from parquet files
- **Chunking** large documents into manageable pieces
- **Embedding** text chunks using transformer models
- **Storing** embeddings in Qdrant vector database
- **Querying** documents for RAG applications

## Architecture

```
┌─────────────────────┐
│  Parquet File       │
│ (SEC MDA Data)      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  1. Data Ingest     │
│ (Parse & Validate)  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ 2. Text Chunking    │
│(Split Documents)    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│3. Embeddings        │
│(Vector Generation)  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ 4. Qdrant Store     │
│ (Vector Database)   │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ 5. Query Engine     │
│  (Similarity Search)│
└─────────────────────┘
```

## Key Features

✅ **Modular Design** - Each step is independent and reusable
✅ **Flexible Chunking** - Size-based or section-based strategies
✅ **Efficient Embeddings** - Batch processing with configurable models
✅ **Production-Ready** - Error handling, validation, statistics
✅ **Qdrant Integration** - Modern vector database with full-featured search
✅ **Easy Deployment** - Docker support, minimal dependencies

## File Structure

```
├── 01_data_ingest.py          # Load and parse parquet files
├── 02_text_chunking.py        # Split documents into chunks
├── 03_embeddings.py           # Generate vector embeddings
├── 04_qdrant_store.py         # Qdrant vector store integration
├── 05_rag_pipeline.py         # Orchestrate full pipeline
├── 06_example_usage.py        # Complete usage examples
├── requirements.txt           # Python dependencies
├── docker-compose.yml         # Qdrant deployment
├── .env.example              # Configuration template
├── SETUP_GUIDE.md            # Detailed setup instructions
└── README.md                 # This file
```

## Quick Start

### 1. Install Dependencies

```bash
pip install -r requirements.txt
```

### 2. Start Qdrant

```bash
docker-compose up -d
```

Or using plain Docker:
```bash
docker run -p 6333:6333 qdrant/qdrant:latest
```

### 3. Run Pipeline

```python
from rag_pipeline import RAGPipeline

# Initialize and build
rag = RAGPipeline(
    parquet_path="mda_24_25_merged.parquet",
    collection_name="sec-mda-filings"
)
rag.build()

# Execute query
result = rag.query("What are the main business risks?", top_k=5)

# Print results
for res in result.results:
    print(f"Score: {res.score:.4f} | {res.text[:100]}...")
```

### 4. Run Examples

```bash
python 06_example_usage.py
```

## Module Details

### 01_data_ingest.py

Handles loading SEC filing data from parquet files.

**Key Classes:**
- `SECDocument` - Data structure for documents
- `SecDataIngest` - Main ingest handler

**Usage:**
```python
from data_ingest import SecDataIngest

ingest = SecDataIngest("path/to/data.parquet")
documents = ingest.run()
```

### 02_text_chunking.py

Splits documents into chunks for embedding.

**Key Classes:**
- `TextChunk` - Data structure for chunks
- `TextChunker` - Chunking engine

**Strategies:**
- `chunk_by_size()` - Fixed-size chunks with overlap
- `chunk_by_sections()` - Preserve document structure

**Usage:**
```python
from text_chunking import TextChunker

chunker = TextChunker(chunk_size=512, overlap=50)
chunks = chunker.run(documents, strategy='size')
```

### 03_embeddings.py

Generates vector embeddings for text chunks.

**Key Classes:**
- `EmbeddedChunk` - Chunk with embedding vector
- `EmbeddingGenerator` - Embedding engine

**Models:**
- `all-MiniLM-L6-v2` (384 dims) - Recommended for production
- `all-mpnet-base-v2` (768 dims) - Higher quality
- `all-large-v2` (1024 dims) - Best quality

**Usage:**
```python
from embeddings import EmbeddingGenerator

embedder = EmbeddingGenerator()
embedded = embedder.run(chunks)
```

### 04_qdrant_store.py

Manages storage and retrieval from Qdrant vector database.

**Key Classes:**
- `SearchResult` - Query result
- `QdrantVectorStore` - Vector store handler

**Features:**
- Automatic collection creation
- Batch insertion with error handling
- Similarity search with scoring
- Collection statistics

**Usage:**
```python
from qdrant_store import QdrantVectorStore

store = QdrantVectorStore(collection_name="sec-mda")
store.run(embedded_chunks)

results = store.search(query_embedding, top_k=5)
```

### 05_rag_pipeline.py

Orchestrates the complete RAG pipeline.

**Key Classes:**
- `RAGResult` - Query result with metadata
- `RAGPipeline` - Pipeline orchestration

**Usage:**
```python
from rag_pipeline import RAGPipeline

rag = RAGPipeline(parquet_path="data.parquet")
rag.build()
result = rag.query("Your question here", top_k=5)
```

### 06_example_usage.py

Complete working examples of all usage patterns.

**Examples:**
1. Complete end-to-end pipeline
2. Step-by-step execution
3. Custom queries with result processing
4. Batch query processing
5. Configuration variations

**Run all examples:**
```bash
python 06_example_usage.py
```

## Configuration

Create `.env` file:

```env
# Qdrant
QDRANT_HOST=localhost
QDRANT_PORT=6333
QDRANT_COLLECTION=sec-mda-filings

# Embedding Model
EMBEDDING_MODEL=sentence-transformers/all-MiniLM-L6-v2

# Chunking
CHUNK_SIZE=512
CHUNK_OVERLAP=50

# Data
PARQUET_FILE=mda_24_25_merged.parquet
```

Load configuration:
```python
from dotenv import load_dotenv
import os

load_dotenv()
qdrant_host = os.getenv('QDRANT_HOST')
```

## Data Format

Expected parquet schema:

| Column | Type | Description |
|--------|------|-------------|
| document_id | string | Unique identifier |
| mda_text | string | **Required** - MDA content |
| filing_date | string | Filing date (YYYY-MM-DD) |
| company | string | Company name |
| cik | string | SEC CIK number |
| ... | ... | Additional metadata |

## Performance Guidelines

### Chunk Size Impact

| Size | Chunks | Time | Quality |
|------|--------|------|---------|
| 256 | Many | Slower | Focused |
| 512 | Medium | Balanced | Good |
| 1024 | Few | Faster | Broader |

### Embedding Model Impact

| Model | Dimension | Speed | Quality |
|-------|-----------|-------|---------|
| MiniLM | 384 | Fast | Good |
| MPNet | 768 | Medium | Better |
| Large | 1024 | Slow | Best |

### Optimization Tips

1. **For Large Datasets**: Use smaller batch size, consider distributed processing
2. **For Fast Retrieval**: Use smaller embedding dimension (384)
3. **For Quality**: Use larger embedding dimension (768+)
4. **For Memory**: Reduce chunk size or batch size

## Troubleshooting

### Connection Error

```
ConnectionError: Could not connect to Qdrant at localhost:6333
```

**Solution**: Start Qdrant container:
```bash
docker-compose up -d
# or
docker run -p 6333:6333 qdrant/qdrant:latest
```

### Out of Memory

```
CUDA out of memory
```

**Solution**: Reduce batch size:
```python
embedder.embed_chunks(chunks, batch_size=16)
```

### Parquet Error

```
ParquetError: Column not found
```

**Solution**: Verify parquet schema:
```python
df = pd.read_parquet("file.parquet")
print(df.columns)
print(df.dtypes)
```

## Advanced Usage

### Custom Chunking Strategy

```python
from text_chunking import TextChunker

class CustomChunker(TextChunker):
    def custom_strategy(self, text, doc_id, metadata):
        # Implement your logic
        pass
```

### Custom Embedding Model

```python
from embeddings import EmbeddingGenerator

embedder = EmbeddingGenerator(
    model_name='sentence-transformers/your-model'
)
```

### Batch Processing

```python
queries = [
    "Risk factors",
    "Revenue growth",
    "Market competition"
]
results = rag.batch_query(queries, top_k=5)
```

## Performance Monitoring

### Check Vector Store

```python
from qdrant_store import QdrantVectorStore

store = QdrantVectorStore("sec-mda")
stats = store.get_collection_stats()
print(stats)
```

### Query Statistics

```python
result = rag.query("question")
print(f"Retrieval time: {result.retrieval_time:.3f}s")
print(f"Results found: {result.num_results}")
```

## Integration with LLMs

To use with language models for answer generation:

```python
import anthropic

# Get relevant chunks
rag_result = rag.query("question", top_k=3)

# Create context from retrieved chunks
context = "\n".join([r.text for r in rag_result.results])

# Query LLM with context
client = anthropic.Anthropic()
response = client.messages.create(
    model="claude-3-5-sonnet-20241022",
    max_tokens=1024,
    system=f"Context:\n{context}",
    messages=[{"role": "user", "content": "question"}]
)
print(response.content[0].text)
```

## Migration from Milvus

Key differences from Milvus implementation:

| Aspect | Milvus | Qdrant |
|--------|--------|--------|
| Distance Metric | Multiple | Cosine (recommended) |
| Collection Type | Dynamic/Static | Automatic |
| Schema | Explicit | Implicit from vectors |
| Insert API | `insert()` | `upsert()` |
| Search | Index-required | Always indexed |
| Python Client | `pymilvus` | `qdrant-client` |

**Conversion Notes:**
- Qdrant uses cosine distance by default
- No need for explicit index configuration
- `upsert()` handles both insert and update
- Simpler client API

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

## License

MIT License

## Support

For issues or questions:
- Check SETUP_GUIDE.md for detailed instructions
- Review example_usage.py for usage patterns
- Check module docstrings for API details
- See Troubleshooting section above

## Related Resources

- [Qdrant Documentation](https://qdrant.tech/documentation/)
- [Sentence Transformers](https://www.sbert.net/)
- [SEC EDGAR Database](https://www.sec.gov/edgar)
- [RAG Best Practices](https://docs.anthropic.com/claude/reference/getting-started-with-the-api)

---

**Last Updated**: May 2026
**Version**: 1.0.0
