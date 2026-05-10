# RAG Pipeline - Quick Reference Guide

Fast lookup for common tasks and API usage.

## Installation & Setup

```bash
# Install dependencies
pip install -r requirements.txt

# Start Qdrant
docker-compose up -d

# Run tests
python test_pipeline.py

# Run examples
python 06_example_usage.py
```

## Common Usage Patterns

### Complete Pipeline in 10 Lines

```python
from rag_pipeline import RAGPipeline

rag = RAGPipeline("mda_24_25_merged.parquet")
rag.build()
result = rag.query("What are the risks?", top_k=5)
for r in result.results:
    print(f"Score: {r.score:.4f}")
    print(f"Text: {r.text[:200]}...")
```

### Step-by-Step Pipeline

```python
from data_ingest import SecDataIngest
from text_chunking import TextChunker
from embeddings import EmbeddingGenerator
from qdrant_store import QdrantVectorStore

# Step 1: Ingest
ingest = SecDataIngest("data.parquet")
docs = ingest.run()

# Step 2: Chunk
chunker = TextChunker(chunk_size=512)
chunks = chunker.run(docs)

# Step 3: Embed
embedder = EmbeddingGenerator()
embedded = embedder.run(chunks)

# Step 4: Store
store = QdrantVectorStore("collection-name")
store.run(embedded)
```

## API Reference

### RAGPipeline

```python
from rag_pipeline import RAGPipeline

# Initialize
rag = RAGPipeline(
    parquet_path="data.parquet",
    collection_name="sec-mda",
    chunk_size=512,
    embedding_model="sentence-transformers/all-MiniLM-L6-v2"
)

# Build
stats = rag.build()

# Query
result = rag.query("question", top_k=5)

# Batch queries
results = rag.batch_query(["q1", "q2", "q3"], top_k=5)

# Get info
info = rag.get_pipeline_info()
```

### SecDataIngest

```python
from data_ingest import SecDataIngest

ingest = SecDataIngest("path.parquet")
documents = ingest.run()

# Manual steps
df = ingest.load_parquet()
ingest.validate_data()
docs = ingest.extract_documents()
stats = ingest.get_statistics()
```

### TextChunker

```python
from text_chunking import TextChunker

chunker = TextChunker(chunk_size=512, overlap=50)

# Strategy: size or section
chunks = chunker.run(documents, strategy='size')

# Or manually
chunks = chunker.chunk_by_size(text, doc_id, metadata)
chunks = chunker.chunk_by_sections(text, doc_id, metadata)
```

### EmbeddingGenerator

```python
from embeddings import EmbeddingGenerator

embedder = EmbeddingGenerator(
    model_name='sentence-transformers/all-MiniLM-L6-v2'
)

# Generate embeddings
embedded = embedder.run(chunks)

# Single embedding
emb = embedder.generate_embedding("text")

# Batch embedding
embedded = embedder.embed_chunks(chunks, batch_size=32)
```

### QdrantVectorStore

```python
from qdrant_store import QdrantVectorStore

store = QdrantVectorStore(
    collection_name="sec-mda",
    vector_size=384
)

# Store vectors
stored = store.run(embedded_chunks)
stored = store.store_embeddings(chunks, batch_size=100)

# Search
results = store.search(query_embedding, top_k=5)

# Info
stats = store.get_collection_stats()

# Cleanup
store.delete_collection()
```

## Configuration Examples

### Small Dataset (Fast Testing)

```python
rag = RAGPipeline(
    parquet_path="data.parquet",
    chunk_size=256,
    chunk_overlap=25,
    embedding_model="sentence-transformers/all-MiniLM-L6-v2"
)
```

### Large Dataset (Production)

```python
rag = RAGPipeline(
    parquet_path="data.parquet",
    chunk_size=512,
    chunk_overlap=50,
    embedding_model="sentence-transformers/all-mpnet-base-v2"
)
```

### High Quality (Slow)

```python
rag = RAGPipeline(
    parquet_path="data.parquet",
    chunk_size=1024,
    chunk_overlap=100,
    embedding_model="sentence-transformers/all-large-v2"
)
```

## Performance Tuning

### Embedding Model Speed vs Quality

```
Fast ◄─────────────────────────────────► Quality

MiniLM-L6-v2    MPNet-base-v2    Large-v2
(384 dims)      (768 dims)       (1024 dims)
~2ms/text       ~5ms/text        ~10ms/text
```

### Chunk Size Recommendations

| Use Case | Size | Notes |
|----------|------|-------|
| Dense Q&A | 256 | Focused retrieval |
| General Search | 512 | Balanced (default) |
| Context Rich | 1024 | More context |

### Batch Size Settings

```python
# GPU with plenty of memory
embedder.embed_chunks(chunks, batch_size=128)

# GPU with limited memory
embedder.embed_chunks(chunks, batch_size=32)

# CPU only
embedder.embed_chunks(chunks, batch_size=8)
```

## Query Examples

### Financial Performance Questions

```python
queries = [
    "What is the company's revenue growth rate?",
    "How does profitability trend over time?",
    "What are key financial metrics?",
]
```

### Risk Analysis

```python
queries = [
    "What are the main business risks?",
    "How do market conditions affect operations?",
    "What regulatory challenges exist?",
]
```

### Competitive Analysis

```python
queries = [
    "How does the company compete?",
    "What is the competitive landscape?",
    "Who are the main competitors?",
]
```

## Troubleshooting

### Connection Issues

```bash
# Check Qdrant is running
curl http://localhost:6333/health

# View logs
docker logs qdrant-vector-db

# Restart
docker-compose restart
```

### Memory Issues

```python
# Reduce batch size
embedder.embed_chunks(chunks, batch_size=8)

# Reduce chunk size
chunker = TextChunker(chunk_size=256)

# Process in smaller groups
for batch in batches:
    rag.vector_store.store_embeddings(batch)
```

### Import Errors

```bash
# Check installation
python -c "import qdrant_client; print(qdrant_client.__version__)"

# Reinstall
pip install --upgrade -r requirements.txt
```

## Monitoring & Statistics

### Pipeline Statistics

```python
# After building
stats = rag.build()
print(f"Documents: {stats['documents_ingested']}")
print(f"Chunks: {stats['chunks_created']}")
print(f"Build time: {stats['build_time_seconds']:.2f}s")

# Collection stats
info = rag.vector_store.get_collection_stats()
print(f"Vectors stored: {info['point_count']}")
```

### Query Metrics

```python
result = rag.query("question")
print(f"Results: {result.num_results}")
print(f"Time: {result.retrieval_time:.3f}s")
print(f"Top score: {result.results[0].score:.4f}")
```

## Data Format

### Expected Parquet Structure

```
document_id  filing_date  company    cik         mda_text
───────────  ───────────  ────────   ──────────  ──────────────
AAPL_10K_24  2024-01-15   Apple Inc  0000320193  Apple Inc man...
MSFT_10K_24  2024-01-20   Microsoft  0000789019  Microsoft man...
```

### Required Columns

- `mda_text` ✓ **REQUIRED**

### Optional Columns

- `document_id`
- `filing_date`
- `company`
- `cik`
- Any other metadata

## Common Patterns

### Process Custom Data

```python
import pandas as pd

# Load custom data
df = pd.DataFrame({
    'document_id': ['doc1', 'doc2'],
    'mda_text': ['text1', 'text2'],
    'company': ['Company A', 'Company B']
})

# Save as parquet
df.to_parquet('custom_data.parquet')

# Use in pipeline
rag = RAGPipeline('custom_data.parquet')
rag.build()
```

### Custom Embedding Model

```python
embedder = EmbeddingGenerator(
    model_name='sentence-transformers/your-custom-model'
)
```

### Multiple Collections

```python
# Different collections for different purposes
rag_risks = RAGPipeline(
    parquet_path="data.parquet",
    collection_name="sec-risks"
)

rag_financials = RAGPipeline(
    parquet_path="data.parquet",
    collection_name="sec-financials"
)
```

### Batch Query Processing

```python
questions = [
    "What is revenue?",
    "What are risks?",
    "Who competes?",
    "How is R&D funded?",
    "What regulatory challenges exist?"
]

results = rag.batch_query(questions, top_k=3)

for question, result in zip(questions, results):
    print(f"\nQ: {question}")
    for r in result.results:
        print(f"  • {r.text[:100]}...")
```

## Environment Variables

```env
# See .env.example
QDRANT_HOST=localhost
QDRANT_PORT=6333
QDRANT_COLLECTION=sec-mda
EMBEDDING_MODEL=sentence-transformers/all-MiniLM-L6-v2
CHUNK_SIZE=512
CHUNK_OVERLAP=50
```

## Integration Example

```python
# With Claude/LLM
from rag_pipeline import RAGPipeline
import anthropic

rag = RAGPipeline("data.parquet")
rag.build()

# Get relevant context
context = rag.query("What are the risks?", top_k=5)
context_text = "\n".join([r.text for r in context.results])

# Call LLM with context
client = anthropic.Anthropic()
response = client.messages.create(
    model="claude-3-5-sonnet-20241022",
    max_tokens=1024,
    system=f"Context:\n{context_text}",
    messages=[{
        "role": "user",
        "content": "Based on the context, summarize the risks"
    }]
)
print(response.content[0].text)
```

## Useful Commands

```bash
# Install all dependencies
pip install -r requirements.txt

# Run validation tests
python test_pipeline.py

# Run examples
python 06_example_usage.py

# Start Qdrant
docker-compose up -d

# Stop Qdrant
docker-compose down

# View Qdrant status
curl http://localhost:6333/collections

# Check Python version
python --version

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

## File Reference

| File | Purpose |
|------|---------|
| `01_data_ingest.py` | Load parquet data |
| `02_text_chunking.py` | Split into chunks |
| `03_embeddings.py` | Generate embeddings |
| `04_qdrant_store.py` | Vector database |
| `05_rag_pipeline.py` | Orchestration |
| `06_example_usage.py` | Examples |
| `test_pipeline.py` | Validation |
| `requirements.txt` | Dependencies |
| `docker-compose.yml` | Qdrant setup |
| `README.md` | Full documentation |
| `SETUP_GUIDE.md` | Setup instructions |
| `MIGRATION_GUIDE.md` | Milvus migration |

---

**Last Updated:** May 2026
**Version:** 1.0.0
