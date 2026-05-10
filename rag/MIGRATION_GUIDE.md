# Migration Guide: Milvus to Qdrant

This guide explains how to migrate an existing RAG implementation from Milvus to Qdrant.

## Overview

Qdrant is a modern vector database that simplifies the development and deployment of RAG systems compared to Milvus. This guide covers the key differences and migration steps.

## Key Differences

### 1. Database Architecture

**Milvus:**
- Requires separate components (proxy, meta service, message queue, object storage)
- Complex deployment with multiple services
- Dynamic vs static collections

**Qdrant:**
- Single, unified service
- Significantly simpler deployment
- Automatic collection management

### 2. Vector Configuration

**Milvus:**
```python
from pymilvus import FieldSchema, CollectionSchema, DataType

fields = [
    FieldSchema(name="id", dtype=DataType.INT64, is_primary=True),
    FieldSchema(name="text", dtype=DataType.VARCHAR, max_length=65535),
    FieldSchema(name="embedding", dtype=DataType.FLOAT_VECTOR, dim=384),
]
schema = CollectionSchema(fields=fields, description="SEC MDA")
collection = Collection(name="sec-mda", schema=schema)
```

**Qdrant:**
```python
from qdrant_client.models import VectorParams, Distance

vector_store.client.create_collection(
    collection_name="sec-mda",
    vectors_config=VectorParams(size=384, distance=Distance.COSINE)
)
```

### 3. Data Insertion

**Milvus:**
```python
collection.insert([ids, texts, embeddings])
```

**Qdrant:**
```python
from qdrant_client.models import PointStruct

points = [
    PointStruct(id=i, vector=emb, payload={"text": text})
    for i, (text, emb) in enumerate(zip(texts, embeddings))
]
client.upsert(collection_name="sec-mda", points=points)
```

### 4. Search Operations

**Milvus:**
```python
results = collection.search(
    data=[query_embedding],
    anns_field="embedding",
    param={"metric_type": "L2", "params": {"nprobe": 10}},
    limit=5
)
```

**Qdrant:**
```python
results = client.search(
    collection_name="sec-mda",
    query_vector=query_embedding,
    limit=5
)
```

### 5. Distance Metrics

**Milvus Options:**
- L2 (Euclidean)
- IP (Inner Product)
- COSINE

**Qdrant Options:**
- Euclidean
- Cosine (recommended)
- Dot Product
- Manhattan

## Step-by-Step Migration

### Step 1: Update Dependencies

**Before (Milvus):**
```bash
pip install pymilvus==2.3.0
```

**After (Qdrant):**
```bash
pip install qdrant-client>=2.7.0
```

### Step 2: Update Vector Store Class

**Before:**
```python
from pymilvus import Collection, connections

class MilvusVectorStore:
    def __init__(self, collection_name="sec-mda"):
        connections.connect(
            alias="default",
            host="localhost",
            port=19530
        )
        self.collection = Collection(collection_name)
```

**After:**
```python
from qdrant_client import QdrantClient

class QdrantVectorStore:
    def __init__(self, collection_name="sec-mda"):
        self.client = QdrantClient(
            host="localhost",
            port=6333
        )
        self.collection_name = collection_name
```

### Step 3: Update Insertion Logic

**Before:**
```python
def insert_embeddings(self, embeddings, texts, ids):
    self.collection.insert([
        ids,
        texts,
        embeddings
    ])
```

**After:**
```python
from qdrant_client.models import PointStruct

def insert_embeddings(self, embeddings, texts, ids):
    points = [
        PointStruct(
            id=id,
            vector=emb,
            payload={"text": text}
        )
        for id, text, emb in zip(ids, texts, embeddings)
    ]
    self.client.upsert(
        collection_name=self.collection_name,
        points=points
    )
```

### Step 4: Update Search Logic

**Before:**
```python
def search(self, query_embedding, top_k=5):
    results = self.collection.search(
        data=[query_embedding],
        anns_field="embedding",
        param={"metric_type": "COSINE", "params": {"nprobe": 10}},
        limit=top_k,
        output_fields=["text"]
    )
    return results
```

**After:**
```python
def search(self, query_embedding, top_k=5):
    results = self.client.search(
        collection_name=self.collection_name,
        query_vector=query_embedding,
        limit=top_k
    )
    return results
```

### Step 5: Update Collection Management

**Before:**
```python
def create_collection(self):
    fields = [
        FieldSchema(name="id", dtype=DataType.INT64, is_primary=True),
        FieldSchema(name="text", dtype=DataType.VARCHAR, max_length=65535),
        FieldSchema(name="embedding", dtype=DataType.FLOAT_VECTOR, dim=384),
    ]
    schema = CollectionSchema(fields=fields)
    self.collection = Collection("sec-mda", schema=schema)
    self.collection.create_index("embedding", {"metric_type": "COSINE"})

def delete_collection(self):
    self.collection.drop()
```

**After:**
```python
from qdrant_client.models import VectorParams, Distance

def create_collection(self):
    self.client.create_collection(
        collection_name=self.collection_name,
        vectors_config=VectorParams(
            size=384,
            distance=Distance.COSINE
        )
    )
    # Qdrant automatically indexes

def delete_collection(self):
    self.client.delete_collection(self.collection_name)
```

## Deployment Changes

### Milvus Deployment (Before)

Docker Compose with multiple services:
```yaml
version: '3'
services:
  etcd:
    image: quay.io/coreos/etcd:v3.5.0
  minio:
    image: minio/minio:latest
  pulsar:
    image: apachepulsar/pulsar:latest
  milvus:
    image: milvusdb/milvus:latest
    depends_on:
      - etcd
      - minio
      - pulsar
```

### Qdrant Deployment (After)

Simple single-service deployment:
```yaml
version: '3.8'
services:
  qdrant:
    image: qdrant/qdrant:latest
    ports:
      - "6333:6333"
    volumes:
      - ./qdrant_storage:/qdrant/storage
```

## Performance Considerations

### Indexing

**Milvus:**
- Explicit index creation required
- Multiple index types available
- Manual tuning needed

**Qdrant:**
- Automatic indexing
- Single, optimized index type (HNSW)
- Simpler performance profile

### Query Optimization

**Milvus:**
- Manual parameter tuning (nprobe, nlist)
- Complex configuration

**Qdrant:**
- No manual tuning required
- Built-in optimization
- Consistent performance

## Data Migration Strategy

### Option 1: Full Re-Index (Recommended)

```python
# 1. Export from Milvus
milvus_embeddings = extract_from_milvus()

# 2. Re-embed with Qdrant
qdrant_store = QdrantVectorStore()
qdrant_store.insert_embeddings(
    embeddings=milvus_embeddings['vectors'],
    texts=milvus_embeddings['texts'],
    ids=milvus_embeddings['ids']
)
```

### Option 2: Direct Vector Transfer

```python
# 1. Export vectors from Milvus
vectors = collection.query(expr="id > 0", output_fields=["*"])

# 2. Import to Qdrant
points = [
    PointStruct(
        id=v['id'],
        vector=v['embedding'],
        payload=v['payload']
    )
    for v in vectors
]
qdrant_client.upsert(collection_name="sec-mda", points=points)
```

## Testing Strategy

### Create Compatibility Test

```python
def compare_results(query_embedding, top_k=5):
    # Get results from both systems
    milvus_results = milvus_store.search(query_embedding, top_k)
    qdrant_results = qdrant_store.search(query_embedding, top_k)
    
    # Compare
    assert len(milvus_results) == len(qdrant_results)
    
    for milvus_res, qdrant_res in zip(milvus_results, qdrant_results):
        assert milvus_res.id == qdrant_res.id
        assert abs(milvus_res.score - qdrant_res.score) < 0.01
```

## Benefits of Migration

| Aspect | Milvus | Qdrant |
|--------|--------|--------|
| **Deployment** | Complex (5+ services) | Simple (1 service) |
| **Configuration** | Extensive tuning | Minimal configuration |
| **Learning Curve** | Steep | Gentle |
| **Documentation** | Good | Excellent |
| **Production Ready** | Yes, but complex | Yes, simpler |
| **API Simplicity** | Moderate | High |
| **Memory Footprint** | Large | Smaller |

## Common Issues and Solutions

### Issue: Connection Timeout

**Cause:** Milvus/Qdrant server not running

**Milvus Solution:**
```bash
docker-compose -f milvus-deployment.yml up -d
```

**Qdrant Solution:**
```bash
docker-compose up -d
```

### Issue: Vector Dimension Mismatch

**Milvus:**
```python
# Error from creating collection with wrong dimension
FieldSchema(name="embedding", dtype=DataType.FLOAT_VECTOR, dim=384)
```

**Qdrant:**
```python
# Solution: Check vector size matches
vectors_config=VectorParams(size=384, distance=Distance.COSINE)
```

### Issue: Distance Metric Differences

**Milvus:** Uses L2 (Euclidean) by default
**Qdrant:** Uses Cosine distance (recommended)

**Migration Note:** Normalize embeddings when switching from L2 to Cosine:
```python
import numpy as np

normalized = embedding / np.linalg.norm(embedding)
```

## Rollback Plan

### Keep Milvus Running During Migration

```python
class DualVectorStore:
    def __init__(self):
        self.milvus = MilvusVectorStore()
        self.qdrant = QdrantVectorStore()
    
    def insert(self, embeddings, texts, ids):
        self.milvus.insert(embeddings, texts, ids)
        self.qdrant.insert(embeddings, texts, ids)
    
    def search(self, query_embedding, use_qdrant=True):
        if use_qdrant:
            return self.qdrant.search(query_embedding)
        return self.milvus.search(query_embedding)
```

## Timeline

### Week 1: Preparation
- [ ] Set up Qdrant locally
- [ ] Update dependencies
- [ ] Review code differences

### Week 2: Implementation
- [ ] Update vector store class
- [ ] Update insertion logic
- [ ] Update search logic

### Week 3: Testing
- [ ] Unit tests
- [ ] Integration tests
- [ ] Performance benchmarks

### Week 4: Migration
- [ ] Parallel running period
- [ ] Gradual traffic switch
- [ ] Milvus shutdown

## Resources

- [Qdrant Documentation](https://qdrant.tech/documentation/)
- [Qdrant Python Client](https://github.com/qdrant/qdrant-client)
- [RAG Patterns](https://www.anthropic.com/news/retrieval-augmented-generation)

## Support

For migration assistance:
1. Review SETUP_GUIDE.md for Qdrant configuration
2. Check module docstrings in qdrant_store.py
3. Run test_pipeline.py to validate setup
4. Review example_usage.py for patterns

---

**Migration Status:** [Update as you progress]
- [ ] Dependencies updated
- [ ] Code refactored
- [ ] Testing completed
- [ ] Production migration
- [ ] Milvus decommissioned
