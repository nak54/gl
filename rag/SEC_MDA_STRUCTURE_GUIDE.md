# SEC MD&A Structure Guide

## Overview

The text chunking module has been upgraded to use the **actual structure of SEC Item 7 (Management Discussion & Analysis)** instead of generic document chunking. This ensures that embeddings preserve the semantic meaning of SEC filings.

## What is SEC Item 7?

**Item 7: Management's Discussion and Analysis of Financial Condition and Results of Operations (MD&A)**

This is a required section in SEC filings (10-K, 10-Q) where company management discusses:
- Financial performance
- Business trends
- Risk factors
- Accounting policies
- Forward-looking statements

## MD&A Standard Subsections

The updated chunker recognizes these common subsections within Item 7:

### 1. **Overview**
- Summary of business and key metrics
- Period-over-period comparison highlights
- Significant events or changes

### 2. **Results of Operations**
- Revenue analysis
- Operating income discussion
- Segment performance
- Year-over-year comparisons

### 3. **Liquidity and Capital Resources**
- Cash flow analysis
- Working capital discussion
- Debt and financing arrangements
- Liquidity sources and uses

### 4. **Critical Accounting Policies and Estimates**
- Policies requiring significant judgment
- Estimates with high uncertainty
- Changes in accounting principles
- Impact on financial statements

### 5. **Risk Factors and Uncertainties**
- Business risks
- Market risks
- Operational risks
- Compliance risks

### 6. **Market Risk and Interest Rate Risk**
- Sensitivity to interest rates
- Foreign exchange exposure
- Commodity price exposure
- Derivative instruments

### 7. **Quantitative and Qualitative Disclosure of Market Risk**
- Tabular presentation of market risks
- Sensitivity analyses
- Value-at-risk disclosures

### 8. **Off-Balance Sheet Arrangements**
- Special purpose entities
- Operating leases
- Unconsolidated entities

### 9. **Contractual Obligations**
- Debt repayment schedules
- Operating leases
- Other commitments
- Payment obligations timeline

### 10. **Related Party Transactions**
- Affiliate dealings
- Conflicts of interest
- Transaction terms

### 11. **Segment Information**
- Operating segment results
- Segment revenue and profit
- Geographic breakdown

### 12. **Impact of Inflation**
- Effect on revenues
- Effect on costs
- Pricing strategies

### 13. **Environmental Matters**
- Compliance costs
- Remediation obligations
- Environmental risks

### 14. **Legal Proceedings**
- Pending litigation
- Regulatory proceedings
- Settlement obligations

### 15. **Recently Issued Accounting Standards**
- New pronouncements
- Adoption dates
- Expected impact

## Implementation Details

### How It Works

```python
from text_chunking import TextChunker

# Create chunker with 15% overlap (default for SEC filings)
chunker = TextChunker(chunk_size=512, overlap_percent=15)

# Use MD&A structure-aware chunking
chunks = chunker.run(documents, strategy='mda')
```

### Key Features

1. **Structure Recognition**: Automatically identifies MD&A subsections
2. **Intelligent Chunking**: Respects section boundaries
3. **15% Overlap**: Default overlap for SEC documents
4. **Section Metadata**: Each chunk knows which MD&A section it came from
5. **Fallback Handling**: Uses fixed-size chunking if structure not found

### Configuration

```python
# Default: 512 tokens, 15% overlap (recommended)
chunker = TextChunker(chunk_size=512, overlap_percent=15)

# For longer context: 1024 tokens, 15% overlap
chunker = TextChunker(chunk_size=1024, overlap_percent=15)

# For shorter, focused chunks: 256 tokens, 15% overlap
chunker = TextChunker(chunk_size=256, overlap_percent=15)
```

### Overlap Calculation

15% overlap is calculated as:
```
overlap_chars = (15 / 100) * (chunk_size * 4)
overlap_chars = (15 / 100) * (512 * 4)
overlap_chars = 307 characters
```

This means each chunk overlaps with the previous chunk by 307 characters.

## Chunking Strategies

### 1. **MD&A Structure-Aware (Default - RECOMMENDED)**

```python
chunks = chunker.run(documents, strategy='mda')
```

**Advantages:**
- Preserves semantic context
- Respects document structure
- Better for RAG applications
- Section-aware embeddings

**When to use:**
- SEC filings with Item 7 structure ✓
- General corporate documents
- Structured regulatory filings

### 2. **Fixed-Size Chunks (Fallback)**

```python
chunks = chunker.run(documents, strategy='size')
```

**Advantages:**
- Consistent chunk sizes
- Simple implementation
- Good for unstructured text

**When to use:**
- Unstructured documents
- Documents without clear sections
- Testing and quick prototyping

### 3. **Legacy Section-Based**

```python
chunks = chunker.run(documents, strategy='section')
```

**Note**: This now uses MD&A structure (same as 'mda')

## Section-Aware Metadata

Each chunk includes the section it came from:

```python
chunk.section_name  # e.g., "Liquidity and Capital Resources"
chunk.metadata      # Includes company, filing_date, cik, etc.
chunk.text          # The actual chunk content
```

Example:
```python
for chunk in chunks:
    if "Risk Factors" in chunk.section_name:
        print(f"Risk disclosure: {chunk.text[:100]}...")
```

## Recommended Settings for SEC Filings

### Development/Testing
```python
chunker = TextChunker(
    chunk_size=256,        # Smaller for quick feedback
    overlap_percent=15     # 15% overlap (standard)
)
```

### Production - Balanced
```python
chunker = TextChunker(
    chunk_size=512,        # Standard (recommended)
    overlap_percent=15     # 15% overlap
)
```

### Production - High Context
```python
chunker = TextChunker(
    chunk_size=1024,       # Larger chunks, more context
    overlap_percent=15     # 15% overlap
)
```

## Example: Complete Pipeline

```python
from data_ingest import SecDataIngest
from text_chunking import TextChunker
from embeddings import EmbeddingGenerator
from qdrant_store import QdrantVectorStore

# 1. Ingest SEC data
ingest = SecDataIngest("mda_24_25_merged.parquet")
documents = ingest.run()

# 2. Chunk using MD&A structure
chunker = TextChunker(chunk_size=512, overlap_percent=15)
chunks = chunker.run(documents, strategy='mda')

# 3. Generate embeddings
embedder = EmbeddingGenerator()
embedded = embedder.run(chunks)

# 4. Store in Qdrant
store = QdrantVectorStore("sec-mda-filings")
store.run(embedded)

# 5. Query with context awareness
results = store.search(query_embedding, top_k=5)

# Results include section information
for result in results:
    print(f"Section: {result.metadata.get('section_name')}")
    print(f"Company: {result.metadata.get('company')}")
    print(f"Text: {result.text[:200]}...")
```

## Handling Different MD&A Variations

Companies may use slightly different subsection names. The chunker uses **fuzzy pattern matching** to handle variations:

### Recognized Variations

**Results of Operations:**
- "Results of Operations"
- "Results of Operation"
- "Financial Performance"

**Liquidity:**
- "Liquidity and Capital Resources"
- "Liquidity, Capital Resources"
- "Capital Resources and Liquidity"

**Critical Accounting:**
- "Critical Accounting Policies and Estimates"
- "Critical Accounting Policies"
- "Accounting Policies and Estimates"

**Risk Factors:**
- "Risk Factors"
- "Risks and Uncertainties"
- "Risk Disclosure"

## Statistics & Metrics

After chunking, review these metrics:

```python
chunker = TextChunker(chunk_size=512, overlap_percent=15)
chunks = chunker.run(documents)

# Get statistics
stats = chunker.get_statistics()

print(f"Total chunks: {stats['total_chunks']}")
print(f"Avg chunk size: {stats['avg_chunk_length']} chars")
print(f"Chunks per document: {stats['avg_chunks_per_document']}")
```

### Key Metrics

- **total_chunks**: Number of chunks created
- **avg_chunk_length**: Average characters per chunk
- **min/max_chunk_length**: Size range
- **unique_documents**: Number of source documents
- **avg_chunks_per_document**: Chunking granularity

## Performance Considerations

### Chunking Speed

- **MD&A structure recognition**: ~5-10ms per document
- **Chunking operation**: ~1-2ms per 1000 characters
- **Overlap calculation**: Negligible

### Storage Impact

```
chunks_stored = documents × avg_chunks_per_doc
vector_storage = chunks_stored × embedding_size

Example:
50 docs × 15 chunks/doc = 750 chunks
750 chunks × 384 dims × 4 bytes = ~1.1 MB vectors
```

## Troubleshooting

### Issue: Chunks too small

**Symptom**: Average chunk size much smaller than chunk_size setting

**Solution**: Increase chunk_size
```python
chunker = TextChunker(chunk_size=1024, overlap_percent=15)
```

### Issue: Chunks missing section context

**Symptom**: Section names not appearing in metadata

**Solution**: Verify MD&A structure in document. Use fallback if needed:
```python
chunks = chunker.run(documents, strategy='size')
```

### Issue: No sections detected

**Symptom**: Fallback to size-based chunking without structure

**Solution**: The document may not have clear MD&A structure. This is OK - the fallback method still works well with 15% overlap.

## Integration with RAG

### Query Filtering by Section

```python
# Retrieve only risk-related chunks
risk_results = [r for r in results if "Risk" in r.metadata.get('section_name', '')]

# Retrieve only financial results
financials = [r for r in results if "Results of Operations" in r.metadata.get('section_name', '')]
```

### LLM Integration

```python
# Group results by section for LLM context
context = ""
current_section = None
for result in results:
    section = result.metadata.get('section_name', 'General')
    if section != current_section:
        context += f"\n## {section}\n"
        current_section = section
    context += result.text + "\n"

# Pass to LLM with section structure preserved
response = llm.generate(
    system=context,
    user_message="question"
)
```

## Migration from Old Chunking

If upgrading from non-structure-aware chunking:

### Old Code
```python
chunker = TextChunker(chunk_size=512, overlap=50)
chunks = chunker.run(documents, strategy='size')
```

### New Code
```python
chunker = TextChunker(chunk_size=512, overlap_percent=15)
chunks = chunker.run(documents, strategy='mda')  # or 'mda' is default
```

### Backward Compatibility

The old `overlap` parameter is replaced with `overlap_percent`:

```python
# Old (still works for size-based):
chunker = TextChunker(chunk_size=512, overlap=50)

# New (recommended):
chunker = TextChunker(chunk_size=512, overlap_percent=15)
```

## Best Practices

1. **Always use 'mda' strategy for SEC filings** - Preserves structure
2. **Use 15% overlap** - Recommended default for SEC documents
3. **Monitor chunk statistics** - Ensure reasonable sizes
4. **Test with examples** - Verify chunking before production
5. **Preserve section metadata** - Use it for filtering/analysis
6. **Consider filing type** - 10-K vs 10-Q may have variations

## Further Reading

- [SEC Item 7 Requirements](https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0000789019&type=10-K&dateb=&owner=exclude&count=100)
- [MD&A Disclosure Guide](https://www.sec.gov/corpfin/guidance-management-discussion-analysis)
- [Plain English Handbook](https://www.sec.gov/pdf/handbook.pdf)

## Support

For questions about SEC MD&A structure:
- Review example chunks to see how they're split
- Check `chunk.section_name` for section information
- Refer to actual SEC 10-K filings for structure reference
- Test with different `chunk_size` values

---

**Version**: 1.0.0  
**Updated**: May 8, 2026  
**Status**: Production Ready
