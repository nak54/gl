# SEC MD&A Pipeline (Item 7 RAG)

Re-implementation of the pipeline described in `claude.md`.

```
Documents
   ↓
Extract Tables  →  SQLite (cik, ein, sic, naics3, year,
   ↓                       extracted_table1, extracted_table2, ...)
Cleaned Text (tables removed)
   ↓
Chunk (SEC MD&A structure-aware, 15% overlap)
   ↓
Embed (sentence-transformers/all-MiniLM-L6-v2)
   ↓
Qdrant (local file mode by default — no server needed)
```

## Layout

| File | Stage |
|---|---|
| `data_ingest.py` | Step 1 — load parquet, build `SECDocument`s |
| `table_extraction.py` | Step 2 — detect/remove tables, write SQLite |
| `text_chunking.py` | Step 3 — MD&A-aware chunking, 15% overlap |
| `embeddings.py` | Step 4 — sentence-transformers embeddings |
| `qdrant_store.py` | Step 5 — Qdrant vector store (local or remote) |
| `pipeline.py` | Orchestrator (`RAGPipeline`) |
| `run_pipeline.py` | CLI entry point |

## Quick start (Google Colab Pro)

Open `mda_pipeline_colab.ipynb` in the parent folder. The notebook:

1. Installs the dependencies in `requirements.txt`.
2. Mounts Google Drive (optional).
3. Reads `mda_merged_sic_naics_24_25.parquet` (any path you point it to).
4. Builds the full pipeline.
5. Runs example RAG queries and example SQL queries.

## Quick start (local)

```bash
pip install -r mda_pipeline/requirements.txt

python -m mda_pipeline.run_pipeline \
    --parquet mda_merged_sic_naics_24_25.parquet \
    --qdrant-path ./qdrant_db \
    --sqlite-path sec_tables.db \
    --collection sec_mda \
    --query "What are the principal risk factors?" \
    --top-k 5
```

## Chunking strategy

* `chunk_size_tokens=512` → ~2048 characters per chunk.
* `overlap_percent=15` → ~307 characters of overlap.
* `_find_mda_sections()` recognises **28** standard MD&A subsection
  headers (Overview, Results of Operations, Liquidity, Risk Factors,
  Market Risk, Critical Accounting Policies, Contractual Obligations,
  Off-Balance Sheet Arrangements, Forward-Looking Statements, …).
* `chunk_by_mda_structure()` splits the document along these
  boundaries; each chunk records its section in
  `TextChunk.section_name`, which is preserved through embedding into
  the Qdrant payload.
* If no section structure is found, `chunk_by_size()` runs the same
  overlapping splitter at the document level.

## SQLite schema

```sql
-- one row per extracted table, indexed by cik/year/naics3
CREATE TABLE tables_long (
  id INTEGER PRIMARY KEY,
  table_id TEXT UNIQUE,
  source_document_id TEXT,
  table_index INTEGER,
  table_title TEXT,
  table_data TEXT,       -- JSON [["h1","h2",...], ["v1","v2",...], ...]
  table_html TEXT,
  cik TEXT, ein TEXT, sic TEXT, naics3 TEXT, year TEXT,
  metadata TEXT,
  created_at TIMESTAMP
);

-- one row per filing, with extracted_table1..N columns
CREATE TABLE filings_tables (
  source_document_id TEXT PRIMARY KEY,
  cik TEXT, ein TEXT, sic TEXT, naics3 TEXT, year TEXT,
  n_tables INTEGER,
  extracted_table1 TEXT,
  extracted_table2 TEXT,
  ...
);
```

## Querying

```python
from mda_pipeline import RAGPipeline

pipe = RAGPipeline("mda_merged_sic_naics_24_25.parquet")
pipe.build()

# Vector search (text-only)
pipe.query("What are the principal sources of liquidity?", top_k=5)

# Filtered search by year
pipe.query("supply chain disruptions",
           filter_dict={"year": "2024"})

# SQL search over extracted tables
rows = pipe.query_tables(year="2024", naics3="311", limit=20)
```
