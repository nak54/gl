# Table Extraction Migration Checklist

Use this checklist to implement the table extraction feature in your RAG system.

## Phase 1: Code Integration

- [ ] Copy `06_table_extraction.py` to your project directory
- [ ] Replace your `05_rag_pipeline.py` with the updated version
- [ ] Verify file sizes match:
  - `06_table_extraction.py`: ~15.5 KB
  - `05_rag_pipeline.py`: ~11.6 KB

## Phase 2: Dependencies Check

- [ ] Python 3.7+ available
- [ ] Required modules installed:
  ```bash
  pip install pandas sentence-transformers qdrant-client sqlite3
  ```
- [ ] (Optional) For export features:
  ```bash
  pip install openpyxl  # For Excel export
  ```

## Phase 3: Testing (Before Production)

### Unit Tests
- [ ] Test TableExtractor with sample HTML tables
  ```python
  from table_extraction import TableExtractor
  extractor = TableExtractor()
  # Test with your sample documents
  ```

- [ ] Test SQLiteTableStore
  ```python
  from table_extraction import SQLiteTableStore
  db = SQLiteTableStore("test.db")
  # Test store and query operations
  ```

- [ ] Test RAGPipeline integration
  ```python
  from rag_pipeline import RAGPipeline
  rag = RAGPipeline(db_path="test.db")
  # Verify attributes and methods exist
  ```

### Integration Tests
- [ ] Run EXAMPLE_TABLE_QUERIES.py examples
- [ ] Verify pipeline builds without errors
- [ ] Check database file created successfully
- [ ] Confirm tables extracted correctly
- [ ] Verify text chunks don't contain tables

### Performance Tests
- [ ] Time table extraction phase
- [ ] Compare with previous pipeline timing
- [ ] Measure database file size
- [ ] Test query performance

## Phase 4: Data Migration (If Upgrading)

### Backup Existing Data
- [ ] Backup current Qdrant collection
- [ ] Backup any existing databases
- [ ] Export current vector statistics

```bash
# Example backup
cp -r ~/.qdrant/ ~/.qdrant_backup/
```

### Build New Pipeline
- [ ] Initialize new RAGPipeline with `db_path` parameter
- [ ] Run `build()` on your data
- [ ] Verify completion without errors

### Validation
- [ ] Compare document counts (should match)
- [ ] Verify table extraction count
- [ ] Check text chunk count (should be less due to removed tables)
- [ ] Validate sample queries

## Phase 5: Configuration

### Paths and Settings
- [ ] Set `db_path` for SQLite database
- [ ] Update any hardcoded paths
- [ ] Configure environment variables (optional):
  ```bash
  export RAG_DB_PATH=sec_tables.db
  export RAG_DATA_PATH=mda_24_25_merged.parquet
  ```

### Qdrant Settings (if changed)
- [ ] Verify Qdrant host/port configuration
- [ ] Update collection name if needed
- [ ] Confirm connection settings

### Model Settings (if changed)
- [ ] Verify embedding model path
- [ ] Check chunk size settings
- [ ] Confirm overlap percentage

## Phase 6: Integration with Existing Code

### Update Imports
- [ ] Review code importing from `rag_pipeline`
- [ ] Update any hardcoded RAGPipeline initialization
- [ ] Add table query functionality where needed

### Update Queries
```python
# Old way (text only)
results = rag.query("question", top_k=5)

# New way (with table support)
text_results = rag.query("question", top_k=5)
table_results = rag.query_tables(year="2024", limit=50)
```

### Update Scripts
- [ ] Review all pipeline usage in scripts
- [ ] Add db_path parameter to RAGPipeline initialization
- [ ] Update any direct database access code
- [ ] Add table query examples where relevant

## Phase 7: Documentation

- [ ] Update project README with table extraction info
- [ ] Add database management section to docs
- [ ] Document new methods (query_tables)
- [ ] Include schema diagram in docs
- [ ] Add troubleshooting guide for new components

## Phase 8: Production Deployment

### Pre-Deployment
- [ ] Code review completed
- [ ] All tests passed
- [ ] Documentation reviewed
- [ ] Performance validated

### Deployment
- [ ] Deploy updated code to staging environment
- [ ] Run full pipeline on production data
- [ ] Verify all outputs correct
- [ ] Monitor resource usage
- [ ] Backup production data

### Post-Deployment
- [ ] Monitor pipeline performance
- [ ] Check error logs
- [ ] Validate query results
- [ ] Confirm database integrity

## Phase 9: Monitoring & Maintenance

### Regular Checks
- [ ] Database size monitoring
- [ ] Query performance metrics
- [ ] Extraction success rates
- [ ] Error/warning logs

### Maintenance Tasks
- [ ] Periodic database VACUUM (if needed)
- [ ] Backup SQLite database regularly
- [ ] Archive old data if needed
- [ ] Review extracted tables for quality

```python
# Maintenance example
import sqlite3

conn = sqlite3.connect("sec_tables.db")
conn.execute("VACUUM")
conn.close()
```

### Optimization
- [ ] Monitor slow queries
- [ ] Adjust indexes if needed
- [ ] Consider table partitioning for large datasets
- [ ] Profile embedding generation

## Phase 10: Team Communication

- [ ] Brief team on new features
- [ ] Share documentation
- [ ] Provide usage examples
- [ ] Explain database structure
- [ ] Discuss query options

## Rollback Plan (If Needed)

If issues arise:

1. **Data Rollback**
   ```bash
   rm sec_tables.db  # Remove new database
   cp ~/.qdrant_backup/ ~/.qdrant/  # Restore Qdrant
   ```

2. **Code Rollback**
   ```bash
   git checkout HEAD -- 05_rag_pipeline.py  # Revert pipeline
   rm 06_table_extraction.py  # Remove table extraction
   ```

3. **Verify**
   - Test old pipeline still works
   - Confirm data integrity
   - Monitor for issues

## Success Criteria

Mark as complete when:

- [ ] All code integrated
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Team trained
- [ ] Production deployed
- [ ] Monitoring active

## Quick Reference

### Files Modified
- `05_rag_pipeline.py` - Added table extraction integration

### Files Added
- `06_table_extraction.py` - Table extraction implementation
- `TABLE_EXTRACTION_GUIDE.md` - Technical documentation
- `IMPLEMENTATION_SUMMARY.md` - Implementation details
- `EXAMPLE_TABLE_QUERIES.py` - Usage examples
- `TABLE_EXTRACTION_INDEX.md` - Quick start guide
- `MIGRATION_CHECKLIST.md` - This file

### Key Changes
- Table extraction before text chunking
- SQLite database for structured table data
- Updated RAGPipeline with db_path parameter
- New query_tables() method

### Database Schema
```
extracted_tables (SQLite):
- table_id (PRIMARY, UNIQUE)
- source_document_id
- table_title
- table_data (JSON)
- cik (INDEXED) *required
- ein
- sic (INDEXED)
- naics
- year (INDEXED)
- extraction_date
- metadata (JSON)
```

## Common Issues & Solutions

| Issue | Solution |
|-------|----------|
| Database locked | Close other connections, restart pipeline |
| Tables not extracted | Check HTML/format patterns, adjust regex |
| Large DB file | Run VACUUM, consider archiving |
| Slow queries | Add indexes, optimize SQL |
| Import errors | Install dependencies, check paths |

## Support Resources

- **Quick Start**: TABLE_EXTRACTION_INDEX.md
- **Full Guide**: TABLE_EXTRACTION_GUIDE.md
- **Technical Details**: IMPLEMENTATION_SUMMARY.md
- **Code Examples**: EXAMPLE_TABLE_QUERIES.py
- **Source Code**: 06_table_extraction.py, 05_rag_pipeline.py

---

**Status**: [ ] Not Started | [ ] In Progress | [ ] Complete

**Completed Date**: _______________

**Verified By**: _______________

**Notes**:
```
[Add any implementation notes here]
```
